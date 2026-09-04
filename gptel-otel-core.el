;;; gptel-otel-core.el --- OTLP trace construction  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Author: Andrew Giessel
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, processes

;;; Commentary:
;; Typed OTLP span construction and trace collection.  Transport and durable
;; queuing live in `gptel-otel-transport'.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup gptel-otel nil "OpenTelemetry tracing for gptel." :group 'tools)

(defcustom gptel-otel-service-name "gptel"
  "Value of the `service.name' resource attribute."
  :type 'string :group 'gptel-otel)

(defcustom gptel-otel-resource-attributes nil
  "Additional typed resource attributes as (KEY . ANYVALUE)."
  :type '(alist :key-type string :value-type sexp) :group 'gptel-otel)

(defcustom gptel-otel-random-bytes-function #'gptel-otel-emacs-random-bytes
  "Function returning the requested number of random bytes."
  :type 'function :group 'gptel-otel)

(cl-defstruct (gptel-otel-span (:constructor gptel-otel--make-span))
  trace-id span-id parent-span-id name start-time-unix-nano end-time-unix-nano
  attributes status ended-p exported-p)

(cl-defstruct (gptel-otel-trace (:constructor gptel-otel--make-trace))
  id root spans terminal-p queued-p (outstanding 0))

(defun gptel-otel-emacs-random-bytes (count)
  "Return COUNT random bytes using Emacs' entropy-seeded PRNG."
  (random t)
  (apply #'unibyte-string (cl-loop repeat count collect (random 256))))

(defun gptel-otel--random-bytes (count)
  (let ((bytes (funcall gptel-otel-random-bytes-function count)))
    (unless (and (stringp bytes) (= (string-bytes bytes) count))
      (error "Random byte provider returned an invalid value"))
    bytes))

(defun gptel-otel--hex-id (bytes)
  (let (id)
    (while (or (null id) (string-match-p "\\`0+\\'" id))
      (setq id (mapconcat (lambda (byte) (format "%02x" byte))
                          (string-to-list (gptel-otel--random-bytes bytes)) "")))
    id))

(defun gptel-otel-generate-trace-id () (gptel-otel--hex-id 16))
(defun gptel-otel-generate-span-id () (gptel-otel--hex-id 8))

(defun gptel-otel--unix-nano ()
  "Return the current Unix timestamp as exact decimal nanoseconds.
Construct only second- and subsecond-sized integers, which keeps this safe on
Emacs 27 builds whose fixnums cannot represent a nanosecond epoch integer."
  (let* ((time (current-time))
         (seconds (+ (* (car time) 65536) (cadr time)))
         (microseconds (or (car (cddr time)) 0))
         (picoseconds (or (cadr (cddr time)) 0))
         (nanoseconds (+ (* microseconds 1000) (/ picoseconds 1000))))
    (format "%d%09d" seconds nanoseconds)))

(defun gptel-otel-value-string (value) `((stringValue . ,(format "%s" value))))
(defun gptel-otel-value-bool (value) `((boolValue . ,(if value t :json-false))))
(defun gptel-otel-value-int (value) `((intValue . ,(number-to-string value))))
(defun gptel-otel-value-double (value) `((doubleValue . ,value)))
(defun gptel-otel-value-bytes (value) `((bytesValue . ,(base64-encode-string value t))))
(defun gptel-otel-value-array (&rest values)
  `((arrayValue . ((values . ,(vconcat values))))))
(defun gptel-otel-value-kvlist (attributes)
  `((kvlistValue . ((values . ,(gptel-otel--attributes attributes))))))

(defun gptel-otel--attributes (attributes)
  (vconcat (mapcar (lambda (entry) `((key . ,(car entry)) (value . ,(cdr entry))))
                   attributes)))

(defun gptel-otel-start-span (name &optional parent attributes trace-id)
  "Start NAME under PARENT, with typed ATTRIBUTES and optional TRACE-ID."
  (gptel-otel--make-span
   :trace-id (cond (parent (gptel-otel-span-trace-id parent))
                   (trace-id trace-id) (t (gptel-otel-generate-trace-id)))
   :span-id (gptel-otel-generate-span-id)
   :parent-span-id (and parent (gptel-otel-span-span-id parent))
   :name name :start-time-unix-nano (gptel-otel--unix-nano)
   :attributes (copy-tree attributes)))

(defun gptel-otel-span-set-attribute (span key value)
  (setf (gptel-otel-span-attributes span)
        (cons (cons key value) (assoc-delete-all key (gptel-otel-span-attributes span))))
  span)

(defun gptel-otel-end-span (span &optional status)
  (unless (gptel-otel-span-ended-p span)
    (setf (gptel-otel-span-end-time-unix-nano span) (gptel-otel--unix-nano)
          (gptel-otel-span-status span) status
          (gptel-otel-span-ended-p span) t))
  span)

(defun gptel-otel-status-ok () '((code . 1)))
(defun gptel-otel-status-error (&optional message)
  (append '((code . 2)) (and message `((message . ,(format "%s" message))))))

(defun gptel-otel-trace-create (root-name &optional attributes)
  "Create a trace with root span ROOT-NAME."
  (let* ((root (gptel-otel-start-span root-name nil attributes))
         (trace (gptel-otel--make-trace :id (gptel-otel-span-trace-id root)
                                        :root root :spans nil)))
    (setf (gptel-otel-trace-spans trace) (list root)) trace))

(defun gptel-otel-trace-start-span (trace name parent attributes)
  "Start and register a span in TRACE."
  (let ((span (gptel-otel-start-span name (or parent (gptel-otel-trace-root trace))
                                     attributes (gptel-otel-trace-id trace))))
    (setf (gptel-otel-trace-spans trace)
          (append (gptel-otel-trace-spans trace) (list span)))
    (cl-incf (gptel-otel-trace-outstanding trace))
    span))

(defun gptel-otel-trace-end-span (trace span &optional status)
  "End SPAN in TRACE and decrement its outstanding-operation count once."
  (let ((already-ended (gptel-otel-span-ended-p span)))
    (gptel-otel-end-span span status)
    (when (and (not already-ended)
               (not (eq span (gptel-otel-trace-root trace))))
      (setf (gptel-otel-trace-outstanding trace)
            (max 0 (1- (gptel-otel-trace-outstanding trace))))))
  span)

(defun gptel-otel--span-json (span)
  (unless (gptel-otel-span-ended-p span) (error "Cannot export an unended span"))
  (append `((traceId . ,(gptel-otel-span-trace-id span))
            (spanId . ,(gptel-otel-span-span-id span)))
          (and (gptel-otel-span-parent-span-id span)
               `((parentSpanId . ,(gptel-otel-span-parent-span-id span))))
          `((name . ,(gptel-otel-span-name span)) (kind . 1)
            (startTimeUnixNano . ,(gptel-otel-span-start-time-unix-nano span))
            (endTimeUnixNano . ,(gptel-otel-span-end-time-unix-nano span))
            (attributes . ,(gptel-otel--attributes (gptel-otel-span-attributes span))))
          (and (gptel-otel-span-status span) `((status . ,(gptel-otel-span-status span))))))

(defun gptel-otel-export-request (spans)
  "Build an OTLP ExportTraceServiceRequest for ended SPANS."
  (unless spans (error "Refusing to create an empty trace envelope"))
  (let ((attrs (cons (cons "service.name" (gptel-otel-value-string gptel-otel-service-name))
                     (assoc-delete-all "service.name" gptel-otel-resource-attributes))))
    `((resourceSpans .
       [((resource . ((attributes . ,(gptel-otel--attributes attrs))))
         (scopeSpans . [((scope . ((name . "gptel-otel") (version . "0.3.0")))
                         (spans . ,(vconcat (mapcar #'gptel-otel--span-json spans))))]))]))))

(defun gptel-otel-trace-request (trace)
  "Return one request containing every ended, not-yet-exported span in TRACE."
  (let ((spans (cl-remove-if-not
                (lambda (s) (and (gptel-otel-span-ended-p s)
                                 (not (gptel-otel-span-exported-p s))))
                (gptel-otel-trace-spans trace))))
    (when spans (gptel-otel-export-request spans))))

(provide 'gptel-otel-core)
;;; gptel-otel-core.el ends here
