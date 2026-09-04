;;; gptel-otel-transport.el --- Durable OTLP delivery  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'auth-source)
(require 'subr-x)
(require 'gptel-otel-core)

(defcustom gptel-otel-endpoint "http://localhost:4318/v1/traces"
  "Complete OTLP/HTTP traces endpoint."
  :type 'string :group 'gptel-otel)
(defcustom gptel-otel-endpoint-kind 'collector
  "Endpoint type, either `collector' or `langfuse'."
  :type '(choice (const collector) (const langfuse)) :group 'gptel-otel)
(defcustom gptel-otel-langfuse-base-url nil
  "Langfuse base URL; when non-nil it determines the v1 traces endpoint."
  :type '(choice (const nil) string) :group 'gptel-otel)
(defcustom gptel-otel-headers-function nil
  "Function returning dynamic extra HTTP headers."
  :type '(choice (const nil) function) :group 'gptel-otel)
(defcustom gptel-otel-langfuse-auth-function #'gptel-otel-langfuse-auth-source
  "Function returning (PUBLIC-KEY . SECRET-KEY), or nil."
  :type 'function :group 'gptel-otel)
(defcustom gptel-otel-auth-source-user nil
  "Langfuse public key to match as the auth-source user.
When nil, accept the user returned by the selected auth-source entry."
  :type '(choice (const nil) string) :group 'gptel-otel)
(defcustom gptel-otel-auth-source-account nil
  "Optional auth-source account selector, distinct from the public-key user."
  :type '(choice (const nil) string) :group 'gptel-otel)
(defcustom gptel-otel-spool-directory
  (expand-file-name "gptel-otel/" (or (getenv "XDG_STATE_HOME")
                                      (expand-file-name ".local/state/" "~")))
  "Private directory containing durable trace envelopes."
  :type 'directory :group 'gptel-otel)
(defcustom gptel-otel-cloud-max-bytes (* 5 1024 1024)
  "Maximum Langfuse Cloud request body size."
  :type 'integer :group 'gptel-otel)
(defcustom gptel-otel-retry-base-seconds 1.0
  "Initial retry delay."
  :type 'number :group 'gptel-otel)
(defcustom gptel-otel-retry-max-seconds 300.0
  "Maximum retry delay."
  :type 'number :group 'gptel-otel)
(defcustom gptel-otel-delivery-timeout-seconds 30.0
  "Seconds to wait for a delivery callback before retrying its queue entry."
  :type 'number :group 'gptel-otel)
(defcustom gptel-otel-lease-timeout-seconds 120.0
  "Seconds after which another Emacs process may recover a delivery lease."
  :type 'number :group 'gptel-otel)
(defcustom gptel-otel-delivery-function #'gptel-otel--http-deliver
  "Asynchronous function called as (PAYLOAD CALLBACK)."
  :type 'function :group 'gptel-otel)

(defvar gptel-otel--delivery-timer nil)
(defvar gptel-otel--watchdog-timer nil)
(defvar gptel-otel--delivery-active nil)
(defvar gptel-otel--last-delivery nil)

(defun gptel-otel--endpoint ()
  (if (and (eq gptel-otel-endpoint-kind 'langfuse) gptel-otel-langfuse-base-url)
      (concat (string-remove-suffix "/" gptel-otel-langfuse-base-url)
              "/api/public/otel/v1/traces")
    gptel-otel-endpoint))

(defun gptel-otel-langfuse-auth-source ()
  "Read Langfuse public and secret keys from auth-source.
The Basic username is always the matched entry's `:user' (the Langfuse public
key).  `gptel-otel-auth-source-account' is an independent optional selector."
  (let* ((parsed (url-generic-parse-url (gptel-otel--endpoint)))
         ;; Authinfo conventionally names TLS ports by service ("https"),
         ;; while `url-port' resolves the same endpoint to 443.  Prefer the
         ;; service name so ordinary `machine HOST port https' entries match.
         (query (append (list :host (url-host parsed)
                              :port (url-type parsed))
                        (and gptel-otel-auth-source-user
                             (list :user gptel-otel-auth-source-user))
                        (and gptel-otel-auth-source-account
                             (list :account gptel-otel-auth-source-account))
                        (list :max 1 :require '(:user :secret))))
         (match (car (apply #'auth-source-search query)))
         (public-key (plist-get match :user))
         (secret (plist-get match :secret)))
    (when (and public-key secret)
      (cons public-key (if (functionp secret) (funcall secret) secret)))))

(defun gptel-otel--reserved-header-p (header)
  (member (downcase (car header))
          '("authorization" "content-type" "x-langfuse-ingestion-version")))

(defun gptel-otel-http-headers ()
  "Return unambiguous endpoint headers without persisting credentials.
Caller-provided duplicates of reserved headers are ignored."
  (let* ((extra (and gptel-otel-headers-function
                     (funcall gptel-otel-headers-function)))
         (keys (and (eq gptel-otel-endpoint-kind 'langfuse)
                    gptel-otel-langfuse-auth-function
                    (funcall gptel-otel-langfuse-auth-function))))
    (append
     '(("Content-Type" . "application/json"))
     (when (eq gptel-otel-endpoint-kind 'langfuse)
       (append '(("x-langfuse-ingestion-version" . "4"))
               (when (and (car-safe keys) (cdr-safe keys))
                 `(("Authorization" .
                    ,(concat "Basic " (base64-encode-string
                                       (concat (car keys) ":" (cdr keys)) t)))))))
     (cl-remove-if #'gptel-otel--reserved-header-p extra))))

(defun gptel-otel-retryable-p (status &optional connection-error)
  "Return non-nil when STATUS or CONNECTION-ERROR should be retried."
  (or connection-error (memq status '(429 502 503 504))))

(defun gptel-otel--ensure-spool ()
  (unless (file-directory-p gptel-otel-spool-directory)
    (make-directory gptel-otel-spool-directory t))
  (set-file-modes gptel-otel-spool-directory #o700))

(defun gptel-otel--entry-files ()
  (when (file-directory-p gptel-otel-spool-directory)
    (sort (directory-files gptel-otel-spool-directory t "\\.json\\'" t) #'string<)))

(defun gptel-otel--lease-files ()
  (when (file-directory-p gptel-otel-spool-directory)
    (directory-files gptel-otel-spool-directory t "\\.json\\.lease\\." t)))

(defun gptel-otel--base-payload-file (payload-file)
  (if (string-match "\\(.*\\.json\\)\\.lease\\." payload-file)
      (match-string 1 payload-file)
    payload-file))

(defun gptel-otel--metadata-file (payload-file)
  (concat (file-name-sans-extension
           (gptel-otel--base-payload-file payload-file)) ".meta"))

(defun gptel-otel--write-private (file content &optional literal)
  (let ((coding-system-for-write (if literal 'no-conversion 'utf-8-unix)))
    (write-region content nil file nil 'silent))
  (set-file-modes file #o600))

(defun gptel-otel--atomic-write (file content &optional literal)
  "Atomically replace FILE with CONTENT, optionally as LITERAL bytes."
  (let ((temp (make-temp-file (concat file ".tmp-") nil)))
    (unwind-protect
        (progn (gptel-otel--write-private temp content literal)
               (rename-file temp file t))
      (when (file-exists-p temp) (ignore-errors (delete-file temp))))))

(defun gptel-otel-enqueue (request)
  "Append REQUEST durably before scheduling asynchronous delivery.
Return the payload file, or nil for an empty envelope."
  (when (and request (let ((rs (cdr (assq 'resourceSpans request))))
                       (and (vectorp rs) (> (length rs) 0))))
    (condition-case err
        (progn
          (gptel-otel--ensure-spool)
          (let* ((payload (encode-coding-string (json-encode request) 'utf-8))
                 (base (format "%020d-%s" (truncate (* 1000000 (float-time)))
                               (gptel-otel-generate-span-id)))
                 (file (expand-file-name (concat base ".json") gptel-otel-spool-directory))
                 (meta (gptel-otel--metadata-file file))
                 (oversized (and (eq gptel-otel-endpoint-kind 'langfuse)
                                 (> (string-bytes payload) gptel-otel-cloud-max-bytes))))
            ;; Encode once and publish the payload only after its metadata is visible.
            (gptel-otel--atomic-write meta (prin1-to-string
                                       (list :attempts 0 :state (if oversized 'permanent 'pending)
                                             :error (and oversized "payload exceeds endpoint size limit")
                                             :bytes (length payload))))
            (gptel-otel--atomic-write file payload t)
            (setq gptel-otel--last-delivery
                  (list :state (if oversized 'permanent 'queued) :file file
                        :bytes (length payload)))
            (unless oversized (gptel-otel-flush))
            file))
      (error (display-warning 'gptel-otel (format "Could not spool trace: %s" err) :warning)
             nil))))

(defun gptel-otel--read-meta (file)
  (condition-case nil
      (with-temp-buffer (insert-file-contents (gptel-otel--metadata-file file))
                        (read (current-buffer)))
    (error (list :attempts 0 :state 'pending))))

(defun gptel-otel--write-meta (file meta)
  (gptel-otel--atomic-write (gptel-otel--metadata-file file)
                            (prin1-to-string meta)))

(defun gptel-otel-status ()
  "Return bounded status for the durable queue."
  (let ((files (gptel-otel--entry-files)) (pending 0) (permanent 0) (bytes 0))
    (dolist (file files)
      (let ((meta (gptel-otel--read-meta file)))
        (cl-incf bytes (or (plist-get meta :bytes) 0))
        (if (eq (plist-get meta :state) 'permanent)
            (cl-incf permanent) (cl-incf pending))))
    (list :pending (or pending 0) :permanent (or permanent 0)
          :bytes (or bytes 0) :active (and gptel-otel--delivery-active t)
          :last gptel-otel--last-delivery)))

(defun gptel-otel--retry-delay (attempts retry-after)
  (or (and retry-after (string-match-p "\\`[0-9]+\\'" retry-after)
           (string-to-number retry-after))
      (min gptel-otel-retry-max-seconds
           (* gptel-otel-retry-base-seconds (expt 2 (max 0 (1- attempts)))))))

(defun gptel-otel--schedule (delay)
  (when (timerp gptel-otel--delivery-timer) (cancel-timer gptel-otel--delivery-timer))
  (setq gptel-otel--delivery-timer (run-at-time delay nil #'gptel-otel-flush)))

(defun gptel-otel--recover-stale-leases ()
  "Recover leases old enough to be safely retried by this process."
  (let ((now (float-time)))
    (dolist (lease (gptel-otel--lease-files))
      (when (> (- now (float-time (file-attribute-modification-time
                                   (file-attributes lease))))
               gptel-otel-lease-timeout-seconds)
        (let ((base (gptel-otel--base-payload-file lease)))
          (condition-case nil
              (if (file-exists-p base)
                  (delete-file lease)
                (rename-file lease base nil))
            (file-error nil)))))))

(defun gptel-otel--claim-next ()
  "Atomically claim and return the next pending entry, or nil.
A rename is the inter-process claim primitive.  Delivery is intentionally
at-least-once: a process crash after remote acceptance but before local deletion
can cause a recovered stale lease to be delivered again."
  (gptel-otel--recover-stale-leases)
  (catch 'claimed
    (dolist (file (gptel-otel--entry-files))
      (when (eq (plist-get (gptel-otel--read-meta file) :state) 'pending)
        (let ((lease (format "%s.lease.%s.%s" file (emacs-pid)
                             (gptel-otel-generate-span-id))))
          (condition-case nil
              (progn
                (rename-file file lease nil)
                ;; Lease age starts at claim, not at the payload's enqueue time.
                (set-file-times lease)
                (throw 'claimed lease))
            (file-error nil)))))
    nil))

(defun gptel-otel--release-lease (lease)
  (let ((base (gptel-otel--base-payload-file lease)))
    (condition-case nil
        (if (file-exists-p base) (delete-file lease) (rename-file lease base nil))
      (file-error nil))))

(defun gptel-otel--finish-delivery (token lease result)
  "Apply RESULT only when TOKEN still owns LEASE; ignore late callbacks."
  (when (and gptel-otel--delivery-active
             (eq token (plist-get gptel-otel--delivery-active :token)))
    (when (timerp gptel-otel--watchdog-timer)
      (cancel-timer gptel-otel--watchdog-timer))
    (setq gptel-otel--watchdog-timer nil
          gptel-otel--delivery-active nil)
    (let* ((meta (gptel-otel--read-meta lease))
           (attempts (1+ (or (plist-get meta :attempts) 0)))
           (status (plist-get result :status))
           (errorp (plist-get result :connection-error)))
      (cond
       ((plist-get result :ok)
        (ignore-errors (delete-file lease))
        (ignore-errors (delete-file (gptel-otel--metadata-file lease)))
        (setq gptel-otel--last-delivery (list :state 'delivered :status status))
        (gptel-otel--schedule 0))
       ((gptel-otel-retryable-p status errorp)
        (setq meta (plist-put meta :attempts attempts))
        (setq meta (plist-put meta :error (plist-get result :error)))
        (gptel-otel--write-meta lease meta)
        (gptel-otel--release-lease lease)
        (setq gptel-otel--last-delivery (list :state 'retry :status status))
        (gptel-otel--schedule
         (gptel-otel--retry-delay attempts (plist-get result :retry-after))))
       (t
        (setq meta (plist-put meta :state 'permanent))
        (setq meta (plist-put meta :attempts attempts))
        (setq meta (plist-put meta :error (or (plist-get result :error)
                                             (format "HTTP %s" status))))
        (gptel-otel--write-meta lease meta)
        (gptel-otel--release-lease lease)
        (setq gptel-otel--last-delivery
              (list :state 'permanent :status status
                    :file (gptel-otel--base-payload-file lease)))
        (gptel-otel--schedule 0))))))

(defun gptel-otel--delivery-timeout (token lease)
  (gptel-otel--finish-delivery
   token lease '(:ok nil :connection-error t :error "delivery callback timed out")))

(defun gptel-otel-flush ()
  "Asynchronously attempt delivery of the next queued trace.
Queue delivery has at-least-once semantics, including a duplicate crash window
between endpoint acceptance and deletion of the claimed local entry."
  (interactive)
  (unless gptel-otel--delivery-active
    (when-let* ((lease (gptel-otel--claim-next)))
      (let ((token (cons 'delivery nil)))
        (setq gptel-otel--delivery-active (list :token token :lease lease))
        (condition-case err
            (let ((payload (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally lease)
                             (buffer-string))))
              (setq gptel-otel--watchdog-timer
                    (run-at-time gptel-otel-delivery-timeout-seconds nil
                                 #'gptel-otel--delivery-timeout token lease))
              (funcall gptel-otel-delivery-function payload
                       (apply-partially #'gptel-otel--finish-delivery token lease)))
          (error
           (gptel-otel--finish-delivery
            token lease (list :ok nil :connection-error t :error err))))))))

(defun gptel-otel-replay (&optional include-permanent)
  "Replay queued traces; with INCLUDE-PERMANENT reset permanent failures."
  (interactive "P")
  (when include-permanent
    (dolist (file (gptel-otel--entry-files))
      (let ((meta (gptel-otel--read-meta file)))
        (when (eq (plist-get meta :state) 'permanent)
          (let* ((bytes (file-attribute-size (file-attributes file)))
                 (oversized (and (eq gptel-otel-endpoint-kind 'langfuse)
                                 (> bytes gptel-otel-cloud-max-bytes))))
            (setq meta (plist-put meta :bytes bytes))
            (setq meta (plist-put meta :attempts 0))
            (if oversized
                (progn
                  (setq meta (plist-put meta :state 'permanent))
                  (setq meta (plist-put meta :error
                                        "payload exceeds endpoint size limit")))
              (setq meta (plist-put meta :state 'pending))
              (setq meta (plist-put meta :error nil)))
            (gptel-otel--write-meta file meta))))))
  (gptel-otel-flush))

(defun gptel-otel--http-deliver (payload callback)
  (condition-case err
      (let ((url-request-method "POST")
            (url-request-extra-headers (gptel-otel-http-headers))
            (url-request-data payload))
        (url-retrieve
         (gptel-otel--endpoint)
         (lambda (status cb)
           (let* ((connection-error (plist-get status :error))
                  (code (and (boundp 'url-http-response-status) url-http-response-status))
                  (retry-after (and (fboundp 'mail-fetch-field)
                                    (mail-fetch-field "Retry-After")))
                  (result (list :ok (and (not connection-error) code
                                         (<= 200 code) (< code 300))
                                :status code :connection-error connection-error
                                :error connection-error :retry-after retry-after)))
             (unwind-protect (condition-case nil (funcall cb result) (error nil))
               (kill-buffer (current-buffer)))))
         (list callback) t t))
    (error (condition-case nil
               (funcall callback (list :ok nil :connection-error t :error err))
             (error nil)))))

(defun gptel-otel-export (spans &optional callback)
  "Durably enqueue ended SPANS, then invoke CALLBACK with queue success."
  (condition-case err
      (let ((file (and spans (gptel-otel-enqueue (gptel-otel-export-request spans)))))
        (when callback (funcall callback (and file t))) file)
    (error (display-warning 'gptel-otel (format "Export failed: %s" err) :warning)
           (when callback (condition-case nil (funcall callback nil) (error nil))) nil)))

(provide 'gptel-otel-transport)
;;; gptel-otel-transport.el ends here
