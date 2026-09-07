;;; gptel-otel-adapter.el --- gptel private lifecycle adapter  -*- lexical-binding: t; -*-

;; This is the private gptel boundary.  It owns FSM and tool lifecycle seams;
;; gptel-otel.el owns generic trace/span semantics.

(require 'cl-lib)

(defvaralias 'gptel-otel--installed-advices 'gptel-otel--base-installed-advices)
(defvar gptel-otel--base-installed-advices nil)
(defvar gptel-otel--user-turn nil)
(defvar gptel-otel--adapter-capabilities nil)
(defvar gptel-otel--parent-fsm nil)
(defvar gptel-otel--agent-bindings)

(declare-function gptel-otel--set-semantic-attributes "gptel-otel")
(declare-function gptel-fsm-state "gptel")
(declare-function gptel-fsm-info "gptel")
(declare-function gptel-otel--context "gptel-otel")
(declare-function gptel-otel--finish-generation "gptel-otel")
(declare-function gptel-otel--finalize "gptel-otel")
(declare-function gptel-otel--observation-attributes "gptel-otel")
(declare-function gptel-otel--maybe-export "gptel-otel")
(declare-function gptel-otel-trace-start-span "gptel-otel-core")
(declare-function gptel-otel-trace-end-span "gptel-otel-core")
(declare-function gptel-otel-status-ok "gptel-otel-core")
(declare-function gptel-otel-status-error "gptel-otel-core")

(defconst gptel-otel--base-seams
  '((root (gptel-send (&optional arg) gptel-otel--around-send :around))
    (generation
     (gptel--handle-wait (fsm) gptel-otel--before-wait :before)
     (gptel--fsm-transition (machine &optional new-state)
                            gptel-otel--around-transition :around))
    (tool
     (gptel--handle-pre-tool (fsm) gptel-otel--before-pre-tool :before)
     (gptel--process-tool-call (fsm tool-spec tool-call result)
                               gptel-otel--around-process-tool :around)
     (gptel--handle-tool-use (fsm) gptel-otel--around-tool-use :around))))

(defun gptel-otel--signature-equal-p (symbol expected)
  (and (fboundp symbol) (equal (help-function-arglist symbol t) expected)))

(defun gptel-otel--seam-status (seam)
  (pcase-let ((`(,symbol ,args ,_advice ,_where) seam))
    (list :symbol symbol :expected args
          :actual (and (fboundp symbol) (help-function-arglist symbol t))
          :supported (gptel-otel--signature-equal-p symbol args))))

(defun gptel-otel--base-adapter-capabilities ()
  "Return compatibility facts for private gptel lifecycle seams."
  (let (result)
    (dolist (layer gptel-otel--base-seams)
      (let* ((name (car layer)) (statuses (mapcar #'gptel-otel--seam-status (cdr layer)))
             (supported (cl-every (lambda (status) (plist-get status :supported)) statuses)))
        (setq result (plist-put result name (list :supported supported :seams statuses)))))
    (plist-put result 'base
               (and (plist-get (plist-get result 'root) :supported)
                    (plist-get (plist-get result 'generation) :supported)))))

(defun gptel-otel--warn-unsupported-layer (name status)
  (when-let* ((failed (cl-find-if-not (lambda (seam) (plist-get seam :supported))
                                       (plist-get status :seams))))
    (display-warning 'gptel-otel
                     (format "%s instrumentation unavailable: %s signature is %S, expected %S"
                             name (plist-get failed :symbol) (plist-get failed :actual)
                             (plist-get failed :expected)) :warning)))

(defun gptel-otel--base-install-advice (seam)
  (pcase-let ((`(,symbol ,_args ,advice ,where) seam))
    (unless (advice-member-p advice symbol)
      (advice-add symbol where advice))
    (cl-pushnew (cons symbol advice) gptel-otel--base-installed-advices :test #'equal)))

(defun gptel-otel--seams-active-p (seams)
  "Return non-nil only when every advice in SEAMS is installed."
  (and seams
       (cl-every (lambda (seam) (advice-member-p (nth 2 seam) (car seam))) seams)
       t))

(defun gptel-otel--base-adapter-active-p (&optional tools)
  "Return whether the base advice, and optionally TOOLS, is installed."
  (and (gptel-otel--seams-active-p (cdr (assq 'root gptel-otel--base-seams)))
       (gptel-otel--seams-active-p (cdr (assq 'generation gptel-otel--base-seams)))
       (or (not tools)
           (gptel-otel--seams-active-p (cdr (assq 'tool gptel-otel--base-seams))))))

(defun gptel-otel--base-adapter-uninstall ()
  "Remove every base lifecycle advice, including stale incompatible advice."
  (dolist (pair gptel-otel--base-installed-advices)
    (advice-remove (car pair) (cdr pair)))
  (dolist (layer gptel-otel--base-seams)
    (dolist (seam (cdr layer))
      (pcase-let ((`(,symbol ,_args ,advice ,_where) seam))
        (when (advice-member-p advice symbol) (advice-remove symbol advice)))))
  (setq gptel-otel--base-installed-advices nil))

(defun gptel-otel--base-adapter-install ()
  "Reconcile base advice with current compatible private seams."
  (gptel-otel--base-adapter-uninstall)
  (let ((capabilities (gptel-otel--base-adapter-capabilities)))
    (setq gptel-otel--adapter-capabilities capabilities)
    (if (plist-get capabilities 'base)
        (progn
          (mapc #'gptel-otel--base-install-advice (cdr (assq 'root gptel-otel--base-seams)))
          (mapc #'gptel-otel--base-install-advice (cdr (assq 'generation gptel-otel--base-seams)))
          (if (plist-get (plist-get capabilities 'tool) :supported)
              (mapc #'gptel-otel--base-install-advice (cdr (assq 'tool gptel-otel--base-seams)))
            (gptel-otel--warn-unsupported-layer "Tool" (plist-get capabilities 'tool))))
      (gptel-otel--warn-unsupported-layer
       "Required gptel lifecycle"
       (if (plist-get (plist-get capabilities 'root) :supported)
           (plist-get capabilities 'generation) (plist-get capabilities 'root))))
    capabilities))

(defun gptel-otel--around-send (orig &rest args)
  ;; The binding is consumed by the prompt transform invoked within ORIG.
  (let ((gptel-otel--user-turn t)) (apply orig args)))

(defun gptel-otel--around-transition (orig machine &optional new-state)
  "Contain telemetry before and after ORIG without altering its behavior."
  (let ((old-state (condition-case nil (gptel-fsm-state machine) (error nil))) result)
    (condition-case upstream
        (setq result (funcall orig machine new-state))
      (error
       (condition-case nil
           (progn (when (eq old-state 'TYPE) (gptel-otel--finish-generation machine))
                  (gptel-otel--finalize machine 'error))
         (error nil))
       (signal (car upstream) (cdr upstream))))
    (condition-case telemetry
        (progn
          (when (eq old-state 'TYPE) (gptel-otel--finish-generation machine))
          (when (memq (gptel-fsm-state machine) '(DONE ERRS ABRT))
            (gptel-otel--finalize machine (gptel-fsm-state machine))))
      (error (display-warning 'gptel-otel (format "Transition telemetry failed: %s" telemetry) :warning)))
    result))

(defun gptel-otel--before-pre-tool (fsm)
  (condition-case err
      (when-let* ((context (gptel-otel--context fsm)))
        (dolist (call (plist-get (gptel-fsm-info fsm) :tool-use))
          (unless (or (plist-get call :result) (gethash call (gptel-otel--context-tool-spans context)))
            (let ((span (gptel-otel-trace-start-span
                         (gptel-otel--context-trace context)
                         (format "execute_tool %s" (plist-get call :name))
                         (gptel-otel--context-root context)
                         (append (gptel-otel--observation-attributes context "tool" (plist-get call :args) nil call)
                                 (and (plist-get call :id)
                                      `(("tool.call.id" . ,(gptel-otel-value-string (plist-get call :id)))))))))
              (puthash call span (gptel-otel--context-tool-spans context))
              ;; The optional agent module consumes this identity mapping;
              ;; no provider-visible tool arguments are modified.
              (when (equal (plist-get call :name) "Agent")
                (puthash (plist-get call :args) (list fsm call span)
                         gptel-otel--agent-bindings))))))
    (error (display-warning 'gptel-otel (format "Tool start failed: %s" err) :warning))))

(defun gptel-otel--finish-tool (fsm tool-call result error)
  (condition-case telemetry
      (when-let* ((context (gptel-otel--context fsm))
                  (span (gethash tool-call (gptel-otel--context-tool-spans context))))
        (gptel-otel--set-semantic-attributes span :kind 'observation-finish :type "tool"
                                              :output result :tool-call tool-call)
        (gptel-otel-trace-end-span (gptel-otel--context-trace context) span
                                   (if (or error (plist-get tool-call :error))
                                       (gptel-otel-status-error (and error (error-message-string error)))
                                     (gptel-otel-status-ok)))
        (gptel-otel--maybe-export (gptel-otel--context-trace context)))
    (error (display-warning 'gptel-otel (format "Tool finish failed: %s" telemetry) :warning))))

(defun gptel-otel--around-process-tool (orig fsm tool-spec tool-call result)
  (condition-case upstream
      (let ((value (funcall orig fsm tool-spec tool-call result)))
        (condition-case nil (gptel-otel--finish-tool fsm tool-call result nil) (error nil)) value)
    (error
     (condition-case nil (gptel-otel--finish-tool fsm tool-call result upstream) (error nil))
     (signal (car upstream) (cdr upstream)))))

(defun gptel-otel--reconcile-tool-spans (fsm &optional terminal)
  "Finish tracked tool spans that bypass normal gptel result processing."
  (when-let* ((context (gptel-otel--context fsm)))
    (let ((calls (plist-get (gptel-fsm-info fsm) :tool-use)) pending)
      (maphash
       (lambda (call span)
         (unless (gptel-otel-span-ended-p span)
           (cond ((plist-member call :result) (push (list call (plist-get call :result) nil) pending))
                 ((not (memq call calls)) (push (list call (plist-get call :args) nil) pending))
                 ((and terminal (not (cl-some
                                      (lambda (child) (and (not (gptel-otel-span-ended-p child))
                                                       (equal (gptel-otel-span-parent-span-id child)
                                                              (gptel-otel-span-span-id span))))
                                      (gptel-otel-trace-spans (gptel-otel--context-trace context)))))
                  (push (list call nil '(error "Tool call abandoned")) pending)))))
       (gptel-otel--context-tool-spans context))
      (dolist (entry pending) (gptel-otel--finish-tool fsm (nth 0 entry) (nth 1 entry) (nth 2 entry))))))

(defun gptel-otel--around-tool-use (orig fsm)
  (let ((gptel-otel--parent-fsm fsm))
    (unwind-protect (funcall orig fsm)
      (condition-case nil (gptel-otel--reconcile-tool-spans fsm) (error nil)))))

(provide 'gptel-otel-adapter)
;;; gptel-otel-adapter.el ends here
