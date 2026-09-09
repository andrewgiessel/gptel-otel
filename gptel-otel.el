;;; gptel-otel.el --- OpenTelemetry instrumentation for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Author: Andrew Giessel <andrew.giessel@gmail.com>
;; Maintainer: Andrew Giessel <andrew.giessel@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9"))
;; Keywords: convenience, tools
;; URL: https://github.com/andrewgiessel/gptel-otel

;;; Commentary:
;; OpenTelemetry tracing for stock gptel, with optional gptel-agent support.
;; The global minor mode records one trace per gptel request, with nested
;; model-generation, tool-execution, and subagent observations when available.
;; Completed spans are durably spooled and exported via OTLP/HTTP JSON.  Langfuse is the polished default
;; backend profile; generic OTLP/HTTP collectors are also supported.
;;
;; gptel currently lacks public lifecycle hooks with all required correlation
;; data.  This package therefore uses signature-guarded private seams and
;; disables only an incompatible instrumentation layer when a seam changes.
;;
;; See the repository README for installation, privacy, and operation details.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'gptel)
(require 'gptel-otel-core)
(require 'gptel-otel-transport)

(defcustom gptel-otel-capture-payloads nil
  "When non-nil retain complete logical inputs and outputs; never truncate."
  :type 'boolean :group 'gptel-otel)
(defcustom gptel-otel-inhibit nil
  "When non-nil, do not start tracing gptel sends originating in this buffer.
This option is automatically buffer-local.  Its value is captured when a
request starts, so later changes do not affect in-flight requests.  It cannot
enable tracing while `gptel-otel-mode' is disabled.

Use (setq-local gptel-otel-inhibit t) to suppress new `gptel-send' requests
from this buffer, or let-bind it around a send for one request.  Tool calls
and child agents inherit the request's decision.  Previously collected
telemetry is not removed.  Direct `gptel-request' calls without a send are
not covered by this option."
  :type 'boolean :group 'gptel-otel)
(make-variable-buffer-local 'gptel-otel-inhibit)
(defcustom gptel-otel-trace-metadata-function #'gptel-otel-default-trace-metadata
  "Function called with FSM, returning generic trace metadata plist.
Recognized keys are :name, :session-id, :user-id, :tags and :metadata.
The :metadata value is an alist of string keys and string values that Langfuse
can filter as trace metadata."
  :type 'function :group 'gptel-otel)
(defcustom gptel-otel-model-parameters-function #'gptel-otel-default-model-parameters
  "Function called with request INFO, returning model parameters."
  :type 'function :group 'gptel-otel)
(defcustom gptel-otel-attribute-provider-functions nil
  "Semantic attribute providers, or nil to use the backend profile defaults.
Each function receives an event plist and returns typed attribute cons cells.
This separates gptel lifecycle capture from backend and semantic conventions."
  :type '(repeat function) :group 'gptel-otel)

(cl-defstruct (gptel-otel--context (:constructor gptel-otel--make-context))
  trace root current-generation generations tool-spans metadata terminal-p)

(defvar gptel-otel--contexts (make-hash-table :test #'eq))
;; FSMs own request state; weak keys prevent an abandoned nonterminal FSM from
;; being retained solely by its captured tracing decision.
(defvar gptel-otel--request-decisions (make-hash-table :test #'eq :weakness 'key))
(defvar gptel-otel--request-decision nil)
(defvar gptel-otel--user-turn nil)
(defvar gptel-otel--pending-agent nil)
(defvar gptel-otel--parent-fsm nil)
(defvar gptel-otel--agent-bindings)
(defvar gptel-otel-mode nil)
;; Adapter modules own private upstream compatibility checks and advice state.

(defun gptel-otel-default-trace-metadata (_fsm)
  "Return default generic trace metadata."
  (list :name "gptel.chat" :tags []))

(defun gptel-otel-default-model-parameters (info)
  "Return common model parameters from INFO request data."
  (let ((data (plist-get info :data)) result)
    (dolist (key '(:temperature :max_tokens :max-output-tokens :top_p :top-k :stream))
      (when (and (listp data) (plist-member data key))
        (setq result (plist-put result key (plist-get data key)))))
    result))

(defun gptel-otel--serialize (object)
  (condition-case nil (json-encode object) (error (prin1-to-string object t))))

(defun gptel-otel--role-name (value)
  "Return VALUE as a normalized lowercase role name."
  (and value (downcase (format "%s" value))))

(defun gptel-otel--latest-user-input (data)
  "Return the latest user message from provider request DATA.
Fall back to DATA only when no provider message collection can be identified."
  (let* ((messages (or (plist-get data :messages)
                       (plist-get data :input)
                       (plist-get data :contents)))
         (items (cond ((vectorp messages) (append messages nil))
                      ((listp messages) messages)))
         found)
    (dolist (message items)
      (when (and (listp message)
                 (member (gptel-otel--role-name
                          (or (plist-get message :role)
                              (plist-get message :author)))
                         '("user" "human")))
        (setq found (or (plist-get message :content)
                        (plist-get message :parts)
                        (plist-get message :text)
                        message))))
    (or found (and (null messages) data))))

(defun gptel-otel--string-attr (key value)
  (and value (cons key (gptel-otel-value-string value))))

(defun gptel-otel--langfuse-context-attributes (metadata)
  (delq nil
        (append
         (list (gptel-otel--string-attr "langfuse.trace.name" (plist-get metadata :name))
               (gptel-otel--string-attr "langfuse.session.id" (plist-get metadata :session-id))
               (gptel-otel--string-attr "langfuse.user.id" (plist-get metadata :user-id))
               (when-let* ((tags (plist-get metadata :tags)))
                 (cons "langfuse.trace.tags"
                       (apply #'gptel-otel-value-array
                              (mapcar #'gptel-otel-value-string (append tags nil))))))
         (mapcar (lambda (entry)
                   (gptel-otel--string-attr
                    (concat "langfuse.trace.metadata." (car entry)) (cdr entry)))
                 (plist-get metadata :metadata)))))

(defun gptel-otel-langfuse-attribute-provider (event)
  "Return Langfuse attributes for lifecycle EVENT."
  (let ((kind (plist-get event :kind)) (metadata (plist-get event :metadata))
        (type (plist-get event :type)) (input (plist-get event :input))
        (output (plist-get event :output)) (info (plist-get event :info)))
    (delq nil
          (append
           (and (memq kind '(root-start observation-start))
                (gptel-otel--langfuse-context-attributes metadata))
           (and type (list (gptel-otel--string-attr "langfuse.observation.type" type)))
           (and gptel-otel-capture-payloads input
                (list (gptel-otel--string-attr "langfuse.observation.input"
                                               (gptel-otel--serialize input))))
           (and gptel-otel-capture-payloads output
                (list (gptel-otel--string-attr "langfuse.observation.output"
                                               (gptel-otel--serialize output))))
           (and (eq kind 'generation-finish)
                (list (gptel-otel--string-attr "langfuse.observation.model.name"
                                               (plist-get info :model))
                      (gptel-otel--string-attr
                       "langfuse.observation.model.parameters"
                       (gptel-otel--serialize
                        (funcall gptel-otel-model-parameters-function info)))
                      (and (plist-get info :tokens)
                           (gptel-otel--string-attr
                            "langfuse.observation.usage_details"
                            (gptel-otel--serialize (plist-get info :tokens))))))))))

(defun gptel-otel--token-value (tokens &rest keys)
  (catch 'value
    (dolist (key keys)
      (when (and (listp tokens) (plist-member tokens key))
        (throw 'value (plist-get tokens key))))))

(defun gptel-otel-genai-attribute-provider (event)
  "Return portable OpenTelemetry GenAI attributes for lifecycle EVENT."
  (let* ((kind (plist-get event :kind)) (type (plist-get event :type))
         (input (plist-get event :input)) (output (plist-get event :output))
         (info (plist-get event :info)) (tool (plist-get event :tool-call))
         (tokens (plist-get info :tokens))
         (input-tokens (gptel-otel--token-value tokens :input :input_tokens :prompt))
         (output-tokens (gptel-otel--token-value tokens :output :output_tokens :completion))
         (cache-tokens (gptel-otel--token-value tokens :cache :cache_read
                                                :cache_read_input_tokens))
         (langfuse-p
          (eq 'langfuse
              (gptel-otel-backend-profile-name
               (gptel-otel-active-backend-profile)))))
    (delq nil
          (append
           (cond
            ((equal type "generation")
             (list (gptel-otel--string-attr "gen_ai.operation.name" "chat")))
            ((equal type "tool")
             (list (gptel-otel--string-attr "gen_ai.operation.name" "execute_tool")))
            ((equal type "agent")
             (list (gptel-otel--string-attr "gen_ai.operation.name" "invoke_agent"))))
           (and (eq kind 'generation-finish)
                (list (gptel-otel--string-attr "gen_ai.request.model"
                                               (plist-get info :model))
                      (and input-tokens (cons "gen_ai.usage.input_tokens"
                                              (gptel-otel-value-int input-tokens)))
                      (and output-tokens (cons "gen_ai.usage.output_tokens"
                                               (gptel-otel-value-int output-tokens)))
                      (and cache-tokens
                           (cons "gen_ai.usage.cache_read_input_tokens"
                                 (gptel-otel-value-int cache-tokens)))))
           (and tool
                (list (gptel-otel--string-attr "gen_ai.tool.name"
                                               (plist-get tool :name))
                      (gptel-otel--string-attr "gen_ai.tool.call.id"
                                               (plist-get tool :id))))
           (and (equal type "agent")
                (list (gptel-otel--string-attr
                       "gen_ai.agent.name" (plist-get input :type))))
           ;; The generic profile uses portable content as its canonical full
           ;; representation for every logical operation.  Langfuse uses only
           ;; langfuse.observation.input/output for those bodies.
           (and (not langfuse-p) gptel-otel-capture-payloads input
                (list (cons "gen_ai.input.messages"
                            (gptel-otel-value-array
                             (gptel-otel-value-string
                              (gptel-otel--serialize input))))))
           (and (not langfuse-p) gptel-otel-capture-payloads output
                (list (cons "gen_ai.output.messages"
                            (gptel-otel-value-array
                             (gptel-otel-value-string
                              (gptel-otel--serialize output))))))))))

(defun gptel-otel--attribute-providers ()
  (or gptel-otel-attribute-provider-functions
      (gptel-otel-backend-profile-attribute-providers
       (gptel-otel-active-backend-profile))))

(defun gptel-otel--semantic-attributes (&rest event)
  "Run configured providers for EVENT, containing provider failures."
  (let (attributes)
    (dolist (provider (gptel-otel--attribute-providers))
      (condition-case err
          ;; Later providers have deterministic precedence.
          (dolist (attribute (funcall provider event))
            (when attribute
              (setq attributes
                    (cons attribute (assoc-delete-all (car attribute) attributes)))))
        (error (display-warning 'gptel-otel
                                (format "Attribute provider %s failed: %s" provider err)
                                :warning))))
    (nreverse attributes)))

(defun gptel-otel--set-semantic-attributes (span &rest event)
  (dolist (attribute (apply #'gptel-otel--semantic-attributes event))
    (when attribute (gptel-otel-span-set-attribute span (car attribute) (cdr attribute)))))

(defun gptel-otel--observation-attributes (context type &optional input output tool-call)
  (gptel-otel--semantic-attributes
   :kind 'observation-start :metadata (gptel-otel--context-metadata context)
   :type type :input input :output output :tool-call tool-call))

(defun gptel-otel--new-context (fsm &optional parent-context parent-span)
  (let* ((metadata (condition-case nil
                       (funcall gptel-otel-trace-metadata-function fsm)
                     (error nil)))
         (root-name (or (plist-get metadata :name) "chat-turn"))
         (trace (if parent-context (gptel-otel--context-trace parent-context)
                  (gptel-otel-trace-create
                   root-name (gptel-otel--semantic-attributes
                              :kind 'root-start :metadata metadata :type "span"))))
         (root (if parent-context parent-span (gptel-otel-trace-root trace)))
         (context (gptel-otel--make-context
                   :trace trace :root root :tool-spans (make-hash-table :test #'eq)
                   :metadata (or metadata (and parent-context
                                               (gptel-otel--context-metadata parent-context))))))
    (puthash fsm context gptel-otel--contexts)
    context))

(defun gptel-otel--context (fsm)
  (gethash fsm gptel-otel--contexts))

(defun gptel-otel--request-decision-for-fsm (fsm)
  "Return the tracing decision captured for FSM, if any."
  (gethash fsm gptel-otel--request-decisions))

(defun gptel-otel--instrument-request (fsm)
  "Capture FSM's tracing decision and establish its trace root when enabled."
  (condition-case err
      (when-let* ((decision (or (gptel-otel--request-decision-for-fsm fsm)
                                gptel-otel--request-decision
                                (and gptel-otel--user-turn :trace))))
        ;; This runs synchronously while gptel starts asynchronous transforms,
        ;; so later callbacks consult the recorded FSM decision, never their
        ;; current buffer's local value.
        (puthash fsm decision gptel-otel--request-decisions)
        (when (and (eq decision :trace)
                   (not (gptel-otel--context fsm))
                   (not gptel-otel--pending-agent))
          (gptel-otel--new-context fsm)))
    (error (display-warning 'gptel-otel (format "Root instrumentation failed: %s" err) :warning)))
  nil)

(defun gptel-otel--generation-callback (span original response info &rest raw)
  "Call ORIGINAL in order, then capture RESPONSE for generation SPAN."
  ;; Preserve the caller's arity exactly: gptel invokes callbacks with either
  ;; two arguments or an additional RAW value, and some callbacks accept only
  ;; the documented two-argument form.
  (prog1 (apply original response info raw)
    (condition-case err
        (when (and (not (gptel-otel-span-ended-p span))
                   (not (eq response t)))
          (let ((old (gptel-otel-span-get-output span)))
            (gptel-otel-span-put-output
             span
             (cond
              ((and (stringp old) (stringp response)) (concat old response))
              ((null old) response)
              ((and (listp old) (eq (car-safe old) :gptel-otel-parts))
               (append old (list response)))
              (t (list :gptel-otel-parts old response))))))
      (error (display-warning 'gptel-otel (format "Response capture failed: %s" err) :warning)))))

;; Store adapter-only transient data without extending the public struct.
(defvar gptel-otel--span-data (make-hash-table :test #'eq))
(defun gptel-otel-span-put-output (span value) (puthash span value gptel-otel--span-data))
(defun gptel-otel-span-get-output (span) (gethash span gptel-otel--span-data))

(defun gptel-otel--before-wait (fsm)
  (condition-case err
      (let* ((decision (or (gptel-otel--request-decision-for-fsm fsm)
                           gptel-otel--request-decision))
             ;; Agent children can reach WAIT before their task wrapper has
             ;; returned.  Record its inherited decision at that synchronous
             ;; boundary so later lifecycle callbacks are buffer-independent.
             (_recorded (and decision
                             (puthash fsm decision gptel-otel--request-decisions)))
             (parent-context (and gptel-otel--parent-fsm
                                  (gptel-otel--context gptel-otel--parent-fsm)))
             (agent gptel-otel--pending-agent)
             (context (or (gptel-otel--context fsm)
                          (and agent parent-context
                               (gptel-otel--new-context fsm parent-context agent))
                          ;; An inhibited request has no context by design.
                          ;; Do not let an asynchronous lifecycle callback
                          ;; create one after its originating buffer changed.
                          (unless (eq decision :inhibit)
                            (gptel-otel--new-context fsm)))))
        (when context
          ;; Never silently displace an unended generation on a repeated WAIT.
          (when-let* ((previous (gptel-otel--context-current-generation context))
                      ((not (gptel-otel-span-ended-p previous))))
            (gptel-otel-trace-end-span
             (gptel-otel--context-trace context) previous
             (gptel-otel-status-error "Generation replaced before completion"))
            (setf (gptel-otel--context-current-generation context) nil)
            (gptel-otel--maybe-export (gptel-otel--context-trace context)))
          (let* ((info (gptel-fsm-info fsm))
                 (parent (or agent (gptel-otel--context-root context)))
                 (input (plist-get info :data))
                 (attrs (gptel-otel--observation-attributes context "generation" input nil))
                 (model (format "%s" (or (plist-get info :model) "unknown")))
                 (span (gptel-otel-trace-start-span
                        (gptel-otel--context-trace context)
                        (format "chat %s" model) parent attrs))
                 (callback (plist-get info :callback)))
            (setf (gptel-otel--context-current-generation context) span
                  (gptel-otel--context-generations context)
                  (append (gptel-otel--context-generations context) (list span)))
            (when (functionp callback)
              (plist-put info :callback
                         (apply-partially #'gptel-otel--generation-callback span callback))))))
    (error (display-warning 'gptel-otel (format "Generation start failed: %s" err) :warning)))
  nil)

(defun gptel-otel--finish-generation (fsm)
  (when-let* ((context (gptel-otel--context fsm))
              (span (gptel-otel--context-current-generation context))
              ((not (gptel-otel-span-ended-p span))))
    (let* ((info (gptel-fsm-info fsm))
           (output (gptel-otel-span-get-output span)))
      (gptel-otel--set-semantic-attributes
       span :kind 'generation-finish :type "generation" :output output :info info)
      (gptel-otel-trace-end-span
       (gptel-otel--context-trace context) span
       (if (plist-get info :error)
           (gptel-otel-status-error (plist-get info :status))
         (gptel-otel-status-ok)))
      (setf (gptel-otel--context-current-generation context) nil)
      (gptel-otel--maybe-export (gptel-otel--context-trace context)))))

(defun gptel-otel--reconcile-generation-spans (fsm &optional reason)
  "Finish every unended generation owned by FSM with error REASON."
  (when-let* ((context (gptel-otel--context fsm)))
    (dolist (span (gptel-otel--context-generations context))
      (unless (gptel-otel-span-ended-p span)
        (gptel-otel-trace-end-span
         (gptel-otel--context-trace context) span
         (gptel-otel-status-error (or reason "Generation abandoned")))))
    (setf (gptel-otel--context-current-generation context) nil)
    (gptel-otel--maybe-export (gptel-otel--context-trace context))))


(defun gptel-otel--response-text (info)
  (let ((start (plist-get info :position))
        (end (or (plist-get info :tracking-marker) (plist-get info :position))))
    (when (and (markerp start) (marker-buffer start) (markerp end)
               (eq (marker-buffer start) (marker-buffer end)))
      (with-current-buffer (marker-buffer start)
        (buffer-substring-no-properties (marker-position start) (marker-position end))))))

(defun gptel-otel--finalize (fsm terminal)
  (when-let* ((context (gptel-otel--context fsm))
              ((not (gptel-otel--context-terminal-p context))))
    (setf (gptel-otel--context-terminal-p context) t)
    (when-let* ((generation (gptel-otel--context-current-generation context)))
      (gptel-otel--finish-generation fsm))
    ;; Terminal fallback for generations displaced by unusual FSM paths,
    ;; reloads, aborts, or callback failures.
    (gptel-otel--reconcile-generation-spans
     fsm (format "request terminated in %s" terminal))
    ;; Confirmation can be canceled and transport errors can terminate a
    ;; request before every tool reaches `gptel--process-tool-call'.  Closing
    ;; those spans here prevents one abandoned call from suppressing the trace.
    (gptel-otel--reconcile-tool-spans fsm terminal)
    (let ((root (gptel-otel--context-root context)) (info (gptel-fsm-info fsm)))
      ;; Child contexts share an agent span as root; its callback owns that span.
      (when (eq root (gptel-otel-trace-root (gptel-otel--context-trace context)))
        (gptel-otel--set-semantic-attributes
         root :kind 'root-finish :type "span"
         :input (gptel-otel--latest-user-input (plist-get info :data))
         :output (gptel-otel--response-text info) :info info)
        (gptel-otel-end-span root (if (eq terminal 'DONE) (gptel-otel-status-ok)
                                   (gptel-otel-status-error (or (plist-get info :status)
                                                               (format "%s" terminal)))))
        (setf (gptel-otel-trace-terminal-p (gptel-otel--context-trace context)) t)
        (gptel-otel--maybe-export (gptel-otel--context-trace context)))))
  ;; Inhibited requests have no context to release, but their captured
  ;; decision must not outlive their terminal FSM.
  (unless (gptel-otel--context fsm)
    (remhash fsm gptel-otel--request-decisions)))

(defun gptel-otel--release-trace-state (trace spans)
  "Release completed TRACE and SPANS state after durable enqueue."
  (maphash (lambda (fsm context)
             (when (eq trace (gptel-otel--context-trace context))
               (maphash (lambda (call _span)
                          (remhash (plist-get call :args)
                                   gptel-otel--agent-bindings))
                        (gptel-otel--context-tool-spans context))
               (remhash fsm gptel-otel--contexts)
               (remhash fsm gptel-otel--request-decisions)))
           gptel-otel--contexts)
  (dolist (span spans) (remhash span gptel-otel--span-data))
  (setf (gptel-otel-trace-spans trace) nil))

(defun gptel-otel--maybe-export (trace)
  "Durably enqueue ended, unexported spans from TRACE.
Child observations are exported incrementally.  A terminal root is exported
after all outstanding operations are reconciled."
  (let ((spans
         (cl-remove-if-not
          (lambda (span)
            (and (gptel-otel-span-ended-p span)
                 (not (gptel-otel-span-exported-p span))
                 (or (not (eq span (gptel-otel-trace-root trace)))
                     (and (gptel-otel-trace-terminal-p trace)
                          (zerop (gptel-otel-trace-outstanding trace))))))
          (gptel-otel-trace-spans trace))))
    (when (and spans (gptel-otel-enqueue-spans spans))
      (dolist (span spans)
        (setf (gptel-otel-span-exported-p span) t)
        (remhash span gptel-otel--span-data))
      (when (and (gptel-otel-trace-terminal-p trace)
                 (zerop (gptel-otel-trace-outstanding trace))
                 (gptel-otel-span-exported-p (gptel-otel-trace-root trace)))
        (setf (gptel-otel-trace-queued-p trace) t)
        (gptel-otel--release-trace-state
         trace (gptel-otel-trace-spans trace))))))

(defun gptel-otel--export-trace (trace)
  "Export all currently eligible observations from TRACE."
  (unless (gptel-otel-trace-queued-p trace)
    (gptel-otel--maybe-export trace)))

;;;###autoload
(defun gptel-otel-cleanup-stale-contexts ()
  "Reconcile and export retained contexts whose FSMs are terminal.
This is safe to run after reloads or interrupted callbacks.  Active requests
are left unchanged.  Return the number of terminal contexts examined."
  (interactive)
  (let (terminal traces (count 0))
    (maphash
     (lambda (fsm context)
       (when (or (gptel-otel--context-terminal-p context)
                 (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
         (push (cons fsm context) terminal)))
     gptel-otel--contexts)
    (dolist (entry terminal)
      (pcase-let ((`(,fsm . ,context) entry))
        (cl-incf count)
        (gptel-otel--reconcile-generation-spans
         fsm "Stale terminal generation")
        (gptel-otel--reconcile-tool-spans fsm (gptel-fsm-state fsm))
        (let* ((trace (gptel-otel--context-trace context))
               (root (gptel-otel--context-root context)))
          ;; Only the real trace root is finalized here.  Child contexts share
          ;; an agent span whose callback owns its terminal output/status.
          (when (and (eq root (gptel-otel-trace-root trace))
                     (not (gptel-otel-span-ended-p root)))
            (let ((info (gptel-fsm-info fsm)))
              (gptel-otel--set-semantic-attributes
               root :kind 'root-finish :type "span"
               :input (gptel-otel--latest-user-input (plist-get info :data))
               :output (gptel-otel--response-text info) :info info)
              (gptel-otel-end-span
               root
               (if (eq (gptel-fsm-state fsm) 'DONE)
                   (gptel-otel-status-ok)
                 (gptel-otel-status-error
                  (or (plist-get info :status)
                      (format "%s" (gptel-fsm-state fsm))))))
              (setf (gptel-otel-trace-terminal-p trace) t))))
        (cl-pushnew (gptel-otel--context-trace context) traces :test #'eq)))
    (dolist (trace traces)
      (gptel-otel--maybe-export trace))
    (when (called-interactively-p 'interactive)
      (message "gptel-otel examined %d terminal context%s"
               count (if (= count 1) "" "s")))
    count))

(require 'gptel-otel-adapter)
(require 'gptel-otel-agent-adapter)

(defun gptel-otel--mode-reconcile-prompt (base)
  "Reconcile request-root hook with active required base lifecycle BASE."
  (remove-hook 'gptel-prompt-transform-functions #'gptel-otel--instrument-request)
  (when (and gptel-otel-mode (plist-get base 'base))
    (add-hook 'gptel-prompt-transform-functions #'gptel-otel--instrument-request t)))

;;;###autoload
(defun gptel-otel-compatibility-status ()
  "Display and return bounded private-upstream compatibility diagnostics.
The returned plist reports whether required base tracing, optional tool
tracing, and optional gptel-agent correlation can be installed coherently."
  (interactive)
  (let* ((base (gptel-otel--base-adapter-capabilities))
         (agent (gptel-otel--agent-adapter-status base))
         (base-compatible (plist-get base 'base))
         (tool-compatible (and base-compatible (plist-get (plist-get base 'tool) :supported)))
         (base-active (and gptel-otel-mode base-compatible
                           (gptel-otel--base-adapter-active-p)
                           (memq #'gptel-otel--instrument-request
                                 (default-value 'gptel-prompt-transform-functions)) t))
         (tool-active (and base-active tool-compatible
                           (gptel-otel--base-adapter-active-p t)))
         (agent-active (and tool-active (plist-get agent :supported)
                            (gptel-otel--seams-active-p gptel-otel--agent-seams)))
         (failure (or (cl-find-if-not (lambda (s) (plist-get s :supported))
                                      (plist-get (plist-get base 'root) :seams))
                      (cl-find-if-not (lambda (s) (plist-get s :supported))
                                      (plist-get (plist-get base 'generation) :seams))
                      (cl-find-if-not (lambda (s) (plist-get s :supported))
                                      (plist-get (plist-get base 'tool) :seams))
                      (and (plist-get agent :available)
                           (cl-find-if-not (lambda (s) (plist-get s :supported))
                                           (plist-get agent :seams)))))
         (status (list :mode gptel-otel-mode
                       :runtime-emacs emacs-version :runtime-gptel (and (boundp 'gptel-version) gptel-version)
                       :base-compatible base-compatible :base-active base-active
                       :tool-compatible tool-compatible :tool-active tool-active
                       :agent-available (plist-get agent :available)
                       :agent-compatible (plist-get agent :supported) :agent-active agent-active
                       :agent-reason (unless (plist-get agent :available) "optional gptel-agent not loaded")
                       :failed-symbol (plist-get failure :symbol)
                       :failed-expected (plist-get failure :expected)
                       :failed-actual (plist-get failure :actual)
                       ;; Compatibility aliases retained for callers of the
                       ;; initial diagnostics API; these mean compatibility.
                       :base base-compatible :tool tool-compatible
                       :agent (plist-get agent :supported))))
      (when (called-interactively-p 'interactive)
        (message "gptel-otel: base=%s tool=%s agent=%s%s"
                 base-active tool-active
                 (if (plist-get status :agent-available)
                     agent-active "unavailable")
                 (if (plist-get status :agent-available) "" " (optional gptel-agent not loaded)")))
      status))

;;;###autoload
(define-minor-mode gptel-otel-mode
  "Globally instrument gptel while containing all telemetry failures."
  :global t :group 'gptel-otel
  (if gptel-otel-mode
      (let ((base (gptel-otel--base-adapter-install)))
        (gptel-otel--mode-reconcile-prompt base)
        (gptel-otel--agent-adapter-install base)
        (gptel-otel-cleanup-stale-contexts)
        (gptel-otel-replay))
    (remove-hook 'gptel-prompt-transform-functions #'gptel-otel--instrument-request)
    (gptel-otel--agent-adapter-uninstall)
    (gptel-otel--base-adapter-uninstall)))

(provide 'gptel-otel)
;;; gptel-otel.el ends here
