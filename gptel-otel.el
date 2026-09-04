;;; gptel-otel.el --- OpenTelemetry instrumentation for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9"))

;;; Commentary:
;; Root, generation, tool and subagent spans for guarded gptel seams.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'gptel)
(require 'gptel-otel-core)
(require 'gptel-otel-transport)

(defcustom gptel-otel-capture-payloads t
  "When non-nil retain complete logical inputs and outputs; never truncate."
  :type 'boolean :group 'gptel-otel)
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
(defvar gptel-otel--parent-fsm nil)
(defvar gptel-otel--pending-agent nil)
(defvar gptel-otel--agent-bindings (make-hash-table :test #'eq))
(defvar gptel-otel--next-agent-execution nil)
(defvar gptel-otel--user-turn nil)
(defvar gptel-otel--installed-advices nil)

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
                                                :cache_read_input_tokens)))
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
           ;; Portable message fields apply only to model generations.  Keep
           ;; Langfuse's complete payload attributes on all observation kinds.
           (and (equal type "generation") gptel-otel-capture-payloads input
                (list (cons "gen_ai.input.messages"
                            (gptel-otel-value-array
                             (gptel-otel-value-string
                              (gptel-otel--serialize input))))))
           (and (equal type "generation") gptel-otel-capture-payloads output
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
         (root-name (or (plist-get metadata :name) "gptel.chat"))
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

(defun gptel-otel--instrument-request (fsm)
  "Public prompt transformer establishing one root per request."
  (condition-case err
      (when (and gptel-otel--user-turn
                 (not (gptel-otel--context fsm))
                 (not gptel-otel--pending-agent))
        (gptel-otel--new-context fsm))
    (error (display-warning 'gptel-otel (format "Root instrumentation failed: %s" err) :warning)))
  nil)

(defun gptel-otel--around-send (orig &rest args)
  (let ((gptel-otel--user-turn t)) (apply orig args)))

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
      (let* ((parent-context (and gptel-otel--parent-fsm
                                  (gptel-otel--context gptel-otel--parent-fsm)))
             (agent gptel-otel--pending-agent)
             (context (or (gptel-otel--context fsm)
                          (and agent parent-context
                               (gptel-otel--new-context fsm parent-context agent))
                          (gptel-otel--new-context fsm)))
             (info (gptel-fsm-info fsm))
             (parent (or agent (gptel-otel--context-root context)))
             (input (plist-get info :data))
             (attrs (gptel-otel--observation-attributes context "generation" input nil))
             (span (gptel-otel-trace-start-span
                    (gptel-otel--context-trace context) "gptel.generation" parent attrs))
             (callback (plist-get info :callback)))
        (setf (gptel-otel--context-current-generation context) span
              (gptel-otel--context-generations context)
              (append (gptel-otel--context-generations context) (list span)))
        (when (functionp callback)
          (plist-put info :callback
                     (apply-partially #'gptel-otel--generation-callback span callback))))
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
      (setf (gptel-otel--context-current-generation context) nil))))

(defun gptel-otel--around-transition (orig machine &optional new-state)
  (let ((old-state (gptel-fsm-state machine)) result)
    (condition-case err
        (setq result (funcall orig machine new-state))
      (error
       (condition-case nil
           (progn (when (eq old-state 'TYPE) (gptel-otel--finish-generation machine))
                  (gptel-otel--finalize machine 'error))
         (error nil))
       (signal (car err) (cdr err))))
    (condition-case telemetry-error
        (progn
          (when (eq old-state 'TYPE) (gptel-otel--finish-generation machine))
          (when (memq (gptel-fsm-state machine) '(DONE ERRS ABRT))
            (gptel-otel--finalize machine (gptel-fsm-state machine))))
      (error (display-warning 'gptel-otel
                              (format "Transition telemetry failed: %s" telemetry-error) :warning)))
    result))

(defun gptel-otel--before-pre-tool (fsm)
  (condition-case err
      (when-let* ((context (gptel-otel--context fsm)))
        (dolist (call (plist-get (gptel-fsm-info fsm) :tool-use))
          (unless (or (plist-get call :result)
                      (gethash call (gptel-otel--context-tool-spans context)))
            (let ((span (gptel-otel-trace-start-span
                         (gptel-otel--context-trace context)
                         (format "tool.%s" (plist-get call :name))
                         (or (car (last (gptel-otel--context-generations context)))
                             (gptel-otel--context-root context))
                         (append (gptel-otel--observation-attributes
                                  context "tool" (plist-get call :args) nil call)
                                 (and (plist-get call :id)
                                      `(("tool.call.id" .
                                         ,(gptel-otel-value-string (plist-get call :id)))))))))
              (puthash call span (gptel-otel--context-tool-spans context))
              (when (equal (plist-get call :name) "Agent")
                ;; The exact argument plist is passed unchanged to
                ;; `gptel--map-tool-args'.  Keying by its identity preserves
                ;; exact concurrent-call attribution without injecting private
                ;; values into provider/tool data.
                (puthash (plist-get call :args) (list fsm call span)
                         gptel-otel--agent-bindings))))))
    (error (display-warning 'gptel-otel (format "Tool start failed: %s" err) :warning))))

(defun gptel-otel--around-process-tool (orig fsm tool-spec tool-call result)
  (let (value)
    (condition-case err
        (setq value (funcall orig fsm tool-spec tool-call result))
      (error
       (gptel-otel--finish-tool fsm tool-call result err)
       (signal (car err) (cdr err))))
    (gptel-otel--finish-tool fsm tool-call result nil)
    value))

(defun gptel-otel--finish-tool (fsm tool-call result error)
  (condition-case telemetry-error
      (when-let* ((context (gptel-otel--context fsm))
                  (span (gethash tool-call (gptel-otel--context-tool-spans context))))
        (gptel-otel--set-semantic-attributes
         span :kind 'observation-finish :type "tool" :output result :tool-call tool-call)
        ;; String results are intentionally not inferred to be exceptions.
        (gptel-otel-trace-end-span
         (gptel-otel--context-trace context) span
         (if (or error (plist-get tool-call :error))
             (gptel-otel-status-error (and error (error-message-string error)))
           (gptel-otel-status-ok)))
        (gptel-otel--maybe-export (gptel-otel--context-trace context)))
    (error (display-warning 'gptel-otel (format "Tool finish failed: %s" telemetry-error) :warning))))

(defun gptel-otel--reconcile-tool-spans (fsm &optional terminal)
  "Finish tool spans for FSM that bypassed the normal result seam.
A call that disappeared from `:tool-use' was consumed internally by gptel
(currently its structured-output ersatz tool).  At TERMINAL, any remaining
open call is conservatively recorded as abandoned.  Nonterminal calls still
present without results are left open because they may be asynchronous or
awaiting confirmation."
  (when-let* ((context (gptel-otel--context fsm)))
    (let ((calls (plist-get (gptel-fsm-info fsm) :tool-use))
          pending)
      (maphash
       (lambda (call span)
         (unless (gptel-otel-span-ended-p span)
           (cond
            ((plist-member call :result)
             (push (list call (plist-get call :result)
                         (and (plist-get call :error)
                              '(error "Tool call marked as failed")))
                   pending))
            ((not (memq call calls))
             ;; gptel's structured-output pseudo-tool is consumed without
             ;; `gptel--process-tool-call'; its arguments are the output.
             (push (list call (plist-get call :args) nil) pending))
            ((and terminal
                  ;; An Agent tool can still be legitimately running after the
                  ;; root request reaches a terminal UI state.  Its child span
                  ;; proves that the tool has not been abandoned yet.
                  (not (cl-some
                        (lambda (child)
                          (and (not (gptel-otel-span-ended-p child))
                               (equal (gptel-otel-span-parent-span-id child)
                                      (gptel-otel-span-span-id span))))
                        (gptel-otel-trace-spans
                         (gptel-otel--context-trace context)))))
             (push (list call nil '(error "Tool call abandoned")) pending)))))
       (gptel-otel--context-tool-spans context))
      (dolist (entry pending)
        (gptel-otel--finish-tool fsm (nth 0 entry) (nth 1 entry) (nth 2 entry))))))

(defun gptel-otel--around-tool-use (orig fsm)
  (let ((gptel-otel--parent-fsm fsm)
        result)
    (unwind-protect
        (setq result (funcall orig fsm))
      ;; Reconcile calls such as gptel's structured-output pseudo-tool that
      ;; are consumed without reaching `gptel--process-tool-call'.
      (condition-case err
          (gptel-otel--reconcile-tool-spans fsm)
        (error (display-warning 'gptel-otel
                                (format "Tool reconciliation failed: %s" err)
                                :warning))))
    result))

(defun gptel-otel--around-map-tool-args (orig tool-spec args)
  "Bind the exact Agent call represented by ARGS immediately before execution."
  (prog1 (funcall orig tool-spec args)
    (when (equal (gptel-tool-name tool-spec) "Agent")
      (setq gptel-otel--next-agent-execution
            (gethash args gptel-otel--agent-bindings)))))

(defun gptel-otel--agent-callback (span context original value)
  (prog1 (funcall original value)
    (condition-case err
        (progn
          (gptel-otel--set-semantic-attributes
           span :kind 'observation-finish :type "agent" :output value)
          (gptel-otel-trace-end-span
           (gptel-otel--context-trace context) span
           (if (and (stringp value) (string-prefix-p "Error:" value))
               (gptel-otel-status-error value)
             (gptel-otel-status-ok)))
          (gptel-otel--maybe-export (gptel-otel--context-trace context)))
      (error (display-warning 'gptel-otel (format "Agent finish failed: %s" err) :warning)))))

(defun gptel-otel--around-agent-task (orig main-cb agent-type description prompt)
  (let* ((execution (prog1 gptel-otel--next-agent-execution
                      (setq gptel-otel--next-agent-execution nil)))
         (parent-fsm (or (car-safe execution) gptel-otel--parent-fsm))
         (parent-context (and parent-fsm (gptel-otel--context parent-fsm)))
         (tool-span (nth 2 execution))
         (span (and parent-context tool-span
                    (gptel-otel-trace-start-span
                     (gptel-otel--context-trace parent-context)
                     (format "agent.%s" agent-type) tool-span
                     (append
                      (gptel-otel--observation-attributes
                       parent-context "agent"
                       (list :type agent-type :description description :prompt prompt) nil)
                      (delq nil
                            (list
                             (gptel-otel--string-attr
                              "langfuse.observation.metadata.subagent_type"
                              agent-type)
                             (gptel-otel--string-attr
                              "langfuse.observation.metadata.description"
                              description)))))))
         (wrapped (if span (apply-partially #'gptel-otel--agent-callback
                                            span parent-context main-cb)
                    main-cb))
         (gptel-otel--pending-agent span)
         child)
    (condition-case err
        (setq child (funcall orig wrapped agent-type description prompt))
      (error (when span
               (gptel-otel-trace-end-span
                (gptel-otel--context-trace parent-context) span
                (gptel-otel-status-error (error-message-string err)))
               (gptel-otel--maybe-export (gptel-otel--context-trace parent-context)))
             (signal (car err) (cdr err))))
    (when (and span child (not (gptel-otel--context child)))
      (gptel-otel--new-context child parent-context span))
    child))

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
    ;; Confirmation can be canceled and transport errors can terminate a
    ;; request before every tool reaches `gptel--process-tool-call'.  Closing
    ;; those spans here prevents one abandoned call from suppressing the trace.
    (gptel-otel--reconcile-tool-spans fsm terminal)
    (let ((root (gptel-otel--context-root context)) (info (gptel-fsm-info fsm)))
      ;; Child contexts share an agent span as root; its callback owns that span.
      (when (eq root (gptel-otel-trace-root (gptel-otel--context-trace context)))
        (gptel-otel--set-semantic-attributes
         root :kind 'root-finish :type "span" :input (plist-get info :data)
         :output (gptel-otel--response-text info) :info info)
        (gptel-otel-end-span root (if (eq terminal 'DONE) (gptel-otel-status-ok)
                                   (gptel-otel-status-error (or (plist-get info :status)
                                                               (format "%s" terminal)))))
        (setf (gptel-otel-trace-terminal-p (gptel-otel--context-trace context)) t)
        (gptel-otel--maybe-export (gptel-otel--context-trace context))))))

(defun gptel-otel--release-trace-state (trace spans)
  "Release completed adapter state for TRACE after durable enqueue."
  (maphash (lambda (fsm context)
             (when (eq trace (gptel-otel--context-trace context))
               (maphash (lambda (call _span)
                          (remhash (plist-get call :args)
                                   gptel-otel--agent-bindings))
                        (gptel-otel--context-tool-spans context))
               (remhash fsm gptel-otel--contexts)))
           gptel-otel--contexts)
  (dolist (span spans) (remhash span gptel-otel--span-data))
  (setf (gptel-otel-trace-spans trace) nil))

(defun gptel-otel--maybe-export (trace)
  "Durably enqueue TRACE once root is terminal and no operations remain."
  (when (and (gptel-otel-trace-terminal-p trace)
             (zerop (gptel-otel-trace-outstanding trace)))
    (gptel-otel--export-trace trace)))

(defun gptel-otel--export-trace (trace)
  (unless (gptel-otel-trace-queued-p trace)
    (let ((spans (cl-remove-if-not #'gptel-otel-span-ended-p
                                   (gptel-otel-trace-spans trace))))
      (when spans
        (when (gptel-otel-enqueue (gptel-otel-export-request spans))
          (dolist (span spans) (setf (gptel-otel-span-exported-p span) t))
          (setf (gptel-otel-trace-queued-p trace) t)
          (gptel-otel--release-trace-state trace spans))))))

(defconst gptel-otel--guarded-seams
  '((gptel-send (&optional arg) gptel-otel--around-send :around root)
    (gptel--handle-wait (fsm) gptel-otel--before-wait :before generation)
    (gptel--fsm-transition (machine &optional new-state) gptel-otel--around-transition :around generation)
    (gptel--handle-pre-tool (fsm) gptel-otel--before-pre-tool :before tool)
    (gptel--process-tool-call (fsm tool-spec tool-call result) gptel-otel--around-process-tool :around tool)
    (gptel--handle-tool-use (fsm) gptel-otel--around-tool-use :around tool)
    (gptel--map-tool-args (tool-spec args) gptel-otel--around-map-tool-args :around tool)
    (gptel-agent--task (main-cb agent-type description prompt) gptel-otel--around-agent-task :around agent)))

(defun gptel-otel--signature-equal-p (symbol expected)
  (and (fboundp symbol) (equal (help-function-arglist symbol t) expected)))

(defun gptel-otel--setup-advice ()
  (dolist (seam gptel-otel--guarded-seams)
    (pcase-let ((`(,symbol ,args ,advice ,where ,layer) seam))
      (when (or (not (eq layer 'agent)) (require 'gptel-agent-tools nil t))
        (if (gptel-otel--signature-equal-p symbol args)
            (unless (advice-member-p advice symbol)
              (advice-add symbol where advice)
              (push (cons symbol advice) gptel-otel--installed-advices))
          (display-warning 'gptel-otel
                           (format "%s instrumentation disabled: %s signature is %S, expected %S"
                                   layer symbol (and (fboundp symbol)
                                                     (help-function-arglist symbol t)) args)
                           :warning))))))

(defun gptel-otel--teardown-advice ()
  (dolist (pair gptel-otel--installed-advices)
    (advice-remove (car pair) (cdr pair)))
  (setq gptel-otel--installed-advices nil))

;;;###autoload
(define-minor-mode gptel-otel-mode
  "Globally instrument gptel while containing all telemetry failures."
  :global t :group 'gptel-otel
  (if gptel-otel-mode
      (progn
        (add-hook 'gptel-prompt-transform-functions #'gptel-otel--instrument-request t)
        (gptel-otel--setup-advice)
        (gptel-otel-replay))
    (remove-hook 'gptel-prompt-transform-functions #'gptel-otel--instrument-request)
    (gptel-otel--teardown-advice)))

(provide 'gptel-otel)
;;; gptel-otel.el ends here
