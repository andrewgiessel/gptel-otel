;;; gptel-otel-test.el --- Instrumentation tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'gptel-otel)

(defmacro gptel-otel-test--isolated (&rest body)
  `(let ((gptel-otel--contexts (make-hash-table :test #'eq))
         (gptel-otel-backend-profile 'langfuse)
         (gptel-otel--span-data (make-hash-table :test #'eq))
         (gptel-otel-trace-metadata-function
          (lambda (_fsm) (list :name "trace" :session-id "session"
                               :user-id "user" :tags ["one" "two"]))))
     (let ((gptel-otel--user-turn t)) ,@body)))

(defun gptel-otel-test--fsm (&optional info state)
  (gptel-make-fsm :info (or info (list :data '(:messages ["input"])
                                        :model 'model :callback #'ignore))
                  :state (or state 'WAIT)))

(ert-deftest gptel-otel-adapter-registers-and-is-idempotent ()
  (unwind-protect
      (progn
        (gptel-otel-mode 1) (gptel-otel-mode 1)
        (should (= 1 (cl-count #'gptel-otel--instrument-request
                               gptel-prompt-transform-functions)))
        (should (advice-member-p #'gptel-otel--around-transition
                                 'gptel--fsm-transition)))
    (gptel-otel-mode -1))
  (should-not (advice-member-p #'gptel-otel--around-transition
                               'gptel--fsm-transition)))

(ert-deftest gptel-otel-signature-guard-degrades-one-layer ()
  (let ((gptel-otel--installed-advices nil)
        (gptel-otel--guarded-seams
         '((gptel--handle-wait (wrong) gptel-otel--before-wait :before generation))))
    (gptel-otel--setup-advice)
    (should-not gptel-otel--installed-advices)))

(ert-deftest gptel-otel-generation-loop-and-per-generation-usage ()
  (gptel-otel-test--isolated
   (let* ((info (list :data '(:messages ["hello"]) :model 'm
                      :tokens '(:input 3 :output 4) :callback #'ignore))
          (fsm (gptel-otel-test--fsm info 'WAIT)))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-wait fsm)
     (let ((first (gptel-otel--context-current-generation (gptel-otel--context fsm))))
       (gptel-otel--generation-callback first #'ignore "answer" info)
       (gptel-otel--finish-generation fsm)
       (should (gptel-otel-span-ended-p first))
       (should (assoc "langfuse.observation.usage_details"
                      (gptel-otel-span-attributes first))))
     (gptel-otel--before-wait fsm)
     (should (= 2 (length (gptel-otel--context-generations
                           (gptel-otel--context fsm))))))))

(ert-deftest gptel-otel-langfuse-preset-emits-langfuse-and-portable-attributes ()
  (gptel-otel-test--isolated
   (let* ((info (list :data '(:messages ["hello"]) :model 'model-x
                      :tokens '(:input 3 :output 4 :cache_read 2)
                      :callback #'ignore))
          (fsm (gptel-otel-test--fsm info 'WAIT)))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-wait fsm)
     (let ((span (gptel-otel--context-current-generation
                  (gptel-otel--context fsm))))
       (gptel-otel--generation-callback span #'ignore "answer" info)
       (gptel-otel--finish-generation fsm)
       (dolist (key '("langfuse.observation.output"
                      "langfuse.observation.model.name"
                      "langfuse.observation.usage_details"
                      "gen_ai.operation.name"
                      "gen_ai.request.model"
                      "gen_ai.input.messages"
                      "gen_ai.output.messages"
                      "gen_ai.usage.input_tokens"
                      "gen_ai.usage.output_tokens"
                      "gen_ai.usage.cache_read_input_tokens"))
         (should (assoc key (gptel-otel-span-attributes span))))))))

(ert-deftest gptel-otel-custom-semantic-provider-is-used-by-lifecycle ()
  (gptel-otel-test--isolated
   (let ((gptel-otel-attribute-provider-functions
          (list (lambda (event)
                  (list (cons "community.kind"
                              (gptel-otel-value-string (plist-get event :kind))))))))
     (let ((fsm (gptel-otel-test--fsm)))
       (gptel-otel--instrument-request fsm)
       (should (assoc "community.kind"
                      (gptel-otel-span-attributes
                       (gptel-otel--context-root (gptel-otel--context fsm)))))))))

(ert-deftest gptel-otel-generation-callback-forwards-optional-raw ()
  (gptel-otel-test--isolated
   (let* ((fsm (gptel-otel-test--fsm))
          (context (progn (gptel-otel--instrument-request fsm)
                          (gptel-otel--context fsm)))
          (span (gptel-otel-trace-start-span
                 (gptel-otel--context-trace context) "generation"
                 (gptel-otel--context-root context) nil))
          seen)
     (should (eq 'returned
                 (gptel-otel--generation-callback
                  span (lambda (response info &optional raw)
                         (setq seen (list response info raw))
                         'returned)
                  "answer" (gptel-fsm-info fsm) '(:raw t))))
     (should (equal '(:raw t) (nth 2 seen))))))

(ert-deftest gptel-otel-generation-callback-preserves-two-argument-arity ()
  (gptel-otel-test--isolated
   (let* ((fsm (gptel-otel-test--fsm))
          (context (progn (gptel-otel--instrument-request fsm)
                          (gptel-otel--context fsm)))
          (span (gptel-otel-trace-start-span
                 (gptel-otel--context-trace context) "generation"
                 (gptel-otel--context-root context) nil))
          seen)
     (should (eq 'returned
                 (gptel-otel--generation-callback
                  span (lambda (response info)
                         (setq seen (list response info))
                         'returned)
                  "answer" (gptel-fsm-info fsm))))
     (should (= 2 (length seen))))))

(ert-deftest gptel-otel-generation-callback-captures-structured-tool-output ()
  (gptel-otel-test--isolated
   (let* ((fsm (gptel-otel-test--fsm))
          (context (progn (gptel-otel--instrument-request fsm)
                          (gptel-otel--context fsm)))
          (span (gptel-otel-trace-start-span
                 (gptel-otel--context-trace context) "generation"
                 (gptel-otel--context-root context) nil))
          (response '(tool-call . ((:id "call-1" :name "Bash")))))
     (gptel-otel--generation-callback span #'ignore response (gptel-fsm-info fsm))
     (should (equal response (gptel-otel-span-get-output span))))))

(ert-deftest gptel-otel-parallel-same-name-tools-use-object-identity ()
  (gptel-otel-test--isolated
   (let* ((call-a (list :id "a" :name "Bash" :args '(:x 1)))
          (call-b (list :id "b" :name "Bash" :args '(:x 2)))
          (info (list :data nil :model 'm :callback #'ignore
                      :tool-use (list call-a call-b)))
          (fsm (gptel-otel-test--fsm info)))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-pre-tool fsm)
     (let* ((table (gptel-otel--context-tool-spans (gptel-otel--context fsm)))
            (span-a (gethash call-a table)) (span-b (gethash call-b table)))
       (should span-a) (should span-b) (should-not (eq span-a span-b))
       (gptel-otel--finish-tool fsm call-a "one" nil)
       (should (gptel-otel-span-ended-p span-a))
       (should-not (gptel-otel-span-ended-p span-b))
       (gptel-otel--finish-tool fsm call-b "two" nil)
       (should (gptel-otel-span-ended-p span-b))))))

(ert-deftest gptel-otel-reconciles-disappeared-structured-output-tool ()
  (gptel-otel-test--isolated
   (let* ((call (list :id "json" :name "gptel--json" :args '(:answer 42)))
          (info (list :data nil :model 'm :callback #'ignore
                      :tool-use (list call)))
          (fsm (gptel-otel-test--fsm info)))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-pre-tool fsm)
     (let* ((context (gptel-otel--context fsm))
            (trace (gptel-otel--context-trace context))
            (span (gethash call (gptel-otel--context-tool-spans context))))
       (plist-put info :tool-use nil)
       (gptel-otel--reconcile-tool-spans fsm)
       (should (gptel-otel-span-ended-p span))
       (should (= 0 (gptel-otel-trace-outstanding trace)))))))

(ert-deftest gptel-otel-terminal-finalizes-abandoned-tool ()
  (gptel-otel-test--isolated
   (let* ((call (list :id "confirm" :name "Bash" :args '(:command "true")))
          (info (list :data nil :model 'm :callback #'ignore
                      :tool-use (list call) :status "aborted"))
          (fsm (gptel-otel-test--fsm info 'ABRT))
          queued)
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-pre-tool fsm)
     (let* ((context (gptel-otel--context fsm))
            (span (gethash call (gptel-otel--context-tool-spans context))))
       (cl-letf (((symbol-function 'gptel-otel-enqueue)
                  (lambda (_request) (setq queued t) "queued")))
         (gptel-otel--finalize fsm 'ABRT)
         (should queued)
         (should (gptel-otel-span-ended-p span))
         (should (= 2 (cdr (assq 'code (gptel-otel-span-status span))))))))))

(ert-deftest gptel-otel-nonterminal-reconcile-preserves-pending-async-tool ()
  (gptel-otel-test--isolated
   (let* ((call (list :id "async" :name "Agent" :args nil))
          (info (list :data nil :model 'm :callback #'ignore
                      :tool-use (list call)))
          (fsm (gptel-otel-test--fsm info)))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-pre-tool fsm)
     (let* ((context (gptel-otel--context fsm))
            (trace (gptel-otel--context-trace context))
            (span (gethash call (gptel-otel--context-tool-spans context))))
       (gptel-otel--reconcile-tool-spans fsm)
       (should-not (gptel-otel-span-ended-p span))
       (should (> (gptel-otel-trace-outstanding trace) 0))))))

(ert-deftest gptel-otel-subagent-joins-trace-and-nests-under-tool ()
  (gptel-otel-test--isolated
   (let* ((call (list :id "agent-1" :name "Agent" :args nil))
          (parent (gptel-otel-test--fsm
                   (list :data nil :model 'm :callback #'ignore :tool-use (list call))))
          returned child callback-result)
     (gptel-otel--instrument-request parent)
     (gptel-otel--before-pre-tool parent)
     (let* ((context (gptel-otel--context parent))
            (tool-span (gethash call (gptel-otel--context-tool-spans context)))
            (gptel-otel--parent-fsm parent)
            (gptel-otel--next-agent-execution (list parent call tool-span)))
       (setq returned
             (gptel-otel--around-agent-task
              (lambda (cb _type _desc _prompt)
                (setq child (gptel-otel-test--fsm))
                (funcall cb "done") child)
              (lambda (value) (setq callback-result value))
              "researcher" "desc" "prompt"))
       (should (eq returned child))
       (should (equal "done" callback-result))
       (let ((child-context (gptel-otel--context child)))
         (should child-context)
         (should (eq (gptel-otel--context-trace context)
                     (gptel-otel--context-trace child-context)))
         (should (equal (gptel-otel-span-span-id tool-span)
                        (gptel-otel-span-parent-span-id
                         (gptel-otel--context-root child-context)))))))))

(ert-deftest gptel-otel-error-abort-and-export-once ()
  (gptel-otel-test--isolated
   (let* ((fsm (gptel-otel-test--fsm (list :data "in" :model 'm :callback #'ignore
                                            :status "aborted") 'ABRT))
          count)
     (gptel-otel--instrument-request fsm)
     (let* ((context (progn (gptel-otel--instrument-request fsm)
                            (gptel-otel--context fsm)))
            (root (gptel-otel--context-root context)))
       (cl-letf (((symbol-function 'gptel-otel-enqueue)
                  (lambda (_request) (setq count (1+ (or count 0))) "queued")))
         (gptel-otel--finalize fsm 'ABRT)
         (gptel-otel--finalize fsm 'ABRT)
         (should (= 1 count))
         (should (equal 2 (cdr (assq 'code (gptel-otel-span-status root))))))))))

(ert-deftest gptel-otel-root-waits-for-late-agent-callback-and-cleans-up ()
  (gptel-otel-test--isolated
   (let* ((call (list :id "agent" :name "Agent" :args nil))
          (parent (gptel-otel-test--fsm
                   (list :data nil :model 'm :callback #'ignore :tool-use (list call))))
          callback child request)
     (gptel-otel--instrument-request parent)
     (gptel-otel--before-pre-tool parent)
     (let* ((context (gptel-otel--context parent))
            (trace (gptel-otel--context-trace context))
            (tool-span (gethash call (gptel-otel--context-tool-spans context)))
            (gptel-otel--parent-fsm parent)
            (gptel-otel--next-agent-execution (list parent call tool-span)))
       (setq child
             (gptel-otel--around-agent-task
              (lambda (cb _type _desc _prompt)
                (setq callback cb) (gptel-otel-test--fsm))
              #'ignore "researcher" "desc" "prompt"))
       (cl-letf (((symbol-function 'gptel-otel-enqueue)
                  (lambda (value) (setq request value) "queued")))
         (gptel-otel--finalize parent 'DONE)
         (should-not request)
         (funcall callback "done")
         (should-not request)
         (gptel-otel--finish-tool parent call "done" nil)
         (should request)
         (should (gptel-otel-trace-queued-p trace))
         (should-not (gptel-otel--context parent))
         (should-not (gptel-otel--context child))
         (should-not (gptel-otel-trace-spans trace))
         (should (= 0 (hash-table-count gptel-otel--span-data))))))))

(ert-deftest gptel-otel-concurrent-agent-calls-use-exact-tool-spans ()
  (gptel-otel-test--isolated
   (let* ((call-a (list :id "a" :name "Agent" :args (list :prompt "a")))
          (call-b (list :id "b" :name "Agent" :args (list :prompt "b")))
          (parent (gptel-otel-test--fsm
                   (list :data nil :model 'm :callback #'ignore
                         :tool-use (list call-a call-b)))))
     (gptel-otel--instrument-request parent)
     (gptel-otel--before-pre-tool parent)
     (let* ((context (gptel-otel--context parent))
            (table (gptel-otel--context-tool-spans context))
            (tool-a (gethash call-a table)) (tool-b (gethash call-b table))
            child-a child-b)
       (let ((agent-tool (gptel-make-tool :name "Agent" :function #'ignore)))
         (gptel-otel--around-map-tool-args #'list agent-tool (plist-get call-a :args))
         (setq child-a (gptel-otel--around-agent-task
                        (lambda (_cb &rest _) (gptel-otel-test--fsm))
                        #'ignore "same" "a" "a"))
         (gptel-otel--around-map-tool-args #'list agent-tool (plist-get call-b :args))
         (setq child-b (gptel-otel--around-agent-task
                        (lambda (_cb &rest _) (gptel-otel-test--fsm))
                        #'ignore "same" "b" "b")))
       (should (equal (gptel-otel-span-span-id tool-a)
                      (gptel-otel-span-parent-span-id
                       (gptel-otel--context-root (gptel-otel--context child-a)))))
       (should (equal (gptel-otel-span-span-id tool-b)
                      (gptel-otel-span-parent-span-id
                       (gptel-otel--context-root (gptel-otel--context child-b)))))))))

(ert-deftest gptel-otel-nested-agent-lineage-is-exact ()
  (gptel-otel-test--isolated
   (let* ((outer-call (list :id "outer" :name "Agent" :args nil))
          (parent (gptel-otel-test--fsm
                   (list :data nil :model 'm :callback #'ignore :tool-use (list outer-call)))))
     (gptel-otel--instrument-request parent)
     (gptel-otel--before-pre-tool parent)
     (let* ((parent-context (gptel-otel--context parent))
            (outer-tool (gethash outer-call (gptel-otel--context-tool-spans parent-context)))
            (gptel-otel--parent-fsm parent)
            (gptel-otel--next-agent-execution (list parent outer-call outer-tool))
            (child (gptel-otel--around-agent-task
                    (lambda (_cb &rest _) (gptel-otel-test--fsm))
                    #'ignore "outer" "outer" "outer")))
       (let* ((inner-call (list :id "inner" :name "Agent" :args nil))
              (child-info (gptel-fsm-info child)))
         (plist-put child-info :tool-use (list inner-call))
         (gptel-otel--before-pre-tool child)
         (let* ((child-context (gptel-otel--context child))
                (inner-tool (gethash inner-call
                                     (gptel-otel--context-tool-spans child-context)))
                grandchild)
           (let ((agent-tool (gptel-make-tool :name "Agent" :function #'ignore)))
             (gptel-otel--around-map-tool-args
              #'list agent-tool (plist-get inner-call :args))
             (setq grandchild
                   (gptel-otel--around-agent-task
                    (lambda (_cb &rest _) (gptel-otel-test--fsm))
                    #'ignore "inner" "inner" "inner")))
           (should (equal (gptel-otel-span-span-id inner-tool)
                          (gptel-otel-span-parent-span-id
                           (gptel-otel--context-root
                            (gptel-otel--context grandchild)))))))))))

(ert-deftest gptel-otel-preserves-upstream-return-and-errors ()
  (let ((fsm (gptel-otel-test--fsm nil 'TYPE)))
    (should (eq 'return
                (gptel-otel--around-transition (lambda (&rest _) 'return) fsm nil)))
    (should-error (gptel-otel--around-transition
                   (lambda (&rest _) (error "upstream")) fsm nil))))

(ert-deftest gptel-otel-telemetry-failure-is-contained ()
  (gptel-otel-test--isolated
   (let ((fsm (gptel-otel-test--fsm)))
     (gptel-otel--instrument-request fsm)
     (cl-letf (((symbol-function 'gptel-otel-trace-start-span)
                (lambda (&rest _) (error "telemetry"))))
       (should-not (gptel-otel--before-wait fsm))))))

(ert-deftest gptel-otel-agent-correlation-does-not-mutate-tool-arguments ()
  (gptel-otel-test--isolated
   (let* ((args (list :subagent-type "introspector" :description "inspect"
                      :prompt "test"))
          (original (copy-tree args))
          (call (list :id "agent" :name "Agent" :args args))
          (fsm (gptel-otel-test--fsm
                (list :data nil :model 'm :callback #'ignore
                      :tool-use (list call)))))
     (gptel-otel--instrument-request fsm)
     (gptel-otel--before-pre-tool fsm)
     (should (equal original (plist-get call :args)))
     (should-not (plist-member (plist-get call :args) :gptel-otel-token)))))

(provide 'gptel-otel-test)
