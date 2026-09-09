;;; gptel-otel-integration-test.el --- Stock lifecycle contracts -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-otel-test)
(require 'gptel-openai)

(defvar gptel-otel-integration--pending nil)
(defvar gptel-otel-integration--exported nil)

(defmacro gptel-otel-integration--with-request (&rest body)
  "Exercise real request/FSM code, replacing only network and export edges."
  (declare (indent 0))
  `(gptel-otel-test--isolated
     (let ((gptel-otel-integration--pending nil)
           (gptel-otel-integration--exported nil)
           (gptel--known-backends nil)
           (gptel-use-curl t)
           (gptel-stream t)
           (gptel-use-tools t)
           (gptel-use-context nil)
           (gptel-confirm-tool-calls nil)
           (gptel-prompt-transform-functions nil)
           (gptel-pre-tool-call-functions nil)
           (gptel-post-tool-call-functions nil)
           (gptel-post-request-hook nil))
       (cl-letf (((symbol-function 'gptel-curl-get-response)
                  (lambda (fsm)
                    ;; Real transport supplies the UI callback when send did
                    ;; not specify one.  Keep this network stub UI-independent.
                    (unless (plist-get (gptel-fsm-info fsm) :callback)
                      (setf (plist-get (gptel-fsm-info fsm) :callback) #'ignore))
                    (push fsm gptel-otel-integration--pending)
                    (gptel--fsm-transition fsm)))
                 ((symbol-function 'gptel-otel-enqueue-spans)
                  (lambda (spans)
                    (setq gptel-otel-integration--exported
                          (append gptel-otel-integration--exported spans))
                    t)))
         (unwind-protect
             (with-temp-buffer
               (setq-local gptel-backend
                           (gptel-make-openai "otel-test" :key "fake-test-key"
                                              :host "invalid.example" :stream t :models '(test-model)))
               (setq-local gptel-model 'test-model)
               (gptel-otel-mode 1)
               (should (plist-get (gptel-otel-compatibility-status) :base-active))
               ,@body)
           (gptel-otel-mode -1))))))

(defun gptel-otel-integration--respond (fsm response &optional tools error)
  "Simulate the parsed transport response; run stock completion transitions."
  (let ((info (gptel-fsm-info fsm)))
    (should (eq (gptel-fsm-state fsm) 'TYPE))
    (setf (plist-get info :tokens) '(:input 3 :output 2)
          (plist-get info :tool-use) tools
          (plist-get info :error) error
          (plist-get info :status) (if error "test failure" "200"))
    (setf (gptel-fsm-info fsm) info)
    (funcall (plist-get info :callback) response info)
    (when (and (plist-get info :stream) (not error))
      (funcall (plist-get info :callback) t info))
    (gptel--fsm-transition fsm)))

(defun gptel-otel-integration--named (prefix)
  (cl-remove-if-not
   (lambda (span) (string-prefix-p prefix (gptel-otel-span-name span)))
   gptel-otel-integration--exported))

(ert-deftest gptel-otel-integration-request-and-stream ()
  (dolist (stream '(nil t))
    (gptel-otel-integration--with-request
      (let* (received
             (fsm (gptel-request "hello" :stream stream
                    :callback (lambda (response _info) (push response received)))))
        (should (memq fsm gptel-otel-integration--pending))
        (gptel-otel-integration--respond fsm "answer")
        (should (eq (gptel-fsm-state fsm) 'DONE))
        (should (equal (reverse received) (if stream '("answer" t) '("answer"))))
        (should (= 2 (length gptel-otel-integration--exported)))
        (should (= 1 (length (gptel-otel-integration--named "chat "))))
        (should (= 0 (hash-table-count gptel-otel--contexts)))))))

(ert-deftest gptel-otel-integration-send-inhibition-is-buffer-local-and-inflight-stable ()
  (gptel-otel-integration--with-request
    ;; `gptel-send' receives its transform list from this originating buffer.
    ;; Exercise its real request/FSM pipeline rather than calling lifecycle
    ;; handlers directly.
    (setq-local gptel-otel-inhibit t)
    (gptel-send)
    (let ((inhibited (car gptel-otel-integration--pending)))
      (should inhibited)
      (should-not (gptel-otel--context inhibited))
      ;; A later local change does not change this already captured decision.
      (setq-local gptel-otel-inhibit nil)
      (gptel-otel-integration--respond inhibited "not traced")
      (should-not gptel-otel-integration--exported))
    (gptel-send)
    (let ((traced (car gptel-otel-integration--pending)))
      (should (gptel-otel--context traced))
      ;; Conversely, inhibiting the buffer after entry cannot suppress an
      ;; in-flight trace.
      (setq-local gptel-otel-inhibit t)
      (gptel-otel-integration--respond traced "still traced")
      (should (= 2 (length gptel-otel-integration--exported))))))

(ert-deftest gptel-otel-integration-async-transform-keeps-send-decision ()
  (gptel-otel-integration--with-request
    (let (continue)
      (setq-local gptel-prompt-transform-functions
                  (list (lambda (callback _fsm) (setq continue callback))
                        #'gptel-otel--instrument-request))
      (let ((gptel-otel-inhibit t))
        (gptel-send))
      (should continue)
      ;; The transform callback runs after the dynamic binding has expired.
      (setq-local gptel-otel-inhibit nil)
      (funcall continue)
      (let ((fsm (car gptel-otel-integration--pending)))
        (should fsm)
        (should-not (gptel-otel--context fsm))
        (gptel-otel-integration--respond fsm "not traced")
        (should-not gptel-otel-integration--exported)))))


(ert-deftest gptel-otel-integration-concurrent-requests ()
  (gptel-otel-integration--with-request
    (let* ((first (gptel-request "first" :callback #'ignore))
           (second (gptel-request "second" :callback #'ignore))
           (first-root (gptel-otel--context-root (gptel-otel--context first)))
           (second-root (gptel-otel--context-root (gptel-otel--context second))))
      (should-not (equal (gptel-otel-span-trace-id first-root)
                         (gptel-otel-span-trace-id second-root)))
      (gptel-otel-integration--respond second "second answer")
      (should (gptel-otel--context first))
      (gptel-otel-integration--respond first "first answer")
      (should (= 4 (length gptel-otel-integration--exported)))
      (dolist (root (list first-root second-root))
        (let ((generation
               (cl-find (gptel-otel-span-trace-id root)
                        (gptel-otel-integration--named "chat ")
                        :key #'gptel-otel-span-trace-id :test #'equal)))
          (should (equal (gptel-otel-span-parent-span-id generation)
                         (gptel-otel-span-span-id root)))))
      (should (= 0 (hash-table-count gptel-otel--contexts))))))

(ert-deftest gptel-otel-integration-tool-loop-sync-and-async ()
  (dolist (async '(nil t))
    (gptel-otel-integration--with-request
      (let* (continuation
             (gptel--known-tools nil)
             (gptel-tools
              (list (gptel-make-tool
                     :name "echo" :description "Echo test input" :async async
                     :args '((:name "text" :type string :description "Input"))
                     :function (if async
                                   (lambda (cb text) (setq continuation (lambda () (funcall cb text))))
                                 #'identity))))
             ;; Use gptel's UI lifecycle to include its real pre-tool handler.
             (fsm (gptel-make-fsm :table gptel-send--transitions
                                  :handlers gptel-send--handlers)))
        (gptel-request "echo this" :fsm fsm :callback #'ignore)
        (gptel-otel-integration--respond
         fsm nil (list (list :id "echo-1" :name "echo" :args '(:text "hello"))))
        (when async
          (should continuation)
          (funcall continuation))
        (should (eq (gptel-fsm-state fsm) 'TYPE))
        (should (= 2 (length gptel-otel-integration--pending)))
        (gptel-otel-integration--respond fsm "done")
        (should (eq (gptel-fsm-state fsm) 'DONE))
        (should (= 2 (length (gptel-otel-integration--named "chat "))))
        (should (= 1 (length (gptel-otel-integration--named "execute_tool "))))
        (should (= 0 (hash-table-count gptel-otel--contexts)))))))

(ert-deftest gptel-otel-integration-error-and-abort ()
  (dolist (terminal '(ERRS ABRT))
    (gptel-otel-integration--with-request
      (let ((fsm (gptel-request "hello" :callback #'ignore)))
        (if (eq terminal 'ERRS)
            (gptel-otel-integration--respond fsm nil nil '(error "test failure"))
          (let ((gptel--request-alist (list (cons 'test-transport (cons fsm #'ignore)))))
            (gptel-abort (current-buffer))))
        (should (eq (gptel-fsm-state fsm) terminal))
        (should (= 2 (length gptel-otel-integration--exported)))
        (should (cl-every #'gptel-otel-span-ended-p gptel-otel-integration--exported))
        (should (= 0 (hash-table-count gptel-otel--contexts)))))))

(ert-deftest gptel-otel-integration-inhibit-isolates-concurrent-buffers ()
  (gptel-otel-integration--with-request
    (setq-local gptel-otel-inhibit t)
    (gptel-send)
    (let ((suppressed (car gptel-otel-integration--pending))
          (backend gptel-backend))
      (with-temp-buffer
        (setq-local gptel-backend backend)
        (setq-local gptel-model 'test-model)
        (should-not gptel-otel-inhibit)
        (gptel-send)
        (let ((traced (car gptel-otel-integration--pending)))
          (should (gptel-otel--context traced))
          ;; Finish the inhibited request in a different, non-inhibited buffer.
          (gptel-otel-integration--respond suppressed "private")
          (should-not gptel-otel-integration--exported)
          (gptel-otel-integration--respond traced "public")
          (should (= 2 (length gptel-otel-integration--exported)))))
      (should (= 0 (hash-table-count gptel-otel--request-decisions))))))

(ert-deftest gptel-otel-integration-inhibited-send-tool-loop ()
  (dolist (async '(nil t))
    (gptel-otel-integration--with-request
      (let* (continuation
             (gptel--known-tools nil)
             (gptel-tools
              (list (gptel-make-tool
                     :name "echo" :description "Echo input" :async async
                     :args '((:name "text" :type string :description "Input"))
                     :function (if async
                                   (lambda (cb text)
                                     (setq continuation (lambda () (funcall cb text))))
                                 #'identity)))))
        (setq-local gptel-otel-inhibit t)
        (gptel-send)
        (let ((fsm (car gptel-otel-integration--pending)))
          (setq-local gptel-otel-inhibit nil)
          (gptel-otel-integration--respond
           fsm nil (list (list :id "echo-1" :name "echo" :args '(:text "hello"))))
          (when async
            (should continuation)
            (funcall continuation))
          (should (eq (gptel-fsm-state fsm) 'TYPE))
          (should-not (gptel-otel--context fsm))
          (gptel-otel-integration--respond fsm "done")
          (should (eq (gptel-fsm-state fsm) 'DONE))
          (should-not gptel-otel-integration--exported)
          (should (= 0 (hash-table-count gptel-otel--request-decisions))))))))

(ert-deftest gptel-otel-integration-inhibited-agent-inherits-before-and-after-return ()
  (unless (featurep 'gptel-agent) (ert-skip "gptel-agent is optional"))
  (dolist (async '(nil t))
    (gptel-otel-integration--with-request
      (setq-local gptel-otel-inhibit t)
      (gptel-send)
      (let ((parent (car gptel-otel-integration--pending))
            continue child)
        (let ((gptel-otel--parent-fsm parent))
          ;; Exercise real child request/FSM code with the agent adapter.
          ;; Like gptel-agent--task, use a private transform list rather than
          ;; the global root hook.  Cover WAIT both before and after return.
          (setq child
                (gptel-otel--around-agent-task
                 (lambda (_cb _type _description prompt)
                   (gptel-request prompt :callback #'ignore
                     :transforms (when async
                                   (list (lambda (cb _fsm) (setq continue cb))))))
                 #'ignore "test" "child" "hello")))
        (setq-local gptel-otel-inhibit nil)
        (when async (funcall continue))
        (should-not (gptel-otel--context child))
        (gptel-otel-integration--respond child "child done")
        (gptel-otel-integration--respond parent "parent done")
        (should-not gptel-otel-integration--exported)
        (should (= 0 (hash-table-count gptel-otel--request-decisions)))))))

(ert-deftest gptel-otel-integration-local-option-cannot-enable-global-mode ()
  (gptel-otel-integration--with-request
    (gptel-otel-mode -1)
    (setq-local gptel-otel-inhibit nil)
    (gptel-send)
    (let ((fsm (car gptel-otel-integration--pending)))
      (should-not (gptel-otel--context fsm))
      (gptel-otel-integration--respond fsm "done")
      (should-not gptel-otel-integration--exported))))

(provide 'gptel-otel-integration-test)
;;; gptel-otel-integration-test.el ends here
