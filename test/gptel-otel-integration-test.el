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

(provide 'gptel-otel-integration-test)
;;; gptel-otel-integration-test.el ends here
