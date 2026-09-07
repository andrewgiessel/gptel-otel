;;; gptel-otel-adapter-test.el --- Compatibility adapter tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'gptel-otel)

(ert-deftest gptel-otel-adapter-installs-base-and-tool-capabilities-together ()
  (unwind-protect
      (progn
        (gptel-otel--base-adapter-uninstall)
        (let ((status (gptel-otel--base-adapter-install)))
          (should (plist-get status 'base))
          (should (plist-get (plist-get status 'tool) :supported))
          (should (advice-member-p #'gptel-otel--around-transition
                                   'gptel--fsm-transition))
          (should (advice-member-p #'gptel-otel--around-process-tool
                                   'gptel--process-tool-call))))
    (gptel-otel--base-adapter-uninstall)))

(ert-deftest gptel-otel-adapter-repeated-install-and-removal-is-idempotent ()
  (unwind-protect
      (progn
        (gptel-otel--base-adapter-uninstall)
        (gptel-otel--base-adapter-install)
        (gptel-otel--base-adapter-install)
        (should (= 1 (cl-count (cons 'gptel--fsm-transition
                                     #'gptel-otel--around-transition)
                               gptel-otel--installed-advices :test #'equal)))
        (gptel-otel--base-adapter-uninstall)
        (gptel-otel--base-adapter-uninstall)
        (should-not (advice-member-p #'gptel-otel--around-transition
                                     'gptel--fsm-transition)))
    (gptel-otel--base-adapter-uninstall)))

(ert-deftest gptel-otel-agent-adapter-activates-after-late-load ()
  (unless (locate-library "gptel-agent") (ert-skip "gptel-agent is not installed"))
  (unwind-protect
      (progn
        (gptel-otel-mode -1)
        (require 'gptel-agent)
        (gptel-otel-mode 1)
        (should (advice-member-p #'gptel-otel--around-agent-task
                                 'gptel-agent--task))
        (should (plist-get gptel-otel--agent-adapter-capability :supported)))
    (gptel-otel-mode -1)))

(ert-deftest gptel-otel-agent-adapter-is-inactive-when-agent-is-not-loaded ()
  (let ((features (delq 'gptel-agent (copy-sequence features))))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature) (and (not (eq feature 'gptel-agent))
                                  (memq feature features)))))
      (let ((status (gptel-otel--agent-adapter-install)))
        (should-not (plist-get status :available))
        (should-not (plist-get status :supported))))))

(ert-deftest gptel-otel-compatibility-status-is-bounded-and-distinguishes-agent ()
  (let ((status (gptel-otel-compatibility-status)))
    (should (plist-member status :mode))
    (should (plist-member status :base-compatible))
    (should (plist-member status :base-active))
    (should (plist-member status :tool-compatible))
    (should (plist-member status :tool-active))
    (should (plist-member status :base))
    (should (plist-member status :tool))
    (should (plist-member status :agent-available))
    (should (plist-member status :agent))))

(ert-deftest gptel-otel-mode-does-not-add-root-hook-with-rejected-base ()
  (unwind-protect
      (let ((gptel-otel--base-seams
             '((root (gptel-send (wrong) gptel-otel--around-send :around))
               (generation (gptel--handle-wait (fsm) gptel-otel--before-wait :before))
               (tool))))
        (gptel-otel-mode 1)
        (should-not (memq #'gptel-otel--instrument-request
                          gptel-prompt-transform-functions)))
    (gptel-otel-mode -1)))

(ert-deftest gptel-otel-agent-reconciliation-requires-tool-capability ()
  (let ((gptel-otel--base-seams
         '((root (gptel-send (&optional arg) gptel-otel--around-send :around))
           (generation (gptel--handle-wait (fsm) gptel-otel--before-wait :before)
                       (gptel--fsm-transition (machine &optional new-state)
                                              gptel-otel--around-transition :around))
           (tool (gptel--handle-pre-tool (wrong) gptel-otel--before-pre-tool :before)))))
    (gptel-otel--agent-adapter-uninstall)
    (let ((status (gptel-otel--agent-adapter-install
                   (gptel-otel--base-adapter-capabilities))))
      (should-not (plist-get status :supported))
      (should-not (advice-member-p #'gptel-otel--around-agent-task 'gptel-agent--task)))))

(ert-deftest gptel-otel-adapter-status-is-observational-and-checks-all-seams ()
  (unwind-protect
      (progn
        (gptel-otel-mode -1)
        (let ((status (gptel-otel-compatibility-status)))
          (should (plist-get status :base-compatible))
          (should-not (plist-get status :base-active)))
        (gptel-otel-mode 1)
        (let ((before gptel-otel--adapter-capabilities))
          (should (eq t (plist-get (gptel-otel-compatibility-status) :base-active)))
          (should (eq before gptel-otel--adapter-capabilities)))
        (advice-remove 'gptel--handle-wait #'gptel-otel--before-wait)
        (should-not (plist-get (gptel-otel-compatibility-status) :base-active))
        (should-not (plist-get (gptel-otel-compatibility-status) :agent-active))
        (gptel-otel-mode 1)
        (should (plist-get (gptel-otel-compatibility-status) :base-active)))
    (gptel-otel-mode -1)))

(ert-deftest gptel-otel-adapter-revalidation-removes-stale-dependent-advice ()
  (unwind-protect
      (progn
        (gptel-otel-mode 1)
        (let ((gptel-otel--base-seams (copy-tree gptel-otel--base-seams)))
          (setf (nth 1 (cadr (assq 'generation gptel-otel--base-seams))) '(wrong))
          (gptel-otel-mode 1)
          (should-not (plist-get (gptel-otel-compatibility-status) :base-active))
          (should-not gptel-otel--base-installed-advices)
          (should-not gptel-otel--agent-installed-advices)
          (should-not (memq #'gptel-otel--instrument-request
                            gptel-prompt-transform-functions))))
    (gptel-otel-mode -1)))

(ert-deftest gptel-otel-agent-advice-is-not-installed-with-inactive-base ()
  (unwind-protect
      (progn
        (gptel-otel-mode -1)
        (gptel-otel--agent-adapter-install)
        (should-not gptel-otel--agent-installed-advices))
    (gptel-otel-mode -1)))

(ert-deftest gptel-otel-agent-setup-failure-preserves-original-call ()
  (let ((gptel-otel--parent-fsm 'test-parent) (calls 0))
    (cl-letf (((symbol-function 'gptel-otel--context)
               (lambda (&rest _) (error "telemetry setup failed"))))
      (should (eq 'upstream-result
                  (gptel-otel--around-agent-task
                   (lambda (&rest _) (cl-incf calls) 'upstream-result)
                   #'ignore "test" "description" "prompt")))
      (should (= calls 1)))))

(provide 'gptel-otel-adapter-test)
;;; gptel-otel-adapter-test.el ends here
