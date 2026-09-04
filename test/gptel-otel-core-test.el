;;; gptel-otel-core-test.el --- Core tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'json)
(require 'gptel-otel-core)

(ert-deftest gptel-otel-id-shapes-and-nonzero ()
  (should (string-match-p "\\`[[:xdigit:]]\\{32\\}\\'" (gptel-otel-generate-trace-id)))
  (should (string-match-p "\\`[[:xdigit:]]\\{16\\}\\'" (gptel-otel-generate-span-id))))

(ert-deftest gptel-otel-unix-nano-is-exact-decimal-without-bignum ()
  (cl-letf (((symbol-function 'current-time) (lambda () '(30000 12345 678901 234000))))
    (let ((stamp (gptel-otel--unix-nano)))
      (should (equal stamp "1966092345678901234"))
      (should (string-match-p "\\`[0-9]\\{19\\}\\'" stamp)))))

(ert-deftest gptel-otel-span-parentage-and-idempotent-end ()
  (let* ((parent (gptel-otel-start-span "parent"))
         (child (gptel-otel-start-span "child" parent)))
    (gptel-otel-end-span child (gptel-otel-status-ok))
    (let ((end (gptel-otel-span-end-time-unix-nano child)))
      (gptel-otel-end-span child (gptel-otel-status-error "late"))
      (should (equal end (gptel-otel-span-end-time-unix-nano child))))
    (should (equal (gptel-otel-span-trace-id parent) (gptel-otel-span-trace-id child)))
    (should (equal (gptel-otel-span-span-id parent)
                   (gptel-otel-span-parent-span-id child)))))

(ert-deftest gptel-otel-typed-attributes-and-complete-payload ()
  (let* ((payload (concat (make-string 200000 ?x) "THE-END"))
         (span (gptel-otel-start-span
                "operation" nil
                `(("s" . ,(gptel-otel-value-string payload))
                  ("i" . ,(gptel-otel-value-int 42))
                  ("b" . ,(gptel-otel-value-bool nil))
                  ("tags" . ,(gptel-otel-value-array
                               (gptel-otel-value-string "a")))))))
    (gptel-otel-end-span span)
    (let ((encoded (json-encode (gptel-otel-export-request (list span)))))
      (should (string-match-p "THE-END" encoded))
      (should (string-match-p "\\\"intValue\\\":\\\"42\\\"" encoded))
      (should (string-match-p "\\\"boolValue\\\":false" encoded)))))

(ert-deftest gptel-otel-trace-collects-ended-unexported-only ()
  (let* ((trace (gptel-otel-trace-create "root"))
         (root (gptel-otel-trace-root trace))
         (child (gptel-otel-trace-start-span trace "child" root nil)))
    (gptel-otel-end-span child)
    (should (gptel-otel-trace-request trace))
    (setf (gptel-otel-span-exported-p child) t)
    (should-not (gptel-otel-trace-request trace))))

(ert-deftest gptel-otel-refuses-empty-envelope ()
  (should-error (gptel-otel-export-request nil)))

(provide 'gptel-otel-core-test)
