;;; gptel-otel-transport-test.el --- Transport tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'gptel-otel-transport)

(defmacro gptel-otel-test--with-spool (&rest body)
  `(let* ((dir (make-temp-file "gptel-otel-test" t))
          (gptel-otel-spool-directory dir)
          (gptel-otel--delivery-active nil)
          (gptel-otel--delivery-timer nil)
          (gptel-otel--watchdog-timer nil))
     (unwind-protect (progn ,@body)
       (when (timerp gptel-otel--delivery-timer) (cancel-timer gptel-otel--delivery-timer))
       (when (timerp gptel-otel--watchdog-timer) (cancel-timer gptel-otel--watchdog-timer))
       (delete-directory dir t))))

(defun gptel-otel-test--request (&optional payload)
  (let ((span (gptel-otel-start-span
               "test" nil (and payload `(("input" . ,(gptel-otel-value-string payload)))))))
    (gptel-otel-end-span span)
    (gptel-otel-export-request (list span))))

(ert-deftest gptel-otel-queue-persists-private-and-replays ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-delivery-function (lambda (_payload _callback) nil)))
     (let ((file (gptel-otel-enqueue (gptel-otel-test--request "full"))))
       (when gptel-otel--delivery-active
         (gptel-otel--release-lease (plist-get gptel-otel--delivery-active :lease))
         (setq gptel-otel--delivery-active nil))
       (should (file-exists-p file))
       (should (= #o600 (file-modes file)))
       (should (= #o700 (file-modes dir)))
       (setq gptel-otel--delivery-active nil)
       (let ((gptel-otel-delivery-function
              (lambda (_payload callback) (funcall callback '(:ok t :status 200)))))
         (gptel-otel-replay)
         (should-not (file-exists-p file)))))))

(ert-deftest gptel-otel-oversized-langfuse-is-preserved-permanent ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-endpoint-kind 'langfuse)
         (gptel-otel-cloud-max-bytes 100))
     (let ((file (gptel-otel-enqueue (gptel-otel-test--request (make-string 500 ?x)))))
       (should (file-exists-p file))
       (should (= 1 (plist-get (gptel-otel-status) :permanent)))
       (should (string-match-p (make-string 50 ?x)
                               (with-temp-buffer (insert-file-contents file) (buffer-string))))))))

(ert-deftest gptel-otel-retry-classification-and-backoff ()
  (dolist (code '(429 502 503 504)) (should (gptel-otel-retryable-p code)))
  (should (gptel-otel-retryable-p nil t))
  (should-not (gptel-otel-retryable-p 400))
  (should (= 17 (gptel-otel--retry-delay 4 "17")))
  (let ((gptel-otel-retry-base-seconds 2) (gptel-otel-retry-max-seconds 10))
    (should (= 8 (gptel-otel--retry-delay 3 nil)))
    (should (= 10 (gptel-otel--retry-delay 9 nil)))))

(ert-deftest gptel-otel-langfuse-endpoint-and-headers ()
  (let ((gptel-otel-endpoint-kind 'langfuse)
        (gptel-otel-langfuse-base-url "https://cloud.langfuse.com/")
        (gptel-otel-langfuse-auth-function (lambda () '("pk" . "sk"))))
    (should (equal "https://cloud.langfuse.com/api/public/otel/v1/traces"
                   (gptel-otel--endpoint)))
    (let ((headers (gptel-otel-http-headers)))
      (should (equal "application/json" (cdr (assoc "Content-Type" headers))))
      (should (equal "4" (cdr (assoc "x-langfuse-ingestion-version" headers))))
      (should (equal (concat "Basic " (base64-encode-string "pk:sk" t))
                     (cdr (assoc "Authorization" headers)))))))

(ert-deftest gptel-otel-auth-source-uses-matched-user-as-public-key ()
  (let ((gptel-otel-endpoint-kind 'langfuse)
        (gptel-otel-endpoint "https://example.test/v1/traces")
        (gptel-otel-auth-source-user nil)
        (gptel-otel-auth-source-account "production") query)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (setq query args)
                 (list (list :user "pk-real" :secret (lambda () "sk-real"))))))
      (should (equal '("pk-real" . "sk-real")
                     (gptel-otel-langfuse-auth-source)))
      (should (equal "production" (plist-get query :account)))
      (should (equal "https" (plist-get query :port)))
      (should-not (plist-member query :user)))))

(ert-deftest gptel-otel-reserved-headers-are-deduplicated ()
  (let ((gptel-otel-endpoint-kind 'langfuse)
        (gptel-otel-langfuse-auth-function (lambda () '("pk" . "sk")))
        (gptel-otel-headers-function
         (lambda () '(("content-type" . "bad") ("Authorization" . "bad")
                      ("X-Langfuse-Ingestion-Version" . "bad") ("X-Extra" . "yes")))))
    (let ((headers (gptel-otel-http-headers)))
      (should (= 1 (cl-count "content-type" headers :key (lambda (h) (downcase (car h)))
                             :test #'equal)))
      (should (= 1 (cl-count "authorization" headers :key (lambda (h) (downcase (car h)))
                             :test #'equal)))
      (should (= 1 (cl-count "x-langfuse-ingestion-version" headers
                             :key (lambda (h) (downcase (car h))) :test #'equal)))
      (should (equal "yes" (cdr (assoc "X-Extra" headers)))))))

(ert-deftest gptel-otel-generic-authorization-and-custom-headers-pass-through ()
  (let ((gptel-otel-backend-profile 'otlp-http)
        (gptel-otel-headers-function
         (lambda () '(("Authorization" . "Bearer token")
                      ("x-honeycomb-team" . "team")
                      ("X-Tenant-ID" . "tenant")
                      ("authorization" . "Basic ignored")))))
    (let ((headers (gptel-otel-http-headers)))
      (should (equal "Bearer token" (cdr (assoc "Authorization" headers))))
      (should (equal "team" (cdr (assoc "x-honeycomb-team" headers))))
      (should (equal "tenant" (cdr (assoc "X-Tenant-ID" headers))))
      (should (= 1 (cl-count "authorization" headers
                             :key (lambda (h) (downcase (car h))) :test #'equal))))))

(ert-deftest gptel-otel-langfuse-required-headers-win-and-dedupe ()
  (let ((gptel-otel-backend-profile 'langfuse)
        (gptel-otel-langfuse-auth-function (lambda () '("pk" . "sk")))
        (gptel-otel-headers-function
         (lambda () '(("Authorization" . "Bearer bad")
                      ("X-Langfuse-Ingestion-Version" . "bad")
                      ("X-Tenant" . "kept")))))
    (let ((headers (gptel-otel-http-headers)))
      (should (equal (concat "Basic " (base64-encode-string "pk:sk" t))
                     (cdr (assoc "Authorization" headers))))
      (should (equal "4" (cdr (assoc "x-langfuse-ingestion-version" headers))))
      (should (equal "kept" (cdr (assoc "X-Tenant" headers)))))))

(ert-deftest gptel-otel-generic-byte-limit-is-enforced ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-backend-profile 'otlp-http)
         (gptel-otel-generic-max-bytes 100))
     (let ((file (gptel-otel-enqueue
                  (gptel-otel-test--request (make-string 500 ?x)))))
       (should (eq 'permanent (plist-get (gptel-otel--read-meta file) :state)))))))

(ert-deftest gptel-otel-queue-destination-mismatch-is-visible ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-backend-profile 'otlp-http)
         (gptel-otel-endpoint "https://one.test/v1/traces")
         (gptel-otel-delivery-function (lambda (&rest _))))
     (let ((file (gptel-otel-enqueue (gptel-otel-test--request))))
       (when gptel-otel--delivery-active
         (gptel-otel--release-lease (plist-get gptel-otel--delivery-active :lease))
         (setq gptel-otel--delivery-active nil))
       (let ((gptel-otel-endpoint "https://two.test/v1/traces"))
         (should-not (gptel-otel--claim-next))
         (let ((meta (gptel-otel--read-meta file)))
           (should (eq 'mismatched (plist-get meta :state)))
           (should (string-match-p "destination mismatch" (plist-get meta :error)))
           (should (= 1 (plist-get (gptel-otel-status) :mismatched)))))))))

(ert-deftest gptel-otel-same-endpoint-different-account-is-mismatched ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-backend-profile 'otlp-http)
         (gptel-otel-endpoint "https://collector.test/v1/traces")
         (gptel-otel-generic-account-id "tenant-a")
         (gptel-otel-delivery-function (lambda (&rest _))))
     (let ((file (gptel-otel-enqueue (gptel-otel-test--request))))
       (when gptel-otel--delivery-active
         (gptel-otel--release-lease (plist-get gptel-otel--delivery-active :lease))
         (setq gptel-otel--delivery-active nil))
       (let ((gptel-otel-generic-account-id "tenant-b"))
         (should-not (gptel-otel--claim-next))
         (should (eq 'mismatched
                     (plist-get (gptel-otel--read-meta file) :state))))))))

(ert-deftest gptel-otel-legacy-queue-requires-explicit-migration ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-delivery-function (lambda (&rest _))))
     (gptel-otel--ensure-spool)
     (let* ((file (expand-file-name "legacy.json" dir))
            (meta (gptel-otel--metadata-file file))
            (destination-id (plist-get (gptel-otel--destination) :destination-id)))
       (gptel-otel--write-private file "{}" t)
       (gptel-otel--write-private meta "(:attempts 0 :state pending)")
       (should-not (gptel-otel--claim-next))
       (should-error (gptel-otel-migrate-legacy-queue "wrong"))
       (should (= 1 (gptel-otel-migrate-legacy-queue destination-id)))
       (should (equal destination-id
                      (plist-get (gptel-otel--read-meta file) :destination-id)))))))

(ert-deftest gptel-otel-otlp-partial-success-is-preserved ()
  (let ((result (gptel-otel--default-response-interpreter
                 '(:ok t :status 200
                   :body "{\"partialSuccess\":{\"rejectedSpans\":2,\"errorMessage\":\"bad spans\"}}"))))
    (should-not (plist-get result :ok))
    (should (plist-get result :partial))
    (should (= 2 (plist-get result :rejected-spans)))
    (should (equal "bad spans" (plist-get result :error)))))

(ert-deftest gptel-otel-otlp-partial-success-zero-and-snake-case ()
  (should (plist-get
           (gptel-otel--default-response-interpreter
            '(:ok t :status 200 :body "{\"partialSuccess\":{\"rejectedSpans\":0}}"))
           :ok))
  (let ((result (gptel-otel--default-response-interpreter
                 '(:ok t :status 200
                   :body "{\"partial_success\":{\"rejected_spans\":\"3\",\"error_message\":\"bad\"}}"))))
    (should (plist-get result :partial))
    (should (equal "3" (plist-get result :rejected-spans)))))

(ert-deftest gptel-otel-profile-is-extensible ()
  (let* ((profile (gptel-otel-make-backend-profile
                   :name 'community
                   :endpoint-function (lambda () "https://community.test/traces")
                   :headers-function (lambda () '(("X-Community" . "yes")))
                   :max-bytes-function (lambda () 123)
                   :response-function #'identity
                   :destination-id-function
                   (lambda (_profile _endpoint) "community:stable")))
         (gptel-otel-backend-profile profile))
    (should (equal "https://community.test/traces" (gptel-otel--endpoint)))
    (should (= 123 (gptel-otel--max-request-bytes)))
    (should (equal "community:stable"
                   (plist-get (gptel-otel--destination) :destination-id)))
    (should (equal "yes" (cdr (assoc "X-Community" (gptel-otel-http-headers)))))))

(ert-deftest gptel-otel-unicode-bytes-persist-literally ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-delivery-function (lambda (_payload _callback) nil))
         (gptel-otel-endpoint-kind 'langfuse)
         (gptel-otel-cloud-max-bytes 1)
         (text "snowman ☃"))
     (let* ((file (gptel-otel-enqueue (gptel-otel-test--request text)))
            (bytes (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file)
                     (buffer-string))))
       (should (string-match-p (regexp-quote (encode-coding-string text 'utf-8)) bytes))
       (should (= (length bytes) (plist-get (gptel-otel--read-meta file) :bytes)))))))

(ert-deftest gptel-otel-atomic-claim-and-stale-lease-recovery ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-delivery-function (lambda (_payload _callback) nil))
         (gptel-otel-endpoint-kind 'langfuse)
         (gptel-otel-cloud-max-bytes 1))
     (let* ((file (gptel-otel-enqueue (gptel-otel-test--request)))
            (meta (gptel-otel--read-meta file))
            (_ (progn (setq meta (plist-put meta :state 'pending))
                      (gptel-otel--write-meta file meta)))
            (lease (gptel-otel--claim-next)))
       (should lease)
       (should-not (file-exists-p file))
       (should-not (gptel-otel--claim-next))
       (set-file-times lease (seconds-to-time 0))
       (let ((gptel-otel-lease-timeout-seconds 1))
         (gptel-otel--recover-stale-leases))
       (should (file-exists-p file))))))

(ert-deftest gptel-otel-watchdog-recovers-and-ignores-late-callback ()
  (gptel-otel-test--with-spool
   (let (callback)
     (let ((gptel-otel-delivery-function
            (lambda (_payload cb) (setq callback cb))))
       (let ((file (gptel-otel-enqueue (gptel-otel-test--request))))
         (should gptel-otel--delivery-active)
         (let* ((active gptel-otel--delivery-active)
                (token (plist-get active :token)) (lease (plist-get active :lease)))
           (gptel-otel--delivery-timeout token lease)
           (should-not gptel-otel--delivery-active)
           (should (file-exists-p file))
           (funcall callback '(:ok t :status 200))
           (should (file-exists-p file))))))))

(ert-deftest gptel-otel-replay-revalidates-oversize-reason ()
  (gptel-otel-test--with-spool
   (let ((gptel-otel-endpoint-kind 'langfuse)
         (gptel-otel-cloud-max-bytes 100))
     (let ((file (gptel-otel-enqueue (gptel-otel-test--request (make-string 500 ?x)))))
       (gptel-otel-replay t)
       (let ((meta (gptel-otel--read-meta file)))
         (should (eq 'permanent (plist-get meta :state)))
         (should (equal "payload exceeds endpoint size limit" (plist-get meta :error))))))))

(ert-deftest gptel-otel-queue-no-empty-envelope ()
  (gptel-otel-test--with-spool (should-not (gptel-otel-enqueue nil))))

(provide 'gptel-otel-transport-test)
