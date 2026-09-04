;;; gptel-otel-transport.el --- Durable OTLP delivery  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Giessel
;; Version: 0.3.0
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
  "Complete OTLP/HTTP traces endpoint for the generic or legacy profile."
  :type 'string :group 'gptel-otel)
(defcustom gptel-otel-endpoint-kind 'collector
  "Legacy backend selector, either `collector' or `langfuse'.
Used when `gptel-otel-backend-profile' is nil."
  :type '(choice (const collector) (const langfuse)) :group 'gptel-otel)
(defcustom gptel-otel-langfuse-base-url nil
  "Langfuse base URL; when non-nil it determines the v1 traces endpoint."
  :type '(choice (const nil) string) :group 'gptel-otel)
(defcustom gptel-otel-headers-function nil
  "Function returning dynamic HTTP headers.
For a generic profile these headers may include Authorization and arbitrary
tenant headers.  Header names are deduplicated case-insensitively.  A profile's
required headers take precedence, and Content-Type is always controlled by the
transport.  Header values are evaluated only at delivery and never spooled."
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
  "Maximum Langfuse request body size.
This legacy variable remains the limit used by the Langfuse preset."
  :type 'integer :group 'gptel-otel)
(defcustom gptel-otel-generic-max-bytes nil
  "Optional request body limit for the generic OTLP/HTTP preset."
  :type '(choice (const nil) integer) :group 'gptel-otel)
(defcustom gptel-otel-generic-account-id "default"
  "Stable non-secret tenant/account identity for generic OTLP delivery."
  :type 'string :group 'gptel-otel)
(defcustom gptel-otel-langfuse-project-id nil
  "Optional stable non-secret Langfuse project identity.
When nil, derive a one-way identity from the public project key."
  :type '(choice (const nil) string) :group 'gptel-otel)
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
  "Asynchronous delivery escape hatch called as (PAYLOAD CALLBACK).
The built-in function uses the active backend profile.  Custom functions keep
the historical two-argument contract; destination mismatch checks still run
before they are called."
  :type 'function :group 'gptel-otel)

(cl-defstruct (gptel-otel-backend-profile
               (:constructor gptel-otel-make-backend-profile))
  "An extensible OTLP/HTTP backend description.
NAME is a stable, non-secret profile name.  ENDPOINT-FUNCTION returns a complete
traces URL, HEADERS-FUNCTION returns dynamic profile-owned headers,
MAX-BYTES-FUNCTION returns nil or a request limit, RESPONSE-FUNCTION interprets
an HTTP response plist, DESTINATION-ID-FUNCTION returns a stable non-secret
identity, and ATTRIBUTE-PROVIDERS lists semantic attribute provider functions."
  name endpoint-function headers-function max-bytes-function response-function
  destination-id-function attribute-providers)

(defcustom gptel-otel-backend-profile nil
  "Backend profile, or nil to use `gptel-otel-endpoint-kind' compatibility.
The value may be `langfuse', `otlp-http', a `gptel-otel-backend-profile' object,
or a function returning one."
  :type '(choice (const nil) (const langfuse) (const otlp-http) function sexp)
  :group 'gptel-otel)

(defvar gptel-otel--delivery-timer nil)
(defvar gptel-otel--watchdog-timer nil)
(defvar gptel-otel--delivery-active nil)
(defvar gptel-otel--last-delivery nil)
(defvar gptel-otel--delivery-snapshot nil
  "Immutable resolved profile snapshot dynamically visible to delivery.")

(defun gptel-otel--generic-endpoint () gptel-otel-endpoint)

(defun gptel-otel--langfuse-endpoint ()
  (if gptel-otel-langfuse-base-url
      (concat (string-remove-suffix "/" gptel-otel-langfuse-base-url)
              "/api/public/otel/v1/traces")
    gptel-otel-endpoint))

(defun gptel-otel--profile-destination-id (profile endpoint)
  (let ((function (gptel-otel-backend-profile-destination-id-function profile)))
    (if function (funcall function profile endpoint)
      (format "%s|sha256:%s" (gptel-otel-backend-profile-name profile)
              (secure-hash 'sha256 (gptel-otel--redacted-endpoint endpoint))))))

(defun gptel-otel--generic-destination-id (profile endpoint)
  (format "%s|account:%s|sha256:%s"
          (gptel-otel-backend-profile-name profile)
          gptel-otel-generic-account-id
          (secure-hash 'sha256 (gptel-otel--redacted-endpoint endpoint))))

(defun gptel-otel--langfuse-destination-id (profile endpoint)
  (let* ((keys (and gptel-otel-langfuse-auth-function
                    (funcall gptel-otel-langfuse-auth-function)))
         (project (or gptel-otel-langfuse-project-id
                      (and (car-safe keys)
                           (concat "pk-sha256:"
                                   (secure-hash 'sha256 (car keys))))
                      "unknown-project")))
    (format "%s|project:%s|sha256:%s"
            (gptel-otel-backend-profile-name profile) project
            (secure-hash 'sha256 (gptel-otel--redacted-endpoint endpoint)))))

(defun gptel-otel--redacted-endpoint (endpoint)
  "Return ENDPOINT metadata with userinfo, query, and fragment removed."
  (condition-case nil
      (let* ((urlobj (url-generic-parse-url endpoint))
             (scheme (url-type urlobj)) (host (url-host urlobj))
             (port (url-port urlobj)) (filename (url-filename urlobj)))
        (setq filename (car (split-string (or filename "") "[?#]")))
        (format "%s://%s%s%s" scheme host
                (if (and port
                         (not (or (and (equal scheme "https") (= port 443))
                                  (and (equal scheme "http") (= port 80)))))
                    (format ":%s" port) "")
                filename))
    (error "<redacted-endpoint>")))

(defun gptel-otel--default-response-interpreter (response)
  "Interpret generic OTLP/HTTP RESPONSE, including partialSuccess."
  (let* ((code (plist-get response :status))
         (connection-error (plist-get response :connection-error))
         (body (plist-get response :body))
         parsed partial rejected message)
    (when (and (stringp body) (not (string-empty-p (string-trim body))))
      (condition-case nil
          (setq parsed (json-parse-string body :object-type 'alist
                                          :array-type 'array
                                          :null-object nil :false-object :json-false))
        (error nil)))
    (setq partial (and parsed (or (alist-get 'partialSuccess parsed)
                                  (alist-get 'partial_success parsed)))
          rejected (and partial (or (alist-get 'rejectedSpans partial)
                                    (alist-get 'rejected_spans partial)))
          message (and partial (or (alist-get 'errorMessage partial)
                                   (alist-get 'error_message partial))))
    (cond
     (connection-error response)
     ((and code (<= 200 code) (< code 300)
           rejected (> (if (stringp rejected) (string-to-number rejected) rejected) 0))
      (list :ok nil :partial t :permanent t :status code
            :rejected-spans rejected
            :error (or message (format "OTLP partial success rejected %s spans" rejected))))
     ((and code (<= 200 code) (< code 300))
      (list :ok t :status code :partial-success (and partial t)
            :error message))
     (t response))))

(defun gptel-otel--langfuse-response-interpreter (response)
  "Interpret Langfuse's OTLP/HTTP RESPONSE."
  (gptel-otel--default-response-interpreter response))

(defun gptel-otel--generic-profile-headers ()
  (and gptel-otel-headers-function (funcall gptel-otel-headers-function)))

(defun gptel-otel--langfuse-profile-headers ()
  (let ((keys (and gptel-otel-langfuse-auth-function
                   (funcall gptel-otel-langfuse-auth-function))))
    (append '(("x-langfuse-ingestion-version" . "4"))
            (when (and (car-safe keys) (cdr-safe keys))
              `(("Authorization" .
                 ,(concat "Basic " (base64-encode-string
                                     (concat (car keys) ":" (cdr keys)) t)))))
            (and gptel-otel-headers-function
                 (funcall gptel-otel-headers-function)))))

(defun gptel-otel--generic-max-bytes () gptel-otel-generic-max-bytes)
(defun gptel-otel--langfuse-max-bytes () gptel-otel-cloud-max-bytes)

(defun gptel-otel--generic-profile ()
  (gptel-otel-make-backend-profile
   :name 'otlp-http :endpoint-function #'gptel-otel--generic-endpoint
   :headers-function #'gptel-otel--generic-profile-headers
   :max-bytes-function #'gptel-otel--generic-max-bytes
   :response-function #'gptel-otel--default-response-interpreter
   :destination-id-function #'gptel-otel--generic-destination-id
   :attribute-providers '(gptel-otel-genai-attribute-provider)))

(defun gptel-otel--langfuse-profile ()
  (gptel-otel-make-backend-profile
   :name 'langfuse :endpoint-function #'gptel-otel--langfuse-endpoint
   :headers-function #'gptel-otel--langfuse-profile-headers
   :max-bytes-function #'gptel-otel--langfuse-max-bytes
   :response-function #'gptel-otel--langfuse-response-interpreter
   :destination-id-function #'gptel-otel--langfuse-destination-id
   :attribute-providers '(gptel-otel-langfuse-attribute-provider
                          gptel-otel-genai-attribute-provider)))

(defun gptel-otel-active-backend-profile ()
  "Return the active resolved backend profile."
  (let ((value (or gptel-otel-backend-profile
                   (if (eq gptel-otel-endpoint-kind 'langfuse)
                       'langfuse 'otlp-http))))
    (when (functionp value) (setq value (funcall value)))
    (pcase value
      ('langfuse (gptel-otel--langfuse-profile))
      ('otlp-http (gptel-otel--generic-profile))
      ((pred gptel-otel-backend-profile-p) value)
      (_ (error "Invalid gptel-otel backend profile: %S" value)))))

(defun gptel-otel--resolve-delivery-snapshot ()
  "Resolve one immutable delivery snapshot without persisting credentials."
  (let* ((profile (gptel-otel-active-backend-profile))
         (endpoint (funcall (gptel-otel-backend-profile-endpoint-function profile)))
         (headers-function (gptel-otel-backend-profile-headers-function profile))
         (limit-function (gptel-otel-backend-profile-max-bytes-function profile)))
    (list :profile-object profile
          :profile (format "%s" (gptel-otel-backend-profile-name profile))
          :endpoint endpoint
          :redacted-endpoint (gptel-otel--redacted-endpoint endpoint)
          :headers (gptel-otel--dedupe-headers
                    (cons '("Content-Type" . "application/json")
                          (cl-remove-if
                           (lambda (header)
                             (equal "content-type" (gptel-otel--header-name header)))
                           (and headers-function (funcall headers-function)))))
          :max-bytes (and limit-function (funcall limit-function))
          :response-function
          (gptel-otel-backend-profile-response-function profile)
          :destination-id (gptel-otel--profile-destination-id profile endpoint))))

(defun gptel-otel--destination ()
  "Return current non-secret destination metadata."
  (let ((snapshot (gptel-otel--resolve-delivery-snapshot)))
    (list :profile (plist-get snapshot :profile)
          :endpoint (plist-get snapshot :redacted-endpoint)
          :destination-id (plist-get snapshot :destination-id))))

(defun gptel-otel--endpoint ()
  "Return the active profile's complete traces endpoint."
  (let* ((profile (gptel-otel-active-backend-profile))
         (function (gptel-otel-backend-profile-endpoint-function profile)))
    (funcall function)))

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

(defun gptel-otel--header-name (header) (downcase (format "%s" (car header))))

(defun gptel-otel--dedupe-headers (headers)
  "Return HEADERS with first occurrence winning, case-insensitively."
  (let (seen result)
    (dolist (header headers (nreverse result))
      (let ((name (gptel-otel--header-name header)))
        (unless (member name seen)
          (push name seen)
          (push header result))))))

(defun gptel-otel-http-headers ()
  "Return active profile headers without persisting credentials.
Content-Type wins over all supplied values.  Profile-required headers then win
over duplicates.  Generic caller headers, including Authorization, otherwise
pass through with first occurrence winning case-insensitively."
  (if gptel-otel--delivery-snapshot
      (plist-get gptel-otel--delivery-snapshot :headers)
    (let* ((profile (gptel-otel-active-backend-profile))
           (function (gptel-otel-backend-profile-headers-function profile))
           (headers (and function (funcall function))))
      (gptel-otel--dedupe-headers
       (cons '("Content-Type" . "application/json")
             (cl-remove-if (lambda (header)
                             (equal "content-type" (gptel-otel--header-name header)))
                           headers))))))

(defun gptel-otel--max-request-bytes ()
  (if gptel-otel--delivery-snapshot
      (plist-get gptel-otel--delivery-snapshot :max-bytes)
    (let* ((profile (gptel-otel-active-backend-profile))
           (function (gptel-otel-backend-profile-max-bytes-function profile)))
      (and function (funcall function)))))

(defun gptel-otel--destination-mismatch-reason (meta &optional destination-id)
  (let ((queued (plist-get meta :destination-id))
        (current (or destination-id
                     (plist-get (gptel-otel--destination) :destination-id))))
    (cond
     ((null queued)
      "legacy queue entry has no destination identity; run gptel-otel-migrate-legacy-queue")
     ((not (equal queued current))
      (format "destination mismatch: queued for %s, configured for %s"
              queued current)))))

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
                 (destination (gptel-otel--destination))
                 (limit (gptel-otel--max-request-bytes))
                 (oversized (and limit (> (string-bytes payload) limit))))
            ;; Encode once and publish the payload only after its metadata is visible.
            (gptel-otel--atomic-write meta (prin1-to-string
                                       (append (list :attempts 0 :state (if oversized 'permanent 'pending)
                                             :error (and oversized "payload exceeds endpoint size limit")
                                             :bytes (length payload)) destination)))
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

(defun gptel-otel-migrate-legacy-queue (expected-destination-id)
  "Stamp destination-less queue entries for EXPECTED-DESTINATION-ID.
The caller must supply the currently configured destination ID exactly; this
explicit confirmation prevents an upgrade from silently rerouting old traces."
  (interactive
   (list (read-string "Confirm current destination ID: "
                      (plist-get (gptel-otel--destination) :destination-id))))
  (let* ((destination (gptel-otel--destination))
         (current (plist-get destination :destination-id))
         (count 0))
    (unless (equal expected-destination-id current)
      (user-error "Destination confirmation does not match current profile"))
    (dolist (file (gptel-otel--entry-files))
      (let ((meta (gptel-otel--read-meta file)))
        (when (null (plist-get meta :destination-id))
          (setq meta (append meta destination))
          (setq meta (plist-put meta :state 'pending))
          (setq meta (plist-put meta :error nil))
          (gptel-otel--write-meta file meta)
          (cl-incf count))))
    (when (> count 0) (gptel-otel-flush))
    count))

(defun gptel-otel--write-meta (file meta)
  (gptel-otel--atomic-write (gptel-otel--metadata-file file)
                            (prin1-to-string meta)))

(defun gptel-otel-status ()
  "Return bounded status for the durable queue."
  (let ((files (gptel-otel--entry-files)) (pending 0) (permanent 0)
        (mismatched 0) (partial 0) (bytes 0))
    (dolist (file files)
      (let ((meta (gptel-otel--read-meta file)))
        (cl-incf bytes (or (plist-get meta :bytes) 0))
        (pcase (plist-get meta :state)
          ('permanent (cl-incf permanent))
          ('mismatched (cl-incf mismatched))
          ('partial (cl-incf partial))
          (_ (cl-incf pending)))))
    (list :pending pending :permanent permanent :mismatched mismatched :partial partial
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

(defun gptel-otel--claim-next (&optional destination-id)
  "Atomically claim and return the next pending entry, or nil.
A non-nil DESTINATION-ID is the already-resolved immutable delivery target.
A rename is the inter-process claim primitive.  Delivery is intentionally
at-least-once: a process crash after remote acceptance but before local deletion
can cause a recovered stale lease to be delivered again."
  (gptel-otel--recover-stale-leases)
  (catch 'claimed
    (dolist (file (gptel-otel--entry-files))
      (let* ((meta (gptel-otel--read-meta file))
             (reason (and (eq (plist-get meta :state) 'pending)
                          (gptel-otel--destination-mismatch-reason
                           meta destination-id))))
        (when reason
          (setq meta (plist-put meta :state 'mismatched))
          (setq meta (plist-put meta :error reason))
          (gptel-otel--write-meta file meta)
          (setq gptel-otel--last-delivery
                (list :state 'mismatched :file file :error reason)))
        (when (and (eq (plist-get meta :state) 'pending) (not reason))
          (let ((lease (format "%s.lease.%s.%s" file (emacs-pid)
                               (gptel-otel-generate-span-id))))
            (condition-case nil
                (progn
                  (rename-file file lease nil)
                  (set-file-times lease)
                  (throw 'claimed lease))
              (file-error nil))))))
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
       ((plist-get result :partial)
        (setq meta (plist-put meta :state 'partial))
        (setq meta (plist-put meta :attempts attempts))
        (setq meta (plist-put meta :error (plist-get result :error)))
        (setq meta (plist-put meta :rejected-spans
                              (plist-get result :rejected-spans)))
        (gptel-otel--write-meta lease meta)
        (gptel-otel--release-lease lease)
        (setq gptel-otel--last-delivery
              (list :state 'partial :status status
                    :rejected-spans (plist-get result :rejected-spans)
                    :file (gptel-otel--base-payload-file lease)))
        (gptel-otel--schedule 0))
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
    (let* ((snapshot (gptel-otel--resolve-delivery-snapshot))
           (destination-id (plist-get snapshot :destination-id)))
      (when-let* ((lease (gptel-otel--claim-next destination-id)))
        (let ((token (cons 'delivery nil)))
        (setq gptel-otel--delivery-active
              (list :token token :lease lease :snapshot snapshot))
        (condition-case err
            (let ((payload (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally lease)
                             (buffer-string))))
              (setq gptel-otel--watchdog-timer
                    (run-at-time gptel-otel-delivery-timeout-seconds nil
                                 #'gptel-otel--delivery-timeout token lease))
              (if (not (equal (plist-get snapshot :destination-id)
                              (plist-get (gptel-otel--destination) :destination-id)))
                  (gptel-otel--finish-delivery
                   token lease '(:ok nil :permanent t
                                 :error "destination changed before delivery"))
                (let ((gptel-otel--delivery-snapshot snapshot))
                  (funcall gptel-otel-delivery-function payload
                           (apply-partially #'gptel-otel--finish-delivery token lease)))))
          (error
           (gptel-otel--finish-delivery
            token lease (list :ok nil :connection-error t :error err)))))))))

(defun gptel-otel-replay (&optional include-permanent)
  "Replay queued traces; with INCLUDE-PERMANENT reset safe permanent failures.
Partial-success and destination-mismatched entries are deliberately not reset:
replaying a whole partially accepted batch can duplicate accepted spans, while
migrating destinations requires explicit payload-level user action."
  (interactive "P")
  (when include-permanent
    (dolist (file (gptel-otel--entry-files))
      (let ((meta (gptel-otel--read-meta file)))
        (when (eq (plist-get meta :state) 'permanent)
          (let* ((bytes (file-attribute-size (file-attributes file)))
                 (limit (gptel-otel--max-request-bytes))
                 (oversized (and limit (> bytes limit)))
                 (reason (gptel-otel--destination-mismatch-reason meta)))
            (setq meta (plist-put meta :bytes bytes))
            (setq meta (plist-put meta :attempts 0))
            (cond
             (reason
              (setq meta (plist-put meta :state 'mismatched))
              (setq meta (plist-put meta :error reason)))
             (oversized
                (progn
                  (setq meta (plist-put meta :state 'permanent))
                  (setq meta (plist-put meta :error
                                        "payload exceeds endpoint size limit"))))
             (t
              (setq meta (plist-put meta :state 'pending))
              (setq meta (plist-put meta :error nil))))
            (gptel-otel--write-meta file meta))))))
  (gptel-otel-flush))

(defun gptel-otel--http-deliver (payload callback)
  (condition-case err
      (let ((url-request-method "POST")
            (url-request-extra-headers (gptel-otel-http-headers))
            (url-request-data payload))
        (url-retrieve
         (or (plist-get gptel-otel--delivery-snapshot :endpoint)
             (gptel-otel--endpoint))
         (lambda (status cb)
           (let* ((connection-error (plist-get status :error))
                  (code (and (boundp 'url-http-response-status) url-http-response-status))
                  (retry-after (and (fboundp 'mail-fetch-field)
                                    (mail-fetch-field "Retry-After")))
                  (body (condition-case nil
                            (progn (goto-char (if (boundp 'url-http-end-of-headers)
                                           url-http-end-of-headers (point-min)))
                                   (buffer-substring-no-properties (point) (point-max)))
                          (error nil)))
                  (raw (list :ok (and (not connection-error) code
                                      (<= 200 code) (< code 300))
                             :status code :connection-error connection-error
                             :error connection-error :retry-after retry-after :body body))
                  (interpreter
                   (or (plist-get gptel-otel--delivery-snapshot :response-function)
                       (gptel-otel-backend-profile-response-function
                        (gptel-otel-active-backend-profile))))
                  (result (if interpreter (funcall interpreter raw) raw)))
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
