(in-package :data-ui)

(def-suite deploy-incoming
  :description "Deploy handoff machinery: exclusive-create tokens under
models/local/deploy-incoming/, the <token>.registry contract (strict
parse shared with the shell scanner), spawn without pipe deadlock, and
the startup reaper. Pure file/process tests; the registry cases here
mirror tests/scripts/checkout-tests.sh's scanner cases.")

(in-suite deploy-incoming)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun di-temp-root ()
  "A fresh throwaway package-root for one test, removed by the test at
the end. Unique per test so runs are order-independent."
  (let ((dir (merge-pathnames
               (format nil "data-ui-incoming-test-~a/"
                       (deploy-model-generate-token))
               (uiop:temporary-directory))))
    (ensure-directories-exist dir)))

(defun di-cleanup (root)
  "Best-effort removal of a test root."
  (ignore-errors
    (uiop:delete-directory-tree
      (truename root)
      :validate t :if-does-not-exist :ignore)))

(defun di-incoming (root &rest segments)
  "A path under root's models/local/deploy-incoming/."
  (merge-pathnames
    (format nil "models/local/deploy-incoming/~{~a~}"
            segments)
    root))

(defun di-write-raw (root filename text)
  "Write raw bytes into the incoming dir (malformed-record tests)."
  (ensure-directories-exist (di-incoming root ""))
  (let ((path (di-incoming root filename)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))

(defun di-valid-record (root token state worker child)
  "A well-formed record's raw text."
  (di-write-raw
    root (format nil "~a.registry" token)
    (format nil "state=~a~%token=~a~%worker-pid=~a~%child-pid=~a~%path=models/local/deploy-incoming/~a.lisp~%"
            state token worker child token)))

(defun di-touch-incoming (root token &optional (text "model"))
  (di-write-raw root (format nil "~a.lisp" token) text))

(defun di-dead-pid ()
  "A PID that is certainly not alive: the kernel's next PID."
  (let ((last
          (ignore-errors
            (parse-integer
              (string-trim '(#\Newline #\Space)
                           (with-open-file (in "/proc/sys/kernel/ns_last_pid")
                             (read-line in)))))))
    (if last (1+ last) 999999)))

(defun di-report-kinds (reports)
  (mapcar #'first reports))

;;; ---------------------------------------------------------------------------
;;; deploy-model-write-file: exclusive create
;;; ---------------------------------------------------------------------------

(test write-file/exclusive-create-regenerates
  (let* ((root (di-temp-root))
         (calls 0)
         (*deploy-model-token-generator*
           (lambda ()
             (incf calls)
             (if (= calls 1) "taken" "freshone")))
         (occupied "(:name occupied original bytes)"))
    (unwind-protect
         (progn
           ;; A pre-existing file at the first generated token must force
           ;; regeneration, never truncation or overwrite.
           (di-write-raw root "taken.lisp" occupied)
           (multiple-value-bind (rel name abs)
               (deploy-model-write-file "'(:name excl :types ())"
                 (list :name "excl") root)
             (declare (ignore name))
             (is (equal "models/local/deploy-incoming/freshone.lisp" rel)
                 "the regenerated token is used")
             (is (equal occupied
                        (with-open-file (in (di-incoming root "taken.lisp"))
                          (read-line in)))
                 "the colliding file is byte-identical afterwards")
             (is (uiop:file-exists-p abs))
             ;; Publication is atomic: no temp file remains beside it.
             (is (null (intersection
                         '((".freshone.lisp" . "tmp"))
                         (mapcar (lambda (p) (cons (pathname-name p)
                                                   (pathname-type p)))
                                 (uiop:directory-files
                                   (di-incoming root "")))
                         :test #'equal))
                 "no .freshone temp remains")
              (is (= 2 calls))))
      (di-cleanup root))))

(test write-file/bounded-retry-signals
  (let* ((root (di-temp-root))
         (*deploy-model-token-generator* (lambda () "taken")))
    (unwind-protect
         (progn
           (di-write-raw root "taken.lisp" "occupied")
           (signals error
             (deploy-model-write-file "'(:name excl :types ())"
               (list :name "excl") root))
           ;; The occupied file survived every failed attempt.
           (is (equal "occupied"
                      (with-open-file (in (di-incoming root "taken.lisp"))
                        (read-line in)))))
      (di-cleanup root))))

(test write-file/unique-tokens-for-same-model-name
  ;; Two workers sharing an internal model name each get their own
  ;; immutable file (the token, not the name, keys the path).
  (let ((root (di-temp-root)))
    (unwind-protect
         (let ((plist (list :name "shared")))
           (multiple-value-bind (rel1 n1 a1)
               (deploy-model-write-file "'(:name shared :types ())" plist root)
             (multiple-value-bind (rel2 n2 a2)
                 (deploy-model-write-file "'(:name shared :types ())" plist root)
               (declare (ignore n2))
               (is (equal n1 "shared"))
               (is (not (equal rel1 rel2)))
               (is (not (equal a1 a2)))
               (is (uiop:file-exists-p a1))
               (is (uiop:file-exists-p a2)))))
      (di-cleanup root))))

(test registry/valid-states-parse
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           (di-valid-record root "tok1" "pending" (deploy-model-self-pid) "-")
           (multiple-value-bind (state worker child path ok reason)
               (parse-deploy-registry-record
                 (di-incoming root "tok1.registry"))
             (declare (ignore reason))
             (is-true ok)
             (is (equal "pending" state))
             (is (eql (deploy-model-self-pid) worker))
             (is-false child)
             (is (equal "models/local/deploy-incoming/tok1.lisp" path)))
           (di-valid-record root "tok2" "ready" 111 222)
           (multiple-value-bind (state worker child _p ok)
               (parse-deploy-registry-record
                 (di-incoming root "tok2.registry"))
             (declare (ignore _p))
             (is-true ok)
             (is (equal "ready" state))
             (is (eql 111 worker))
             (is (eql 222 child)))
           (di-valid-record root "tok3" "done" 111 222)
           (multiple-value-bind (state _w _c _p ok)
               (parse-deploy-registry-record
                 (di-incoming root "tok3.registry"))
             (declare (ignore _w _c _p))
             (is-true ok)
             (is (equal "done" state))))
      (di-cleanup root))))

(test registry/malformed-records-are-unrecognizable
  (let ((root (di-temp-root))
        (cases
          (list
            ;; (label . raw-record-body) — bodies are format strings so
            ;; ~% becomes the record's real newlines.
            (cons "unknown key"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=-~%mystery=x~%"))
            (cons "key order swapped"
                  (format nil "token=tokX~%state=pending~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "missing key"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=-~%"))
            (cons "too many lines"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%extra=1~%"))
            (cons "bad state"
                  (format nil "state=running~%token=tokX~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "non-numeric worker-pid"
                  (format nil "state=pending~%token=tokX~%worker-pid=abc~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "child-pid dash outside pending"
                  (format nil "state=ready~%token=tokX~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "numeric child-pid in pending"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=2~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "token mismatch with file name"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tokX.lisp~%"))
            (cons "path mismatch"
                  (format nil "state=pending~%token=tokX~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/other.lisp~%"))
            (cons "token with bad character"
                  (format nil "state=pending~%token=tok!X~%worker-pid=1~%child-pid=-~%path=models/local/deploy-incoming/tok!X.lisp~%")))))
    (unwind-protect
         (loop for (label . body) in cases
               for filename = (format nil "case~a.registry"
                               (position (cons label body) cases :test #'equal))
               do (let ((path (di-write-raw root filename body)))
                    (multiple-value-bind (_s _w _c _p ok reason)
                        (parse-deploy-registry-record path)
                      (declare (ignore _s _w _c _p))
                      (is-false ok "~a" label)
                      (is (stringp reason) "~a" label))))
      (di-cleanup root))))

;;; ---------------------------------------------------------------------------
;;; The reaper
;;; ---------------------------------------------------------------------------

(test reap/done-and-dead-subjects-are-reaped
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           ;; done with the file still present: provable completion.
           (di-touch-incoming root "tokdone")
           (di-valid-record root "tokdone" "done" 111 222)
           ;; pending with a dead worker.
           (di-touch-incoming root "tokpend")
           (di-valid-record root "tokpend" "pending" (di-dead-pid) "-")
           ;; ready with a dead child.
           (di-touch-incoming root "tokready")
           (di-valid-record root "tokready" "ready" 111 (di-dead-pid))
           (let ((reports (reap-deploy-incoming-orphans root)))
             (is (equal '(:reaped :reaped :reaped) (di-report-kinds reports)))
             (is-false (uiop:file-exists-p (di-incoming root "tokdone.lisp")))
             (is-false (uiop:file-exists-p (di-incoming root "tokdone.registry")))
             (is-false (uiop:file-exists-p (di-incoming root "tokpend.lisp")))
             (is-false (uiop:file-exists-p (di-incoming root "tokpend.registry")))
             (is-false (uiop:file-exists-p (di-incoming root "tokready.lisp")))
             (is-false (uiop:file-exists-p (di-incoming root "tokready.registry"))))
           ;; A second reaper run (two profiles booting together) is a
           ;; silent no-op, not an error.
           (is (null (reap-deploy-incoming-orphans root))))
      (di-cleanup root))))

(test reap/live-subjects-are-retained
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           ;; pending with a LIVE worker (this process): retain.
           (di-touch-incoming root "toklive")
           (di-valid-record root "toklive" "pending" (deploy-model-self-pid) "-")
           ;; A live child: spawn a sleeper.
           (let* ((proc (uiop:launch-program
                          (list "sleep" "30")))
                  (child (uiop:process-info-pid proc)))
             (unwind-protect
                  (progn
                    (di-touch-incoming root "tokchild")
                    (di-valid-record root "tokchild" "ready" 111 child)
                    (let ((reports (reap-deploy-incoming-orphans root)))
                      (is (null (remove-if
                                  (lambda (r) (eq (first r) :reaped))
                                  reports))
                          "nothing but provable completions reaped")
                      (is (uiop:file-exists-p (di-incoming root "toklive.lisp")))
                      (is (uiop:file-exists-p
                            (di-incoming root "toklive.registry")))
                      (is (uiop:file-exists-p (di-incoming root "tokchild.lisp")))
                      (is (uiop:file-exists-p
                            (di-incoming root "tokchild.registry")))))
               (ignore-errors (uiop:terminate-process proc)))))
      (di-cleanup root))))

(test reap/unrecognizable-and-unknown-entries-are-retained-and-reported
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           ;; Malformed record: retained, reported, never reaped.
           (di-touch-incoming root "tokbad")
           (di-write-raw
             root "tokbad.registry"
             "state=pending~%token=tokbad~%worker-pid=notanumber~%child-pid=-~%path=models/local/deploy-incoming/tokbad.lisp~%")
           ;; Incoming file with no record: retained, reported.
           (di-touch-incoming root "tokorphan")
           ;; Stale temp without a reapable record: reported stale-temp.
           (di-write-raw root ".tokstale.lisp.tmp" "partial")
           (let ((reports (reap-deploy-incoming-orphans root))
                 (kinds (di-report-kinds
                          (reap-deploy-incoming-orphans root))))
             (declare (ignore kinds))
             (let ((seen (di-report-kinds reports)))
               (is (member :unrecognizable seen))
               (is (member :no-record seen))
               (is (member :stale-temp seen)))
             (is (uiop:file-exists-p (di-incoming root "tokbad.lisp")))
             (is (uiop:file-exists-p (di-incoming root "tokbad.registry")))
             (is (uiop:file-exists-p (di-incoming root "tokorphan.lisp")))
             (is (uiop:file-exists-p (di-incoming root ".tokstale.lisp.tmp")))))
      (di-cleanup root))))

(test reap/stale-temp-removed-with-its-reapable-record
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           (di-touch-incoming root "tokgone")
           (di-valid-record root "tokgone" "done" 111 222)
           (di-write-raw root ".tokgone.lisp.tmp" "partial")
           (di-write-raw root ".tokgone.registry.tmp" "partial")
           (let ((reports (reap-deploy-incoming-orphans root)))
             (declare (ignore reports))
             ;; The temp files went WITH the reapable record.
             (is-false (uiop:file-exists-p
                         (di-incoming root ".tokgone.lisp.tmp")))
             (is-false (uiop:file-exists-p
                         (di-incoming root ".tokgone.registry.tmp")))
             (is-false (uiop:file-exists-p (di-incoming root "tokgone.lisp")))
             (is-false (uiop:file-exists-p
                         (di-incoming root "tokgone.registry")))))
      (di-cleanup root))))

(test reap/absent-directory-is-a-silent-no-op
  (let ((root (di-temp-root)))
    (unwind-protect
         (is (null (reap-deploy-incoming-orphans root)))
      (di-cleanup root))))

;;; ---------------------------------------------------------------------------
;;; deploy-model-run-script: spawn without pipe deadlock
;;; ---------------------------------------------------------------------------

(defun di-write-stub-script (root)
  "A stub scripts/data-ui that floods >64 KiB on BOTH streams, records
its MODEL_FILE, and exits 0."
  (let ((script (merge-pathnames "scripts/data-ui" root))
        (env-out (merge-pathnames "stub-env.txt" root)))
    (ensure-directories-exist (merge-pathnames "scripts/" root))
    (with-open-file (out script :direction :output :if-exists :supersede)
      (format out "#!/bin/bash~%
for i in $(seq 1 2000); do
  echo \"stdout-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-$i\"
  echo \"stderr-bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-$i\" >&2
done
printf 'MODEL_FILE=%s\\n' \"$MODEL_FILE\" > ~s
exit 0~%"
              (namestring env-out)))
    (uiop:run-program (list "chmod" "+x" (namestring script)))
    script))

(test run-script/more-than-64kib-on-both-streams-completes
  ;; Under the old :output :string pipes this deadlocks; with private
  ;; temp files it completes and the registry shows a real child PID.
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           (di-write-stub-script root)
           (multiple-value-bind (rel _name _abs)
               (deploy-model-write-file "'(:name big :types ())"
                 (list :name "big") root)
             (declare (ignore _name _abs))
             (let ((token (deploy-model-token-from-path rel)))
               (deploy-model-registry-create token (deploy-model-self-pid) rel root)
               (multiple-value-bind (stdout stderr exit-code)
                   (deploy-model-run-script root "big" rel token)
                 (is (zerop exit-code))
                 (is (> (length stdout) 65536)
                     "stdout exceeded the 64 KiB pipe buffer")
                 (is (> (length stderr) 65536)
                     "stderr exceeded the 64 KiB pipe buffer")
                 ;; The registry advanced to ready with a numeric child.
                 (multiple-value-bind (state _w child _p ok)
                     (parse-deploy-registry-record
                       (di-incoming root (format nil "~a.registry" token)))
                   (declare (ignore _w _p))
                   (is-true ok)
                   (is (equal "ready" state))
                   (is (integerp child))
                   ;; Exactly one MODEL_FILE entry, pointing at the
                   /// unique incoming file.
                   (is (equal
                         (format nil "MODEL_FILE=~a~%" rel)
                         (with-open-file (in (merge-pathnames
                                               "stub-env.txt" root))
                           (let ((s (make-string (file-length in))))
                             (read-sequence s in)
                             s)))))))))
      (di-cleanup root))))

;;; ---------------------------------------------------------------------------
;;; Two workers: one worker's exit never deletes the other's input
;;; ---------------------------------------------------------------------------

(test two-workers/exit-does-not-touch-the-other
  (let ((root (di-temp-root)))
    (unwind-protect
         (progn
           (di-write-stub-script root)
           (multiple-value-bind (rel-a name-a abs-a)
               (deploy-model-write-file "'(:name duo :types ())"
                 (list :name "duo") root)
             (declare (ignore name-a abs-a))
             (multiple-value-bind (rel-b name-b abs-b)
                 (deploy-model-write-file "'(:name duo :types ())"
                   (list :name "duo") root)
               (declare (ignore name-b abs-b))
               (let ((tok-a (deploy-model-token-from-path rel-a))
                     (tok-b (deploy-model-token-from-path rel-b)))
                 (is (not (equal tok-a tok-b)))
                 (deploy-model-registry-create tok-a (deploy-model-self-pid) rel-a
                                               root)
                 (deploy-model-registry-create tok-b (deploy-model-self-pid) rel-b
                                               root)
                 ;; Worker A completes its full teardown.
                 (deploy-model-run-script root "duo" rel-a tok-a)
                 (deploy-model-registry-done tok-a root)
                 (delete-file (di-incoming root
                                           (format nil "~a.lisp" tok-a)))
                 (deploy-model-registry-remove tok-a root)
                 ;; Worker B's input and record are untouched.
                 (is (uiop:file-exists-p
                       (di-incoming root (format nil "~a.lisp" tok-b))))
                 (multiple-value-bind (state _w _c _p ok)
                     (parse-deploy-registry-record
                       (di-incoming root
                                    (format nil "~a.registry" tok-b)))
                   (declare (ignore _w _c _p))
                   (is-true ok)
                   (is (equal "pending" state)))))))
      (di-cleanup root))))
