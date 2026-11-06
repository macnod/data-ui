(in-package :data-ui)

(def-suite model-text-suite
  :description "read-model-text: closed lexical vocabulary for
model text from users and LLMs")

(in-suite model-text-suite)

;; Probe for non-execution checks: model text must never run code,
;; so a #. form that writes T here proves the read was not reached.
(defvar *mt-probe* nil)

(defmacro mt-rejects (text &body checks)
  "Helper: TEXT must signal validation-error; the condition's
message is bound to MSG for the body's checks."
  `(let ((*mt-probe* nil))
     (handler-case
         (progn
           (read-model-text ,text)
           (fail "read-model-text accepted invalid text"))
       (validation-error (e)
         (let ((msg (format nil "~a" e)))
           (declare (ignorable msg))
           (progn ,@checks))))))

(defmacro mt-accepts (text &body checks)
  "Helper: TEXT must read without signaling; the form read is bound
to FORM for the body's checks."
  `(let ((*mt-probe* nil))
     (handler-case
         (let ((form (read-model-text ,text)))
           (declare (ignorable form))
           (progn ,@checks))
       (validation-error (e)
         (declare (ignorable e))
         (fail "read-model-text rejected valid text: ~a" e)))))

;;; ---------------------------------------------------------------------------
;;; Code execution
;;; ---------------------------------------------------------------------------

(test mt-sharp-dot-rejected
  "#. is rejected without running."
  (mt-rejects "'(:a #.(setf *mt-probe* t))"
    (is-false *mt-probe*)))

(test mt-sharp-dot-in-string-accepted
  "#. inside a string is just text."
  (mt-accepts "'(:help \"#.(foo)\")"
    (is (equal '(:HELP "#.(foo)") (cadr form)))))

(test mt-validate-deploy-sharp-dot
  "validate-deploy-model-text returns (:error ...) for #. — it
does not signal — and the probe stays nil."
  (let ((*mt-probe* nil)
        (result (validate-deploy-model-text
                  "'(:name \"x\" :types (:a #.(setf *mt-probe* t)))")))
    (is (getf result :error))
    (is-false *mt-probe*)))

(test mt-validate-deploy-after-header
  "Generate path shape: header + bad body. The error names the
line of the #. (header lines + 1, the body's first line)."
  (let* ((*mt-probe* nil)
         (header (generate-model-header "x"))
         (header-lines (1+ (count #\Newline header)))
         (result (validate-deploy-model-text
                   (format nil "~a~%~a" header
                     "'(:a #.(setf *mt-probe* t))"))))
    (is (getf result :error))
    (is-false *mt-probe*)
    (is (search
          (format nil "line ~a" (1+ header-lines))
          (getf result :error))
        "line number counts from the header")))

(test mt-set-model-rejects-probe-file
  "set-model on a probe file in models/local/ signals
validation-error before any table work, and the probe stays nil.
The file is deleted in unwind-protect; nothing malicious is
committed."
  (let* ((*mt-probe* nil)
         (name (format nil "mt-probe-~a"
                     (subseq (u:random-hex-number 8) 0 8)))
         (dir (u:join-paths *package-root* "models" "local"))
         (path (u:join-paths dir (format nil "~a.lisp" name))))
    (ensure-directories-exist dir)
    (unwind-protect
        (progn
          (u:spew "'(:a #.(with-open-file (s \"/tmp/opencode/mt-pwned\"
                          :direction :output :if-exists :supersede))
  :types (:a (:fields (:b (:type :text)))))"
            path)
          (signals validation-error (set-model name))
          (is-false *mt-probe*)
          (is-false (probe-file "/tmp/opencode/mt-pwned")))
      (delete-file path))))

(test mt-read-llm-config-sharp-dot
  "read-llm-config with a #. probe in the secret returns (:error
...), not a signaling read, and the probe stays nil."
  (let* ((*mt-probe* nil)
         (name (format nil "mt-llm-~a"
                     (subseq (u:random-hex-number 8) 0 8)))
         (id (deploy-model-find-secret-id name)))
    (unwind-protect
        (progn
          (if id
            (be-update :secrets id
              (list :value "(#.(setf *mt-probe* t))") "admin")
            (be-insert :secrets
              (list :name name :value "(#.(setf *mt-probe* t))")
              "admin"))
          (let ((admin-secrets
                  (be-list :secrets "admin" :limit nil))
                (old nil))
            ;; Point read-llm-config at the probe value by name
            ;; swap: rename probe row to llm-config, keep old.
            (loop for record in (getf admin-secrets :records)
              when (equal (getf record :name) "llm-config")
              do (setf old record))
            (when old
              (be-update :secrets (getf old :id)
                (list :name (format nil "~a-old" name)) "admin"))
            (let ((probe-id (deploy-model-find-secret-id name)))
              (be-update :secrets probe-id
                (list :name "llm-config") "admin"))
            (unwind-protect
                (let ((result (read-llm-config)))
                  (is (getf result :error))
                  (is-false *mt-probe*))
              ;; restore
              (let ((probe-id (deploy-model-find-secret-id
                                "llm-config")))
                (when probe-id
                  (be-delete :secrets probe-id "admin")))
              (when old
                (be-update :secrets (getf old :id)
                  (list :name "llm-config") "admin"))))
          ;; cleanup probe row under its temporary name too
          (let ((probe-id (deploy-model-find-secret-id name)))
            (when probe-id (be-delete :secrets probe-id "admin")))))))

(test mt-read-llm-config-temperature
  "A valid llm-config with :temperature 0.3 parses to a float."
  (let* ((name (format nil "mt-llm-~a"
                     (subseq (u:random-hex-number 8) 0 8)))
         (old-id (deploy-model-find-secret-id "llm-config")))
    (when old-id
      (be-update :secrets old-id
        (list :name (format nil "~a-old" name)) "admin"))
    (unwind-protect
        (progn
          (be-insert :secrets
            (list :name "llm-config"
              :value "(:url \"https://x\" :model \"m\"
                       :api-key \"k\" :temperature 0.3)")
            "admin")
          (let ((result (read-llm-config)))
            (is (getf result :ok))
            (is (= 0.3 (getf (getf result :ok) :temperature)))))
      (let ((id (deploy-model-find-secret-id "llm-config")))
        (when id (be-delete :secrets id "admin")))
      (when old-id
        (be-update :secrets old-id
          (list :name "llm-config") "admin")))))

;;; ---------------------------------------------------------------------------
;;; Lexical rules
;;; ---------------------------------------------------------------------------

(test mt-shadow-comment-and-string
  "A ; inside a string is not a comment; a \" inside a comment is
not a string. The shadow blanks both correctly."
  (is (equal "'(:a \"\" 
 :b 2)"
        (model-text-shadow "'(:a \"x;y\" ; \"z
 :b 2)"))))

(test mt-escapes-in-strings
  "\\\" and \\\\ inside a string are accepted; a \\ outside a
string is rejected."
  (mt-accepts "'(:a \"he said \\\"hi\\\" and \\\\ok\")"
    (is (equal "he said \"hi\" and \\ok"
          (getf (cadr form) :a))))
  (mt-rejects "'(:a b\\c)"))

(test mt-disallowed-characters
  "Block comments, char literals, |symbols|, backquote, comma and
non-ASCII letters outside strings are rejected, naming the line."
  (mt-rejects "'(:a #|x|# 1)" (is (search "line 1" msg)))
  (mt-rejects "'(:a #\\b)" (is (search "line 1" msg)))
  (mt-rejects "'(:a |sym|)" (is (search "line 1" msg)))
  (mt-rejects "'(:a `b)" (is (search "line 1" msg)))
  (mt-rejects "'(:a ,b)" (is (search "line 1" msg)))
  (mt-rejects "'(:a é)" (is (search "line 1" msg)))
  (mt-accepts "'(:a \"é è ü\")"))

(test mt-unterminated-string
  "An unterminated string is rejected with its opening line."
  (mt-rejects "'(:a \"oops
:b 2)"
    (is (search "line 1" msg))))

(test mt-paren-balance
  "Extra ), unclosed (, and content after the closing paren are
each rejected."
  (mt-rejects "'(:a 1))")
  (mt-rejects "'(:a (:b)"
    (is (search "unclosed" msg)))
  (mt-rejects "'(:a 1) trailing"
    (is (search "content after" msg))))

(test mt-quote-rules
  "Missing quote, second quote, and an atom after the quote are
rejected."
  (mt-rejects "(:a 1)"
    (is (search "quoted plist" msg)))
  (mt-rejects "'(:a 1) '(:b 2)"
    (is (search "content after" msg)))
  (mt-rejects "'not-a-plist (:types (:x))"
    (is (search "quoted plist" msg)
        "atom after the quote cannot hide a trailing list")))

(test mt-depth-cap
  "Depth 64 is accepted; depth 65 and a 100,000-paren nest are
rejected by the scanner (no stack exhaustion)."
  (mt-accepts
    (concatenate 'string "'"
      (make-string 64 :initial-element #\()
      (make-string 64 :initial-element #\))))
  (mt-rejects
    (concatenate 'string "'"
      (make-string 65 :initial-element #\()
      (make-string 65 :initial-element #\)))
    (is (search "deeper than 64" msg)))
  (mt-rejects
    (concatenate 'string "'"
      (make-string 100000 :initial-element #\()
      (make-string 100001 :initial-element #\))))
  (is-false *mt-probe*))

(test mt-length-cap
  "Exactly 262144 characters is accepted; 262145 is rejected."
  (let ((pad (- 262144 (length "'(:a 1)"))))
    (mt-accepts
      (concatenate 'string "'(:a 1)"
        (make-string pad :initial-element #\Space))))
  (let ((pad (- 262145 (length "'(:a 1)"))))
    (mt-rejects
      (concatenate 'string "'(:a 1)"
        (make-string pad :initial-element #\Space))
      (is (search "exceeds" msg)))))

(test mt-length-cap-counts-comments
  "Through validate-deploy-model-text, a valid model behind a
leading comment header that pushes the total past the cap is
rejected with the length message."
  (let* ((model "'(:name \"x\" :types (:a (:fields (:b (:type :text)))))")
         (pad (- 262145 (length model)))
         (header (format nil ";; ~a" (make-string pad :initial-element #\x)))
         (result (validate-deploy-model-text
                   (concatenate 'string header "~%" model))))
    (is (getf result :error))
    (is (search "exceeds" (getf result :error))
        "the cap fires before the comments-only check")))

;;; ---------------------------------------------------------------------------
;;; Tokens and data
;;; ---------------------------------------------------------------------------

(test mt-token-accepts
  "Integers, negative integers, simple floats (<= 7 digits), T and
NIL in either case."
  (mt-accepts "'(:a 1 :b -2 :c 1.5 :d -0.25 :e 123456.7 :f 0.000123)"
    (is (= 1.23e-4 (getf (cadr form) :f)) "7-digit float survives"))
  (mt-accepts "'(:a T :b NIL :c t :d nil)"))

(test mt-float-digit-cap
  "Floats with more than 7 digits are rejected, naming the line."
  (mt-rejects "'(:a 1234567.89)" (is (search "more than 7" msg)))
  (mt-rejects "'(:a 0.0000001)" (is (search "more than 7" msg)))
  (mt-rejects "'(:a 1.12345678)" (is (search "more than 7" msg))))

(test mt-token-rejects
  "Exponents, suffixes, bare points, signs, dotted pairs, symbols
and package markers are invalid tokens."
  (loop for token in '("1e3" "1e-3" "1.5e3" "1d0" "1f0" ".5" "1."
                       "1.5x" "foo" "a:b" "::x" ":a:b")
    do (mt-rejects (format nil "'(:a ~a)" token)
         (is (search "invalid token" msg)
             "token ~a rejected" token)))
  (mt-rejects "'(:a . :b)" (is (search "invalid token" msg))))

(test mt-long-token-message
  "A 1000-character invalid token produces '(N characters)', not
the token."
  (mt-rejects
    (format nil "'(:a ~a)" (make-string 1000 :initial-element #\z))
    (is (search "(1000 characters)" msg))
    (is-false (search (make-string 40 :initial-element #\z) msg))))

(test mt-wide-list-no-stack
  "A wide list at depth 1 is accepted (length cap respected)
without deepening the stack. 40,000 integers — member, not getf,
extracts the tail so an odd count cannot misread."
  (let* ((ints (format nil "~{~a ~}"
                (loop repeat 40000 collect "1")))
         (text (concatenate 'string "'(:kws " ints ")")))
    (is (< (length text) 262144))
    (mt-accepts text
      (let ((tail (cdr (member :kws (cadr form)))))
        (is (= 40000 (length tail)))))))

;;; ---------------------------------------------------------------------------
;;; Compatibility
;;; ---------------------------------------------------------------------------

(defun mt-model-files ()
  "Every .lisp under models/ and models/test/, skipping widgets
(the ::model: template placeholder — unreadable by design)."
  (remove-if
    (lambda (path)
      (equal "widgets.lisp" (u:filename-only path)))
    (append
      (u:directory-listing
        (u:join-paths *package-root* "models")
        :files-only t :abs-filter
        (format nil "^~a/[^/]+\\.lisp$"
          (u:join-paths *package-root* "models")))
      (u:directory-listing
        (u:join-paths *package-root* "models" "test")
        :files-only t))))

(test mt-all-models-pass
  "Every checked-in model and fixture passes read-model-text, and
the form read equals a plain read with *read-eval* nil. A future
demo that breaks the lexical rules fails here."
  (loop for path in (mt-model-files-collect)
    do (let ((text (u:slurp path)))
         (is (equal (let ((*read-eval* nil)) (cadr (read-from-string text)))
               (cadr (read-model-text text)))
             "~a diverges from a plain read" path))))

(defun mt-model-files-collect ()
  "Materialize mt-model-files once (directory walks are not
cheap)."
  (mt-model-files))

(test mt-deploy-write-file-verbatim
  "deploy-model-write-file writes the validated text verbatim:
the file equals the text byte for byte and reads back to the same
form, comments included."
  (let* ((text ";; a comment
'(:name \"mt-rt\" :types (:a (:fields (:b (:type :text)))))")
         (plist (cadr (read-model-text text)))
         (tmp (u:join-paths *package-root* "models" "local")))
    (ensure-directories-exist tmp)
    (unwind-protect
        (multiple-value-bind (rel name path)
            (deploy-model-write-file text plist *package-root*)
          (declare (ignore rel))
          (is (equal "mt-rt" name))
          (is (equal text (u:slurp path)))
          (is (equal plist (cadr (read-model-text (u:slurp path)))))
          (delete-file path))
      (when (probe-file (u:join-paths tmp "mt-rt.lisp"))
        (delete-file (u:join-paths tmp "mt-rt.lisp"))))))

(test mt-deploy-write-floats-verbatim
  "A model with 0.0005 and 123456.7 floats writes and reads back
without exponent notation appearing in the file."
  (let* ((text "'(:name \"mt-fl\" :types (:a (:fields (:b (:type :text))))
 :small 0.0005 :big 123456.7)")
         (plist (cadr (read-model-text text)))
         (tmp (u:join-paths *package-root* "models" "local")))
    (ensure-directories-exist tmp)
    (unwind-protect
        (multiple-value-bind (rel name path)
            (deploy-model-write-file text plist *package-root*)
          (declare (ignore rel name))
          (is (search "0.0005" (u:slurp path))
              "float spelling preserved")
          (is (search "123456.7" (u:slurp path))
              "float spelling preserved")
          (delete-file path))
      (when (probe-file (u:join-paths tmp "mt-fl.lisp"))
        (delete-file (u:join-paths tmp "mt-fl.lisp"))))))
