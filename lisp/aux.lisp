(in-package :data-ui)

(defparameter *guru-meditation-number-function*
  (make-hash-table :test 'equal))

(define-condition validation-error (simple-error error)
  ((context :initarg :context :reader context))
  (:report (lambda (c s)
             (apply #'format s
               (simple-condition-format-control c)
               (simple-condition-format-arguments c)))))

(defun signal-validation-error (format-control &rest format-arguments)
  (error 'validation-error
    :format-control format-control
    :format-arguments format-arguments
    :context 'validation))

(defun eformat (s &rest params)
  ":private: Formats S with PARAMS, just like the FORMAT function would. But, it
first replaces any single newline in S with a space *unless* that newline is
followed by whitespace (to preserve indented lists, bullets, poems, code
blocks, etc.). Paragraph breaks (two or more consecutive newlines) are
preserved. The result is trimmed of leading/trailing whitespace. This is useful
for formatting error messages and multi-line strings. It's main purpose is to
make long texts readable in the source code without affecting the formatting of
the final string."
  (let ((ss (re:regex-replace-all "([^\\n])\\n([^\\s])" s "\\1 \\2")))
    (u:trim (apply #'format (append (list nil ss) params)))))

(defun plist-repeated-keys (plist)
  ":private: Returns a list of non-distinct keys in PLIST. If there are no
repeated keys, returns NIL."
  (loop with seen = (make-hash-table)
    for key in plist by #'cddr
    do (incf (gethash key seen 0))
    finally (return (remove-if-not
                      (lambda (k) (> (gethash k seen) 1))
                      (u:hash-keys seen)))))

(defun add-to-plist (plist plist-new)
  ":private: Returns a new plist that is the result of adding the key-value
pairs from PLIST-NEW to PLIST. If a key exists in both PLIST and PLIST-NEW,
the value from PLIST-NEW is used. This function works only for PLISTS that
have unique keys. If there are duplicate keys, this function throws an error.
The function does not modify the original PLIST or PLIST-NEW."
  (let ((repeated-in-plist (plist-repeated-keys plist))
         (repeated-in-plist-new (plist-repeated-keys plist-new)))
    (when repeated-in-plist
      (error "PLIST has repeated keys ~{~s~^, ~}" repeated-in-plist))
    (when repeated-in-plist-new
      (error "PLIST-NEW has repeated keys ~{~s~^, ~}" repeated-in-plist-new)))
  (loop with keys = (u:distinct-values
                      (append
                        (u:plist-keys plist)
                        (u:plist-keys plist-new)))
    for key in keys
    for value = (or (getf plist-new key) (getf plist key))
    appending (list key value)))

(defun remove-null-value-pairs (plist)
  ":private: Returns a new plist that is the result of removing any key-value
pairs from PLIST where the value is NIL. This function does not modify the
original PLIST."
  (loop for k in plist by #'cddr
    for v in (cdr plist) by #'cddr
    when v append (list k v)))

(defun gmn-fname (guru-meditation-number)
  (gethash
    (subseq guru-meditation-number 0 (position #\- guru-meditation-number))
    *guru-meditation-number-function*))

(defun guru-meditation-number (function-name format-string &key
                                (condition #'error)
                                params
                                log)
  ":private: Generates a log entry and signals an error condition with a Guru
Guru Meditation Number. FUNCTION-NAME is a string with the name of the function
where the error occurred. FORMAT-STRING is a format string like the one the
FORMAT function accepts. CONDITION is the function to call to signal the error
condition. It defaults to ERROR. LOG is a plist that will be used to make a log
entry. If LOG does not include the :in key, that key is filled in with the value
from FUNCTION-NAME. If LOG does not include the :error key, then the string
created from the format string and parameters is used. If the LOG does not
include the :gmn key, then the key is added to LOG with a Guru Meditation Number
generated with the function name (converted into a 6-digit hexadecimal hash) and
a random hexadecimal number with 3 digits. The format of the Guru Meditation
Number is XXXXXX-XXX. The function name can be extracted from that number with
the GMN-FNAME funcition."
  (let* ((fname-hash (u:hash-string function-name :size 6))
          (number (format nil "~a-~a" fname-hash (u:random-hex-number 3)))
          (in (getf log :in function-name))
          (text (apply #'format (append (list nil format-string) params)))
          (text-gmn (format nil "~a [Guru Meditation Number ~a]" text number))
          (logpl (add-to-plist
                   log
                   (list :in in :error text :guru-meditation-number number))))
    (setf (gethash fname-hash *guru-meditation-number-function*) function-name)
    (pl:plog :error logpl)
    (funcall condition text-gmn)))

(defun gmn-ve (function-name format-string &key params log)
  ":private: Shortcut for calling GURU-MEDITATION-NUMBER with the
SIGNAL-VALIDATION-ERROR function."
  (guru-meditation-number function-name format-string
    :params params :log log :condition #'signal-validation-error))

(defmacro report-ve (function-name format-string &rest var-specs)
  "Report a validation error. FUNCTION-NAME is the name of the function where
REPORT-VE is called. FORMAT-STRING is a format string like the ones that the
FORMAT function accepts. VAR-SPECS is a list of variable names. If a variable
name in this list is prefixed with a tilde (~), it forms part of the list of
variables to be passed to FORMAT-STRING. The variables are passed to
FORMAT-STRING without the tilde. All variables, prefixed by a tilde or not
are passed to the logger, stripped of any tilde."
  (let* ((cleaned (mapcar (lambda (spec)
                            (if (and (symbolp spec)
                                  (char= (char (symbol-name spec) 0) #\~))
                              (intern (subseq (symbol-name spec) 1)
                                (symbol-package spec))
                              spec))
                    var-specs))
          (param-symbols (remove-if-not
                           (lambda (spec)
                             (and (symbolp spec)
                               (char= (char (symbol-name spec) 0) #\~)))
                           var-specs))
          (param-cleaned (mapcar (lambda (sym)
                                   (intern (subseq (symbol-name sym) 1)
                                     (symbol-package sym)))
                           param-symbols))
          (log-pairs (mapcan (lambda (sym)
                               (list (u:make-keyword sym) sym))
                       cleaned)))
    `(gmn-ve ,function-name ,format-string
       :params (list ,@param-cleaned)
       :log (list ,@log-pairs))))

(defun gmn-e (function-name format-string &key params log)
  ":private: Shortcut for calling GURU-MEDITATION-NUMBER with the ERROR
function."
  (guru-meditation-number function-name format-string
    :params params :log log :condition #'error))

(defmacro report-e (function-name format-string &rest var-specs)
  "Report an error with automatic param and log construction.

   Symbols starting with ~ are included in :params (without the ~).
   All symbols go into :log (without the ~)."
  (let* ((cleaned (mapcar (lambda (spec)
                            (if (and (symbolp spec)
                                     (char= (char (symbol-name spec) 0) #\~))
                                (intern (subseq (symbol-name spec) 1)
                                        (symbol-package spec))
                                spec))
                          var-specs))
         (param-symbols (remove-if-not
                         (lambda (spec)
                           (and (symbolp spec)
                                (char= (char (symbol-name spec) 0) #\~)))
                         var-specs))
         (param-cleaned (mapcar (lambda (sym)
                                  (intern (subseq (symbol-name sym) 1)
                                          (symbol-package sym)))
                                param-symbols))
         (log-pairs (mapcan (lambda (sym)
                              (list (u:make-keyword sym) sym))
                            cleaned)))
    `(gmn-e ,function-name ,format-string
       :params (list ,@param-cleaned)
       :log (list ,@log-pairs))))

;; TODO: Tests
(defun add-to-list (existing-list &rest new-elements)
  (let ((to-add (set-difference new-elements existing-list :test 'equal)))
    (append existing-list to-add)))

(defun stable-set-difference (list-1 list-2 &key (test #'equal))
  ":private: Like set-difference, but retains the order of LIST-1."
  (loop with keys = (set-difference list-1 list-2 :test test)
    for key in list-1
    when (u:has keys key) collect key))

(defun scoped-path (type-key path)
  (u:join-paths (format nil "~(~a~)" type-key) path))

(defun placeholders (fields &key (start-at 1))
  (loop for a from start-at to (1- (+ start-at (length fields)))
    collect (format nil "$~d" a)))

(defun value-or-nil (value)
  ":private: Returns value unless value is the keyword :null, in which case this
function returns NIL. This is useful for dealing the values that the database
returns for fields that are null."
  (if (equal value :null) nil value))

(defparameter *model-text-max-length* 262144
  ":private: Cap on model text accepted by READ-MODEL-TEXT (characters,
comments included). The largest model today is about 17K; 256K leaves room
for heavily spaced and commented large models.")

(defparameter *model-text-max-depth* 64
  ":private: Cap on nesting depth accepted by READ-MODEL-TEXT. The deepest
model today is 8; this cap, not the length cap, keeps a paren nest from
exhausting the stack.")

(defparameter *model-text-max-float-digits* 7
  ":private: Most digits (both sides of the point combined) a float literal
may carry in model text. Seven survive single precision exactly.")

(defun model-text-shadow (text)
  ":private: Shadow copy of TEXT for lexical checks: every string becomes
\"\" (plus any newlines it contained), every ; comment is dropped, and a
comment's newline is kept — so shadow line N is line N of TEXT. Iterative,
one pass, O(n). Signals report-ve on an unterminated string. The shadow is
what VALID-MODEL-SHADOW and VALID-MODEL-TOKENS scan: because all suspicious
characters live only inside strings and comments, and those are blanked
here, the shadow's view of string/comment boundaries matches the reader's."
  (with-output-to-string (out)
    (let ((len (length text))
          (state :code)
          (i 0)
          (line 1))
      (declare (ignorable state))
      (loop
        while (< i len)
        for ch = (char text i)
        do
        (ecase state
          (:code
            (cond
              ((char= ch #\") (write-char #\" out) (setf state :string))
              ((char= ch #\;) (setf state :comment))
              (t (when (char= ch #\Newline) (incf line))
                 (write-char ch out))))
          (:string
            (cond
              ((char= ch #\\)
               (let ((next (and (< (1+ i) len) (char text (1+ i)))))
                 (when (char= (or next #\Null) #\Newline)
                   (write-char #\Newline out))
                 (incf i)))
              ((char= ch #\") (write-char #\" out) (setf state :code))
              ((char= ch #\Newline) (write-char #\Newline out))))
          (:comment
            (when (char= ch #\Newline)
              (write-char #\Newline out)
              (setf state :code))))
        (incf i))
      (when (eq state :string)
        (report-ve "model-text-shadow"
          "model text line ~a: unterminated string" ~line)))))

(defun valid-model-char-p (ch)
  ":private: T when CH is allowed in a model-text shadow: ASCII letters and
digits, dash, colon, dot, parens, the shadow's own \"\" pair, whitespace
(space, tab, newline, return), or the one leading quote."
  (or (char<= #\a ch #\z)
      (char<= #\A ch #\Z)
      (char<= #\0 ch #\9)
      (member ch '(#\- #\: #\. #\( #\) #\" #\' #\Space #\Tab #\Newline
                   #\Return)
              :test #'char=)))

(defun valid-model-quote (shadow)
  ":private: Check the leading quote on SHADOW: the first non-whitespace
character must be ', the next non-whitespace character after it must be (,
and ' must not appear again. Signals report-ve otherwise. Returns T. Runs
last in VALID-MODEL-SHADOW — the atom-after-quote shape ('not-a-plist
(...)) has balanced parens, so only the quote rule catches the trailing
list."
  (flet ((reject ()
           (report-ve "valid-model-quote"
             "model text must be one quoted plist '(...)'")))
    (let ((start (position-if
                  (lambda (ch) (not (member ch '(#\Space #\Tab #\Newline
                                                 #\Return)
                                      :test #'char=)))
                  shadow)))
      (unless (and start (char= (char shadow start) #\'))
        (reject))
      (let ((open (position-if
                   (lambda (ch) (not (member ch '(#\Space #\Tab #\Newline
                                                  #\Return)
                                       :test #'char=)))
                   shadow :start (1+ start))))
        (unless (and open (char= (char shadow open) #\()) (reject))
        (when (position #\' shadow :start (1+ start)) (reject)))
      t)))

(defun valid-model-shadow (shadow)
  ":private: Character, quote, paren and trailing-content checks on SHADOW
(the output of MODEL-TEXT-SHADOW). Signals report-ve on the first violation
with the line number; returns T otherwise. Newlines survive MODEL-TEXT-SHADOW
from comments and strings, so shadow line N is line N of the original text."
  (loop
    with line = 1
    with depth = 0
    with closed = nil
    for ch across shadow
    do
    (unless (valid-model-char-p ch)
      (let ((display
              (handler-case (format nil "~a" ch)
                (error () (format nil "~a" (char-name ch))))))
        (report-ve "valid-model-shadow"
          "model text line ~a: disallowed character ~a" ~line ~display)))
    (when (char= ch #\Newline) (incf line))
    (cond
      ((char= ch #\() (incf depth)
        (let ((max *model-text-max-depth*))
          (when (> depth max)
            (report-ve "valid-model-shadow"
              "model text line ~a: nesting deeper than ~a" ~line ~max))))
      ((char= ch #\))
        (decf depth)
        (when (< depth 0)
          (report-ve "valid-model-shadow"
            "model text line ~a: unbalanced ')'" ~line))
        (when (and (zerop depth) (not closed))
          (setf closed t)))
      ((and closed (not (member ch '(#\Space #\Tab #\Newline #\Return)
                            :test #'char=)))
        (report-ve "valid-model-shadow"
          "model text line ~a: content after the model's closing ')'" ~line)))
    finally
    (unless (zerop depth)
      (report-ve "valid-model-shadow"
        "model text: ~a unclosed '('" ~depth))
    (return (valid-model-quote shadow))))

(defun model-text-token-p (token)
  ":private: T when TOKEN matches an allowed model-text token shape:
keyword (colon + letters/digits/dashes, no second colon), integer
(-?digits), simple float (-?digits.digits with at most
*MODEL-TEXT-MAX-FLOAT-DIGITS* digits total), or t / nil (either case).
Anything else — 1e3, .5, 1., +1, foo, a:b, ::x — is NIL."
  (flet ((digit-p (ch) (char<= #\0 ch #\9)))
    (cond
      ((member token '("t" "T" "nil" "NIL") :test #'equal) t)
      ((and (>= (length token) 2)
            (char= (char token 0) #\:)
            (notevery (lambda (ch) (char= ch #\:)) token)
            (every
              (lambda (ch)
                (or (char<= #\a ch #\z)
                    (char<= #\A ch #\Z)
                    (digit-p ch)
                    (char= ch #\-)))
              (subseq token 1)))
       t)
      (t
       (let* ((dash-p (and (> (length token) 1)
                           (char= (char token 0) #\-)))
              (body (if dash-p (subseq token 1) token))
              (dot (position #\. body)))
         (if dot
           ;; simple float: digits.digits, one dot, digit on both sides
           (and (every
                  (lambda (ch)
                    (or (char<= #\0 ch #\9) (char= ch #\.)))
                  body)
             (= (count #\. body :test #'char=) 1)
             (/= dot 0)
             (/= dot (1- (length body)))
             (<= (count-if
                   (lambda (ch) (char<= #\0 ch #\9)) body)
               *model-text-max-float-digits*))
           ;; integer: digits only
           (and (plusp (length body))
             (every (lambda (ch) (char<= #\0 ch #\9)) body))))))))

(defun model-text-float-shape-p (token)
  ":private: T when TOKEN matches -?digits.digits regardless of digit
count (the shape alone). VALID-MODEL-TOKENS uses this to give floats with
too many digits their own message."
  (let* ((dash-p (and (> (length token) 1)
                      (char= (char token 0) #\-)))
         (body (if dash-p (subseq token 1) token))
         (dot (position #\. body)))
    (and dot
      (every
        (lambda (ch) (or (char<= #\0 ch #\9) (char= ch #\.)))
        body)
      (= (count #\. body :test #'char=) 1)
      (/= dot 0)
      (/= dot (1- (length body))))))

(defun model-text-token-digit-count (token)
  ":private: Digits (0-9) in TOKEN."
  (count-if (lambda (ch) (char<= #\0 ch #\9)) token))

(defun model-text-token-error (token line)
  ":private: Signal the right report-ve for an invalid TOKEN on LINE: a
float shape with more than *MODEL-TEXT-MAX-FLOAT-DIGITS* digits gets the
digit-count message; anything else is an invalid token. Long tokens are
never embedded — the message names their length instead."
  (let* ((digits (model-text-token-digit-count token))
         (shown (if (<= (length token) 40) token
                  (format nil "(~a characters)" (length token))))
         (max *model-text-max-float-digits*))
    (if (and (model-text-float-shape-p token) (> digits max))
      (report-ve "valid-model-tokens"
        "model text line ~a: float ~a has more than ~a digits" ~line ~shown
        ~max)
      (report-ve "valid-model-tokens"
        "model text line ~a: invalid token ~a" ~line ~shown))))

(defun valid-model-tokens (shadow)
  ":private: Check every token in SHADOW against the allowed shapes (see
MODEL-TEXT-TOKEN-P): keywords, integers, simple floats of at most
*MODEL-TEXT-MAX-FLOAT-DIGITS* digits, t, nil. Signals report-ve on the first
violation with its line. Runs only after VALID-MODEL-SHADOW, so the shadow
holds only allowed characters. Bare symbols never reach the reader, so they
are never interned. Returns T."
  (loop
    with line = 1
    with start = nil
    for i from 0 to (length shadow)
    for done = (= i (length shadow))
    for ch = (and (not done) (char shadow i))
    do
    (when (or done
              (member ch '(#\( #\) #\" #\' #\Space #\Tab #\Newline #\Return)
                :test #'char=))
      (when start
        (let ((token (subseq shadow start i)))
          (unless (model-text-token-p token)
            (model-text-token-error token line)))
        (setf start nil))
      (when (and ch (char= ch #\Newline)) (incf line)))
    (unless (or done start)
      (when (and ch (not (member ch '(#\( #\) #\" #\' #\Space #\Tab
                                      #\Newline #\Return)
                            :test #'char=)))
        (setf start i))))
  t)

(defun valid-model-data (form)
  ":private: Walk FORM (the plist read by READ-MODEL-TEXT) and signal
report-ve on any value that is not model data: strings, integers,
single-floats, keywords, t, nil, or lists of those. A backstop behind the
token rule — it fires only if the token grammar and the reader disagree.
Lists are walked with dolist and recursion is only into list elements, so
recursion depth equals paren depth (at most *MODEL-TEXT-MAX-DEPTH*); a list
with 100,000 elements does not deepen the stack. Returns T."
  (labels
    ((bad (value)
       (let* ((kind (format nil "~a" (type-of value)))
              (text (format nil "~s" value))
              (short (when (<= (length text) 40)
                       (format nil " ~a" text))))
         (report-ve "valid-model-data"
           "model text: ~a~a not allowed in model data" ~kind ~short)))
     (valid-value (value)
       (typecase value
         (null t)
         ((or keyword string integer) t)
         (single-float t)
         (symbol (if (member value '(t nil)) t (bad value)))
         (cons (dolist (element value) (valid-value element)) t)
         (otherwise (bad value)))))
    (valid-value form)))

(defun read-model-text (text)
  ":public: Read model text (an LLM or user artifact) under a closed
lexical vocabulary, returning (quote <plist>) like a plain READ of '(...).
The text is capped at *MODEL-TEXT-MAX-LENGTH* characters; a shadow copy
(strings and ; comments blanked, newlines kept — see MODEL-TEXT-SHADOW) is
checked for allowed characters, exactly one leading ' before (, balanced
and depth-capped parens, and nothing after the closing paren; every token
must be a keyword, an integer, a simple float of at most
*MODEL-TEXT-MAX-FLOAT-DIGITS* digits, t or nil. The original text is then
read with *READ-EVAL* nil as a backstop, and the plist read is walked by
VALID-MODEL-DATA. Signals report-ve (validation-error) on any violation.
Callers take CADR, as with plain READ."
  (unless (and (stringp text) (<= (length text)
                                 *model-text-max-length*))
    (report-ve "read-model-text"
      "model text exceeds ~a characters"
      ~*model-text-max-length*))
  (let* ((shadow (model-text-shadow text))
         (valid-shadow (valid-model-shadow shadow))
         (valid-tokens (valid-model-tokens shadow))
         (form (handler-case
                 (let ((*read-eval* nil))
                   (read-from-string text))
                 (error ()
                   (report-ve "read-model-text"
                     "model text could not be read")))))
    (declare (ignore valid-shadow valid-tokens))
    (valid-model-data (cadr form))
    (list 'quote (cadr form))))

(defun get-deployed-admin-password (model-name)
  ":private: ADMIN_PASSWORD from the deploy script's secrets.env for
MODEL-NAME, or NIL when there is no such file. The path must match
DEPLOY_STATE_DIR in scripts/data-ui (DATA_UI_STATE/deploy/<name>), which
moved off ~/.local/state/data-ui-deploy when DATA_UI_STATE was introduced."
  (handler-case
    (fourth
      (re:split
        "\\n|="
        (u:slurp
          (u:join-paths
            (u:getenv "DATA_UI_STATE" :default "/data/data-ui")
            "deploy" model-name "secrets.env"))))
    (file-error () nil)))
