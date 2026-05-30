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
  (let* ((number (format nil "~a-~a"
                   (u:hash-string function-name :size 6)
                   (u:random-hex-number 3)))
          (in (getf log :in function-name))
          (text (apply #'format (append (list nil format-string) params)))
          (text-gmn (format nil "~a [Guru Meditation Number ~a]" text number))
          (logpl (add-to-plist
                   log
                   (list :in in :error text :guru-meditation-number number))))
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

(defun fs-path (type-key path)
  (u:join-paths *doc-root* (scoped-path type-key path)))
