(in-package :data-ui)

;;
;; plist -> JSON
;;

(defun escape-json-string (str)
  "Escapes a string for safe inclusion in JSON.
   Handles \", \\, and all control characters (including newlines)."
  (with-output-to-string (out)
    (loop for char across str
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\backspace (write-string "\\b" out))
               (#\page (write-string "\\f" out))
               (#\newline (write-string "\\n" out))
               (#\return (write-string "\\r" out))
               (#\tab (write-string "\\t" out))
               (t
                (if (and (char<= #\nul char) (char<= char #\us)) ; control characters
                    (format out "\\u~4,'0x" (char-code char))
                    (write-char char out)))))))

(defun plist-to-json-atom (x)
  "Convert an atom to its JSON representation."
  (cond
    ((null x) "[]")                    ; nil -> empty array
    ((eq x :false) "false")
    ((eq x :null) "null")
    ((stringp x)
     (format nil "\"~a\"" (escape-json-string x)))
    ((numberp x)
     (princ-to-string x))
    ((symbolp x)
     (format nil "\"~(~a~)\"" x))
    (t
     (princ-to-string x))))

(defun plist-to-json-plist (plist)
  "Convert a plist to a JSON object."
  (with-output-to-string (s)
    (write-char #\{ s)
    (loop for (key value) on plist by #'cddr
          for first = t then nil
          do (let ((key-str (format nil "~(~a~)" key))
                   (val-str (plist-to-json-aux value)))
               (unless first
                 (write-char #\, s))
               (format s "\"~a\":~a" key-str val-str)))
    (write-char #\} s)))

(defun plist-to-json-list (lst)
  "Convert a regular list (not a plist) to a JSON array."
  (with-output-to-string (s)
    (write-char #\[ s)
    (loop for el in lst
          for first = t then nil
          do (let ((el-str (plist-to-json-aux el)))
               (unless first
                 (write-char #\, s))
               (write-string el-str s)))
    (write-char #\] s)))

(defun plist-to-json-aux (x)
  "Internal recursive dispatcher for plist-to-json."
  (cond
    ((atom x) (plist-to-json-atom x))
    ((u:plistp x) (plist-to-json-plist x))
    (t (plist-to-json-list x))))

(defun plist-to-json (data)
  "Converts a nested plist (or regular list) into a JSON string according to the specified rules.
   Requires `u:plistp` to be available."
  (plist-to-json-aux data))


;;
;; JSON -> plist
;;

(defun json-string-to-symbol (str)
  "Convert a JSON key string to a keyword symbol (upper-cased)."
  (intern (string-upcase str) :keyword))

(defun parse-json-atom (token)
  "Parse a JSON atomic value (string, number, true, false, null)."
  (cond
    ((stringp token)
     token)
    ((numberp token)
     token)
    ((eq token t) t)                    ; JSON true
    ((eq token :false) :false)          ; we will produce this from the parser
    ((eq token :null) :null)
    (t
     (error "Unexpected JSON atom: ~S" token))))

(defun json-to-plist-object (obj)
  "Convert a parsed JSON object (alist of (string . value)) to a plist."
  (loop for (key . value) in obj
        nconc (list (json-string-to-symbol key)
                    (json-to-plist-aux value))))

(defun json-to-plist-array (arr)
  "Convert a parsed JSON array (list) to a regular Lisp list."
  (mapcar #'json-to-plist-aux arr))

(defun json-to-plist-aux (x)
  "Internal recursive dispatcher."
  (cond
    ((null x) nil)                      ; [] or null in some contexts → nil
    ((and (listp x) (every (lambda (pair) (and (consp pair) (stringp (car pair)))) x))
     ;; It's a JSON object (represented as alist with string keys)
     (json-to-plist-object x))
    ((listp x)
     ;; It's a JSON array
     (json-to-plist-array x))
    (t
     ;; atom: string, number, t, :false, :null
     (parse-json-atom x))))

(defun json-to-plist (json)
  "Converts a JSON string into a nested plist / list structure (inverse of
plist-to-json).  Returns a plist for top-level objects, a list for top-level
arrays."
  (let ((yason:*parse-object-as* :alist))
    (json-to-plist-aux (yason:parse json))))
