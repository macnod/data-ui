(in-package :data-ui)

;;
;; Public pages: the request-time renderer (todo "Bot-crawlable
;; public pages", child 1) and its routes (child 2, below).
;;
;; Every render function is a pure function of (the live
;; *compiled-model*, guest reads through be-list / be-rec, origin) to
;; a string or nil. No tree, no pass, no worker, no manifest, no
;; set-model coupling. The anonymous reach of /p/ never exceeds the
;; guest token's reach, and is a strict subset of it: public-type-p
;; requires :guest-allowed t (the same switch that lets anyone log in
;; as guest) plus type-level guest read.
;;

;;
;; BEGIN Origin
;;

(defparameter *public-origin-regex*
  "^https?://[a-zA-Z0-9][a-zA-Z0-9.-]*(:[0-9]+)?/?$"
  "An absolute http or https origin with no userinfo, no path other
  than empty or a single /, no query, and no fragment.")

(defun valid-public-origin (&optional (origin *public-origin*))
  ":private: The canonical PUBLIC_ORIGIN every public URL is built
from. Accepts one absolute http or https scheme, a host, an optional
port, no userinfo, no path other than empty or exactly one /, no
query, no fragment; signals report-e (naming PUBLIC_ORIGIN)
otherwise. Returns ORIGIN with a single trailing / removed and
nothing else changed, so PUBLIC_ORIGIN=https://blog.example/ and
=https://blog.example= produce identical URLs and //p/ cannot occur.
Stored-as-read *public-origin* is validated here at the start of
every render, not at load, so a REPL can fix it."
  (let* ((string (format nil "~a" origin))
         (match (re:scan *public-origin-regex* string)))
    (unless match
      (report-e "valid-public-origin"
        "PUBLIC_ORIGIN must be an absolute http or https origin with ~
         no userinfo, path, query, or fragment; got ~a."
        ~string))
    (re:regex-replace "/$" string "")))

(defun public-origin-absolute (origin path)
  ":private: ORIGIN (already canonical) + PATH, a site-absolute path
  string starting with /. One / between them, never two."
  (format nil "~a~a" origin path))

;;
;; END Origin
;;

;;
;; BEGIN Eligibility and projection
;;

(defun public-type-p (type-key)
  ":private: T when TYPE-KEY is renderable at /p/. Requires
(model-guest-allowed) first — a model with guest login off publishes
nothing, whatever its :type-roles say. The compiled type must not be
:base, :built-in, :internal, :is-joiner, a rollup
(:phase-a-shape :measure), :user-setting, or :tree (directories and
fs-backed file types both carry :tree; their bytes are served by the
file route, they are not documents), must have no view-level scope
(be-list applies :scope :user, be-rec does not, so a scoped type's
index and record pages would disagree), must be :display t, must
declare a :view-form whose :fields is t or a non-empty list, and the
guest must hold type-level read. Base types stay out even with a
guest grant: their rows have no per-record resources, so a type-level
grant cannot express per-record publication."
  (and (model-guest-allowed)
    (let ((type-def (u:tree-get *compiled-model* type-key)))
      (and type-def
        (not (getf type-def :base))
        (not (getf type-def :built-in))
        (not (getf type-def :internal))
        (not (getf type-def :is-joiner))
        (not (eq (getf type-def :phase-a-shape) :measure))
        (not (getf type-def :user-setting))
        (not (getf type-def :tree))
        (not (u:tree-get type-def :views :main :scope))
        (getf type-def :display)
        (let ((fields (u:tree-get type-def :view-form :fields)))
          (and fields
            (or (eq fields t) (and (listp fields) fields))))
        (a:user-allowed *rbac* *guest-user* "read"
          (type-resource-name type-key))))))

(defun public-types ()
  ":private: The eligible type keys in model declaration order."
  (loop for type-key in *compiled-model* by #'cddr
    when (public-type-p type-key)
    collect type-key))

(defun public-view-field-keys (type-key)
  ":private: The rendered field body's allowlist, in View-form order
(the author's :fields list when explicit, model field order when t):
form-field-keys on :view-form minus :id, :created-at, :updated-at,
:roles, and any field whose :type is :password / :file / :button or
whose :ui :widget is :hidden. form-field-keys drops :file / :button
only when :fields is t; an explicit list naming one still returns it,
so this second filter is mandatory. Markdown, scalars, singular
targets, M2M lists, and image widgets stay."
  (loop for key in (form-field-keys type-key :view-form)
    for field-def = (u:tree-get *compiled-model* type-key :fields key)
    unless (member key '(:id :created-at :updated-at :roles))
    unless (member (getf field-def :type) '(:password :file :button))
    unless (eq (u:tree-get field-def :ui :widget) :hidden)
    collect key))

;;
;; END Eligibility and projection
;;

;;
;; BEGIN Guest reads
;;

(defun public-record-sibling-keys (type-key field-keys)
  ":private: The record-permalink \"<field>-id\" sibling keys for the
singular-target fields in FIELD-KEYS that are not suppression-marked
(decorate-record-with-target-ids emits exactly these on be-rec and
list-result records)."
  (loop for key in field-keys
    for field-def = (u:tree-get *compiled-model* type-key :fields key)
    for sibling = (u:make-keyword (format nil "~a-id" key))
    when (and (singular-target-field-p field-def)
           (not (getf field-def :suppress-sibling)))
    collect sibling))

(defun project-public-record (type-key record field-keys)
  ":private: Rebuild RECORD keeping only :id, the FIELD-KEYS, and the
  \"<field>-id\" siblings present on the record. The envelope's own
  decorations (:roles, :update / :delete caps) never survive the
  rebuild, and neither does any field the View form did not name."
  (let ((keys (append (cons :id field-keys)
                (public-record-sibling-keys type-key field-keys))))
    (loop for key in keys
      for value = (getf record key)
      when value
      append (list key value))))

(defun timestamp-or-nil (value)
  ":private: The view-result timestamp conversion applied to a raw
query value: a universal-time integer becomes the backend's UTC ISO
string; nil and :null stay absent."
  (when (and value (not (equal value :null)))
    (dt:timestamp-string :universal-time value)))

(defun record-timestamps (type-key ids)
  ":private: One query for the created / updated timestamps of IDS —
the projection never carries them (an explicit :view-form :fields
list holds only :id plus the named fields, so nothing downstream
could recover them). Returns an alist of id to (created-at
updated-at) strings in the backend's UTC ISO shape. Skips the query
on an ineligible type or an empty id list."
  (when (and ids (public-type-p type-key))
    (let* ((table (u:tree-get *compiled-model* type-key :table-name))
           (sql (format nil
                   "select id, created_at, updated_at from ~a ~
                    where id in (~{~a~^, ~})"
                 table (placeholders ids))))
      (loop for row in (a:with-rbac (*rbac*)
                         (a:rbac-query (cons sql ids)))
        collect (cons (getf row :id)
                 (list (timestamp-or-nil (getf row :created-at))
                   (timestamp-or-nil (getf row :updated-at))))))))

(defun attach-timestamps (records timestamps)
  ":private: Pair each projected record wrapper with its timestamps
from the record-timestamps alist."
  (loop for rec in records
    for entry = (assoc (getf (getf rec :record) :id) timestamps
                   :test #'equal)
    collect (list :record (getf rec :record)
              :created-at (first (cdr entry))
              :updated-at (second (cdr entry)))))

(defun guest-records (type-key &key sort limit filters)
  ":private: Every guest-readable row of TYPE-KEY as a renderer
wrapper (:record <projected plist> :created-at <string> :updated-at
<string>) — the field body and the timestamps are kept apart so the
body printer never sees them. Reads (be-list TYPE-KEY *guest-user*
:form :view-form :skip-allowed-values t) with the given :sort, :limit
(nil unless given), and :filters; projects each record through
public-view-field-keys plus :id plus the \"<field>-id\" siblings; and
attaches record-timestamps once for the batch. NIL immediately when
(public-type-p TYPE-KEY) is false — eligibility lives in the read
helpers, not only in the route, so a direct call on a scoped,
guest-disabled, user-setting, or role-less type reads nothing. Never
admin reads filtered after the fact."
  (when (public-type-p type-key)
    (let* ((result (be-list type-key *guest-user*
                     :form :view-form
                     :sort sort
                     :limit limit
                     :filters filters
                     :skip-allowed-values t))
           (records (getf result :records))
           (field-keys (public-view-field-keys type-key))
           (ids (loop for r in records collect (getf r :id)))
           (timestamps (record-timestamps type-key ids)))
      (attach-timestamps
        (loop for r in records
          collect (list :record
                    (project-public-record type-key r field-keys)))
        timestamps))))

(defun guest-record (type-key id)
  ":private: One guest-readable row as a renderer wrapper, or nil
when the row is missing or unreadable — \"no page\", never an error.
The authorization gate for the record page and for every FK link
target. be-rec defaults to :update-form; the renderer must pass
:view-form."
  (when (public-type-p type-key)
    (let* ((result (be-rec id *guest-user*
                     :form :view-form :type-key type-key))
           (record (getf result :record))
           (field-keys (public-view-field-keys type-key)))
      (when record
        (first
          (attach-timestamps
            (list (list :record
                    (project-public-record type-key record field-keys)))
            (record-timestamps type-key (list id))))))))

;;
;; END Guest reads
;;

;;
;; BEGIN Timestamps as UTC
;;

(defun parse-utc-timestamp (string)
  ":private: The backend's UTC ISO shape (2026-09-26T01:04:10, T or
space) to a universal-time integer, or nil when STRING does not match
*timestamp-regex* (a nil yields no date tag rather than a malformed
one). The database runs UTC (Etc/UTC), so zone 0 is exact."
  (let ((v (nth-value 1
              (re:scan-to-strings
                "^(\\d{4})-(\\d\\d)-(\\d\\d)[T ](\\d\\d):(\\d\\d):(\\d\\d)$"
                (or string "")))))
    (when v
      (encode-universal-time
        (parse-integer (aref v 5))
        (parse-integer (aref v 4))
        (parse-integer (aref v 3))
        (parse-integer (aref v 2))
        (parse-integer (aref v 1))
        (parse-integer (aref v 0))
        0))))

(defun iso-utc (string)
  ":private: The backend ISO string with the UTC zone suffix
(2026-09-26T01:04:10Z), or nil when STRING is absent or does not
match the timestamp shape."
  (when (and string (re:scan *timestamp-regex* string))
    (format nil "~aZ" string)))

(defun iso-utc-date (string)
  ":private: The YYYY-MM-DD date part of iso-utc's input, or nil."
  (when (and string (re:scan *timestamp-regex* string))
    (subseq string 0 10)))

(defparameter *rfc822-days*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *rfc822-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun rfc822-utc (string)
  ":private: RFC 822 with an explicit UTC zone suffix (Fri, 26 Sep
2026 01:04:10 +0000), or nil when STRING does not match the
timestamp shape."
  (let ((ut (parse-utc-timestamp string)))
    (when ut
      (multiple-value-bind (second minute hour date month year day)
        (decode-universal-time ut 0)
        (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d +0000"
          (aref *rfc822-days* day)
          date
          (aref *rfc822-months* (1- month))
          year hour minute second)))))

;;
;; END Timestamps as UTC
;;

;;
;; BEGIN Titles and descriptions
;;

(defun nonblank-string-p (value)
  (and (stringp value) (plusp (length (u:trim value))) value))

(defun collapse-whitespace (string)
  ":private: Every run of whitespace to one space, trimmed — the
one-line plain text titles and descriptions are built from."
  (u:trim
    (re:regex-replace-all "\\s+"
      (remove-if (lambda (c) (not (xml-char-p c)))
        (format nil "~a" string))
      " ")))

(defun first-markdown-field (type-key fields)
  ":private: The first :widget :markdown field key in View-form order
whose value is a string (the title fallback and description source)."
  (loop for key in (public-view-field-keys type-key)
    for def = (u:tree-get *compiled-model* type-key :fields key)
    when (and (eq (u:tree-get def :ui :widget) :markdown)
           (stringp (getf fields key)))
    do (return key)))

(defun public-title (type-key rec)
  ":private: The record's presentation title: the first field in
identity-keys (model order — that is the order identity-keys uses)
whose projected value is a nonblank string; else the first nonblank
line of the first :widget :markdown field in View-form order, as
plain text; else the humanized type key plus the first eight
characters of the uuid. The address is always the full uuid; the
title is presentation only."
  (let* ((fields (getf rec :record))
         (by-identity (loop for key in (identity-keys type-key)
                        for value = (nonblank-string-p (getf fields key))
                        when value do (return value)))
         (md-key (first-markdown-field type-key fields))
         (by-markdown (when md-key
                        (first-nonblank-line
                          (plain-markdown-text (getf fields md-key))))))
    (or by-identity
      (and by-markdown (nonblank-string-p by-markdown))
      (format nil "~:(~a~) ~a" type-key (subseq (getf fields :id) 0 8)))))

(defun first-nonblank-line (text)
  (when (stringp text)
    (loop for line in (re:split "\\n" text)
      for trimmed = (u:trim line)
      when (plusp (length trimmed))
      do (return trimmed))))

(defun public-description (type-key rec)
  ":private: Up to 200 characters of the plain text of the first
markdown field (View-form order), or of the title when there is none,
collapsed to one line, cut on a word boundary when one exists in the
last 40 characters. Empty means omit the description tags."
  (let* ((fields (getf rec :record))
         (md-key (first-markdown-field type-key fields))
         (text (collapse-whitespace
                 (or (and md-key (plain-markdown-text (getf fields md-key)))
                   (public-title type-key rec)))))
    (when (plusp (length text))
      (if (<= (length text) 200)
        text
        (let* ((hard (subseq text 0 200))
               (cut (position #\space hard :from-end t :start 160)))
          (if cut (subseq hard 0 cut) hard))))))

;;
;; END Titles and descriptions
;;

;;
;; BEGIN HTML helpers
;;

(defun html-escape (string)
  ":private: Escape & < > \" ' for HTML text and attribute contexts."
  (with-output-to-string (s)
    (loop for ch across (format nil "~a" string)
      do (case ch
           (#\& (write-string "&amp;" s))
           (#\< (write-string "&lt;" s))
           (#\> (write-string "&gt;" s))
           (#\" (write-string "&quot;" s))
           (#\' (write-string "&#39;" s))
           (t (write-char ch s))))))

(defun attribute-safe-p (string)
  ":private: NIL when STRING holds NUL or a C0 control (tab, newline,
and return excepted — legal inside attribute values); attribute
output drops such a value rather than interpolate it."
  (not (find-if
         (lambda (c)
           (let ((code (char-code c)))
             (and (< code 32)
               (not (member c '(#\Tab #\Newline #\Return))))))
         string)))

(defun html-attribute (value)
  ":private: VALUE escaped for a double-quoted attribute, or nil when
it is absent or holds NUL / C0 controls (the attribute is dropped,
not interpolated)."
  (let ((string (and value (format nil "~a" value))))
    (when (and string (plusp (length string)) (attribute-safe-p string))
      (html-escape string))))

(defun url-encode-segment (segment)
  ":private: Percent-encode one path segment (UTF-8 bytes; alphanumerics
and - . _ ~ kept) — the spelling 02's decoder reverses."
  (with-output-to-string (s)
    (loop for byte across (b:string-to-octets
                             (format nil "~a" segment) :encoding :utf-8)
      for ch = (code-char byte)
      do (if (or (alphanumericp ch)
               (member ch '(#\- #\. #\_ #\~)))
           (write-char ch s)
           (format s "%~2,'0x" byte)))))

(defun html-link (href text &key title external)
  ":private: An anchor with everything escaped; EXTERNAL (an absolute
http(s) URL) carries target / rel like the SPA's Markdown.tsx."
  (let ((href-attr (html-attribute href))
         (title-attr (html-attribute title))
         (external-attr (when external
                          " target=\"_blank\" rel=\"noopener noreferrer\"")))
    (if href-attr
      (format nil "<a href=\"~a\"~@[ title=\"~a\"~]~a>~a</a>"
        href-attr title-attr
        (or external-attr "")
        (html-escape text))
      (html-escape text))))

(defun html-document (head body)
  ":private: The full HTML document."
  (format nil "<!DOCTYPE html>~%<html lang=\"en\">~%<head>~a~%</head>~%<body>~a~%</body>~%</html>"
    head body))

(defparameter +public-style+
  "body{margin:0;font-family:system-ui,-apple-system,sans-serif;background:#ffffff;color:#1a1a1a;line-height:1.6}
header.site{background:#f5f5f5;border-bottom:1px solid #eee;padding:.75rem 1rem;display:flex;align-items:baseline;gap:1rem;flex-wrap:wrap}
header.site h1.site{margin:0;font-size:1.15rem}
header.site h1.site a{color:inherit;text-decoration:none}
header.site nav{margin-left:auto}
main{max-width:44rem;margin:1.5rem auto;padding:0 1rem}
.meta{color:#888;font-size:.9rem;margin:.25rem 0 1rem}
a{color:#6cf}
img{max-width:100%;height:auto}
figure{margin:1rem 0}
figcaption{color:#888;font-size:.9rem}
dl.fields{display:grid;grid-template-columns:max-content 1fr;gap:.25rem 1rem;margin:1rem 0}
dl.fields dt{color:#888;font-size:.9rem}
dl.fields dd{margin:0}
table{border-collapse:collapse;width:100%;margin-top:1rem}
th,td{border:1px solid #ccc;padding:8px;text-align:left}
th{background:#f5f5f5}
blockquote{margin:.5rem 0;padding:.1rem 0 .1rem .9rem;border-left:3px solid #ccc;color:#888}
blockquote p{margin:.35rem 0}
pre{background:#f5f5f5;border:1px solid #eee;padding:.75rem;overflow-x:auto}
code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}
section.children{margin-top:2rem;border-top:1px solid #eee;padding-top:1rem}
section.children h2{font-size:1.1rem}
.child{margin:.75rem 0;padding:.5rem .75rem;border:1px solid #eee;border-radius:6px}
.child .meta{margin:.15rem 0 .35rem}
footer.site{border-top:1px solid #eee;margin-top:2rem;padding:.75rem 1rem;color:#888;font-size:.9rem}
ul.index{list-style:none;padding:0}
ul.index li{margin:.5rem 0}"
  "The public pages' inline stylesheet: copied light-theme values of
the SPA's custom properties in web/src/index.css (--bg, --fg, --link,
--muted, --border-light, --header-bg, the system-ui font) so the pages
read as the same product. Copied values, not a build artifact; no SPA
parity, no dark theme in v1.")

;;
;; END HTML helpers
;;

;;
;; BEGIN Markdown: parse, plain text, and our own HTML walker
;;

(defun markdown-doc (source)
  ":private: SOURCE parsed with the tables and code-blocks extensions
enabled for parsing only (3bmd's own HTML printer is never called —
it interpolates URLs unescaped)."
  (let ((mdt:*tables* t)
         (mdc:*code-blocks* t))
    (mdg:parse-doc (md-ext:expand-tabs (or source "") :add-newlines t))))

(defun md-node-plain (node)
  ":private: The plain text of one Markdown AST node — strings as-is,
code and verbatim included, images contributing their alt, links
their label. The source of the no-identity title,
public-description, and the feed <description>."
  (cond
    ((stringp node) node)
    ((consp node)
     (case (car node)
       (:heading (md-nodes-plain (getf (cdr node) :contents)))
       (:explicit-link (md-nodes-plain (getf (cdr node) :label)))
       (:reference-link (md-nodes-plain (getf (cdr node) :label)))
       (:image (md-image-plain (cdr node)))
       (:reference "")
       ((3bmd-code-blocks::code-block)
        (getf (cdr node) :content))
       (t (md-nodes-plain (cdr node)))))
    (t "")))

(defun md-nodes-plain (nodes)
  (apply #'concatenate 'string (mapcar #'md-node-plain nodes)))

(defun md-image-plain (rest)
  "The image node's label plain text (the node wraps its link plist
one level deeper than :explicit-link — printer.lisp does
(cdr (first rest)))."
  (md-nodes-plain (getf (cdr (first rest)) :label)))

(defun plain-markdown-text (source)
  ":private: The Markdown SOURCE as one plain-text string, whitespace
collapsed — never a regex over the raw Markdown."
  (collapse-whitespace (md-nodes-plain (markdown-doc source))))

(defun md-label-key (label)
  ":private: Label nodes to their concatenated text — the key the
reference alist is built from, the way extract-refs keys
print-label-to-string."
  (u:trim (md-nodes-plain label)))

(defun md-references (doc)
  ":private: Top-level :reference definitions as an alist of label
text to (source title) — the walker's own resolution table."
  (loop for node in doc
    when (and (consp node) (eq (car node) :reference))
    collect (cons (md-label-key (getf (cdr node) :label))
             (list (getf (cdr node) :source)
               (getf (cdr node) :title)))))

(defun strip-url-noise (url)
  ":private: URL with NUL, C0 controls, whitespace, and DEL removed;
nil when nothing remains. Applied before any classification, so an
obfuscated java<TAB>script: cannot slip through."
  (let ((clean (remove-if
                 (lambda (c)
                   (let ((code (char-code c)))
                     (or (<= code 32) (= code 127))))
                 (format nil "~a" url))))
    (unless (zerop (length clean)) clean)))

(defun public-url-policy (url &key image)
  ":private: The one URL policy for everything the Markdown walker
emits. Returns the URL to emit, or nil (nil means print the label as
text / no anchor / no <img>). Links: keep a relative /p/ path, a
same-document fragment, a mailto: address with no @ tricks, or an
absolute http / https URL (case-insensitive scheme) with no
userinfo. Dropped: javascript:, data:, file:, vbscript: and any other
scheme, protocol-relative //, and anything with credentials. Images
(IMAGE t): keep only a site-absolute /p/files/... source — external,
protocol-relative, data:, and /api/file sources become alt text."
  (let ((clean (strip-url-noise url)))
    (cond
      ((null clean) nil)
      (image
        (when (re:scan "^/p/files/[a-zA-Z0-9-]+/" clean) clean))
      ((re:scan "^/p/" clean) clean)
      ((re:scan "^#" clean) clean)
      ((re:scan "^mailto:" clean)
        (when (re:scan "^mailto:[^@\\s]+@[^@\\s]+$" clean) clean))
      ((re:scan "^(?i)https?://" clean)
        (when (re:scan "^(?i)https?://[^/@]+([/?#]|$)" clean) clean))
      (t nil))))

(defun external-link-p (url)
  ":private: T for an absolute http(s) URL — the target / rel case."
  (re:scan "^(?i)https?://" url))

(defun md-entity-html (strings)
  ":private: A :entity node passes through only when it matches
&#?[A-Za-z0-9]+;, else it is escaped like any text."
  (let ((text (apply #'concatenate 'string strings)))
    (if (re:scan "^&#?[A-Za-z0-9]+;$" text)
      text
      (html-escape text))))

(defun md-link-html (source label-html &key title)
  ":private: One anchor through the URL policy: an accepted source
renders a link (external http(s) carries target / rel like the SPA's
Markdown.tsx); a dropped source prints the label as text."
  (let ((url (and (stringp source) (public-url-policy source))))
    (if url
      (html-link url label-html
        :title title
        :external (external-link-p url))
      label-html)))

(defun md-code-block-class (lang)
  ":private: The class attribute value for a fenced block's language,
only when it matches ^[A-Za-z0-9_+-]+$ (else no class)."
  (when (and lang (re:scan "^[A-Za-z0-9_+-]+$" lang))
    (html-attribute (format nil "language-~a" lang))))

(defun md-node-html (node refs)
  ":private: Our own walker over the 3bmd AST — 3bmd's HTML printer
interpolates URLs unescaped, so this file adds a fourth printer. One
case per node tag the parser and the tables / code-blocks extensions
emit, plus a default that prints the node's string content escaped.
Every string goes through html-escape; :html / :raw-html print as
escaped text so a literal <script> is visible source; every URL goes
through public-url-policy. No regex post-processing anywhere."
  (cond
    ((null node) "")
    ((stringp node) (html-escape node))
    ((consp node)
     (case (car node)
       (:heading
         (let ((level (getf (cdr node) :level)))
           (format nil "<h~d>~a</h~d>"
             level (md-nodes-html (getf (cdr node) :contents) refs)
             level)))
       (:paragraph
         (format nil "<p>~a</p>" (md-nodes-html (cdr node) refs)))
       (:plain (md-nodes-html (cdr node) refs))
       (:emph (format nil "<em>~a</em>" (md-nodes-html (cdr node) refs)))
       (:strong
         (format nil "<strong>~a</strong>" (md-nodes-html (cdr node) refs)))
       (:code (format nil "<code>~a</code>" (md-nodes-html (cdr node) refs)))
       (:verbatim
         (format nil "<pre><code>~a</code></pre>"
           (md-nodes-html (cdr node) refs)))
       (:block-quote
         (format nil "<blockquote>~a</blockquote>"
           (md-nodes-html (cdr node) refs)))
       (:bullet-list
         (format nil "<ul>~a</ul>" (md-nodes-html (cdr node) refs)))
       (:counted-list
         (format nil "<ol>~a</ol>" (md-nodes-html (cdr node) refs)))
       (:list-item
         (format nil "<li>~a</li>" (md-nodes-html (cdr node) refs)))
       (:line-break "<br/>")
       (:horizontal-rule "<hr/>")
       (:entity (md-entity-html (cdr node)))
       (:link
         (let ((url (car (cdr node))))
           (if (public-url-policy url)
             (html-link (public-url-policy url) url
               :external (external-link-p (public-url-policy url)))
             (html-escape url))))
       (:mailto
         (let* ((addr (car (cdr node)))
                 (url (public-url-policy (format nil "mailto:~a" addr))))
           (if url
             (html-link url addr)
             (html-escape addr))))
       (:explicit-link
         (md-link-html (getf (cdr node) :source)
           (md-nodes-html (getf (cdr node) :label) refs)
           :title (getf (cdr node) :title)))
       (:reference-link
         (let* ((label (getf (cdr node) :label))
                 (def (or (getf (cdr node) :definition) label))
                 (entry (assoc (md-label-key def) refs :test #'equal))
                 (label-html (md-nodes-html label refs)))
           (if entry
             (md-link-html (first (cdr entry)) label-html
               :title (second (cdr entry)))
             (format nil "[~a]" label-html))))
       (:image
         ;; The image node wraps its link plist one level deeper than
         ;; :explicit-link: (setf rest (cdr (first rest))).
         (md-image-html (cdr (first (cdr node))) refs))
       ((:reference) "")
       (:html (html-escape (md-nodes-plain (cdr node))))
       (:raw-html (html-escape (md-nodes-plain (cdr node))))
       (3bmd::table (md-table-html (cdr node) refs))
       (3bmd-code-blocks::code-block
         (let* ((lang (getf (cdr node) :lang))
                 (class (md-code-block-class lang)))
           (format nil "<pre><code~@[ class=\"~a\"~]>~a</code></pre>"
             class (html-escape (getf (cdr node) :content)))))
       (t (html-escape (md-nodes-plain (cdr node))))))
    (t "")))

(defun md-nodes-html (nodes refs)
  (apply #'concatenate 'string
    (mapcar (lambda (n) (md-node-html n refs)) nodes)))

(defun md-image-html (rest refs)
  ":private: An image through the image policy: only a site-absolute
/p/files/... source renders an <img> (alt is the plain text of the
label through html-attribute); every other source — external,
protocol-relative, data:, /api/file — becomes the escaped alt text
with no <img>. The route authorizes the row at fetch time."
  (declare (ignore refs))
  (let* ((source (getf rest :source))
         (alt (md-nodes-plain (getf rest :label)))
         (alt-attr (html-attribute alt))
         (alt-text (html-escape alt))
         (title (getf rest :title))
         (title-attr (html-attribute title))
         (url (and (stringp source)
                 (public-url-policy source :image t))))
    (if url
      (format nil "<img src=\"~a\" alt=\"~a\"~@[ title=\"~a\"~]>"
        (html-attribute url) (or alt-attr "") title-attr)
      alt-text)))

(defun md-table-html (rest refs)
  ":private: A 3bmd::table node: :head holds one row of (th contents
align) cells, :body a list of rows; align is left / center / right or
nil (the align attribute is omitted)."
  (labels
    ((cell (node)
       (let ((tag (first node))
              (contents (second node))
              (align (third node)))
         (format nil "<~a~a>~a</~a>"
           (string-downcase (string tag))
           (if align
             (format nil " align=\"~(~a~)\"" align)
             "")
           (md-nodes-html contents refs)
           (string-downcase (string tag)))))
     (row (cells)
       (format nil "<tr>~{~a~}</tr>" (mapcar #'cell cells))))
    (let ((head (getf rest :head))
          (body (getf rest :body)))
      (format nil "<table>~a~a</table>"
        (if head
          (format nil "<thead>~{~a~}</thead>" (mapcar #'row head))
          "")
        (if body
          (format nil "<tbody>~{~a~}</tbody>" (mapcar #'row body))
          "")))))

(defun markdown-html (source)
  ":private: SOURCE rendered to HTML by our own walker: parse with
the tables and code-blocks extensions (parsing only), collect the
:reference definitions, walk. An absent / empty source is the empty
string."
  (if (or (null source) (zerop (length (format nil "~a" source))))
    ""
    (let* ((doc (markdown-doc source))
           (refs (md-references doc)))
      (format nil "~{~a~^~%~}"
        (mapcar (lambda (n) (md-node-html n refs)) doc)))))

;;
;; END Markdown
;;

;;
;; BEGIN Site chrome
;;

(defun site-chrome ()
  ":private: The one extension point the upcoming publisher / styling
item will own. Today: :title from *top-level-settings*, every other
key absent. Consumers: the header, the footer, og:site_name, the
JSON-LD publisher, and the RSS channel — all read this plist and
nothing else."
  (list :title (or (getf *top-level-settings* :title) "")))

(defun type-label (type-key)
  ":private: The humanized type key (be-types emits no type labels)."
  (format nil "~:(~a~)" type-key))

(defun public-type-index-path (type-key)
  (format nil "/p/~a/" (string-downcase (string type-key))))

(defun public-record-path (type-key id)
  (format nil "/p/~a/~a" (string-downcase (string type-key)) id))

(defun public-feed-path (type-key)
  (format nil "/p/~a/rss.xml" (string-downcase (string type-key))))

(defun public-record-url (origin type-key id)
  (public-origin-absolute origin (public-record-path type-key id)))

(defun render-site-header (chrome open-in-app-href)
  ":private: The site header bar on every public page. :banner-html
(sanitized by the future styling item; never emitted from data in
this child) else the site title linking to /p/, with the visible
\"Open in app\" link that leads to the SPA."
  (let ((title (getf chrome :title))
        (banner (getf chrome :banner-html)))
    (format nil
      "<header class=\"site\">~a<nav><a href=\"~a\">Open in app</a></nav></header>~%"
      (or banner
        (format nil "<h1 class=\"site\"><a href=\"/p/\">~a</a></h1>"
          (html-escape title)))
      (html-attribute open-in-app-href))))

(defun render-site-footer (chrome open-in-app-href type-keys)
  ":private: The site footer: the \"Open in app\" link and one link
per public type index. The publisher name / url / logo and the
tagline are chrome keys that are absent in v1, so they render
nothing today. (Built as separate segments: a ~@[~] whose subclause
consumes more than the test argument would misalign the ~{~} that
follows.)"
  (let* ((publisher-name (getf chrome :publisher-name))
         (publisher-url (html-attribute (getf chrome :publisher-url)))
         (publisher (when (and publisher-name publisher-url)
                      (format nil " &middot; <a href=\"~a\">~a</a>"
                        publisher-url (html-escape publisher-name)))))
    (format nil
      "<footer class=\"site\"><a href=\"~a\">Open in app</a>~a~{ &middot; <a href=\"~a\">~a</a>~}</footer>"
      (html-attribute open-in-app-href)
      (or publisher "")
      (loop for key in type-keys
        append (list (html-attribute (public-type-index-path key))
                 (html-escape (type-label key)))))))

;;
;; END Site chrome
;;

;;
;; BEGIN Document head and structured data
;;

(defun render-head (spec)
  ":private: The <head> contents from a plist (:title :description
:canonical :og-type :image :published :modified :author :feed
:feed-title :json-ld). Every attribute goes through html-attribute /
html-escape; a tag whose value is absent is omitted; the JSON-LD
block (already \\u003c-escaped by json-ld-string) is inserted verbatim
inside the one script element and nowhere else."
  (with-output-to-string (s)
    (format s "<title>~a</title>" (html-escape (getf spec :title)))
    (let ((description (html-attribute (getf spec :description))))
      (when description
        (format s "<meta name=\"description\" content=\"~a\">" description)))
    (let ((canonical (html-attribute (getf spec :canonical))))
      (when canonical
        (format s "<link rel=\"canonical\" href=\"~a\">" canonical)))
    (let ((og-type (html-attribute (getf spec :og-type))))
      (when og-type
        (format s "<meta property=\"og:type\" content=\"~a\">" og-type)))
    (let ((og-title (html-attribute (getf spec :title))))
      (when og-title
        (format s "<meta property=\"og:title\" content=\"~a\">" og-title)))
    (let ((og-description (html-attribute (getf spec :description))))
      (when og-description
        (format
          s "<meta property=\"og:description\" content=\"~a\">"
          og-description)))
    (let ((image (html-attribute (getf spec :image))))
      (when image
        (format s "<meta property=\"og:image\" content=\"~a\">" image)))
    (let ((url (html-attribute (getf spec :canonical))))
      (when url
        (format s "<meta property=\"og:url\" content=\"~a\">" url)))
    (let ((site-name (html-attribute (getf spec :site-name))))
      (when site-name
        (format
          s "<meta property=\"og:site_name\" content=\"~a\">" site-name)))
    (let ((published (html-attribute (getf spec :published))))
      (when published
        (format
          s "<meta property=\"article:published_time\" content=\"~a\">"
          published)))
    (let ((modified (html-attribute (getf spec :modified))))
      (when modified
        (format
          s "<meta property=\"article:modified_time\" content=\"~a\">"
          modified)))
    (let ((feed (html-attribute (getf spec :feed)))
           (feed-title (html-attribute (getf spec :feed-title))))
      (when feed
        (format
          s "<link rel=\"alternate\" type=\"application/rss+xml\"~@[ title=\"~a\"~] href=\"~a\">"
          feed-title feed)))
    (format s "<style>~a</style>" +public-style+)
    (let ((json-ld (getf spec :json-ld)))
      (when json-ld
        (format s "<script type=\"application/ld+json\">~a</script>"
          json-ld)))))

(defun json-ld-hash (pairs)
  ":private: One string-keyed equal hash table from a plist whose
keys are strings (plist-to-json downcases keys, so @type and
datePublished are impossible through it)."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for key in pairs by #'cddr
      for value in (cdr pairs) by #'cddr
      do (setf (gethash key ht) value))
    ht))

(defun json-ld-string (object)
  ":private: OBJECT (nested string-keyed hash tables, lists as
arrays) as a JSON string with every < replaced by \\u003c — JSON has
no < outside string literals, so the global replace is exact, keeps
the block valid JSON, and means no record value can close the script
element. Do not HTML-escape the block."
  (re:regex-replace-all "<"
    (y:with-output-to-string* () (y:encode object))
    "\\\\u003c"))

;;
;; END Document head and structured data
;;

;;
;; BEGIN Record document body
;;

(defun field-label (type-key field-key)
  (or (u:tree-get *compiled-model* type-key :fields field-key :ui :label)
    (format nil "~:(~a~)" field-key)))

(defun author-display-value (type-key fields)
  ":private: The meta line's author: the display value of the first
singular :target :users field on the projection (View-form order),
else nothing. :users is base and never a link."
  (loop for key in (public-view-field-keys type-key)
    for def = (u:tree-get *compiled-model* type-key :fields key)
    when (and (singular-target-field-p def)
           (eq (getf def :target) :users))
    do (return (getf fields key))))

(defun render-meta-line (type-key rec)
  ":private: The article meta line: the created date as <time>, and
the author display value when the projection has one."
  (let* ((created (getf rec :created-at))
         (iso (iso-utc created))
         (date (iso-utc-date created))
         (author (author-display-value type-key (getf rec :record))))
    (when (or iso author)
      (format nil "<p class=\"meta\">~@[<time datetime=\"~a\">~a</time>~]~@[ &middot; <span class=\"author\">~a</span>~]</p>"
        (html-attribute iso) (html-escape (or date ""))
        (when author (html-escape author))))))

(defun markdown-field-keys (type-key)
  ":private: The :widget :markdown field keys in View-form order —
the rendered body and <content:encoded> follow this order."
  (loop for key in (public-view-field-keys type-key)
    for def = (u:tree-get *compiled-model* type-key :fields key)
    when (eq (u:tree-get def :ui :widget) :markdown)
    collect key))

(defun render-markdown-fields (type-key fields)
  ":private: Every markdown field's value rendered through our
walker, in View-form order."
  (format nil "~{~a~^~%~}"
    (loop for key in (markdown-field-keys type-key)
      collect (markdown-html (getf fields key)))))

(defun image-field-pairs (type-key fields)
  ":private: (url alt) pairs for :widget :image / :image-list fields,
View-form order. The asset type is (or (u:tree-get field-def :source
:table) owner-type) — the same :table fe-fields emits. A string value
is one image, a list of strings several, anything else is skipped and
logged. The stored logical path begins with /; exactly one leading
slash is stripped, then each remaining segment is percent-encoded. A
double slash would be a 404 at the route, so the strip is not
optional. The renderer never reads, hashes, copies, or authorizes
bytes — 02's file route authorizes the row as guest at fetch time."
  (loop for key in (public-view-field-keys type-key)
    for def = (u:tree-get *compiled-model* type-key :fields key)
    for widget = (u:tree-get def :ui :widget)
    when (member widget '(:image :image-list))
    append (let* ((asset (or (u:tree-get def :source :table) type-key))
                  (values (getf fields key))
                  (strings (cond
                             ((stringp values) (list values))
                             ((and (listp values)
                                (every #'stringp values))
                               values)
                             (t
                               (pl:pwarn :in "image-field-pairs"
                                 :type-key type-key :field-key key
                                 :value values)
                               nil))))
             (loop for path in strings
               collect (list
                         (format nil "/p/files/~a/~a"
                           (string-downcase (string asset))
                           (encode-image-path path))
                         (public-image-alt type-key fields path))))))

(defun encode-image-path (path)
  ":private: The stored logical path (leading / stripped exactly
once — a double slash would be a 404 at the route) to encoded
segments joined by /."
  (let* ((trimmed (if (u:starts-with path "/")
                    (subseq path 1)
                    path))
         (segments (re:split "/" trimmed)))
    (format nil "~{~a~^/~}" (mapcar #'url-encode-segment segments))))

(defun public-image-alt (type-key fields path)
  ":private: Alt text: the record title else the basename."
  (let ((title (ignore-errors (public-title type-key
                          (list :record fields)))))
    (or (and (stringp title) (plusp (length title)) title)
      (file-namestring-or-basename path))))

(defun file-namestring-or-basename (path)
  (let ((pos (position #\/ path :from-end t)))
    (if pos (subseq path (1+ pos)) path)))

(defun first-image-url (type-key fields)
  ":private: The first image field value's /p/files/ URL (og:image
source; the caller makes it absolute), or nil."
  (first (mapcar #'first (image-field-pairs type-key fields))))

(defun scalar-field-row-p (type-key key fields)
  ":private: T when KEY contributes a <dl> row: a projected value
that is not nil / :null, on a field that is not markdown, not an
image widget, not a singular target (those are FK links), and not an
M2M list (those are the comma run)."
  (let* ((def (u:tree-get *compiled-model* type-key :fields key))
         (widget (u:tree-get def :ui :widget))
         (value (getf fields key)))
    (and value
      (not (equal value :null))
      (not (eq widget :markdown))
      (not (member widget '(:image :image-list)))
      (not (singular-target-field-p def))
      (not (or (getf def :join-table) (eq (getf def :type) :list))))))

(defun scalar-value-html (type-key key value)
  ":private: One <dd> value: booleans Yes / No (a NULL contributes no
row — the view-result fix keeps :null as :null); numbers through :ui
:precision when it is a number; :type :timestamp author fields as the
ISO string with Z; everything else escaped text."
  (let* ((def (u:tree-get *compiled-model* type-key :fields key))
         (precision (u:tree-get def :ui :precision)))
    (cond
      ((member value '(:true :false))
        (if (eq value :true) "Yes" "No"))
      ((and (numberp value) (numberp precision))
        (format nil "~,vf" precision value))
      ((and (numberp value)) (format nil "~a" value))
      ((eq (getf def :type) :timestamp) (html-escape (iso-utc value)))
      (t (html-escape value)))))

(defun render-scalar-dl (type-key fields)
  ":private: The compact <dl> of remaining scalar fields, View-form
order. The identity field and the fields already shown (title,
markdown, images) are not repeated: markdown and image fields never
reach scalar-field-row-p, and the title's identity field appears only
if it is itself a scalar that was not shown as the h1 — the h1 always
shows it, so it is excluded by identity-keys membership."
  (let ((identity (identity-keys type-key)))
    (let ((rows (loop for key in (public-view-field-keys type-key)
                  unless (member key identity)
                  when (scalar-field-row-p type-key key fields)
                  collect (format nil "<dt>~a</dt><dd>~a</dd>"
                            (html-escape (field-label type-key key))
                            (scalar-value-html type-key key
                              (getf fields key))))))
      (when rows
        (format nil "<dl class=\"fields\">~{~a~}</dl>" rows)))))

(defun public-fk-links (type-key rec)
  ":private: The projected singular-target fields (View-form order)
as <dt>/<dd> rows: a :suppress-sibling field, an ineligible target
type, or a :users target renders the display value as text; otherwise
guest-record the \"<field>-id\" sibling — a record yields an anchor
to /p/<target>/<uuid> whose text is the target's public-title, no
record yields the display string. Never resolve-reference-id; no
second resolver. Targets of :users render as text only."
  (let ((fields (getf rec :record)))
    (loop for key in (public-view-field-keys type-key)
      for def = (u:tree-get *compiled-model* type-key :fields key)
      for target = (getf def :target)
      for sibling = (u:make-keyword (format nil "~a-id" key))
      for sibling-id = (getf fields sibling)
      when (singular-target-field-p def)
      collect (let ((display (getf fields key)))
                (format nil "<dt>~a</dt><dd>~a</dd>"
                  (html-escape (field-label type-key key))
                  (if (and sibling-id
                         (not (getf def :suppress-sibling))
                         (public-type-p target)
                         (not (eq target :users)))
                    (let ((target-rec (guest-record target sibling-id)))
                      (if target-rec
                        (html-link
                          (public-record-path target sibling-id)
                          (public-title target target-rec))
                        (html-escape display)))
                    (html-escape display)))))))

(defun render-m2m-runs (type-key fields)
  ":private: M2M list fields as <dt>/<dd> rows — a comma-separated
run of display values (the projection already holds display values)."
  (loop for key in (public-view-field-keys type-key)
    for def = (u:tree-get *compiled-model* type-key :fields key)
    for values = (getf fields key)
    when (and (or (getf def :join-table) (eq (getf def :type) :list))
           (listp values) values)
    collect (format nil "<dt>~a</dt><dd>~a</dd>"
               (html-escape (field-label type-key key))
               (html-escape (format nil "~{~a~^, ~}" values)))))

(defun render-relations-dl (type-key rec)
  ":private: FK links and M2M runs in one <dl>, View-form order
within each group."
  (let ((rows (append (public-fk-links type-key rec)
                (render-m2m-runs type-key (getf rec :record)))))
    (when rows
      (format nil "<dl class=\"fields\">~{~a~}</dl>" rows))))

(defun child-region-spec (type-key)
  ":private: The declared :view-form :children region (exactly one in
v1) with :sort normalized as region-spec-list does — (or (getf spec
:sort) '(:created-at :asc)); passing nil to be-list would select the
child type's :default-sort instead, which is the wrong contract."
  (let ((spec (car (u:tree-get *compiled-model* type-key :view-form
                       :children))))
    (when spec
      (list :type (getf spec :type)
        :via (getf spec :via)
        :sort (or (getf spec :sort) '(:created-at :asc))))))

(defun render-child-region (type-key parent-id)
  ":private: The declared child region, when the child type is
eligible: the children read through be-list as guest with the same
:fk-eq filter the SPA sends, unpaged (a crawler does not click next),
each entry rendered through the child's own View-form projection.
Child regions do not recurse. Omitted entirely when the child type is
ineligible or there are no readable children."
  (let ((spec (child-region-spec type-key)))
    (when spec
      (let* ((child (getf spec :type))
             (via (getf spec :via)))
        (when (public-type-p child)
          (let ((children (guest-records child
                           :filters `((,child ,via :fk-eq ,parent-id))
                           :sort (getf spec :sort))))
            (when children
              (format nil
                "<section class=\"children\"><h2>~a</h2>~{~a~}</section>"
                (html-escape (type-label child))
                (loop for child-rec in children
                  collect (render-child-entry child child-rec))))))))))

(defun render-child-entry (child-type rec)
  ":private: One child entry: title link to the child's own page, its
meta line, its markdown fields rendered — the child's projection,
not the parent's."
  (let* ((fields (getf rec :record))
         (id (getf fields :id))
         (title (public-title child-type rec)))
    (format nil "<div class=\"child\"><h3>~a</h3>~a~a</div>"
      (html-link (public-record-path child-type id) title)
      (or (render-meta-line child-type rec) "")
      (render-markdown-fields child-type fields))))

(defun render-record-body (type-key rec)
  ":private: The <article> per the body-order decision: h1 title,
meta line, markdown fields, image figures, the scalar <dl>, the
relations <dl>, then the child region."
  (let* ((fields (getf rec :record))
         (id (getf fields :id))
         (images (image-field-pairs type-key fields)))
    (format nil "<article><h1>~a</h1>~a~a~@[~{<figure><img src=\"~a\" alt=\"~a\"></figure>~}~]~a~a~a</article>"
      (html-escape (public-title type-key rec))
      (or (render-meta-line type-key rec) "")
      (render-markdown-fields type-key fields)
      (loop for (url alt) in images
        append (list (html-attribute url) (or (html-attribute alt) "")))
      (or (render-scalar-dl type-key fields) "")
      (or (render-relations-dl type-key rec) "")
      (or (render-child-region type-key id) ""))))

;;
;; END Record document body
;;

;;
;; BEGIN Documents
;;

(defun render-record-document (type-key id &key (origin *public-origin*))
  ":public: The record page at /p/<type>/<uuid> as a full HTML
string, or nil when guest-record finds no row (02 turns nil into
404). Signals from valid-public-origin on a bad PUBLIC_ORIGIN. Head:
<title>, meta description, canonical, og:article, article: times, the
feed autodiscovery link, the JSON-LD BlogPosting block, the inline
style — no executable script anywhere on the page."
  (let* ((origin (valid-public-origin origin))
         (rec (guest-record type-key id)))
    (when rec
      (let* ((chrome (site-chrome))
             (fields (getf rec :record))
             (title (public-title type-key rec))
             (canonical (public-record-url origin type-key id))
             (site-title (getf chrome :title))
             (image (first-image-url type-key fields))
             (image-absolute (when image
                               (public-origin-absolute origin image)))
             (author (author-display-value type-key fields))
             (feed (public-origin-absolute origin
                     (public-feed-path type-key)))
             (json-ld (record-json-ld canonical title rec
                         image-absolute author))
             (open-app (format nil "/#/app?type=~a&id=~a"
                         (url-encode-segment
                           (string-downcase (string type-key)))
                         id)))
        (html-document
          (render-head
            (list :title (format nil "~a ~a ~a" title "–" site-title)
              :description (public-description type-key rec)
              :canonical canonical
              :og-type "article"
              :image image-absolute
              :published (iso-utc (getf rec :created-at))
              :modified (iso-utc (getf rec :updated-at))
              :site-name site-title
              :feed feed
              :feed-title (format nil "~a feed" (type-label type-key))
              :json-ld json-ld))
          (format nil "~a<main>~a</main>~a"
            (render-site-header chrome open-app)
            (render-record-body type-key rec)
            (render-site-footer chrome open-app (public-types))))))))

(defun record-json-ld (canonical title rec image author)
  ":private: The BlogPosting JSON-LD object (string-keyed yason hash
tables, lists as arrays — plist-to-json downcases keys, so @type and
datePublished are impossible through it): headline, dates, url and
mainEntityOfPage (the same URL), optional image (absolute), optional
author (the meta-line display value), and the publisher from
site-chrome when it has a name (absent in v1). json-ld-string applies
the \\u003c replacement; render-head inserts the result verbatim."
  (let* ((obj (json-ld-hash
                (list
                  "@context" "https://schema.org"
                  "@type" "BlogPosting"
                  "headline" title
                  "url" canonical
                  "mainEntityOfPage" canonical))))
    (when image (setf (gethash "image" obj) image))
    (let ((published (iso-utc (getf rec :created-at)))
          (modified (iso-utc (getf rec :updated-at))))
      (when published (setf (gethash "datePublished" obj) published))
      (when modified (setf (gethash "dateModified" obj) modified)))
    (when author
      (setf (gethash "author" obj)
        (json-ld-hash (list "@type" "Person" "name" author))))
    (let* ((chrome (site-chrome))
           (publisher-name (getf chrome :publisher-name))
           (publisher-url (getf chrome :publisher-url))
           (publisher-logo (getf chrome :publisher-logo)))
      (when publisher-name
        (setf (gethash "publisher" obj)
          (let ((pub (json-ld-hash
                       (list "@type" "Organization" "name" publisher-name))))
            (when publisher-url (setf (gethash "url" pub) publisher-url))
            (when publisher-logo
              (setf (gethash "logo" pub)
                (json-ld-hash
                  (list "@type" "ImageObject" "url" publisher-logo))))
            pub))))
    (json-ld-string obj)))

(defun render-type-index (type-key &key (origin *public-origin*))
  ":public: The type index at /p/<type>/ as a full HTML string, or
nil for an ineligible type. The type's :page-description rendered
through markdown-html when present, then every guest-readable record
newest-first as an entry with title link, date, and
public-description excerpt. The head carries og:type website, its own
canonical, and the feed autodiscovery link; no JSON-LD."
  (let* ((origin (valid-public-origin origin))
         (eligible (public-type-p type-key)))
    (when eligible
      (let* ((chrome (site-chrome))
             (label (type-label type-key))
             (site-title (getf chrome :title))
             (records (guest-records type-key :sort '(:created-at :desc)))
             (page-description (u:tree-get *compiled-model* type-key
                               :page-description))
             (open-app (format nil "/#/app?type=~a"
                         (url-encode-segment
                           (string-downcase (string type-key))))))
        (html-document
          (render-head
            (list :title (format nil "~a ~a ~a" label "–" site-title)
              :description (when page-description
                             (plain-markdown-text page-description))
              :canonical (public-origin-absolute origin
                           (public-type-index-path type-key))
              :og-type "website"
              :site-name site-title
              :feed (public-origin-absolute origin
                      (public-feed-path type-key))
              :feed-title (format nil "~a feed" label)))
          (format nil "~a<main><h1>~a</h1>~@[~a~]~a</main>~a"
            (render-site-header chrome open-app)
            (html-escape label)
            (when page-description (markdown-html page-description))
            (render-index-entries type-key records)
            (render-site-footer chrome open-app (public-types))))))))

(defun render-index-entries (type-key records)
  ":private: The type index body: every record newest-first as a
title link, date, and public-description excerpt; \"No entries yet\"
when the type is readable but empty (an existing-but-empty container
renders)."
  (if records
    (format nil "<ul class=\"index\">~{~a~}</ul>"
      (loop for rec in records
        collect
        (let* ((fields (getf rec :record))
               (id (getf fields :id))
               (date (iso-utc-date (getf rec :created-at))))
          (format nil
            "<li><a href=\"~a\">~a</a>~@[ <span class=\"meta\">~a</span>~]~@[<p class=\"meta\">~a</p>~]</li>"
            (html-attribute (public-record-path type-key id))
            (html-escape (public-title type-key rec))
            (html-escape (or date ""))
            (html-attribute (public-description type-key rec))))))
    "<p class=\"meta\">No entries yet</p>"))

(defun render-root-index (&key (origin *public-origin*))
  ":public: The root index at /p/ as a full HTML string. Nil only
with guest off (the empty-state matrix row (a)); with guest on and
zero eligible types the page renders with a \"Nothing is published
yet\" line (row (b)); with eligible types it lists them (row (c))."
  (let* ((origin (valid-public-origin origin)))
    (when (model-guest-allowed)
      (let* ((chrome (site-chrome))
             (types (public-types))
             (site-title (getf chrome :title)))
        (html-document
          (render-head
            (list :title site-title
              :canonical (public-origin-absolute origin "/p/")
              :og-type "website"
              :site-name site-title))
          (format nil "~a<main><h1>~a</h1>~a</main>~a"
            (render-site-header chrome "/#/app")
            (html-escape site-title)
            (if types
              (format nil "<ul class=\"index\">~{~a~}</ul>"
                (loop for key in types
                  collect (format nil
                            "<li><a href=\"~a\">~a</a> <a class=\"meta\" href=\"~a\">feed</a></li>"
                            (html-attribute (public-type-index-path key))
                            (html-escape (type-label key))
                            (html-attribute (public-feed-path key)))))
              "<p class=\"meta\">Nothing is published yet</p>")
            (render-site-footer chrome "/#/app" types)))))))

;;
;; END Documents
;;

;;
;; BEGIN Sitemap and feed
;;

(defun xml-char-p (ch)
  ":private: The XML 1.0 Char production: #x9 | #xA | #xD |
#x20–#xD7FF | #xE000–#xFFFD | #x10000–#x10FFFF. Everything else —
the other C0 controls, #xFFFE, #xFFFF, and any surrogate SBCL lets
through — is not a character an XML parser accepts."
  (let ((code (char-code ch)))
    (or (= code 9) (= code 10) (= code 13)
      (and (>= code 32) (<= code #xD7FF))
      (and (>= code #xE000) (<= code #xFFFD))
      (and (>= code #x10000) (<= code #x10FFFF)))))

(defun xml-safe-string (string)
  ":private: STRING with every character outside the XML 1.0 Char
production dropped."
  (remove-if-not #'xml-char-p (format nil "~a" string)))

(defun xml-escape (string)
  ":private: xml-safe-string, then the five predefined XML entities."
  (with-output-to-string (s)
    (loop for ch across (xml-safe-string string)
      do (case ch
           (#\& (write-string "&amp;" s))
           (#\< (write-string "&lt;" s))
           (#\> (write-string "&gt;" s))
           (#\" (write-string "&quot;" s))
           (#\' (write-string "&apos;" s))
           (t (write-char ch s))))))

(defun cdata-wrap (string)
  ":private: STRING inside a CDATA section, with any ]]> split as
]]]]><![CDATA[> — both the text and the split go through
xml-safe-string first."
  (format nil "<![CDATA[~a]]>"
    (re:regex-replace-all "]]>"
      (xml-safe-string (format nil "~a" string))
      "]]]]><![CDATA[>")))

(defun render-sitemap (&key (origin *public-origin*))
  ":public: sitemap.xml as a string: root, every eligible type index,
and every record URL, absolute, XML-escaped, with <lastmod> (the
YYYY-MM-DD part of the fetched updated-at) on records and none on
indexes. Nil when the model has guest login off (there is nothing to
list, and a sitemap naming a 404 root is worse than none; 02 answers
404)."
  (let ((origin (valid-public-origin origin)))
    (when (model-guest-allowed)
      (let ((urls
              (append
                (list (public-origin-absolute origin "/p/"))
                (loop for key in (public-types)
                  collect (public-origin-absolute origin
                             (public-type-index-path key)))
                (loop for key in (public-types)
                  append (loop for rec in (guest-records key)
                           collect (list
                                     (public-record-url origin key
                                       (getf (getf rec :record) :id))
                                     (iso-utc-date
                                       (getf rec :updated-at))))))))
        (format nil
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~{~a~}</urlset>"
          (loop for entry in urls
            collect (if (stringp entry)
              (format nil "<url><loc>~a</loc></url>" (xml-escape entry))
              (let ((lastmod (second entry)))
                (format nil
                  "<url><loc>~a</loc>~@[<lastmod>~a</lastmod>~]</url>"
                  (xml-escape (first entry))
                  (when lastmod (xml-escape lastmod)))))))))))

(defun render-feed (type-key &key (origin *public-origin*))
  ":public: The per-type RSS 2.0 feed at /p/<type>/rss.xml as a
string, or nil for an ineligible type (a readable-but-empty type
renders with the channel and zero items). Items newest-first from the
query (:limit 50 — the limit is in the query, not applied after
hydrating everything): title, link and permalink guid, RFC 822
pubDate, plain <description>, and <content:encoded> with the rendered
markdown fields in a CDATA section."
  (let ((origin (valid-public-origin origin)))
    (when (public-type-p type-key)
      (let* ((chrome (site-chrome))
             (site-title (getf chrome :title))
             (label (type-label type-key))
             (page-description (u:tree-get *compiled-model* type-key
                               :page-description))
             (records (guest-records type-key
                        :sort '(:created-at :desc) :limit 50))
             (feed-url (public-origin-absolute origin
                         (public-feed-path type-key))))
        (format nil
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<rss version=\"2.0\" xmlns:atom=\"http://www.w3.org/2005/Atom\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"><channel><title>~a</title><link>~a</link><description>~a</description><atom:link href=\"~a\" rel=\"self\" type=\"application/rss+xml\"/>~{~a~}</channel></rss>"
          (xml-escape (format nil "~a – ~a" label site-title))
          (xml-escape (public-origin-absolute origin
                        (public-type-index-path type-key)))
          (xml-escape (or (and page-description
                            (plain-markdown-text page-description))
                        site-title))
          (xml-escape feed-url)
          (loop for rec in records
            collect (feed-item type-key origin rec)))))))

(defun feed-item (type-key origin rec)
  ":private: One RSS <item>: title, link and guid isPermaLink=true
(the record URL), pubDate (RFC 822 from created-at), plain
description, and content:encoded with the rendered markdown fields in
CDATA."
  (let* ((fields (getf rec :record))
         (id (getf fields :id))
         (url (public-record-url origin type-key id))
         (content (render-markdown-fields type-key fields)))
    (format nil
      "<item><title>~a</title><link>~a</link><guid isPermaLink=\"true\">~a</guid>~@[<pubDate>~a</pubDate>~]<description>~a</description><content:encoded>~a</content:encoded></item>"
      (xml-escape (public-title type-key rec))
      (xml-escape url)
      (xml-escape url)
      (rfc822-utc (getf rec :created-at))
      (xml-escape (public-description type-key rec))
      (cdata-wrap content))))

;;
;; END Sitemap and feed
;;

;;
;; BEGIN Public routes (todo child 2)
;;

(defparameter *public-image-content-types*
  '(("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp"))
  "The public file route's extension whitelist: the stored basename's
extension (the route does not sniff bytes) must name one of these, so
the public route cannot become a generic download path. svg is
refused.")

(defparameter *public-origin-warned* nil
  "T after decorate-public-url has logged one invalid-origin warning;
one line per process, not one per record.")

(defun public-cut-query (uri)
  ":private: URI with an absolute-form https?://host prefix stripped
and everything from the first ? dropped (query strings are ignored on
every public path — they do not change the document)."
  (let ((path (nth-value 1
                (re:scan-to-strings
                  "^(?:https?://[^/?#]*)?([^?#]*)" uri))))
    (or (and path (aref path 0)) "")))

(defun public-raw-path ()
  ":private: The raw request target (h:request-uri* — Hunchentoot has
already signaled its own 400 for a malformed percent escape before
dispatch, so %2F is still a / in script-name and unusable there),
cut at the first ? with an absolute-form host prefix stripped."
  (let ((uri (h:request-uri*)))
    (when uri (public-cut-query uri))))

(defun hex-byte (hi lo)
  ":private: Two hex characters to a byte value, or nil."
  (let ((h (digit-char-p hi 16))
        (l (digit-char-p lo 16)))
    (and h l (+ (* 16 h) l))))

(defun percent-decode-segment (segment)
  ":private: %XX pairs to bytes, the bytes through UTF-8, nothing
else — never + to space (a file named a+b.png must stay findable).
NIL on anything it cannot decode (defense in depth for a future
Hunchentoot or Flexi-Streams default; over the wire a malformed
escape is Hunchentoot's 400 before dispatch)."
  (with-input-from-string (in segment)
    (loop
      with buffer = (make-array 64
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0 :adjustable t)
      for ch = (read-char in nil nil)
      while ch
      do (cond
           ((char= ch #\%)
             (let ((hi (read-char in nil nil))
                   (lo (read-char in nil nil)))
               (let ((byte (and hi lo (hex-byte hi lo))))
                 (unless byte
                   (return-from percent-decode-segment nil))
                 (vector-push-extend byte buffer))))
           (t
             ;; Only ASCII travels the raw path; anything else has
             ;; already lost its encoding. Refuse, never guess.
             (let ((code (char-code ch)))
               (if (< code 128)
                 (vector-push-extend code buffer)
                 (return-from percent-decode-segment nil)))))
      finally
      (return
        (handler-case
            (b:octets-to-string
              (subseq buffer 0 (fill-pointer buffer))
              :encoding :utf-8)
          (error () nil))))))

(defun public-segment-safe-p (segment)
  ":private: NIL when a decoded path segment is empty, holds NUL, a
C0 control or DEL, a decoded slash, or a backslash, or is .. or . —
no request string becomes a filesystem path except through the file
route's own joins, and a decoded %2F must not become one either."
  (and segment
    (plusp (length segment))
    (not (find-if
           (lambda (c)
             (let ((code (char-code c)))
               (or (< code 32) (= code 127))))
           segment))
    (not (position #\/ segment))
    (not (position #\\ segment))
    (not (member segment '(".." ".") :test #'string=))))

(defun public-decode-segments (raw-segments)
  ":private: Decode a list of raw segments; nil when any fails to
decode or fails path safety."
  (let ((segments
          (loop for raw in raw-segments
            collect (percent-decode-segment raw))))
    (when (every #'public-segment-safe-p segments)
      segments)))

(defun public-headers-for (status kind)
  ":private: The headers every public response carries: caching by
status and artifact (200 documents and robots max-age 60, files
max-age 3600, 301s max-age 3600, every error no-store — a cached
miss must not delay a row or image published a second later),
nosniff always, and the document CSP (no script-src: the pages carry
no executable script; the JSON-LD block is inert data). No
Access-Control-Allow-Origin anywhere."
  (let ((cache
          (case status
            (200 (case kind
                   (:file "public, max-age=3600")
                   (otherwise "public, max-age=60")))
            (301 "public, max-age=3600")
            (otherwise "no-store")))
        (csp
          (when (and (= status 200)
                  (member kind '(:root :type :record)))
            "default-src 'none'; img-src 'self'; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'")))
    (append
      (list (cons "Cache-Control" cache)
        (cons "X-Content-Type-Options" "nosniff"))
      (when csp
        (list (cons "Content-Security-Policy" csp))))))

(defun public-content-type-for (kind)
  ":private: The explicit Content-Type for the artifact kind."
  (case kind
    (:file "application/octet-stream")
    (:html "text/html; charset=utf-8")
    (:feed "application/rss+xml; charset=utf-8")
    (:xml "application/xml; charset=utf-8")
    (t "text/plain; charset=utf-8")))

(defun public-error (status text)
  ":private: A plain error response: text/plain, no-store, a
one-line body — never the SPA shell, never an empty body (nil makes
Hunchentoot substitute its own status document)."
  (values status
    (public-headers-for status :text)
    text
    :text))

(defun public-not-found ()
  (public-error 404 "Not found"))

(defun public-method-not-allowed ()
  ":private: 405 with the allowed methods."
  (values 405
    (append (public-headers-for 405 :text)
      (list (cons "Allow" "GET, HEAD")))
    "Method not allowed"
    :text))

(defun public-unavailable ()
  ":private: 503 while *compiled-model* or *rbac* is nil — never the
SPA shell."
  (values 503
    (append (public-headers-for 503 :text)
      (list (cons "Retry-After" "5")))
    "Service unavailable"
    :text))

(defun public-redirect (location)
  ":private: The permanent-spelling 301 (crawlers consolidate on
permanent canonical spellings) with a one-line text body."
  (values 301
    (append (public-headers-for 301 :text)
      (list (cons "Location" location)))
    (format nil "~a" location)
    :text))

(defun public-document (kind document)
  ":private: A 200 document, or the plain 404 when the render
returned nil (no such public page: guest off, ineligible type,
missing or unreadable row). KIND names the artifact for the header
function (:root / :type / :record get the document CSP; :feed / :xml
/ :text do not); the fourth value is the content kind that names
the Content-Type."
  (if document
    (values 200
      (public-headers-for 200 kind)
      document
      (case kind
        ((:root :type :record) :html)
        (t kind)))
    (public-not-found)))

(defun public-type-key-lookup (type-string)
  ":private: The type key for a public type segment — matched
against public-types (downcased symbol names), so an unknown or
ineligible type is a 404 without a database lookup."
  (let ((key (and (plusp (length type-string))
                (u:make-keyword type-string))))
    (when (member key (public-types)) key)))

(defun public-file-route (type-raw path-raw)
  ":private: /p/files/<type>/<path> — /api/file as guest: the type
segment must name a compiled fs-backed tree leaf with a path field
(a non-fs leaf has no path field and would 500 in fs-path), the
model must allow guests, the path segments must decode safely, the
row must be readable by the guest at fetch time (authorized-file-path
collapses :missing and :forbidden to 404 — a private file and a
missing file are indistinguishable from outside), and the stored
basename's extension must be in the image whitelist. Returns the
values 200 / headers / file-plist / :file, or the 404."
  (let* ((type-key (u:make-keyword type-raw))
         (type-def (u:tree-get *compiled-model* type-key))
         (path-segments
           (public-decode-segments (re:split "/" path-raw))))
    (if (and type-def
          (getf type-def :tree)
          (getf type-def :is-leaf)
          (getf type-def :fs-backed)
          (path-field type-key)
          (model-guest-allowed)
          path-segments)
      (let* ((path (format nil "/~{~a~^/~}" path-segments))
             (extension
               (string-downcase (or (u:file-extension path) "")))
             (content-type
               (cdr (assoc extension *public-image-content-types*
                      :test #'equal))))
        (if content-type
          (multiple-value-bind (fs-path reason)
            (authorized-file-path type-key path *guest-user*)
            (declare (ignore reason))
            (if fs-path
              (values 200
                (public-headers-for 200 :file)
                (list :file fs-path :content-type content-type)
                :file)
              (public-not-found)))
          (public-not-found)))
      (public-not-found))))

(defun public-robots-body (origin)
  ":private: robots.txt: everything allowed, one absolute Sitemap
line — the reason this is a route and not a file in web/public/."
  (format nil "User-agent: *~%Allow: /~%Sitemap: ~a/sitemap.xml~%"
    origin))

(defun public-route (method raw-path &key (origin *public-origin*))
  ":public: The pure public router: METHOD (a keyword) and RAW-PATH
to four values — status, a headers alist, the body (a string, or a
plist (:file <fs-path-string> :content-type <whitelisted type>) for
a file to stream), and the artifact kind (:html :feed :xml :text
:file) that names the Content-Type. Touches no Hunchentoot special,
so direct tests can call it and still see a bad origin signal. The
grammar is matched on the raw path with [^/] captures (a %2F stays
inside one segment) and each capture is percent-decoded afterwards.
503 only while *compiled-model* or *rbac* is nil; a model switch is
an accepted few seconds of 500s (the adapter's boundary). 301, not
302. Query strings are ignored on every public path."
  ;; Defense in depth for direct callers: the query is cut here too,
  ;; not only in public-raw-path.
  (setf raw-path (public-cut-query raw-path))
  (unless (and *compiled-model* *rbac*)
    (return-from public-route (public-unavailable)))
  (unless (member method '(:get :head))
    (return-from public-route (public-method-not-allowed)))
  ;; The canonical origin every render builds URLs from, validated
  ;; here once (signals on a bad PUBLIC_ORIGIN; the adapter's
  ;; boundary turns that into the plain 500).
  (let ((origin (valid-public-origin origin)))
  (labels
    ((groups (regex)
       (nth-value 1 (re:scan-to-strings regex raw-path))))
    (let ((g-type (groups "^/p/([^/]+)$"))
          (g-feed (groups "^/p/([^/]+)/rss\\.xml$"))
          (g-index (groups "^/p/([^/]+)/$"))
          (g-slash (groups "^/p/([^/]+)/([^/]+)/$"))
          (g-record (groups "^/p/([^/]+)/([^/]+)$"))
          (g-file (groups "^/p/files/([^/]+)/(.+)$")))
      (cond
        ((string= raw-path "/p")
          (public-redirect "/p/"))
        ((string= raw-path "/p/")
          (public-document :root (render-root-index :origin origin)))
        (g-type
          ;; only a known public type gets the slash redirect; an
          ;; unknown segment (/p/manifest.json) is the plain 404
          (let ((key (public-type-key-lookup (aref g-type 0))))
            (if key
              (public-redirect (format nil "/p/~a/" (aref g-type 0)))
              (public-not-found))))
        (g-feed
          (let ((key (public-type-key-lookup (aref g-feed 0))))
            (if key
              (public-document :feed (render-feed key :origin origin))
              (public-not-found))))
        (g-index
          (let ((key (public-type-key-lookup (aref g-index 0))))
            (if key
              (public-document :type (render-type-index key :origin origin))
              (public-not-found))))
        (g-slash
          (let ((key (public-type-key-lookup (aref g-slash 0))))
            (if key
              (public-redirect
                (format nil "/p/~a/~a"
                  (aref g-slash 0) (aref g-slash 1)))
              (public-not-found))))
        (g-record
          (let ((key (public-type-key-lookup (aref g-record 0)))
                (id-raw (aref g-record 1)))
            (cond
              ((null key) (public-not-found))
              ((not (re:scan *uuid-regex* id-raw)) (public-not-found))
              (t
                (public-document :record
                  (render-record-document key id-raw :origin origin))))))
        (g-file
          (public-file-route (aref g-file 0) (aref g-file 1)))
        ((string= raw-path "/sitemap.xml")
          (public-document :xml (render-sitemap :origin origin)))
        ((string= raw-path "/robots.txt")
          (values 200
            (public-headers-for 200 :text)
            (public-robots-body origin)
            :text))
        (t (public-not-found)))))))

(defun public-dispatcher (request)
  "The namespace boundary: claims a request only when the raw path is
exactly /p, starts with /p/, or is exactly /sitemap.xml or
/robots.txt; everything else returns nil so dispatch continues to the
SPA (a raw prefix dispatcher for /p would also capture /posts and
/piano — do not use one)."
  (declare (ignore request))
  (when (public-claimed-path-p (public-raw-path))
    #'serve-public-pages))

(defun public-claimed-path-p (raw-path)
  ":private: T for the paths the public namespace owns."
  (and raw-path
    (or (string= raw-path "/p")
      (u:starts-with raw-path "/p/")
      (string= raw-path "/sitemap.xml")
      (string= raw-path "/robots.txt"))))

(defun open-public-file (path)
  ":private: Open PATH as octets and return the stream and its
length — inside the adapter's error boundary, so a vanished file, a
directory, a permission error, or a bad path becomes the plain 500
(the one race between the authorization check and the open; retried
by the crawler)."
  (let ((stream (open path :element-type '(unsigned-byte 8))))
    (values stream (file-length stream))))

(defun serve-public-pages ()
  "The Hunchentoot handler. Phase one runs inside one handler-case on
error (the server runs *show-lisp-errors-p* t, so an unhandled error
would leak the condition in an HTML page): the public-route call and,
for a file body, the open and file-length. Phase two — reached only
with a known outcome, nothing set before it — sets the status and
headers and either returns the string body or streams the file
(content-length*, send-headers, the fixed-buffer loop, the close in
unwind-protect). HEAD runs the GET path; Hunchentoot drops the body.
The 500 is built from nothing, so it never inherits a max-age or an
image content type from the route's own answer."
  (handler-case
      (multiple-value-bind (status headers body kind)
        (public-route (h:request-method*) (public-raw-path))
        (cond
          ((and (listp body) (getf body :file))
            (multiple-value-bind (stream length)
              (open-public-file (getf body :file))
              (unwind-protect
                  (progn
                    (setf (h:return-code*) status)
                    (dolist (header headers)
                      (setf (h:header-out (car header)) (cdr header)))
                    (setf (h:content-type*) (getf body :content-type))
                    (setf (h:content-length*) length)
                    (let ((out (h:send-headers))
                          (buf (make-array 8192
                                 :element-type '(unsigned-byte 8))))
                      (loop
                        for chunk-size = (min 8192 length)
                        while (plusp length)
                        do (let ((read
                                   (read-sequence buf stream
                                     :end chunk-size)))
                             (write-sequence buf out :end read)
                             (decf length read)))
                      (finish-output out)))
                (close stream))))
          (t
            (setf (h:return-code*) status)
            (dolist (header headers)
              (setf (h:header-out (car header)) (cdr header)))
            (setf (h:content-type*) (public-content-type-for kind))
            body)))
    (error (e)
      (pl:perror :in "serve-public-pages"
        :error e
        :method (h:request-method*)
        :path (public-raw-path))
      (setf (h:return-code*) 500)
      (setf (h:header-out "Cache-Control") "no-store")
      (setf (h:header-out "X-Content-Type-Options") "nosniff")
      (setf (h:content-type*) "text/plain; charset=utf-8")
      "Internal error")))

;;
;; BEGIN :public-url on the wire records
;;

(defun decorate-public-url (record type-key &optional readable-ids)
  ":private: Append :public-url \"<origin>/p/<type>/<uuid>\" to RECORD
when the type is public-type-p and the guest can read the row (a
fresh check — there is no lag window; a revoked role drops the key on
the next read). The key is omitted, never nil (plist-to-json renders
nil as [], truthy in JS), and never emitted under an invalid
PUBLIC_ORIGIN: the authenticated API keeps working without the key
while the public routes report the bad origin as a 500. Called from
list-result and be-rec only — never from rec, whose :record feeds
valid-data on update and would reject the unknown key."
  (let ((origin (ignore-errors (valid-public-origin))))
    (cond
      ((not origin)
        (unless *public-origin-warned*
          (setf *public-origin-warned* t)
          (pl:pwarn :in "decorate-public-url"
            :reason "invalid PUBLIC_ORIGIN; key omitted"
            :value *public-origin*))
        record)
      ((not (public-type-p type-key)) record)
      (t
        (let ((id (getf record :id)))
          (if (if readable-ids
                (member id readable-ids :test #'equal)
                (user-allowed-resource *guest-user* id "read"))
            (append record
              (list :public-url
                (public-record-url origin type-key id)))
            record))))))

;;
;; END :public-url on the wire records
;;
