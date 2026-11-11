(in-package :data-ui)

;;; ---------------------------------------------------------------------------
;;; Styling type tests (todo "Styling type")
;;; ---------------------------------------------------------------------------

(def-suite styling-compile-suite
  :description ":styling compile probes (bare compile-model, no DB)")

(def-suite styling-suite
  :description ":styling behavior tests (test-model context)")

(in-suite styling-compile-suite)

(defun th-sty-compile ()
  "Compile a minimal model and return the compiled defs plist."
  (compile-model
    `(:widgets
       (:table t
         :create :auto :update :auto :delete :auto :display t
         :type-roles ("sty-users")
         :fields
         (:name (:type :text :identity t
                  :source (:view :main :column :name :agg :first)
                  :column t :not-null t))
         :list-form (:fields t)
         :view-form (:fields t)))))

(test sty-compile-type-flags
  ":styling compiles built-in, not base, displayed, a styling
type-role (admin still force-attached at compile time), no
suppress-roles (the Add-form palette rides the type's role onto
new rows), category :system, table name styling."
  (let* ((m (th-sty-compile))
         (def (getf m :styling)))
    (is (eq t (getf def :built-in)))
    (is (null (getf def :base)))
    (is (eq t (getf def :display)))
    (is (equal '("styling") (getf def :type-roles)))
    (is (null (getf def :suppress-roles)))
    (is (eq :system (getf def :category)))
    (is (equal "styling" (getf def :table-name)))))

(test sty-compile-hooks
  ":pre-create / :pre-update compile to one-element lists of the
sanitize-styling-value function (resolve-hook-form's functionp
pass-through — the :users :post-create precedent, re-pinned here)."
  (let* ((m (th-sty-compile))
         (def (getf m :styling)))
    (let ((pre-create (getf def :pre-create))
          (pre-update (getf def :pre-update)))
      (is (and (= 1 (length pre-create))
            (eq (car pre-create) #'sanitize-styling-value)))
      (is (and (= 1 (length pre-update))
            (eq (car pre-update) #'sanitize-styling-value))))))

(test sty-compile-name-field
  ":name is :identity t :unique t (the raw-SQL belt), :not-null t,
:searchable t; its :ui is a static select over *styling-slots*
and its :validations include v-options (compile-validations
auto-attaches it from :ui :options — the dropdown is the admin's
discovery surface, the validator the belt). The slot vocabulary
itself is pinned here, not self-referential."
  (let* ((m (th-sty-compile))
         (name-def (getf (getf (getf m :styling) :fields) :name)))
    (is (eq t (getf name-def :identity)))
    (is (eq t (getf name-def :unique)))
    (is (eq t (getf name-def :not-null)))
    (is (eq t (getf name-def :searchable)))
    (is (eq :select (u:tree-get name-def :ui :widget)))
    (is (equal *styling-slots*
          (u:tree-get name-def :ui :options)))
    (is (equal '("banner" "footer" "heading") *styling-slots*)
      "the slot vocabulary is pinned, not self-referential")
    (is (member #'v-options (getf name-def :validations)
          :test #'eq))))

(test sty-compile-value-nullable
  ":value is nullable — an empty value is the slot's off state."
  (let* ((m (th-sty-compile))
         (value-def (getf (getf (getf m :styling) :fields) :value)))
    (is (null (getf value-def :not-null)))))

(test sty-compile-unique-ddl
  "One unique index, one column constraint, no second index:
create-table-sql's :index is ix_styling_identity, the :name
column DDL carries unique, and the sort indexes touch only the
injected timestamps (sort-index-ddl skips :unique fields)."
  (let* ((m (th-sty-compile))
         (ddl (getf (getf m :styling) :create-table-sql)))
    (is (search "styling_name text not null unique"
          (getf ddl :table)))
    (is (search "ix_styling_identity on styling (styling_name)"
          (getf ddl :index)))
    (is (equal 2 (length (getf ddl :sort-index))))
    (is-false (find "styling_name" (getf ddl :sort-index)
                 :test #'(lambda (needle s) (search needle s))))))

(test sty-compile-redeclaration-rejected
  "A :styling redeclaration in an author model is the
valid-base-type-overrides error (only :type-roles may be
overridden on a base type)."
  (signals error
    (compile-model '(:styling (:table t)))))

(test sty-compile-page-description
  ":styling carries :page-description = *styling-page-description*
(a non-blank markdown string naming the slot purpose and the value
grammar), surviving stage-3's remf + re-emit; no
:page-description-open (collapsed by default). One grammar probe
per kept policy tag plus a style property name, so widening
*styling-policy* without touching the string fails here (the
styling-tag-action docstring carries the same manual-sync
burden)."
  (let* ((m (th-sty-compile))
         (def (getf m :styling))
         (desc (getf def :page-description)))
    (is (equal *styling-page-description* desc))
    (is (and (stringp desc)
          (plusp (length (string-trim '(#\space #\tab #\newline
                                        #\return)
                            desc)))))
    (is-false (getf def :page-description-open))
    (loop for tag in (sh:policy-allowed-tags *styling-policy*)
      do (is (search (format nil "`~a`" tag) desc)
           "grammar tag ~a missing from the description" tag))
    (is (search "`text-align`" desc))
    (is (search "`banner`" desc))
    (is (search "`footer`" desc))
    (is (search "`heading`" desc))))

(in-suite styling-suite)

(defun th-sty-clear ()
  "Delete every :styling row. Called on entry and exit of each
row-touching test (order-independence)."
  (loop for r in (getf (be-list :styling "admin" :limit 100) :records)
    do (ignore-errors (be-delete :styling (getf r :id) "admin"))))

(defmacro th-sty-with-rows (&body body)
  "Run BODY with :styling rows cleared before and after — every
insert in the suite is cleaned up, and a leftover from an
aborted test cannot break the next one's unique constraint."
  `(unwind-protect
       (progn (th-sty-clear) ,@body)
     (th-sty-clear)))

(defparameter *sty-test-cases*
  (list
    "<b>x</b>"
    "<script>alert(1)</script>"
    "<div><script>alert(1)</script></div>"
    "<div><div><script>alert(1)</script></div></div>"
    "<div><a href=\"javascript:x\" onclick=\"e()\">t</a></div>"
    "<div><span style=\"background:url(x)\">t</span></div>"
    "<style>body{display:none}</style>after"
    "<a href=\"javascript:alert(1)\">t</a>"
    "<a href=\"https://x.y/z\" onclick=\"e()\">t</a>"
    "<a href=\"/p/posts/\">t</a>"
    "<a href=\"#frag\">t</a>"
    "<a href=\"mailto:a@b.c\">t</a>"
    "<a href=\"mailto:a@b.c?subject=x\">t</a>"
    "<a href=\"mailto:a@b.c@d\">t</a>"
    "<img src=\"/p/files/images/logo.png\" alt=\"L\">"
    "<img src=\"data:image/png;base64,AA\">"
    "<img src=\"https://cdn.example/x.png\" alt=\"our logo\">"
    "<img src=\"/api/file/x\">"
    "<span style=\"color:red;background:url(x)\">t</span>"
    "<span style=\"background:image-set(https://evil/x)\">t</span>"
    "<span style=\"background:red url(x)\">t</span>"
    "<em style=\"width:expression(alert(1))\">t</em>"
    "<span style=\"color:red;;margin:0\">t</span>"
    "<A HREF=\"HTTPS://X.Y/Z\">t</A>"
    "unclosed <strong>a"
    "stray </em> x"
    "&lt;script&gt;"
    "<!-- c -->x"
    "<img alt='a\"b' src=\"/p/files/i/l.png\">"
    "a & b"
    "<div><b>x</b></div>"
    "<a href=\"https://u:p@x.y/z\">t</a>"
    "<a href=\"//evil/x\">t</a>"
    "<a href=\"http://x.y\">t</a>"
    "<em>a<br>b</em>"
    (format nil "<a href=\"~a\">tab-in-scheme</a>"
      (concatenate 'string "ht" (string (code-char 9)) "tps://evil/"))
    "<div><![CDATA[<b>x</b>]]></div>"
    "<!DOCTYPE html><span>t</span>"
    "<?php evil ?><span>t</span>"
    "<img src=\"/p/files/images/logo.png\" alt=\"L\" style=\"height:2em\">"
    "<a href=\"https://x.y/z\" style=\"color:blue\">x</a> <strong style=\"font-weight:700\">y</strong>"
    "<center>hi</center>"
    "<ul><li>a</li><li>b</li></ul>"
    "<div style=\"text-align: center\">x</div>"
    "<p style=\"font-size:0.8em\">s</p>"
    "<hr>"
    "<code>c</code>"
    "<h2>t</h2>"
    "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"/>")
  "The grammar case list (every expectation probe-verified against
the landed sanitize-styling-html; the belt simulation — running
the sanitizer on its own output — matched on every case). The
last case is the heading slot's canonical value in its
self-closing spelling — the session probe: it normalizes to the
paired span, which is a fixed point.")

(test sty-validator-rejects-unknown-name
  "An insert with an out-of-vocabulary :name fails (v-options,
auto-attached from :ui :options) with a message naming both
legal slots."
  (th-sty-with-rows
    (handler-case
        (progn
          (be-insert :styling '(:name "nonsense" :value "x") "admin")
          (fail "insert should have signaled"))
      (validation-error (e)
        (is (search "banner" (format nil "~a" e)))
        (is (search "footer" (format nil "~a" e)))))
    (is-false (getf (be-list :styling "admin") :records))))

(test sty-validator-accepts-both-slots
  "Both v1 slot names insert cleanly."
  (th-sty-with-rows
    (is-true (uuid-p
               (be-insert :styling '(:name "banner" :value "a") "admin")))
    (is-true (uuid-p
               (be-insert :styling '(:name "footer" :value "b") "admin")))))

(test sty-validator-rejects-rename
  "be-update renaming a row to an unknown name fails the same
way (v-options runs on the update path too) and leaves the row
unchanged. No incoming-value gate on :name exists (an update
touching only :value validates :name against the stored value,
which is always a legal slot — adding the gate would hide a
stored illegal name; this test pins the rename rejection)."
  (th-sty-with-rows
    (let ((id (be-insert :styling '(:name "banner" :value "x") "admin")))
      (is-true (uuid-p id))
      (handler-case
          (progn
            (be-update :styling id '(:name "nonsense") "admin")
            (fail "update should have signaled"))
        (validation-error (e)
          (is (search "banner" (format nil "~a" e)))))
      (is (equal "x"
            (getf (getf (be-rec id "admin" :type-key :styling) :record)
              :value))))))

(test sty-sanitizer-grammar
  "The closed grammar, end to end (drop-not-reject): unknown tags
unwrap, raw-text and form elements drop with their content, the
structural layout tags survive with style / class and headings
unwrap (a heading start tag would pop the banner h1 wrapper open
in full-document parse — same markup, different DOM per
surface), the library's nested-unwrap hole is closed by the
authoritative pass, href admits /p/, clean mailto: (one @,
queries allowed), and userinfo-free https only, src admits
/p/files/ only (a dropped src becomes
non-blank alt text, else the img dies), style is a closed
property list with load-smuggling values rejected, structure
and escaping survive (plump escapes text and attribute values
on serialize)."
  (flet ((sty (in out)
           (is (equal out (sanitize-styling-html in))
             "case ~s" in)))
    ;; unknown tags unwrap; raw-text / form elements drop with content
    (sty "<b>x</b>" "x")
    (sty "<script>alert(1)</script>" "")
    (sty "<style>body{display:none}</style>after" "after")
    ;; the library's nested-unwrap hole — markup under a disallowed
    ;; tag escapes sh:sanitize alone; the pass catches it (the pin
    ;; that keeps styling-pass load-bearing; div no longer works as
    ;; the wrapper — it is a kept tag now, so the library itself
    ;; cleans inside it)
    (sty "<b><script>alert(1)</script></b>" "")
    ;; the structural grammar: layout tags survive with content
    ;; (hr is the void serialization), attributes style / class
    ;; only; inside a kept tag the library meets a script itself
    ;; and drops it with content (probe answer: drop, not
    ;; unwrap-to-text); headings stay out — a heading start tag
    ;; would pop the banner h1 wrapper open in full-document parse
    (sty "<center>x</center>" "<center>x</center>")
    (sty "<p>t</p>" "<p>t</p>")
    (sty "<div>x</div>" "<div>x</div>")
    (sty "<div><b>x</b></div>" "<div>x</div>")
    (sty "<div><script>alert(1)</script></div>" "<div></div>")
    (sty "<div><div><script>alert(1)</script></div></div>"
      "<div><div></div></div>")
    (sty "<ul><li>a</li><li>b</li></ul>" "<ul><li>a</li><li>b</li></ul>")
    (sty "<ol><li>a</li></ol>" "<ol><li>a</li></ol>")
    (sty "<li>x</li>" "<li>x</li>")
    (sty "<hr>" "<hr>")
    (sty "<code>c</code>" "<code>c</code>")
    (sty "<small>s</small>" "<small>s</small>")
    (sty "<sup>s</sup>" "<sup>s</sup>")
    (sty "<sub>s</sub>" "<sub>s</sub>")
    (sty "<ul class=\"c\" onclick=\"e()\"><li>a</li></ul>"
      "<ul class=\"c\"><li>a</li></ul>")
    (sty "<h2>t</h2>" "t")
    ;; href policy
    (sty "<div><a href=\"javascript:x\" onclick=\"e()\">t</a></div>"
      "<div><a>t</a></div>")
    (sty "<a href=\"javascript:alert(1)\">t</a>" "<a>t</a>")
    (sty "<a href=\"https://x.y/z\" onclick=\"e()\">t</a>"
      "<a href=\"https://x.y/z\">t</a>")
    (sty "<a href=\"/p/posts/\">t</a>" "<a href=\"/p/posts/\">t</a>")
    (sty "<a href=\"#frag\">t</a>" "<a>t</a>")
    ;; mailto mirrors the markdown walker's arm verbatim: exactly one
    ;; @ in the whole URL, queries allowed (?subject= carries no @),
    ;; case-sensitive prefix (MAILTO: dies), and no second @ — so a
    ;; ?subject=a@b in a query string is rejected too
    (sty "<a href=\"mailto:a@b.c\">t</a>" "<a href=\"mailto:a@b.c\">t</a>")
    (sty "<a href=\"mailto:a@b.c?subject=x\">t</a>"
      "<a href=\"mailto:a@b.c?subject=x\">t</a>")
    (sty "<a href=\"mailto:a@b.c@d\">t</a>" "<a>t</a>")
    (sty "<a href=\"mailto:@b.c\">t</a>" "<a>t</a>")
    (sty "<a href=\"mailto:nobody\">t</a>" "<a>t</a>")
    (sty "<a href=\"MAILTO:a@b.c\">t</a>" "<a>t</a>")
    (sty "<a href=\"https://u:p@x.y/z\">t</a>" "<a>t</a>")
    (sty "<a href=\"//evil/x\">t</a>" "<a>t</a>")
    (sty "<a href=\"http://x.y\">t</a>" "<a>t</a>")
    (sty (format nil "<a href=\"~a\">tab</a>"
            (concatenate 'string "ht" (string (code-char 9))
              "tps://evil/"))
      "<a>tab</a>")
    ;; no rel / target on a surviving link (the library's
    ;; set-safe-defaults additions are stripped)
    (let ((out (sanitize-styling-html
                 "<a href=\"https://x.y/z\" onclick=\"e()\">t</a>")))
      (is-false (search "rel=" out))
      (is-false (search "target=" out)))
    ;; src policy: /p/files/ only
    (sty "<img src=\"/p/files/images/logo.png\" alt=\"L\">"
      "<img src=\"/p/files/images/logo.png\" alt=\"L\">")
    (sty "<img src=\"data:image/png;base64,AA\">" "")
    (sty "<img src=\"https://cdn.example/x.png\" alt=\"our logo\">"
      "our logo")
    (sty "<img src=\"/api/file/x\">" "")
    ;; style policy: closed property list, value shapes that can
    ;; smuggle a load are rejected (whole attribute drops)
    (sty "<span style=\"color:red;background:url(x)\">t</span>"
      "<span style=\"color: red\">t</span>")
    (sty "<span style=\"background:image-set(https://evil/x)\">t</span>"
      "<span>t</span>")
    (sty "<span style=\"background:red url(x)\">t</span>" "<span>t</span>")
    (sty "<em style=\"width:expression(alert(1))\">t</em>" "<em>t</em>")
    (sty "<span style=\"color:red;;margin:0\">t</span>"
      "<span style=\"color: red; margin: 0\">t</span>")
    ;; text-align joins the property list; a load-smuggling pair
    ;; in the same attribute still drops (styling-filter-css, not a
    ;; whole-attribute rule)
    (sty "<span style=\"text-align: center\">x</span>"
      "<span style=\"text-align: center\">x</span>")
    (sty "<div style=\"text-align:center;background:url(x)\">x</div>"
      "<div style=\"text-align: center\">x</div>")
    ;; case preserved on tag and attribute names; the URL check is
    ;; case-insensitive on the scheme
    (sty "<A HREF=\"HTTPS://X.Y/Z\">t</A>" "<A HREF=\"HTTPS://X.Y/Z\">t</A>")
    ;; structure and escaping
    (sty "unclosed <strong>a" "unclosed <strong>a</strong>")
    (sty "stray </em> x" "stray &lt;em&gt; x")
    (sty "&lt;script&gt;" "&lt;script&gt;")
    (sty "<!-- c -->x" "x")
    (sty "<img alt='a\"b' src=\"/p/files/i/l.png\">"
      "<img alt=\"a&quot;b\" src=\"/p/files/i/l.png\">")
    (sty "a & b" "a &amp; b")
    (sty "<em>a<br>b</em>" "<em>a<br>b</em>")
    (sty "<div><![CDATA[<b>x</b>]]></div>" "<div>&lt;b&gt;x&lt;/b&gt;</div>")
    (sty "<!DOCTYPE html><span>t</span>" "<span>t</span>")
    (sty "<?php evil ?><span>t</span>" "<span>t</span>")
    (sty "<img src=\"/p/files/images/logo.png\" alt=\"L\" style=\"height:2em\">"
      "<img src=\"/p/files/images/logo.png\" alt=\"L\" style=\"height: 2em\">")
    (sty "<a href=\"https://x.y/z\" style=\"color:blue\">x</a> <strong style=\"font-weight:700\">y</strong>"
      "<a href=\"https://x.y/z\" style=\"color: blue\">x</a> <strong style=\"font-weight: 700\">y</strong>")
    ;; non-string input degrades to the empty string, never signals
    (is (equal "" (sanitize-styling-html nil)))
    ;; the heading slot's canonical value, kept verbatim (the
    ;; probed font stack with its quoted family names, the
    ;; negative letter-spacing, and the four-property declaration
    ;; all pass; the frontend's headingStyleFromInfo lifts this
    ;; exact shape), and its self-closing spelling normalizes to
    ;; the paired span
    (sty "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"></span>"
      "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"></span>")
    (sty "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"/>"
      "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"></span>")))

(test sty-sanitizer-length-cap
  "A 5000-char input cuts at +styling-max-length+ before the
first parse: <span> + 4090 a's + </span> = 4103."
  (let* ((r (sanitize-styling-html
              (format nil "<span>~a</span>"
                (make-string 5000 :initial-element #\a)))))
    (is (= 4103 (length r)))
    (is (search "</span>" r :start2 (- (length r) 7)))))

(test sty-sanitizer-idempotence
  "sanitize-styling-html is a fixed point on its own output — the
invariant the read-side belt depends on (a belt re-run on a
stored value must not alter it)."
  (loop for c in *sty-test-cases*
    for r = (sanitize-styling-html c)
    do (is (string= r (sanitize-styling-html r))
         "case ~s" c)))

(test sty-hook-write-path
  "The :pre-create / :pre-update hook rewrites :value before it
is stored — be-rec reads back the clean string on both slots."
  (th-sty-with-rows
    (let ((id (be-insert :styling
                 '(:name "banner" :value "<b>x</b><em>y</em>") "admin")))
      (is-true (uuid-p id))
      (is (equal "x<em>y</em>"
            (getf (getf (be-rec id "admin" :type-key :styling) :record)
              :value)))
      (be-update :styling id
        '(:value "<script>alert(1)</script>after") "admin")
      (is (equal "after"
            (getf (getf (be-rec id "admin" :type-key :styling) :record)
              :value))))
    (let ((id (be-insert :styling
                 '(:name "footer" :value "<b>x</b><em>y</em>") "admin")))
      (is (equal "x<em>y</em>"
            (getf (getf (be-rec id "admin" :type-key :styling) :record)
              :value))))))

(test sty-hook-blank-value
  "A nil :value (full-data's :null spelling) passes the hook
untouched, stores NULL, and styling-slot reads nil — blanking a
slot is legal."
  (th-sty-with-rows
    (let ((id (be-insert :styling '(:name "banner" :value nil) "admin")))
      (is-true (uuid-p id))
      (is (null (styling-slot "banner")))
      (is (eq :null
            (query
              (format nil "select styling_value from styling ~
                           where id = '~a'" id)
              :result-type :single))))))

(test sty-unique-index
  "A second row with the same slot name but a different value
fails on the unique constraint (id-from-data does not intercept
it — the lookup keys differ); the database rejects the duplicate
whatever path wrote it."
  (th-sty-with-rows
    (let ((id (be-insert :styling '(:name "banner" :value "one") "admin")))
      (is-true (uuid-p id))
      (signals error
        (be-insert :styling '(:name "banner" :value "two") "admin")))))

(test sty-reader
  "styling-slot: no row → nil; blank value → nil; value → the
clean string; out-of-vocabulary name → nil. Direct SQL with the
system's own authority — there is no user argument and no
permission machinery, pinned by reading while a roleless user
exists. The heading slot reads through the same name-generic
path (the third slot's read pin)."
  (th-sty-with-rows
    (th-make-user "sty-roleless")
    (is (null (styling-slot "banner")))
    (let ((id (be-insert :styling '(:name "banner" :value "  ") "admin")))
      (is-true (uuid-p id))
      (is (null (styling-slot "banner")))
      (be-update :styling id '(:value "<strong>hello</strong>") "admin")
      (is (equal "<strong>hello</strong>" (styling-slot "banner")))
      (is (null (styling-slot "nope"))))
    (let ((hid (be-insert :styling
                  '(:name "heading"
                    :value "<span style=\"font-size: 30px\"></span>")
                  "admin")))
      (is-true (uuid-p hid))
      (is (equal "<span style=\"font-size: 30px\"></span>"
            (styling-slot "heading"))))))

(test sty-reader-belt
  "A row written around the hook (the usage-insert shape:
resource + insert-main-query, no be-insert, no hooks) still
reads back sanitized — the belt in styling-slot — while the
stored column keeps the dirty string."
  (th-sty-with-rows
    (let* ((data '(:name "banner"
                   :value "<script>x</script><strong>ok</strong>"))
           (uuid (a:add-resource *rbac*
                   (make-resource-name :styling data) :roles nil))
           (insert (u:tree-get *compiled-model* :styling :insert-sql)))
      (a:with-rbac (*rbac*)
        (a:rbac-query
          (insert-main-query :styling insert uuid data "admin")))
      ;; the stored value is the dirty string — no hook ran
      (is (search "<script>"
            (query "select styling_value from styling"
              :result-type :single)))
      ;; the reader sanitizes it anyway
      (is (equal "<strong>ok</strong>" (styling-slot "banner"))))))

(test sty-permissions
  "The tab and row creation are gated on the styling role (created
by ensure-model-roles with full CRUD; admin holds it, and the type
resource carries it plus admin): a roleless user is denied be-list
and be-insert and be-types omits the type; a holder sees the tab
and can insert; admin sees it under :category :system."
  (th-sty-with-rows
    (th-make-user "sty-plain")
    (signals validation-error (be-list :styling "sty-plain"))
    (signals validation-error
      (be-insert :styling '(:name "banner" :value "x") "sty-plain"))
    (is-false (find :styling (be-types "sty-plain")
                 :key (lambda (e) (getf e :name))))
    (let ((holder (th-make-user "sty-holder" :roles '("styling"))))
      (is-true (find :styling (be-types holder)
                   :key (lambda (e) (getf e :name))))
      (is (null (getf (be-list :styling holder) :records)))
      ;; the palette is unsuppressed: the type's role is offered
      ;; (the frontend pre-checks it), and a row created with it
      ;; (the checked-boxes shape) is visible to every other holder
      (is (member "styling" (selectable-roles :styling holder)
               :test #'string=))
      (let ((id (be-insert :styling '(:name "banner" :value "h")
                  holder :roles '("styling"))))
        (is-true (uuid-p id))
        (let ((other (th-make-user "sty-other" :roles '("styling"))))
          (is-true (find id (getf (be-list :styling other) :records)
                       :key (lambda (r) (getf r :id)) :test #'equal)))
        (be-delete :styling id "admin")))
    (let ((admin-entry (find :styling (be-types "admin")
                         :key (lambda (e) (getf e :name)))))
      (is-true admin-entry)
      (is (eq :system (getf admin-entry :category)))
      (is (equal *styling-page-description*
            (getf admin-entry :page-description))))))

(test sty-wire-info-settings
  "info-settings carries :banner / :footer / :heading only when
the rows exist (never a nil value — the [] truthy trap), always
:landing-page, never :new-roles / :api-roles. The heading row
carries the canonical span — the exact string the frontend's
headingStyleFromInfo lifts."
  (th-sty-with-rows
    (let ((settings (info-settings "admin")))
      (is (getf settings :landing-page))
      (is-false (getf settings :banner))
      (is-false (getf settings :footer))
      (is-false (getf settings :heading))
      (is-false (getf settings :new-roles))
      (is-false (getf settings :api-roles)))
    (be-insert :styling '(:name "banner" :value "<strong>b</strong>")
      "admin")
    (be-insert :styling '(:name "footer" :value "<em>f</em>") "admin")
    (be-insert :styling
      '(:name "heading"
        :value "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"></span>")
      "admin")
    (let ((settings (info-settings "admin")))
      (is (equal "<strong>b</strong>" (getf settings :banner)))
      (is (equal "<em>f</em>" (getf settings :footer)))
      (is (equal "<span style=\"font-family: Georgia, 'Times New Roman', serif; font-size: 36px; font-weight: 700; letter-spacing: -0.3px\"></span>"
            (getf settings :heading)))
      (is (getf settings :landing-page))
      (is-false (getf settings :new-roles))
      (is-false (getf settings :api-roles)))))

(test sty-public-chrome
  "site-chrome carries :banner-html / :footer-html iff the rows
exist; render-site-header wraps the banner in the same
h1.site heading (the header stops being a home link when the
banner renders) and falls back to the title link;
render-site-footer emits the slot in its wrapper div before the
Open in app link, and nothing when unset."
  (th-sty-with-rows
    (let ((chrome (site-chrome)))
      (is-false (getf chrome :banner-html))
      (is-false (getf chrome :footer-html))
      (is (search "class=\"site\"><a href=\"/p/\">"
            (render-site-header chrome "/#/app")))
      (is-false (search "class=\"slot\""
                 (render-site-footer chrome "/#/app" (public-types)))))
    (be-insert :styling '(:name "banner" :value "<strong>B</strong>")
      "admin")
    (be-insert :styling '(:name "footer" :value "<em>F</em>") "admin")
    (let* ((chrome (site-chrome))
           (header (render-site-header chrome "/#/app"))
           (footer (render-site-footer chrome "/#/app" (public-types))))
      (is (equal "<strong>B</strong>" (getf chrome :banner-html)))
      (is (equal "<em>F</em>" (getf chrome :footer-html)))
      (is (search "<h1 class=\"site\"><strong>B</strong></h1>" header))
      (is-false (search "<a href=\"/p/\">" header))
      (let ((slot (search "<div class=\"slot\"><em>F</em></div>" footer))
            (open (search "Open in app" footer)))
        (is-true slot)
        (is-true open)
        (is (< slot open))))))
