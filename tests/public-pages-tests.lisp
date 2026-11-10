(in-package :data-ui)

;; Public pages tests (todo "Bot-crawlable public pages", children
;; 1-2). Child 1: the renderer pins, called directly against the
;; seeded public-pages-test fixture. Child 2 adds the public-route
;; and live HTTP tests below. The fixture's buttons resolve :test-sync
;; / :test-async from the registry, so action-tests.lisp must load
;; before set-model (run-public-pages-tests does that).

(defparameter *pp-origin* "http://public-pages.test")

(defun pp-image (logical-path &key (roles '("public"))
                  (data "fake image bytes"))
  "Create an images row the way an upload would (th-make-file +
be-insert with the file token). Returns the row id."
  (let ((token (th-make-file :images logical-path
                 :data-string data)))
    (be-insert :images
      (list (path-field :images) logical-path :user "admin")
      "admin" :roles roles :file-token token)))

(defun pp-post (title &key roles (body "Post body.")
                        (featured :true) (rating 3)
                        (tags nil) (cover nil)
                        (api-key "SECRET-API-KEY-7f3a")
                        (internal-note "HIDDEN-NOTE-do-not-publish"))
  "Insert a post with the secret-looking excluded values by default,
so the exclusion tests cannot pass vacuously. Returns the row id."
  (let ((data (append `(:title ,title :body ,body)
                  (when cover `(:cover ,cover))
                  (when featured `(:featured ,featured))
                  `(:rating ,rating)
                  (when tags `(:tags ,tags))
                  `(:author "admin" :api-key ,api-key
                    :internal-note ,internal-note))))
    (if roles
      (be-insert :posts data "admin" :roles roles)
      (be-insert :posts data "admin"))))

(defun pp-comment (body post-title &key (roles nil)
                                    (note "visible note"))
  (let ((data `(:user "admin" :body ,body :post ,post-title
                :secret "COMMENT-SECRET" :note ,note)))
    (if roles
      (be-insert :comments data "admin" :roles roles)
      (be-insert :comments data "admin"))))

(defun pp-seed ()
  "Seed the public-pages-test fixture. Public rows carry the public
role; one post is a draft (no roles); one post has 25 public comments
plus 1 private one; one post has no comments and a NULL :featured;
one image row is private; a :mine row and a :links row carry the
public role. Returns a plist of ids."
  (be-insert :tags '(:name "lisp") "admin" :roles '("public"))
  (be-insert :tags '(:name "web") "admin" :roles '("public"))
  (pp-image "/pp-cover-one.png")
  (pp-image "/pp-cover-two.png")
  (pp-image "/pp-cover-private.png" :roles '("comments-user"))
  (let* ((body "# The first post

Some *markdown* with a table:

| a | b |
|---|:-:|
| 1 | 2 |

> a quote

A [link](https://example.com/x) and an image.")
         (commented (pp-post "Post with comments" :roles '("public")
                      :body body
                      :cover "/pp-cover-one.png"
                      :featured :true :rating 4
                      :tags '("lisp" "web")))
         (lonely (pp-post "Post with no comments" :roles '("public")
                    :body "A quiet post."
                    :cover "/pp-cover-two.png"
                    :featured nil
                    :rating 2 :tags '("web")))
         (false-post (pp-post "Post with false featured"
                        :roles '("public")
                        :body "Third post."
                        :featured :false)))
    ;; 25 public comments + 1 private on the commented post
    (loop for i from 1 to 25
      do (pp-comment (format nil "Public comment ~d *body*" i)
           "Post with comments" :roles '("public")))
    (pp-comment "Private comment" "Post with comments")
    ;; the draft
    (pp-post "Draft post" :body "Draft body.")
    ;; :mine and :links rows with the public role
    (be-insert :mine '(:note "mine note") "admin"
      :roles '("public"))
    (be-insert :links
      `(:first "Post with comments" :second "Post with no comments")
      "admin" :roles '("public"))
    (list :commented commented
      :lonely lonely
      :false-post false-post)))

(defun pp-ids ()
  "Convenient id lookups for tests: fresh queries, so tests stay
order-independent."
  (list
    :commented (be-id :posts
                  '((:posts :title :eq "Post with comments")) "admin")
    :lonely (be-id :posts
                '((:posts :title :eq "Post with no comments")) "admin")
    :false-post (be-id :posts
                   '((:posts :title :eq "Post with false featured"))
                   "admin")
    :draft (be-id :posts '((:posts :title :eq "Draft post")) "admin")
    :comment (be-id :comments
                '((:comments :body :eq "Public comment 1 *body*"))
                "admin")
    ;; :links: both fields read the same posts.title display alias
    ;; (the two-targets-one-type suppression case), so a display
    ;; filter cannot address the row — take the only row's id.
    :links (getf (car (getf (be-list :links "admin") :records)) :id)
    :mine (be-id :mine '((:mine :note :eq "mine note")) "admin")))

(def-suite public-pages-suite
  :description "Public pages renderer tests (child 1)")

(in-suite public-pages-suite)

(test pp-guest-off-publishes-nothing
  (let ((*top-level-settings*
          (list* :guest-allowed nil *top-level-settings*)))
    (is (null (public-types)))
    (is (null (render-root-index :origin *pp-origin*)))
    (is (null (render-type-index :posts :origin *pp-origin*)))
    (is (null (render-record-document :posts (getf (pp-ids) :commented)
                  :origin *pp-origin*)))
    (is (null (render-feed :posts :origin *pp-origin*)))
    (is (null (render-sitemap :origin *pp-origin*)))))

(test pp-root-index-shape
  (let ((html (render-root-index :origin *pp-origin*)))
    (is (stringp html))
    (is (search "Public Pages Test" html))
    (is (null (search "Nothing is published yet" html)))
    (is (search "/p/posts/" html))
    (is (search "/p/comments/" html))
    (is (search "/p/tags/" html))
    (is (search "Open in app" html))
    ;; no entries for ineligible types
    (is (null (search "/p/notes/" html)))
    (is (null (search "/p/private/" html)))
    (is (null (search "/p/mine/" html)))
    (is (null (search "/p/images/" html)))
    (is (null (search "/p/directories/" html)))))

(test pp-styling-chrome
  "Styling slots reach the public pages end to end: the banner
markup in the site header's h1, the footer markup in the site
footer's slot div. Rows are deleted after — the suite's other
tests assert the default chrome."
  (let ((bid (be-insert :styling
                '(:name "banner" :value "<strong>PP Banner</strong>")
                "admin"))
        (fid (be-insert :styling
                '(:name "footer" :value "<em>PP Footer</em>")
                "admin")))
    (unwind-protect
        (let ((html (render-root-index :origin *pp-origin*)))
          (is (search "<h1 class=\"site\"><strong>PP Banner</strong></h1>"
                html))
          (is (search "<div class=\"slot\"><em>PP Footer</em></div>" html)))
      (be-delete :styling bid "admin")
      (be-delete :styling fid "admin"))))

(test pp-type-index-shape
  (let ((html (render-type-index :posts :origin *pp-origin*))
         (ids (pp-ids)))
    (is (stringp html))
    (is (search "Posts" html))
    ;; page description rendered
    (is (search "Everything the" html))
    ;; public posts listed, draft not
    (is (search "Post with comments" html))
    (is (search "Post with no comments" html))
    (is (null (search "Draft post" html)))
    ;; the record URLs point at uuids
    (is (search (getf ids :commented) html))
    ;; feed autodiscovery
    (is (search "<link rel=\"alternate\" type=\"application/rss+xml\""
                 html))
    (is (search "http://public-pages.test/p/posts/rss.xml" html))))

(test pp-record-document-shape
  (let* ((ids (pp-ids))
         (html (render-record-document :posts (getf ids :commented)
                   :origin *pp-origin*)))
    (is (stringp html))
    ;; h1 title, meta line with date and author
    (is (search "<h1>Post with comments</h1>" html))
    (is (search "<span class=\"author\">admin</span>" html))
    (is (search "<time datetime=" html))
    ;; markdown body: table with alignment, blockquote, link
    (is (search "<th align=\"center\">b</th>" html))
    (is (search "<blockquote>" html))
    (is (search "<a href=\"https://example.com/x\" target=\"_blank\"" html))
    ;; dl: Yes, precision number; tags run
    (is (search "<dt>Featured</dt><dd>Yes</dd>" html))
    (is (search "<dt>Rating</dt><dd>4.0</dd>" html))
    (is (search "<dt>Tags</dt><dd>lisp, web</dd>" html))
    ;; head: canonical, og tags, article times, feed, style
    (is (search "<link rel=\"canonical\" href=\"http://public-pages.test/p/posts/"
                 html))
    (is (search "property=\"og:type\" content=\"article\"" html))
    (is (search "property=\"og:site_name\" content=\"Public Pages Test\""
                 html))
    (is (search "property=\"og:image\" content=\"http://public-pages.test/p/files/"
                 html))
    (is (search "article:published_time\" content=" html))
    (is (search "2026-" html))
    (is (search "<style>" html))
    ;; Open in app href with the record grammar
    (is (search (format nil "/#/app?type=posts&amp;id=~a"
                  (getf ids :commented))
                 html))
    ;; no script other than the JSON-LD block
    (is (= 1 (length (re:all-matches-as-strings "<script" html))))
    (is (search "<script type=\"application/ld+json\">" html))))

(test pp-json-ld-block
  (let* ((ids (pp-ids))
         (html (render-record-document :posts (getf ids :commented)
                   :origin *pp-origin*))
         (start (search "<script type=\"application/ld+json\">" html))
         (json (and start
                 (subseq html
                   (+ start (length
                              "<script type=\"application/ld+json\">"))
                   (search "</script>" html :from-end t))))
         (obj (and json (y:parse json))))
    (is-true json)
    (is (null (search "<" json)))
    (is (equal "BlogPosting" (gethash "@type" obj)))
    (is (equal "Post with comments" (gethash "headline" obj)))
    (is (search (getf ids :commented) (gethash "url" obj)))
    (is (search "datePublished" json))
    (is (equal "admin"
            (gethash "name" (gethash "author" obj))))))

(test pp-json-ld-escapes-script-closer
  ;; a headline containing </script> survives as \u003c/script> inside
  ;; the JSON-LD block; the block's own closing tag is the page's
  ;; only </script>
  (let* ((id (be-insert :posts
                `(:title "Before</script>After" :body "b"
                   :featured :true :rating 1 :author "admin"
                   :api-key "k" :internal-note "n")
                "admin" :roles '("public")))
         (html (render-record-document :posts id
                  :origin *pp-origin*))
         (start (search "<script type=\"application/ld+json\">" html))
         (json (and start
                 (subseq html
                   (+ start (length
                              "<script type=\"application/ld+json\">"))
                   (search "</script>" html :from-end t))))
         (close-count
           (length (re:all-matches-as-strings "</script>" html))))
    (unwind-protect
        (progn
          (is (search "Before" json))
          (is (search "\\u003c/script>After" json))
          ;; one </script> total: the block's own closer
          (is (= 1 close-count)))
      (be-delete :posts id "admin"))))

(test pp-draft-and-missing-rows
  (let ((ids (pp-ids)))
    (is (null (render-record-document :posts (getf ids :draft)
                  :origin *pp-origin*)))
    (is (null (render-record-document :posts
                  (u:uuid) :origin *pp-origin*)))))

(test pp-comment-page
  (let* ((ids (pp-ids))
         (html (render-record-document :comments (getf ids :comment)
                   :origin *pp-origin*))
         (post-id (getf ids :commented)))
    (is (stringp html))
    ;; no identity: title is the first line of the body, plain
    (is (search "Public comment 1" html))
    (is (null (search "*body*" html)))
    ;; the post FK renders as an anchor containing the post uuid
    (is (search
          (format nil "<a href=\"/p/posts/~a\">" post-id) html))
    ;; the :user FK renders as text, not a link to a /p/users page
    (is (search "<dt>Author</dt><dd>admin</dd>" html))
    (is (null (search "/p/users/" html)))
    ;; :note projects; :secret does not
    (is (search "visible note" html))
    (is (null (search "COMMENT-SECRET" html)))))

(test pp-child-region
  (let* ((ids (pp-ids))
         (html (render-record-document :posts (getf ids :commented)
                   :origin *pp-origin*))
         (lonely (render-record-document :posts (getf ids :lonely)
                    :origin *pp-origin*)))
    ;; all 25 public comments, oldest first, private excluded
    (is (search "Comments" html))
    (is (search "Public comment 1" html))
    (is (search "Public comment 25" html))
    (is (null (search "Private comment" html)))
    (let ((p1 (search "Public comment 1" html))
           (p25 (search "Public comment 25" html)))
      (is (< p1 p25)))
    ;; the user-read-type-ids fix: still 25 with a tiny page size
    (let ((a:*default-page-size* 5))
      (is (= 25
              (length (guest-records :comments
                         :filters `((:comments :post :fk-eq
                                      ,(getf ids :commented)))
                         :sort '(:created-at :asc)))))) 
    ;; a post with no comments has no region
    (is (null (search "class=\"children\"" lonely)))))

(test pp-public-view-field-keys
  ;; :posts — the second filter drops :id, the password, the hidden
  ;; widget, the file, the button, and the timestamps, in View-form
  ;; order
  (is (equal '(:title :body :cover :featured :rating :tags :author)
          (public-view-field-keys :posts)))
  ;; :comments — the View-form list minus :id
  (is (equal '(:user :body :post :note :moderate-status)
          (public-view-field-keys :comments))))

(test pp-secrets-never-render
  (let* ((ids (pp-ids))
         (record (render-record-document :posts (getf ids :commented)
                    :origin *pp-origin*))
         (index (render-type-index :posts :origin *pp-origin*))
         (feed (render-feed :posts :origin *pp-origin*))
         (sitemap (render-sitemap :origin *pp-origin*)))
    (dolist (doc (list record index feed sitemap))
      (is (null (search "SECRET-API-KEY-7f3a" doc)))
      (is (null (search "HIDDEN-NOTE-do-not-publish" doc)))
      (is (null (search "COMMENT-SECRET" doc)))
      ;; the button and its status companion never render as fields
      ;; ("Publish" alone would match datePublished in the JSON-LD)
      (is (null (search "<dt>Publish" doc)))
      (is (null (search "publish-status" doc))))))

(test pp-nullable-boolean-rows
  (let* ((ids (pp-ids))
         (lonely-html (render-record-document :posts (getf ids :lonely)
                         :origin *pp-origin*))
         (true-html (render-record-document :posts (getf ids :commented)
                        :origin *pp-origin*))
         (false-html (render-record-document :posts (getf ids :false-post)
                         :origin *pp-origin*)))
    ;; NULL renders no row (the view-result fix keeps :null as :null)
    (is (null (search "<dt>Featured</dt>" lonely-html)))
    (is (search "<dt>Featured</dt><dd>Yes</dd>" true-html))
    (is (search "<dt>Featured</dt><dd>No</dd>" false-html))))

(test pp-view-result-null-boolean
  ;; Plan 01 step 4b pin, amended: a NULL boolean no longer reads as
  ;; :true (the old (if value :true :false) turned SQL NULL true).
  ;; It reads as nil — the same :null-collapse (aggregate-values'
  ;; value-or-nil) every other nullable column type goes through —
  ;; and the renderer's no-row promise holds on that nil.
  (let* ((ids (pp-ids))
         (null-rec (getf (be-rec (getf ids :lonely) "admin"
                            :form :view-form :type-key :posts)
                      :record))
         (true-rec (getf (be-rec (getf ids :commented) "admin"
                            :form :view-form :type-key :posts)
                      :record))
         (false-rec (getf (be-rec (getf ids :false-post) "admin"
                             :form :view-form :type-key :posts)
                       :record)))
    (is (null (getf null-rec :featured)))
    (is (equal :true (getf true-rec :featured)))
    (is (equal :false (getf false-rec :featured)))))

(test pp-direct-call-eligibility
  (let ((ids (pp-ids)))
    ;; :mine is scoped: nil even though the row carries the role
    (is (null (render-record-document :mine (getf ids :mine)
                  :origin *pp-origin*)))
    (is (null (guest-records :mine)))
    (is (null (render-type-index :mine :origin *pp-origin*)))
    ;; :private has no public type role: nil even after a row-level
    ;; public grant (removed in cleanup)
    (let ((id (be-insert :private '(:note "p") "admin")))
      (a:add-resource-role *rbac* (id-to-resource-name id) "public")
      (unwind-protect
          (progn
            (is (null (render-record-document :private id
                           :origin *pp-origin*)))
            (is (null (guest-records :private)))
            (is (null (render-type-index :private
                           :origin *pp-origin*))))
        (a:remove-resource-role *rbac*
          (id-to-resource-name id) "public")
        (be-delete :private id "admin")))))

(test pp-links-render-text
  (let* ((ids (pp-ids))
         (html (render-record-document :links (getf ids :links)
                   :origin *pp-origin*)))
    (is (stringp html))
    ;; two singular targets to one type: suppression keeps them text
    ;; (the display value itself is ambiguous — both fields read the
    ;; same posts.title alias — which is the point of the fixture)
    (is (search "<dt>First</dt><dd>" html))
    (is (search "<dt>Second</dt><dd>" html))
    ;; no FK anchors in the relations dl (the footer's own type
    ;; index links are not FK links)
    (is (null (search "<dd><a href=\"/p/posts/" html)))))

(test pp-cover-image-url
  ;; bind *doc-root* to a nonexistent directory: the renderer never
  ;; touches the filesystem
  (let* ((ids (pp-ids))
         (*doc-root* "/nonexistent-doc-root/"))
    (declare (ignorable *doc-root*))
    (let ((html (render-record-document :posts (getf ids :commented)
                   :origin *pp-origin*)))
      (is (search "<img src=\"/p/files/images/pp-cover-one.png\""
                   html))
      (is (null (search "/api/file" html)))
      (is (search "og:image\" content=\"http://public-pages.test/p/files/images/pp-cover-one.png\""
                   html)))))

(test pp-private-image-path-still-emitted
  ;; the renderer does not authorize files; a private image's path
  ;; appears when the author put it on a public record (the route is
  ;; the gate)
  (let* ((id (pp-post "Private cover post" :roles '("public")
                 :body "b" :cover "/pp-cover-private.png")))
    (unwind-protect
        (let ((html (render-record-document :posts id
                       :origin *pp-origin*)))
          (is (search "/p/files/images/pp-cover-private.png" html)))
      (be-delete :posts id "admin"))))

(test pp-timestamps-match-admin-read
  (let* ((ids (pp-ids))
         (id (getf ids :commented))
         (wrappers (guest-records :posts :sort '(:created-at :desc)))
         (wrapper (find id wrappers
                     :key (lambda (w) (getf (getf w :record) :id))
                     :test #'equal))
         (admin (getf (be-rec id "admin"
                        :form :update-form :type-key :posts)
                   :record)))
    (is-true wrapper)
    (is (re:scan *timestamp-regex* (getf wrapper :created-at)))
    (is (equal (getf admin :created-at) (getf wrapper :created-at)))
    (is (equal (getf admin :updated-at) (getf wrapper :updated-at)))))

(test pp-sitemap-shape
  (let* ((ids (pp-ids))
         (xml (render-sitemap :origin *pp-origin*)))
    (is (search "xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\""
                 xml))
    (is (search "<loc>http://public-pages.test/p/</loc>" xml))
    (is (search "<loc>http://public-pages.test/p/posts/</loc>" xml))
    (is (search (format nil "<loc>http://public-pages.test/p/posts/~a"
                  (getf ids :commented))
                 xml))
    ;; lastmod is the YYYY-MM-DD part on records
    (is (re:scan "<lastmod>\\d{4}-\\d{2}-\\d{2}</lastmod>" xml))
    ;; the draft is absent
    (is (null (search "Draft post" xml)))
    (is (null (search (getf ids :draft) xml)))))

(test pp-feed-shape
  (let* ((ids (pp-ids))
         (xml (render-feed :posts :origin *pp-origin*)))
    (is (search "xmlns:atom=\"http://www.w3.org/2005/Atom\"" xml))
    (is (search
          "xmlns:content=\"http://purl.org/rss/1.0/modules/content/\""
          xml))
    (is (search "<atom:link href=\"http://public-pages.test/p/posts/rss.xml\" rel=\"self\" type=\"application/rss+xml\"/>"
                 xml))
    (is (search "<title>Posts – Public Pages Test</title>" xml))
    (is (search "<link>http://public-pages.test/p/posts/</link>" xml))
    ;; items newest-first with permalink guids and RFC 822 dates
    (is (search "isPermaLink=\"true\"" xml))
    (is (re:scan "<pubDate>[A-Z][a-z][a-z], \\d{2} [A-Z][a-z][a-z] \\d{4} \\d{2}:\\d{2}:\\d{2} \\+0000</pubDate>"
           xml))
    (is (search "<content:encoded><![CDATA[" xml))
    ;; the draft is absent
    (is (null (search "Draft post" xml)))
    ;; newest first: the newest public post appears before an older
    ;; one (Post with comments seeded before Post with no comments)
    (let ((a (search "Post with comments" xml))
           (b (search "Post with no comments" xml)))
      (is (and a b (< b a))))))

(test pp-feed-cdata-split
  ;; The walker escapes every bare > in text, so a body containing
  ;; ]]> reaches the feed as entities or &gt; — well-formed by
  ;; construction. cdata-wrap's split is defense-in-depth for a
  ;; literal ]]>; pinned directly.
  (let ((id (pp-post "CDATA post" :roles '("public")
                 :body "before ]]> after")))
    (unwind-protect
        (let ((xml (render-feed :posts :origin *pp-origin*)))
          (is (search "CDATA post" xml))
          ;; the body's ]]> never ends the CDATA section early
          (is (null (search "]]> after" xml))))
      (be-delete :posts id "admin")))
  (is (equal "<![CDATA[before ]]]]><![CDATA[> after]]>"
          (cdata-wrap "before ]]> after"))))

(test pp-feed-limit-50
  ;; 60 public posts -> 50 items; cleanup removes them
  (let ((ids (loop for i from 1 to 60
               collect (pp-post (format nil "Feed limit post ~d" i)
                          :roles '("public")
                          :body "b"))))
    (unwind-protect
        (let ((xml (render-feed :posts :origin *pp-origin*)))
          (is (= 50 (length (re:all-matches-as-strings "<item>" xml))))
          (is (= 50 (length (guest-records :posts
                            :sort '(:created-at :desc)
                            :limit 50)))))
      (dolist (id ids) (be-delete :posts id "admin")))))

(test pp-valid-public-origin
  (is (equal "http://public-pages.test"
          (valid-public-origin "http://public-pages.test/")))
  (is (equal "http://public-pages.test"
          (valid-public-origin "http://public-pages.test")))
  (is (equal "http://public-pages.test"
          (valid-public-origin *pp-origin*)))
  (signals error (valid-public-origin "http://x.test/path"))
  (signals error (valid-public-origin "//x.test"))
  (signals error (valid-public-origin "https://u:p@x.test"))
  (signals error (valid-public-origin "x.test")))

(test pp-xml-safe-string
  (let ((input (concatenate 'string
                  "a" (string #\tab) "b" (string #\newline)
                  "c" (string (code-char #x01)) "d"
                  (string (code-char #x1f)) "e"
                  (string (code-char #xfffe)) "f"
                  (string (code-char #x10ffff)))))
    (is (equal (concatenate 'string
                  "a" (string #\tab) "b" (string #\newline)
                  "cdef" (string (code-char #x10ffff)))
            (xml-safe-string input))))
  ;; a post body containing #x1 renders without it in the feed
  (let ((id (pp-post "Control char post" :roles '("public")
                 :body (format nil "bad ~c char" (code-char #x01)))))
    (unwind-protect
        (let ((xml (render-feed :posts :origin *pp-origin*)))
          (is (search "Control char post" xml))
          (is (null (search (string (code-char #x01)) xml))))
      (be-delete :posts id "admin"))))

;;
;; Markdown walker pins (plan 01 step 16)
;;

(defun pp-md (source)
  (markdown-html source))

(test pp-md-tables-and-structure
  (let ((html (pp-md "| a | b |
|---|:-:|
| 1 | 2 |")))
    (is (search "<table><thead><tr><th>a</th><th align=\"center\">b</th></tr></thead><tbody><tr><td>1</td><td align=\"center\">2</td></tr></tbody></table>"
                 html)))
  (let ((html (pp-md "- a
  - b
- c")))
    (is (search "<ul>" html))
    (is (search "<li>a" html))
    (is (search "<ul>" html :start2 0)))
  (let ((html (pp-md "> quoted

plain")))
    (is (search "<blockquote>" html))
    (is (search "quoted" html)))
  (let ((html (pp-md "## Heading two")))
    (is (search "<h2>Heading two</h2>" html)))
  (let ((html (pp-md "inline `code` here")))
    (is (search "<code>code</code>" html))))

(test pp-md-fenced-blocks
  (let ((html (pp-md "```lisp
(let ((x <y>))
  x)
```")))
    (is (search "<pre><code class=\"language-lisp\">" html))
    (is (search "&lt;y&gt;" html)))
  (let ((html (pp-md "```
plain
```")))
    (is (search "<pre><code>" html))
    (is (null (search "class=" html)))))

(test pp-md-raw-html-as-text
  (let ((html (pp-md "<script>alert(1)</script>

<iframe src=\"https://x.example\"></iframe>

<span onclick=\"evil()\">text</span>")))
    ;; visible source, never markup
    (is (search "&lt;script&gt;alert(1)&lt;/script&gt;" html))
    (is (search "&lt;iframe" html))
    (is (search "&lt;span onclick=&quot;evil()&quot;&gt;" html))
    (is (null (search "<script>" html)))
    (is (null (search "<iframe" html)))
    (is (null (search "<span" html)))
    (is (null (search "onclick=\"" html)))))

(test pp-md-autolink-and-external-link
  (let ((html (pp-md "<https://example.com/auto>")))
    (is (search "<a href=\"https://example.com/auto\"" html))
    (is (search "target=\"_blank\" rel=\"noopener noreferrer\"" html)))
  (let ((html (pp-md "[label](https://example.com/x \"T\")")))
    (is (search
          "<a href=\"https://example.com/x\" title=\"T\" target=\"_blank\" rel=\"noopener noreferrer\">label</a>"
          html))))

(test pp-md-reference-link
  (let ((html (pp-md "see [text][r1] here

[r1]: https://ref.example \"Title\"")))
    (is (search
          "<a href=\"https://ref.example\" title=\"Title\" target=\"_blank\" rel=\"noopener noreferrer\">text</a>"
          html))))

(test pp-md-dropped-schemes
  ;; javascript: prints its label with no anchor
  (let ((html (pp-md "[click](javascript:alert(1))")))
    (is (search "click" html))
    (is (null (search "<a " html))))
  (let ((html (pp-md "[x](JaVaScRiPt:alert(1))")))
    (is (null (search "<a " html))))
  (let ((html (pp-md "[x](java&#9;script:alert(1))")))
    (is (null (search "<a " html))))
  (let ((html (pp-md "[x](https://user:pass@example.com)")))
    (is (null (search "<a " html))))
  (let ((html (pp-md "[x](//example.com)")))
    (is (null (search "<a " html))))
  (let ((html (pp-md "[x](data:text/html,hi)")))
    (is (null (search "<a " html)))))

(test pp-md-images
  ;; alt containing emphasis and a quote is plain, properly quoted
  (let ((html (pp-md "![alt *em* and \"> seq](/p/files/images/x.png)")))
    (is (search "<img src=\"/p/files/images/x.png\" alt=\"alt em and &quot;&gt; seq\""
                 html)))
  ;; external image reduced to alt text, no external <img>
  (let ((html (pp-md "![pic](https://example.com/x.png)")))
    (is (search "pic" html))
    (is (null (search "<img" html))))
  ;; /api/file and data: images reduced to alt text
  (let ((html (pp-md "![a](/api/file?type=images&path=/x.png)")))
    (is (null (search "<img" html))))
  (let ((html (pp-md "![a](data:image/png;base64,AAAA)")))
    (is (null (search "<img" html)))))

(test pp-md-entities
  (let ((html (pp-md "a &amp; b &bogus c")))
    (is (search "a &amp; b" html))
    (is (search "&amp;bogus" html))))

(test pp-md-no-strikethrough
  ;; accepted absence: ~~x~~ renders literally
  (let ((html (pp-md "~~struck~~")))
    (is (search "~~struck~~" html))
    (is (null (search "<del>" html)))))

;;
;; Empty-state matrix rows (b) and (c) run in their own model
;; contexts: run-public-pages-empty-tests (helpers.lisp).
;;

(def-suite public-pages-empty-suite
  :description "Public pages empty-state matrix (rows b and c)")

(in-suite public-pages-empty-suite)

(test pp-empty-zero-eligible-types
  ;; guest on, every public type's roles stripped: the root renders
  ;; "Nothing is published yet", everything else is nil, the sitemap
  ;; lists the root only
  (let ((saved
          (loop for key in '(:posts :comments :tags :links)
            collect (list key
                     (a:list-resource-role-names *rbac*
                       (type-resource-name key))))))
    (unwind-protect
        (progn
          (dolist (key '(:posts :comments :tags :links))
            (be-remove-type-roles key "admin" "public"))
          (is (null (public-types)))
          (let ((root (render-root-index :origin *pp-origin*)))
            (is (stringp root))
            (is (search "Nothing is published yet" root)))
          (is (null (render-type-index :posts :origin *pp-origin*)))
          ;; any uuid: eligibility fails before the row lookup, and
          ;; this unseeded context has no rows to name anyway
          (is (null (render-record-document :posts (u:uuid)
                      :origin *pp-origin*)))
          (is (null (render-feed :posts :origin *pp-origin*)))
          (let ((xml (render-sitemap :origin *pp-origin*)))
            (is (search "<loc>http://public-pages.test/p/</loc>" xml))
            (is (null (search "/p/posts/" xml)))))
      (dolist (entry saved)
        (let ((current (a:list-resource-role-names *rbac*
                          (type-resource-name (first entry)))))
          (dolist (role (second entry))
            (unless (member role current :test #'equal)
              (a:add-resource-role *rbac*
                (type-resource-name (first entry)) role))))))))

(test pp-empty-zero-readable-rows
  ;; guest on, eligible types, zero readable rows (the unseeded
  ;; fixture has none): root lists the types; the type index renders
  ;; with "No entries yet"; the feed renders with zero items; a
  ;; record is nil; the sitemap lists root and indexes only
  (is (equal '(:posts :comments :tags :links) (public-types)))
  (let ((root (render-root-index :origin *pp-origin*)))
    (is (search "/p/posts/" root)))
  (let ((index (render-type-index :posts :origin *pp-origin*)))
    (is (stringp index))
    (is (search "Posts" index))
    (is (search "No entries yet" index)))
  (let ((feed (render-feed :posts :origin *pp-origin*)))
    (is (stringp feed))
    (is (search "<channel>" feed))
    (is (null (search "<item>" feed))))
  (is (null (render-record-document :posts (u:uuid)
              :origin *pp-origin*)))
  (let ((xml (render-sitemap :origin *pp-origin*)))
    (is (search "<loc>http://public-pages.test/p/posts/</loc>" xml))
    (is (null (search "<lastmod>" xml)))))

;;
;; Public route tests (todo child 2) — public-route called directly,
;; plus the wire-record decoration tests. The live HTTP suite below
;; pins what the adapter and Hunchentoot actually send.
;;

(def-suite public-pages-route-suite
  :description "Public route tests (child 2, direct calls)")

(in-suite public-pages-route-suite)

(defun pp-route (method path &key (origin *pp-origin*))
  "Call public-route and return (status headers body kind) as a list."
  (multiple-value-list
    (public-route method path :origin origin)))

(defun pp-header (headers name)
  (cdr (assoc name headers :test #'equalp)))

(test route-redirects
  (let ((r (pp-route :get "/p")))
    (is (equal 301 (first r)))
    (is (equal "/p/" (pp-header (second r) "Location")))
    (is (equal "public, max-age=3600"
          (pp-header (second r) "Cache-Control")))
    (is (plusp (length (third r)))))
  (let ((r (pp-route :get "/p/posts")))
    (is (equal 301 (first r)))
    (is (equal "/p/posts/" (pp-header (second r) "Location"))))
  (let* ((ids (pp-ids))
         (r (pp-route :get
               (format nil "/p/posts/~a/" (getf ids :commented)))))
    (is (equal 301 (first r)))
    (is (equal (format nil "/p/posts/~a" (getf ids :commented))
          (pp-header (second r) "Location")))))

(test route-documents
  (let ((r (pp-route :get "/p/")))
    (is (equal 200 (first r)))
    (is (equal (render-root-index :origin *pp-origin*) (third r)))
    (is (equal :html (fourth r))))
  (let ((r (pp-route :get "/p/posts/")))
    (is (equal 200 (first r)))
    (is (equal (render-type-index :posts :origin *pp-origin*) (third r))))
  (let* ((ids (pp-ids))
         (r (pp-route :get (format nil "/p/posts/~a" (getf ids :commented)))))
    (is (equal 200 (first r)))
    (is (equal (render-record-document :posts (getf ids :commented)
                  :origin *pp-origin*)
          (third r))))
  (let ((r (pp-route :get "/p/posts/rss.xml")))
    (is (equal 200 (first r)))
    (is (equal (render-feed :posts :origin *pp-origin*) (third r)))
    (is (equal :feed (fourth r))))
  (let ((r (pp-route :get "/sitemap.xml")))
    (is (equal 200 (first r)))
    (is (equal (render-sitemap :origin *pp-origin*) (third r))))
  (let ((r (pp-route :get "/robots.txt")))
    (is (equal 200 (first r)))
    (is (equal
          (format nil "User-agent: *~%Allow: /~%Sitemap: ~a/sitemap.xml~%"
            *pp-origin*)
          (third r)))))

(test route-404s
  (let* ((ids (pp-ids))
         (paths
           (list
             (format nil "/p/posts/~a" (getf ids :draft))
             (format nil "/p/posts/~a" (u:uuid))
             "/p/notes/"
             "/p/notes/x"
             "/p/private/"
             "/p/mine/"
             "/p/images/"
             "/p/posts/not-a-uuid"
             "/p/manifest.json"
             (format nil "/p/posts/~a/extra" (getf ids :commented))
             "/p/unknown-type/"
             "/p/x/y/z")))
    (dolist (path paths)
      (let ((r (pp-route :get path)))
        (is (equal 404 (first r)))
        (is (equal "no-store" (pp-header (second r) "Cache-Control")))
        (is (equal :text (fourth r)))
        (is (null (search "<!DOCTYPE" (third r))))))))

(test route-guest-off
  (let ((*top-level-settings*
          (list* :guest-allowed nil *top-level-settings*))
        (*public-origin* *pp-origin*))
    (let ((r (pp-route :get "/p/")))
      (is (equal 404 (first r))))
    (let ((r (pp-route :get "/p/posts/")))
      (is (equal 404 (first r))))
    (let ((r (pp-route :get "/sitemap.xml")))
      (is (equal 404 (first r))))
    (let ((r (pp-route :get "/p/files/images/pp-cover-one.png")))
      (is (equal 404 (first r))))
    ;; robots.txt still serves, with the Sitemap line
    (let ((r (pp-route :get "/robots.txt")))
      (is (equal 200 (first r)))
      (is (search "Sitemap: " (third r))))))

(test route-dispatch-claims
  (is (null (public-claimed-path-p "/posts")))
  (is (null (public-claimed-path-p "/piano")))
  (is (null (public-claimed-path-p "/")))
  (is (null (public-claimed-path-p "/api/list")))
  (is (public-claimed-path-p "/p"))
  (is (public-claimed-path-p "/p/"))
  (is (public-claimed-path-p "/p/x"))
  (is (public-claimed-path-p "/p/files/images/x"))
  (is (public-claimed-path-p "/sitemap.xml"))
  (is (public-claimed-path-p "/robots.txt")))

(test route-path-safety
  ;; bind *doc-root* to a nonexistent directory: any accidental
  ;; filesystem access errors (500), not a silent 200
  (let ((*doc-root* "/nonexistent-doc-root/")
        (paths
          '("/p/files/images/../secrets"
            "/p/files/images/..%2Fsecrets"
            "/p/files/images/%2E%2E/secrets"
            "/p/files/images/a%2Fb.png"
            "/p/files/images//x.png"
            "/p/files/images/."
            "/p/files/images/a\\b.png")))
    (dolist (path paths)
      (let ((r (pp-route :get path)))
        (is (equal 404 (first r))))))
  ;; a path segment containing + is looked up with the literal +
  (let ((r (pp-route :get "/p/files/images/a+b.png")))
    (is (equal 404 (first r))))
  ;; query strings are ignored
  (let ((r (pp-route :get "/p/posts/?x=1")))
    (is (equal 200 (first r)))))

(test route-file-route
  (let ((r (pp-route :get "/p/files/images/pp-cover-one.png")))
    (is (equal 200 (first r)))
    (is (equal "public, max-age=3600"
          (pp-header (second r) "Cache-Control")))
    (is (equal "image/png" (getf (third r) :content-type)))
    (is (search "pp-cover-one.png"
          (format nil "~a" (getf (third r) :file)))))
  ;; private image: 404, not 403
  (let ((r (pp-route :get "/p/files/images/pp-cover-private.png")))
    (is (equal 404 (first r))))
  ;; nonexistent path
  (let ((r (pp-route :get "/p/files/images/no-such.png")))
    (is (equal 404 (first r))))
  ;; posts is not a tree type
  (let ((r (pp-route :get "/p/files/posts/x")))
    (is (equal 404 (first r))))
  ;; folders is a tree leaf that is not fs-backed
  (let ((r (pp-route :get "/p/files/folders/x")))
    (is (equal 404 (first r))))
  ;; directories is a tree, not a leaf
  (let ((r (pp-route :get "/p/files/directories/x")))
    (is (equal 404 (first r)))))

(test route-authorized-file-path-direct
  (multiple-value-bind (path reason)
    (authorized-file-path :images "/pp-cover-one.png" "admin")
    (is (search "pp-cover-one.png" (format nil "~a" path)))
    (is (null reason)))
  (multiple-value-bind (path reason)
    (authorized-file-path :images "/pp-cover-private.png" *guest-user*)
    (is (null path))
    (is (equal :forbidden reason)))
  (multiple-value-bind (path reason)
    (authorized-file-path :images "/no-such.png" "admin")
    (is (null path))
    (is (equal :missing reason))))

(test route-method-and-unavailable
  (let ((r (pp-route :post "/p/")))
    (is (equal 405 (first r)))
    (is (equal "GET, HEAD" (pp-header (second r) "Allow")))
    (is (equal "no-store" (pp-header (second r) "Cache-Control"))))
  (let ((*compiled-model* nil))
    (let ((r (pp-route :get "/p/")))
      (is (equal 503 (first r)))
      (is (equal "5" (pp-header (second r) "Retry-After")))))
  (let ((*rbac* nil))
    (let ((r (pp-route :get "/p/")))
      (is (equal 503 (first r)))))
  ;; HEAD runs the GET path
  (let ((r (pp-route :head "/p/")))
    (is (equal 200 (first r)))))

(test route-headers
  (let ((r (pp-route :get "/p/posts/")))
    (is (equal "public, max-age=60"
          (pp-header (second r) "Cache-Control")))
    (is (equal "nosniff" (pp-header (second r) "X-Content-Type-Options")))
    (is (equal
          "default-src 'none'; img-src 'self'; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'"
          (pp-header (second r) "Content-Security-Policy")))
    (is (null (pp-header (second r) "Access-Control-Allow-Origin"))))
  ;; files and errors carry nosniff and no CSP
  (let ((r (pp-route :get "/p/files/images/pp-cover-one.png")))
    (is (equal "nosniff"
          (pp-header (second r) "X-Content-Type-Options")))
    (is (null (pp-header (second r) "Content-Security-Policy"))))
  (let ((r (pp-route :get "/p/nope")))
    (is (equal "nosniff"
          (pp-header (second r) "X-Content-Type-Options")))))

;;
;; :public-url on the wire records
;;

(def-suite public-pages-wire-suite
  :description "The :public-url wire key (child 2)")

(in-suite public-pages-wire-suite)

(test wire-public-url-present-and-absent
  ;; the wire key is built from *public-origin* (the configured
  ;; origin), so bind it for a deterministic expectation
  (let ((*public-origin* *pp-origin*))
  (let* ((ids (pp-ids))
         (item (getf (be-rec (getf ids :commented) "admin"
                         :form :view-form :type-key :posts)
                   :record))
         (draft-item (getf (be-rec (getf ids :draft) "admin"
                              :form :view-form :type-key :posts)
                       :record))
         (listing (getf (be-list :posts "admin" :form :list-form
                          :limit 100)
                     :records)))
    (is (member :public-url (u:plist-keys item)))
    (is (equal (format nil "~a/p/posts/~a" *pp-origin*
                  (getf ids :commented))
          (getf item :public-url)))
    ;; key membership, never getf (which cannot tell absent from
    ;; present-nil)
    (is (null (member :public-url (u:plist-keys draft-item))))
    ;; list and item agree
    (let ((listed (find (getf ids :commented) listing
                     :key (lambda (r) (getf r :id)) :test #'equal)))
      (is (equal (getf item :public-url)
            (getf listed :public-url))))
    ;; rec's :record does not carry it (be-update stays clean)
    (let ((raw (getf (rec (getf ids :commented) "admin"
                      :form :update-form :type-key :posts)
                  :record)))
      (is (null (member :public-url (u:plist-keys raw))))))))

(test wire-row-level-revocation
  (let* ((ids (pp-ids))
         (resource (id-to-resource-name (getf ids :commented))))
    (unwind-protect
        (progn
          (a:remove-resource-role *rbac* resource "public")
          (let ((item (getf (be-rec (getf ids :commented) "admin"
                             :form :view-form :type-key :posts)
                       :record)))
            (is (null (member :public-url (u:plist-keys item)))))
          ;; another public post keeps it
          (let ((other (getf (be-rec (getf ids :lonely) "admin"
                              :form :view-form :type-key :posts)
                        :record)))
            (is (member :public-url (u:plist-keys other)))))
      (a:add-resource-role *rbac* resource "public"))))

(test wire-type-level-revocation
  (unwind-protect
      (progn
        (be-remove-type-roles :posts "admin" "public")
        (let* ((ids (pp-ids))
               (item (getf (be-rec (getf ids :commented) "admin"
                              :form :view-form :type-key :posts)
                        :record)))
          (is (null (member :public-url (u:plist-keys item))))))
    (a:add-resource-role *rbac* (type-resource-name :posts) "public")))

(test wire-be-update-still-succeeds
  ;; the rec path is clean: updating a public post works
  (let* ((ids (pp-ids))
         (record (getf (rec (getf ids :commented) "admin"
                        :form :update-form :type-key :posts)
                   :record)))
    (is (be-update :posts (getf ids :commented)
          (list :rating (getf record :rating)) "admin"))))

(test wire-invalid-origin-keeps-api-working
  (let ((*public-origin* "not-an-origin"))
    (let* ((ids (pp-ids))
           (listing (be-list :posts "admin" :form :list-form :limit 100))
           (item (getf (be-rec (getf ids :commented) "admin"
                          :form :view-form :type-key :posts)
                    :record)))
      (is (plusp (length (getf listing :records))))
      (is (null (member :public-url (u:plist-keys item))))
      ;; the public routes are what report the bad origin
      (signals error (public-route :get "/p/")))))

(test wire-public-url-field-name-reserved
  ;; a model that declares a :public-url field fails to compile and
  ;; the error names the collision
  (signals error
    (compile-model
      `(:things
         (:table t
           :create :auto :update :auto :delete :auto :display t
           :type-roles ("public")
           :views (:main (:tables (:things)))
           :fields
           (:name (:type :text :identity t
                   :ui (:label "Name" :widget :textbox)
                   :validations (:required)
                   :source (:view :main :column :name :agg :first)
                   :column t :not-null t)
             :public-url (:type :text
                     :ui (:label "URL" :widget :textbox)
                     :source (:view :main :column :url :agg :first)
                     :column t))
           :list-form (:fields (:name))
           :view-form (:fields (:name :public-url))
           :update-form (:fields (:name :public-url))
           :add-form (:fields (:name :public-url)))))))

;;
;; Live HTTP tests (child 2) — what the adapter and Hunchentoot
;; actually send. with-model started the server on *http-host* /
;; *http-port*.
;;

(def-suite public-pages-http-suite
  :description "Public pages live HTTP tests (child 2)")

(in-suite public-pages-http-suite)

(defun pp-http (path &key (method :get) (headers '()))
  "One raw HTTP request against the live server. Returns a plist
(:body :code :headers); the body is an octet vector (decode with
pp-http-text when a string is needed)."
  (multiple-value-bind (body code response-headers)
    (dr:http-request
      (format nil "http://~a:~a~a" *http-host* *http-port* path)
      :method method
      :additional-headers headers
      :decode-content nil
      :preserve-uri t)
    (list :body body :code code
      :headers (loop for h in response-headers
                 collect (cons (car h) (cdr h))))))

(defun pp-http-text (response)
  (b:octets-to-string (getf response :body) :encoding :utf-8))

(defun pp-assert-spa-shell (path)
  "Assert PATH reaches the SPA (serve-frontend), never the public
namespace. When the web dist exists the answer is the 200 index.html
shell; in environments without it (the test host has no /app/web)
serve-frontend's own 404 is still the SPA's answer — what is pinned
is that it is not the public route's plain text 404."
  (let ((r (pp-http path)))
    (let ((index
            (probe-file
              (merge-pathnames "index.html"
                (uiop:ensure-directory-pathname *web-directory*)))))
      (if index
        (progn
          (is (equal 200 (getf r :code)))
          (is (search "text/html" (pp-http-header r "Content-Type"))))
        (is (not (and (equal 404 (getf r :code))
                   (equal "Not found" (pp-http-text r)))))))))

(defun pp-http-header (response name)
  ;; drakma converts response header names to keywords
  (cdr (assoc name (getf response :headers) :test #'string-equal)))

(defun pp-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s)
                 :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(test http-document
  (let ((r (pp-http "/p/")))
    (is (equal 200 (getf r :code)))
    (is (equal "text/html; charset=utf-8"
          (pp-http-header r "Content-Type")))
    (is (equal "public, max-age=60"
          (pp-http-header r "Cache-Control")))
    (is (equal "nosniff" (pp-http-header r "X-Content-Type-Options")))
    (is (equal
          "default-src 'none'; img-src 'self'; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'"
          (pp-http-header r "Content-Security-Policy")))
    (is (equal (render-root-index) (pp-http-text r)))))

(test http-head-record
  (let* ((ids (pp-ids))
         (r (pp-http (format nil "/p/posts/~a" (getf ids :commented))
               :method :head))
         (g (pp-http (format nil "/p/posts/~a" (getf ids :commented)))))
    (is (equal 200 (getf r :code)))
    (is (equal (pp-http-header g "Content-Type")
          (pp-http-header r "Content-Type")))
    (is (equal (pp-http-header g "Cache-Control")
          (pp-http-header r "Cache-Control")))
    (is (equal (pp-http-header g "X-Content-Type-Options")
          (pp-http-header r "X-Content-Type-Options")))
    (is (equal (pp-http-header g "Content-Security-Policy")
          (pp-http-header r "Content-Security-Policy")))
    (is (equal (pp-http-header g "Content-Length")
          (pp-http-header r "Content-Length")))
    (is (zerop (length (getf r :body))))))

(test http-file-served-as-full-200
  (let* ((r (pp-http "/p/files/images/pp-cover-one.png"))
         (stored
           (pp-file-bytes
             (fs-path :images "/pp-cover-one.png"))))
    (is (equal 200 (getf r :code)))
    (is (equal "image/png" (pp-http-header r "Content-Type")))
    (is (equal "public, max-age=3600"
          (pp-http-header r "Cache-Control")))
    (is (equal "nosniff" (pp-http-header r "X-Content-Type-Options")))
    (is (equal (write-to-string (length stored))
          (pp-http-header r "Content-Length")))
    (is (null (pp-http-header r "Accept-Ranges")))
    (is (null (pp-http-header r "Last-Modified")))
    (is (equalp stored (getf r :body)))
    ;; HEAD: same selected headers, no bytes
    (let ((h (pp-http "/p/files/images/pp-cover-one.png"
               :method :head)))
      (is (equal 200 (getf h :code)))
      (is (equal "image/png" (pp-http-header h "Content-Type")))
      (is (zerop (length (getf h :body)))))
    ;; Range is ignored: full 200, never 206
    (let ((rr (pp-http "/p/files/images/pp-cover-one.png"
                 :headers '(("Range" . "bytes=0-3")))))
      (is (equal 200 (getf rr :code)))
      (is (equalp stored (getf rr :body))))
    (let ((rr (pp-http "/p/files/images/pp-cover-one.png"
                 :headers '(("Range" . "bytes=3-0")))))
      (is (equal 200 (getf rr :code)))
      (is (equalp stored (getf rr :body))))
    ;; If-Modified-Since is ignored: 200, never 304
    (let ((rr (pp-http "/p/files/images/pp-cover-one.png"
                 :headers
                   `(("If-Modified-Since" .
                       ,(h:rfc-1123-date
                          (file-write-date
                            (fs-path :images
                              "/pp-cover-one.png"))))))))
      (is (equal 200 (getf rr :code)))
      (is (equalp stored (getf rr :body))))))

(test http-file-error-boundary
  ;; a file deleted before the request: the authorization says
  ;; :missing, and the public route collapses it to 404 (a private
  ;; file and a missing file are indistinguishable from outside)
  (let* ((stored (fs-path :images "/pp-cover-one.png"))
         (backup "/tmp/pp-cover-one.png.bak"))
    (unwind-protect
        (progn
          (u:copy-file stored backup)
          (delete-file stored)
          (let ((r (pp-http "/p/files/images/pp-cover-one.png")))
            (is (equal 404 (getf r :code)))
            (is (equal "no-store" (pp-http-header r "Cache-Control")))
            (is (equal "Not found" (pp-http-text r)))))
      (when (probe-file backup)
        (u:copy-file backup stored)
        (delete-file backup))))
  ;; the open failing after the authorization passed (the race, or a
  ;; permission error): the plain 500, built from nothing
  (let ((old (fdefinition 'open-public-file)))
    (unwind-protect
        (progn
          (setf (fdefinition 'open-public-file)
            (lambda (path)
              (declare (ignore path))
              (error "vanished")))
          (let ((r (pp-http "/p/files/images/pp-cover-one.png")))
            (is (equal 500 (getf r :code)))
            (is (equal "text/plain; charset=utf-8"
                  (pp-http-header r "Content-Type")))
            (is (equal "no-store" (pp-http-header r "Cache-Control")))
            (is (equal "nosniff"
                  (pp-http-header r "X-Content-Type-Options")))
            (is (null (pp-http-header r "Content-Security-Policy")))
            (is (equal "Internal error" (pp-http-text r)))))
      (setf (fdefinition 'open-public-file) old))))

(defun pp-raw-http-status (path)
  "Send one raw request line over a socket and return the status
code as an integer (drakma itself refuses malformed escapes, so the
server's 400 needs a raw client)."
  (let ((stream
          (usocket:socket-stream
            (usocket:socket-connect *http-host* *http-port*
              :element-type 'character))))
    (unwind-protect
        (progn
          (format stream
            "GET ~a HTTP/1.1~c~cHost: x~c~cConnection: close~c~c~c~c"
            path #\return #\linefeed #\return #\linefeed
            #\return #\linefeed #\return #\linefeed)
          (finish-output stream)
          (let ((status-line (read-line stream)))
            (parse-integer
              (subseq status-line
                (1+ (position #\space status-line))
                (position #\space status-line :start
                  (1+ (position #\space status-line)))))))
      (close stream))))

(test http-malformed-escapes-are-hunchentoots-400
  (dolist (path '("/p/posts/%G1" "/p/%" "/p/posts/%FF%FE"))
    (is (equal 400 (pp-raw-http-status path))))
  ;; %2F in the first segment: one segment named p/posts — not
  ;; claimed; the SPA answers
  (pp-assert-spa-shell "/p%2Fposts")
  ;; encoded .. in a file path: 404
  (let ((r (pp-http "/p/posts/%2E%2E/x")))
    (is (equal 404 (getf r :code)))))

(test http-post-and-spa-paths
  (let ((r (pp-http "/p/" :method :post)))
    (is (equal 405 (getf r :code)))
    (is (equal "GET, HEAD" (pp-http-header r "Allow"))))
  ;; /posts is not claimed: the SPA answers
  (pp-assert-spa-shell "/posts"))

(test http-unavailable-and-guest-off
  ;; setf (not let): the server handler runs in another thread and
  ;; must see it
  (let ((saved-model *compiled-model*)
        (saved-settings *top-level-settings*))
    (unwind-protect
        (progn
          (setf *compiled-model* nil)
          (let ((r (pp-http "/p/")))
            (is (equal 503 (getf r :code)))
            (is (equal "5" (pp-http-header r "Retry-After")))))
      (setf *compiled-model* saved-model)))
  (let ((saved-settings *top-level-settings*))
    (unwind-protect
        (progn
          (setf *top-level-settings*
            (list* :guest-allowed nil *top-level-settings*))
          (let ((r (pp-http "/sitemap.xml")))
            (is (equal 404 (getf r :code)))
            (is (equal "no-store" (pp-http-header r "Cache-Control"))))
          (let ((r (pp-http "/robots.txt")))
            (is (equal 200 (getf r :code)))))
      (setf *top-level-settings* saved-settings))))

(test http-error-boundary-hides-condition
  ;; an invalid origin: the plain 500, no condition text, no
  ;; PUBLIC_ORIGIN in the body. setf, not let — the server handler
  ;; runs in another thread and must see it.
  (let ((saved *public-origin*))
    (unwind-protect
        (progn
          (setf *public-origin* "https://bad origin")
          (let ((r (pp-http "/p/")))
            (is (equal 500 (getf r :code)))
            (is (equal "text/plain; charset=utf-8"
                  (pp-http-header r "Content-Type")))
            (is (equal "Internal error" (pp-http-text r)))
            (is (null (search "PUBLIC_ORIGIN" (pp-http-text r))))))
      (setf *public-origin* saved))))

(test http-render-error-boundary
  ;; a render that signals: the same plain 500 without the condition
  (let ((old (fdefinition 'render-root-index)))
    (unwind-protect
        (progn
          (setf (fdefinition 'render-root-index)
            (lambda (&key origin)
              (declare (ignore origin))
              (error "boom")))
          (let ((r (pp-http "/p/")))
            (is (equal 500 (getf r :code)))
            (is (equal "Internal error" (pp-http-text r)))
            (is (null (search "boom" (pp-http-text r))))))
      (setf (fdefinition 'render-root-index) old))))

(test http-api-file-regressions
  ;; the factoring kept /api/file's behavior: these go through the
  ;; real endpoints with tokens
  (let ((r (pp-http "/api/file?type=images&path=/pp-cover-one.png")))
    (is (equal 401 (getf r :code))))
  ;; admin via query token: 200 with bytes
  (let* ((token (login-and-get-token "admin" *admin-password*))
         (r (pp-http
              (format nil "/api/file?type=images&path=/pp-cover-one.png&token=~a"
                token))))
    (is (equal 200 (getf r :code)))))

(defun login-and-get-token (user password)
  "POST /api/login and return the access token."
  (multiple-value-bind (body code)
    (dr:http-request
      (format nil "http://~a:~a/api/login" *http-host* *http-port*)
      :method :post
      :content
        (y:with-output-to-string* ()
          (y:encode
            (json-ld-hash
              (list "username" user "password" password))))
      :content-type "application/json"
      :decode-content nil)
    (declare (ignore code))
    (let ((text (if (stringp body) body
                  (b:octets-to-string body :encoding :utf-8))))
      (getf (getf (json-to-plist text) :result) :access-token))))
