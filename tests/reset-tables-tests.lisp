(in-package :data-ui)

(def-suite reset-tables-suite
  :description "reset-tables deletes the dropped tables' row
resources: one pre-drop UNION ALL delete over the model-derived
row-resourced set. The headline case pins the frozen-snapshot
design against sequential per-table removal at the comment row,
given the fixture's walk order (posts precede comments): the
order-independent guarantee is the statement snapshot, which the
case demonstrates, not a row shape it could exhaust.")

(in-suite reset-tables-suite)

(defun rtt-resource-name (id)
  "The resource_name of the resources row with ID, or nil (the name
column is resource_name — never name). Raw SQL, never be-list: it
permission-filters, and would hide a resource that lost its roles
rather than one that was deleted."
  (query "select resource_name from resources where id = $1"
    :params (list id) :result-type :single))

(defun rtt-resource-role-count (id)
  "resource_roles rows linked to the resource with ID."
  (query "select count(*) from resource_roles where resource_id = $1"
    :params (list id) :result-type :single))

(defun rtt-table-count (table)
  "Row count of TABLE. Table names are literals matching the
compiler's rt_-prefixed output — the same trust level as
drop-table's interpolated drops."
  (query (format nil "select count(*) from ~a" table)
    :result-type :single))

(test rtt-row-resources-gone-including-cascade-dead
  "One pre-drop DELETE removes the resources of every doomed row,
including rows that die only by cross-table cascade: posts'
resources go first, the FK cascade kills the referencing comment
row, and the comment's resource still dies — its id was in the
doomed set at statement start. A sequential per-table removal
walking posts before comments (the fixture's order) would find the
comment row already gone and leak its resource."
  (let* ((tag-id (be-insert :tags '(:name "rtt-tag") "admin"))
         (post-id (be-insert :posts '(:title "rtt-post") "admin"))
         (comment-id (be-insert :comments
                         '(:title "rtt-comment" :body "body"
                           :tags ("rtt-tag"))
                         "admin")))
    ;; uuid-valued FKs are attached with be-set-field-value, never
    ;; through be-insert data (the resolve-reference-id uuid
    ;; short-circuit would attach the wrong row).
    (be-set-field-value :comments comment-id :post post-id "admin")
    (is (rtt-resource-name post-id)
        "post resource exists before the reset")
    (is (rtt-resource-name comment-id)
        "comment resource exists before the reset")
    (set-model "fk-eq-test")
    (is-false (rtt-resource-name post-id) "post resource gone")
    (is-false (rtt-resource-name comment-id)
      "comment resource gone — the row died by cascade, the
      resource by the statement's frozen id set")
    (is (zerop (rtt-table-count "rt_posts")))
    (is (zerop (rtt-table-count "rt_comments")))))

(test rtt-role-links-go-with-the-resource
  "resource_roles rows cascade from the resources delete: a post
inserted with an extra role loses its links with its resource."
  (let ((post-id (be-insert :posts '(:title "rtt-role-post") "admin"
                     :roles '("public"))))
    (is (plusp (rtt-resource-role-count post-id))
        "role links exist before the reset")
    (set-model "fk-eq-test")
    (is (zerop (rtt-resource-role-count post-id))
        "role links gone with the resource")))

(test rtt-usage-resource-gone
  ":usage rows are resources too (built-in non-base type): a usage
row's resource dies with the reset. :styling, :tokens, and :secrets
share the same predicate branch — a test on any one member of that
class is a test on the class."
  (let ((usage-id (record-usage "admin" :list :on :posts)))
    (is (stringp usage-id) "usage row recorded")
    (is (rtt-resource-name usage-id) "usage resource exists")
    (set-model "fk-eq-test")
    (is-false (rtt-resource-name usage-id) "usage resource gone")))

(test rtt-type-resources-and-base-tables-survive
  "type-* resources, users, and roles survive; the settings table
is dropped with only admin/guest settings re-created by set-model's
tail, so a plain user's settings row is gone. Settings is base —
its :id has no :resources target — so the table is dropped, its
rows vanish with it, and no resource cleanup is owed."
  (th-make-user "rtt-plain-user")
  (is (be-value-id :settings :user "rtt-plain-user" "admin")
      "plain user's settings row exists before the reset")
  (set-model "fk-eq-test")
  (is (a:get-id *rbac* "resources" "type-posts") "type-posts survives")
  (is (member "logged-in" (a:list-resource-role-names *rbac* "type-posts")
        :test #'equal)
      "type-posts keeps its declared role")
  (is (a:get-id *rbac* "users" "rtt-plain-user") "users survive")
  (is (be-value-id :settings :user "admin" "admin")
      "admin settings re-created")
  (is (be-value-id :settings :user "guest" "admin")
      "guest settings re-created")
  (is-false (be-value-id :settings :user "rtt-plain-user" "admin")
      "plain user's settings row gone"))

(test rtt-pre-existing-orphans-not-swept
  "A resource with no row behind it survives the reset: the delete
is id membership in the doomed tables, never a name sweep — the
guard against anyone improving the fix into a sweep."
  (let* ((ghost-name (format nil "posts:name:ghost:~a" (u:uuid)))
         (ghost-id (a:add-resource *rbac* ghost-name)))
    (is (stringp ghost-id) "orphan resource inserted")
    (set-model "fk-eq-test")
    (is (equal ghost-id (a:get-id *rbac* "resources" ghost-name))
        "orphan resource untouched")))

(test rtt-joiner-rows-never-in-resources
  "M2M join rows own no resources (internal types' injected :id
has no :resources target): the join row id is absent from
resources before and after the reset — the joiner exclusion holds."
  (let* ((tag-id (be-insert :tags '(:name "rtt-join-tag") "admin"))
         (post-id (be-insert :posts '(:title "rtt-join-post") "admin"))
         (comment-id (be-insert :comments
                         '(:title "rtt-join-comment" :body "body"
                           :tags ("rtt-join-tag"))
                         "admin")))
    (be-set-field-value :comments comment-id :post post-id "admin")
    (let ((join-id (query
                     "select id from rt_comment_tags where comment_id = $1"
                     :params (list comment-id) :result-type :single)))
      (is (stringp join-id) "join row exists")
      (is-false (rtt-resource-name join-id)
          "join row never had a resource")
      (set-model "fk-eq-test")
      (is-false (rtt-resource-name join-id)
          "join row id still absent from resources")
      (is (zerop (rtt-table-count "rt_comment_tags"))
          "join table recreated empty"))))
