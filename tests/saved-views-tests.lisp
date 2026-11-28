(in-package :data-ui)

;;; ---------------------------------------------------------------------------
;;; Saved list views (item "Saved list views"): the hidden built-in
;;; :saved-views type — a per-user store of named list queries — and the
;;; be-update row check that keeps it (and :secrets) private.
;;; ---------------------------------------------------------------------------

(def-suite saved-views-suite
  :description "Saved list views tests (base model, test-model context)")

(in-suite saved-views-suite)

(defmacro th-sv-with-rows (&body body)
  "Run BODY with a local th-sv-insert (type-key data user) that inserts
a test row through be-insert, remembers its id, and returns be-insert's
two values. Every remembered row is deleted as admin afterwards — also
when BODY signals midway (a failed assertion, or an insert that errors
after an earlier one landed) — so the suite stays order-independent."
  `(let ((rows nil))
     (flet ((th-sv-insert (type-key data user)
              (multiple-value-bind (id inserted)
                  (be-insert type-key data user)
                (when (and id inserted)
                  (push (list type-key id) rows))
                (values id inserted))))
       (unwind-protect
           (progn ,@body)
         (loop for (type-key id) in rows
           do (ignore-errors (be-delete type-key id "admin")))))))

(defmacro th-sv-err (expr)
  "Evaluate EXPR, return its error message (or :no-error)."
  `(handler-case
       (progn ,expr :no-error)
     (error (e) (format nil "~a" e))))

(defun th-sv-record (id user)
  "The :saved-views record with ID as USER reads it, or nil when USER
cannot read the row."
  (getf (be-rec id user :type-key :saved-views) :record))

(defun th-sv-names (user)
  "The :name of every :saved-views row USER's be-list returns."
  (mapcar (lambda (r) (getf r :name))
    (getf (be-list :saved-views user) :records)))

(test saved-views-type-compiles
  "The hidden built-in :saved-views type is present in the compiled
model in the :secrets shape: built-in, not base, never displayed, not
internal, roles suppressed, gated on the settings type-role, scoped to
the acting user, one owner-stamping hook on each pre-write slot, and the
full update form be-update reads the record through."
  (let ((def (getf *compiled-model* :saved-views)))
    (is-true def)
    (is (eq t (getf def :built-in)))
    (is (null (getf def :base)))
    (is (null (getf def :display)))
    (is (null (getf def :internal)))
    (is-true (getf def :suppress-roles))
    (is (equal '("settings") (getf def :type-roles)))
    (is (eq :user (u:tree-get def :views :main :scope)))
    (dolist (slot '(:pre-create :pre-update))
      (let ((hooks (getf def slot)))
        (is (= 1 (length hooks)) "~a carries one hook" slot)
        (is (eq #'stamp-saved-view-owner (car hooks))
          "~a is the owner-stamping hook" slot)))
    (is (eq t (u:tree-get def :update-form :fields)))))

(test saved-views-identity-index
  "Identity is user + list type + name: one unique index over exactly
those three columns, in field order."
  (is (search
        (format nil "ix_saved_views_identity on saved_views ~
                     (saved_view_user, saved_view_list_type, ~
                     saved_view_name)")
        (u:tree-get *compiled-model* :saved-views
          :create-table-sql :index))))

(test saved-views-not-in-be-types
  ":display nil keeps the type out of be-types for admin and for an
ordinary user alike — no tab, not even an admin's."
  (th-make-user "sv-plain")
  (dolist (user '("admin" "sv-plain"))
    (is-false (find :saved-views (be-types user)
                 :key (lambda (e) (getf e :name)))
      "~a must not see :saved-views in be-types" user)))

(test saved-views-list-carries-query
  "The list form is the client's only read: a be-list record carries
the list type, the name, and the query."
  (th-sv-with-rows
    (let ((id (th-sv-insert :saved-views
                '(:list-type "todos" :name "working"
                  :query "type=todos&sort=-name")
                "admin")))
      (is-true (uuid-p id))
      (let ((record (find id (getf (be-list :saved-views "admin")
                               :records)
                      :key (lambda (r) (getf r :id))
                      :test #'equal)))
        (is-true record)
        (is (equal "todos" (getf record :list-type)))
        (is (equal "working" (getf record :name)))
        (is (equal "type=todos&sort=-name"
              (getf record :query)))))))

(test saved-views-owner-stamped-on-create
  "The pre-create hook stamps the acting user as the owner. A
client-supplied :user naming someone else is overwritten, so a
signed-in user cannot plant a row in another user's bar."
  (let ((user-a "sv-create-a")
        (user-b "sv-create-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (let ((own (th-sv-insert :saved-views
                    '(:list-type "todos" :name "own"
                      :query "type=todos")
                    user-a))
            (planted (th-sv-insert :saved-views
                        `(:user ,user-b :list-type "todos"
                          :name "planted" :query "type=todos")
                        user-a)))
        (is (equal user-a (getf (th-sv-record own user-a) :user)))
        (is (equal user-a
              (getf (th-sv-record planted user-a) :user)))
        (is (null (th-sv-names user-b)))))))

(test saved-views-owner-pinned-on-update
  "On update the hook pins the record's existing owner, whoever acts:
the owner's own update carrying another user's name keeps its owner,
and an admin repair does not take the row over."
  (let ((user-a "sv-pin-a")
        (user-b "sv-pin-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (let ((id (th-sv-insert :saved-views
                  '(:list-type "todos" :name "pinned"
                    :query "type=todos")
                  user-a)))
        (is (equal id (be-update :saved-views id
                        `(:user ,user-b :query "type=todos&q=x")
                        user-a)))
        (is (equal user-a (getf (th-sv-record id user-a) :user)))
        (is (equal id (be-update :saved-views id
                        '(:query "type=todos&sort=-name") "admin")))
        (is (equal user-a (getf (th-sv-record id "admin") :user)))
        (let ((records (getf (be-list :saved-views user-a) :records)))
          (is (= 1 (length records)))
          (is (equal "type=todos&sort=-name"
            (getf (car records) :query))))))))

(test saved-views-owner-scoped-list
  "be-list is scoped to the acting user: each user sees only their own
rows, and even admin's list shows neither — another user's row is
reached by uuid, never by listing."
  (let ((user-a "sv-scope-a")
        (user-b "sv-scope-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (th-sv-insert :saved-views
        '(:list-type "todos" :name "scope-a" :query "type=todos")
        user-a)
      (th-sv-insert :saved-views
        '(:list-type "todos" :name "scope-b" :query "type=todos")
        user-b)
      (is (equal '("scope-a") (th-sv-names user-a)))
      (is (equal '("scope-b") (th-sv-names user-b)))
      (let ((admin-names (th-sv-names "admin")))
        (is-false (member "scope-a" admin-names :test #'equal))
        (is-false (member "scope-b" admin-names :test #'equal))))))

(test saved-views-other-users-row-is-private
  "Another user's row is private on every path. B cannot read or delete
A's row; admin can read it. B's full-plist be-update — values B may
lawfully put on a row of their own — signals the permission error and
leaves A's row exactly as it was. A query-only payload signals too, but
it is not the pin: it stops in field validation before any write, so the
signal alone passes on an engine that writes the row first."
  (let ((user-a "sv-private-a")
        (user-b "sv-private-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (let ((id (th-sv-insert :saved-views
                  '(:list-type "todos" :name "private"
                    :query "type=todos")
                  user-a)))
        (is-false (th-sv-record id user-b))
        (is (null (be-delete :saved-views id user-b)))
        (is-true (th-sv-record id user-a))
        (is-true (th-sv-record id "admin"))
        (let ((e (th-sv-err
                   (be-update :saved-views id
                     `(:user ,user-b :name "hijacked"
                       :list-type "todos" :query "type=todos")
                     user-b))))
          (is (and (stringp e)
                (re:scan "does not have update permission" e))
            "got: ~a" e))
        (let ((e (th-sv-err
                   (be-update :saved-views id
                     '(:query "type=todos&q=2") user-b))))
          (is (and (stringp e)
                (re:scan "does not have update permission" e))
            "got: ~a" e))
        (let ((record (th-sv-record id user-a)))
          (is (equal "private" (getf record :name)))
          (is (equal "todos" (getf record :list-type)))
          (is (equal "type=todos" (getf record :query)))
          (is (equal user-a (getf record :user))))))))

(test saved-views-secrets-other-users-row-is-private
  "The same row check on :secrets — the hole this item closes. Until
be-update row-checked the non-base built-ins, any settings holder could
overwrite another user's secret by uuid. B's full-plist be-update now
signals the permission error and A's secret keeps its value and owner."
  (let ((user-a "sv-secret-a")
        (user-b "sv-secret-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (let ((id (th-sv-insert :secrets
                  '(:name "sv-api-key" :value "sk-original"
                    :description "a's key")
                  user-a)))
        (is-true (uuid-p id))
        (let ((e (th-sv-err
                   (be-update :secrets id
                     `(:user ,user-b :name "sv-api-key"
                       :value "hijacked" :description "b's now")
                     user-b))))
          (is (and (stringp e)
                (re:scan "does not have update permission" e))
            "got: ~a" e))
        (let ((record (getf (be-rec id user-a :type-key :secrets)
                          :record)))
          (is (equal "sk-original" (getf record :value)))
          (is (equal user-a (getf record :user))))))))

(test saved-views-guest-denied
  "The server gate is the type role. The guest account deliberately
lacks settings, so it can neither insert nor list."
  (is-false (member "settings"
              (a:list-user-role-names *rbac* "guest")
              :test #'string=))
  (signals validation-error
    (be-insert :saved-views
      '(:list-type "todos" :name "g" :query "type=todos") "guest"))
  (signals validation-error (be-list :saved-views "guest")))

(test saved-views-same-name-other-list
  "Identity is per list type: one user, one name, two list types — two
rows."
  (let ((user "sv-two-lists"))
    (th-make-user user)
    (th-sv-with-rows
      (let ((id-todos (th-sv-insert :saved-views
                        '(:list-type "todos" :name "active"
                          :query "type=todos")
                        user))
            (id-tags (th-sv-insert :saved-views
                       '(:list-type "tags" :name "active"
                         :query "type=tags")
                       user)))
        (is-true (uuid-p id-todos))
        (is-true (uuid-p id-tags))
        (is (not (equal id-todos id-tags)))
        (is (= 2 (length (th-sv-names user))))))))

(test saved-views-same-name-other-user
  "Identity is per user: two users, one name, one list type — two rows."
  (let ((user-a "sv-two-users-a")
        (user-b "sv-two-users-b"))
    (th-make-user user-a)
    (th-make-user user-b)
    (th-sv-with-rows
      (let ((id-a (th-sv-insert :saved-views
                    '(:list-type "todos" :name "mine"
                      :query "type=todos")
                    user-a))
            (id-b (th-sv-insert :saved-views
                     '(:list-type "todos" :name "mine"
                       :query "type=todos")
                     user-b)))
        (is-true (uuid-p id-a))
        (is-true (uuid-p id-b))
        (is (not (equal id-a id-b)))))))

(test saved-views-identical-insert-dedups
  "The same payload twice is one row: the second insert returns the
first row's id and inserted nil (id-from-data matches every column)."
  (let ((user "sv-dedup"))
    (th-make-user user)
    (th-sv-with-rows
      (multiple-value-bind (id-1 inserted-1)
          (th-sv-insert :saved-views
            '(:list-type "todos" :name "once" :query "type=todos")
            user)
        (multiple-value-bind (id-2 inserted-2)
            (th-sv-insert :saved-views
              '(:list-type "todos" :name "once" :query "type=todos")
              user)
          (is-true inserted-1)
          (is (null inserted-2))
          (is (equal id-1 id-2))
          (is (= 1 (length (th-sv-names user)))))))))

(test saved-views-duplicate-name-different-query-errors
  "The same user, list type, and name with a different query misses the
lookup and hits the unique index: the insert signals and the first row's
query is unchanged. This is why the client's Replace must be an update by
id, never an insert."
  (let ((user "sv-duplicate"))
    (th-make-user user)
    (th-sv-with-rows
      (let ((id (th-sv-insert :saved-views
                  '(:list-type "todos" :name "working"
                    :query "type=todos")
                  user)))
        (signals error
          (be-insert :saved-views
            '(:list-type "todos" :name "working"
              :query "type=todos&sort=-name")
            user))
        (is (equal "type=todos" (getf (th-sv-record id user) :query)))
        (is (= 1 (length (th-sv-names user))))))))

(test saved-views-replace-is-update
  "Replace is a client-side update by id: the owner's query-only
payload — exactly what the client sends — returns the id, and the row
keeps its name, list type, and owner. Those come back from the record
through the full update form; without it the absent fields would be
filled from autofill and default instead of the stored columns."
  (let ((user "sv-replace"))
    (th-make-user user)
    (th-sv-with-rows
      (let ((id (th-sv-insert :saved-views
                  '(:list-type "todos" :name "working"
                    :query "type=todos")
                  user)))
        (is (equal id (be-update :saved-views id
                        '(:query "type=todos&sort=-name") user)))
        (let ((record (th-sv-record id user)))
          (is (equal "type=todos&sort=-name" (getf record :query)))
          (is (equal "working" (getf record :name)))
          (is (equal "todos" (getf record :list-type)))
          (is (equal user (getf record :user))))))))

(test saved-views-validation
  "An empty name, a name over 80 characters, a query over 8000
characters, and a missing list type each signal a validation error and
leave no row."
  (let ((user "sv-validation"))
    (th-make-user user)
    (th-sv-with-rows
      (flet ((rejected (data)
               (signals validation-error
                 (th-sv-insert :saved-views data user))))
        (rejected '(:list-type "todos" :name "" :query "type=todos"))
        (rejected (list :list-type "todos"
                        :name (make-string 81 :initial-element #\n)
                        :query "type=todos"))
        (rejected (list :list-type "todos" :name "long"
                        :query (format nil "type=todos&q=~a"
                                 (make-string 8000
                                   :initial-element #\x))))
        (rejected '(:name "no-list-type" :query "type=todos")))
      (is (null (th-sv-names user))))))
