(in-package :data-ui)

(def-suite allowed-values-suite
  :description "allowed-values palette suppression: a permission
error on one field's palette query must not fail the parent list
or /api/item; the palette is returned empty (key present, value
nil) and the denial is logged.")

(in-suite allowed-values-suite)

(defun av-seed-fixture ()
  "Seed the allowed-values-test fixture: one non-admin user
(roles: logged-in only) and one admin-created :items row whose
resource carries the logged-in role (so the non-admin can read
that row for the be-rec test)."
  (th-make-user "av-user" :roles '())
  (be-insert :items '(:name "av-item-1") "admin"
    :roles '("logged-in"))
  nil)

;;; ---------------------------------------------------------------------------
;;; Fixture (seeded via av-seed-fixture in tests/helpers.lisp):
;;; one non-admin (av-user, roles logged-in) and one admin-owned
;;; :items row shared with logged-in for the be-rec test.
;;; ---------------------------------------------------------------------------

(test av-non-admin-list-survives
  "The regression the item names: a non-admin be-list on a type
with a :target :users field must not signal when :users is
admin-only. The result carries :records and an :allowed-values
sub-plist with a :user entry."
  (let ((result (be-list :items "av-user")))
    (is (listp (getf result :records)))
    (is (listp (getf result :allowed-values)))
    (is (member :user (getf result :allowed-values)))))

(test av-denied-palette-key-present-value-nil
  "For the non-admin, the :user palette key is present with value
NIL (empty palette, not a missing key); for admin the same getf
is a list of usernames."
  (let ((non-admin (getf (getf (be-list :items "av-user")
                          :allowed-values)
                    :user))
        (admin (getf (getf (be-list :items "admin")
                      :allowed-values)
                :user)))
    (is-true (member :user (getf (be-list :items "av-user")
                            :allowed-values)))
    (is-false non-admin)
    (is (listp admin))
    (is (member "admin" admin :test 'equal))))

(test av-direct-be-list-still-signals
  "The permission check itself is intact: a direct be-list on
the restricted :users type by the non-admin still signals a
validation error."
  (signals validation-error
    (be-list :users "av-user")))

(test av-item-endpoint-survives
  "The /api/item path (rec, via be-rec) is fixed for free: the
non-admin can read a genuinely-readable row (shared via the
logged-in resource role) and receives the record with an empty
:user palette — record delivered, key present, value nil."
  (let* ((id (be-id :items
              `((:items :name :eq "av-item-1")) "admin"))
          (result (be-rec id "av-user" :type-key :items)))
    (is-true result "record must be delivered, not invisible")
    (is (equal :items (getf result :type)))
    (is (listp (getf result :allowed-values)))
    (is (member :user (getf result :allowed-values)))
    (is-false (getf (getf result :allowed-values) :user))))

(test av-options-field-unaffected
  "The :ui :options branch (no DB access) is untouched: the
:rating palette comes back the literal list for both users."
  (let ((non-admin (getf (getf (be-list :items "av-user")
                          :allowed-values)
                    :rating))
        (admin (getf (getf (be-list :items "admin")
                      :allowed-values)
                :rating)))
    (is (equal '("1" "2" "3") non-admin))
    (is (equal '("1" "2" "3") admin))))

(test av-rating-palette-is-literal-strings
  "Pin the palette shape and two fixture surprises that shaped this
fixture. (1) A :ui :options field delivers its literals verbatim —
string options arrive as strings, so :type :text with string
options is the established shape (an :integer field with string
options fails v-options once full-data fills the default). (2) An
insert that omits the field must succeed via :default — the
omitted value reaches db-value as nil, which the driver binds as
SQL false; Postgres rejects that on a text column too, so the
:default is load-bearing."
  (let* ((values (allowed-values :items "av-user"))
         (rating (getf values :rating))
         (id (be-insert :items '(:name "av-item-2") "admin"
                :roles '("logged-in"))))
    (unwind-protect
      (progn
        (is (equal '("1" "2" "3") rating))
        (is-true id "insert omitting :rating must succeed via :default"))
      (when id (be-delete :items id "admin")))))
