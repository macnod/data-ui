(in-package :data-ui)

(def-suite type-roles-override-suite
  :description ":type-roles override on redeclared base-model
types: compile probes (replace-not-union, partial overlay,
allowlist, internal rejection, non-empty list, no mutation) and
behavioral RBAC checks on the type-roles-override-test fixture")

(def-suite type-roles-override-compile-tests
  :in type-roles-override-suite
  :description "Pure compile probes (no model, no DB)")

(def-suite type-roles-override-behavior
  :in type-roles-override-suite
  :description "be-types / be-list RBAC behavior on the
type-roles-override-test fixture")

(in-suite type-roles-override-compile-tests)

;;; --- Helpers ---

(defun tro-item-type ()
  "Ordinary author type for the compile probes."
  (list :items
    (list :table t :create :auto :update :auto :delete :auto
      :display t
      :type-roles '("item-users")
      :fields (list
                :name (list :type :text :identity t
                        :ui (list :label "Name" :widget :textbox)
                        :source (list :view :main :column :name
                                  :agg :first)
                        :column t :not-null t :unique t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun tro-compile (&rest base-overrides)
  "Compile a model of one user type plus BASE-OVERRIDES (base-key
def pairs). Returns the compiled model. Pure compile probe, no
DB. compile-model itself prepends *base-model* via the merge."
  (compile-model
    (append (tro-item-type) base-overrides)))

(defun tro-type-names (user)
  "Type keys from (be-types USER), as a list of keywords.
be-types returns plists — search with (getf entry :name)."
  (loop for entry in (be-types user)
    collect (getf entry :name)))

(defun tro-seed-fixture ()
  "Seed the type-roles-override-test fixture: one non-admin user
with only the item-users role."
  (th-make-user "tro-user" :roles '("item-users"))
  nil)

;;; --- Compile: defaults and override ---

(test tro-no-override-keeps-base-defaults
  "Without an override, compiled :users / :roles / :permissions
carry the base defaults."
  (let ((m (tro-compile)))
    (is (equal '("logged-in" "user-creator")
           (getf (getf m :users) :type-roles)))
    (is (equal '("logged-in" "role-creator")
           (getf (getf m :roles) :type-roles)))
    (is (equal '("logged-in" "permission-creator")
           (getf (getf m :permissions) :type-roles)))
    (is (equal '("settings")
           (getf (getf m :settings) :type-roles)))))

(test tro-users-override-replaces
  ":users (:type-roles (\"admin\")) compiles with exactly that
list — replace, not union."
  (let ((m (tro-compile
             :users (list :type-roles '("admin")))))
    (is (equal '("admin") (getf (getf m :users) :type-roles)))))

(test tro-roles-override-replaces
  "Same for :roles, independently."
  (let ((m (tro-compile
             :roles (list :type-roles '("admin")))))
    (is (equal '("admin") (getf (getf m :roles) :type-roles)))
    (is (equal '("logged-in" "user-creator")
           (getf (getf m :users) :type-roles)))))

(test tro-permissions-override-replaces
  "Same for :permissions, independently."
  (let ((m (tro-compile
             :permissions (list :type-roles '("admin")))))
    (is (equal '("admin") (getf (getf m :permissions) :type-roles)))))

(test tro-permission-form-override
  "The (role-name perm…) form compiles verbatim."
  (let ((m (tro-compile
             :users (list :type-roles '(("staff" :read))))))
    (is (equal '(("staff" :read))
           (getf (getf m :users) :type-roles)))))

(test tro-overlay-keeps-base-shape
  "The overlay is partial: :users fields, views, and the RBAC
create function survive the override."
  (let* ((m (tro-compile
              :users (list :type-roles '("admin"))))
          (users (getf m :users))
          (field-keys (u:plist-keys (getf users :fields))))
    ;; name / password / email / roles plus compiled defaults
    (is (member :name field-keys))
    (is (member :password field-keys))
    (is (member :email field-keys))
    (is (member :roles field-keys))
    (is (functionp (getf users :create)))
    (is (u:tree-get users :views :main :tables))))

(test tro-user-type-still-present
  "User-only types compile alongside the override."
  (let ((m (tro-compile
             :users (list :type-roles '("admin")))))
    (is (getf m :items))
    (is (equal '("item-users")
           (getf (getf m :items) :type-roles)))))

;;; --- Compile: rejection ---

(test tro-reject-extra-keys
  "Any key other than :type-roles on a redeclaration is a
compile error."
  (signals error
    (tro-compile :users (list :type-roles '("admin")
                          :display nil))))

(test tro-reject-internal-override
  "Redeclaring an :internal base type is a compile error."
  (signals error
    (tro-compile :resources (list :type-roles '("admin")))))

(test tro-reject-empty-roles
  ":type-roles nil / () on a redeclaration is a compile error."
  (signals error
    (tro-compile :users (list :type-roles nil))))

(test tro-reject-roles-string
  "A bare string (not a list) as the override value is a compile
error."
  (signals error
    (tro-compile :users (list :type-roles "admin"))))

(test tro-reject-empty-def
  "Redeclaring a base type with an empty plist is a compile
error (why is the type even there?)."
  (signals error
    (tro-compile :users '())))

(test tro-reject-nil-def
  "Redeclaring a base type with a nil def is a compile error."
  (signals error
    (tro-compile :users nil)))

(test tro-reject-missing-type-roles
  "A redeclaration without :type-roles is a compile error."
  (signals error
    (tro-compile :users (list :display t))))

;;; --- Compile: purity ---

(test tro-no-base-model-mutation
  "Compiling with an override does not mutate *base-model*: a
later compile without the override sees the base defaults
again."
  (tro-compile :users (list :type-roles '("admin")))
  (is (equal '("logged-in" "user-creator")
         (getf (getf *base-model* :users) :type-roles)))
  (let ((m (tro-compile)))
    (is (equal '("logged-in" "user-creator")
           (getf (getf m :users) :type-roles)))))

;;; --- Behavioral (fixture + with-model) ---

(in-suite type-roles-override-behavior)

(test tro-admin-sees-restricted-types
  "Admin sees :users / :roles / :permissions in be-types on the
restricted fixture."
  (let ((names (tro-type-names "admin")))
    (is (member :users names))
    (is (member :roles names))
    (is (member :permissions names))))

(test tro-non-admin-does-not-see-restricted-types
  "A non-admin with only item-users does not see :users /
:roles / :permissions."
  (let ((names (tro-type-names "tro-user")))
    (is-false (member :users names))
    (is-false (member :roles names))
    (is-false (member :permissions names))))

(test tro-non-admin-still-sees-items-and-settings
  "The non-admin still sees :items; admin still sees :settings
(not restricted by the fixture)."
  (let ((names (tro-type-names "tro-user")))
    (is (member :items names)))
  (is (member :settings (tro-type-names "admin"))))

(test tro-non-admin-cannot-list-users
  "Non-admin be-list :users signals a permission error."
  (signals error
    (be-list :users "tro-user")))

(test tro-admin-can-list-users
  "Admin be-list :users succeeds on the restricted fixture."
  (is (listp (getf (be-list :users "admin") :records))))
