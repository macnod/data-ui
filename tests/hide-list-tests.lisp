(in-package :data-ui)

(def-suite hide-list-compile
  :description ":hide-list compile acceptance / rejection probes")

(def-suite hide-list-wire
  :description ":hide-list wire emission and backend reads on
hide-list-test")

(defun hl-type (form-plists)
  "A minimal table type whose forms are FORM-PLISTS (a plist of
form-key → form-def, spliced into the type-def)."
  (list :table t
    :create :auto :update :auto :delete :auto
    :type-roles '("hl-users")
    :views (list :main (list :tables '(:wt)))
    :fields (list
              :title (list :type :text :identity t
                        :ui (list :label "Title" :widget :textbox)
                        :source (list :view :main :column :title
                                  :agg :first)
                        :column t :not-null t))
    :list-form (or (getf form-plists :list-form) '(:fields t))
    :add-form (or (getf form-plists :add-form) '(:fields t))
    :update-form (or (getf form-plists :update-form) '(:fields t))
    :view-form (or (getf form-plists :view-form) '(:fields t))))

(defun hl-compile (form-plists)
  "Compile a one-type model built by HL-TYPE; return the compiled
type-def."
  (getf (compile-model (list :wt (hl-type form-plists))) :wt))

(defun hl-with (base overrides)
  "Merge OVERRIDES into BASE: replaced keys keep their position,
new keys append. No duplicate keys (the ru-with pattern)."
  (loop with replaced = (u:plist-keys overrides)
    for k in base by #'cddr
    for v in (cdr base) by #'cddr
    when (member k replaced)
    append (list k (getf overrides k)) into merged
    else
    append (list k v) into merged
    finally
    (return
      (append merged
        (loop for k in overrides by #'cddr
          unless (member k (u:plist-keys merged))
          append (list k (getf overrides k)))))))

(defun hl-model (types)
  "Compile a model of TYPES (a plist of type-key → type-def);
:wt-notes is the fact table the valid rollup counts."
  (compile-model
    (append
      (list
        :wt-notes (list :table t
                    :create :auto :update :auto :delete :auto
                    :type-roles '("hl-users")
                    :views (list :main (list :tables '(:wt-notes :wt)))
                    :fields (list
                              :note (list :type :text
                                      :ui (list :label "Note"
                                            :widget :textbox)
                                      :source (list :view :main
                                                :column :note
                                                :agg :first)
                                      :column t :not-null t)
                              :wt (list :type :text
                                    :ui (list :label "WT"
                                          :widget :textbox)
                                    :target :wt
                                    :source (list :view :main
                                              :table :wt
                                              :column :title
                                              :agg :first)
                                    :column t :not-null t))
                    :list-form '(:fields t)
                    :update-form '(:fields t)
                    :add-form '(:fields t)))
      types)))

(defun hl-valid-rollup ()
  "A known-valid rollup spec: grain :wt, one :count measure
reading from the :wt-notes fact table (a real measure never
reads from the grain). Proven valid by the positive-control
test before any :hide-list variant is compiled."
  (list
    :rollup t
    :grain :wt
    :type-roles '("hl-viewers")
    :views (list :main (list :tables '(:wt :wt-notes)))
    :list-form (list :fields t)
    :fields (list
              (list :title
                (list :source (list :view :main :table :wt
                                :column :title :agg :first)
                  :ui (list :label "Title")))
              (list :total
                (list :type :integer
                  :source (list :view :main :table :wt-notes
                              :column :id :agg :count)
                  :ui (list :label "Total"))))))

(in-suite hide-list-compile)

(test form-keys-closed-set
  "*form-keys* holds exactly :fields and :hide-list."
  (is (equal '(:fields :hide-list) *form-keys*)))

(test hide-list-t-compiles-on-record-forms
  ":hide-list t compiles on each record form and the compiled
form carries it (key membership + value)."
  (dolist (form '(:view-form :update-form :add-form))
    (let ((compiled (hl-compile (list form '(:fields t :hide-list t)))))
      (let ((form-def (getf compiled form)))
        (is (member :hide-list (u:plist-keys form-def)))
        (is-true (getf form-def :hide-list))))))

(test hide-list-present-nil-compiles
  ":hide-list nil compiles with key membership and value nil —
the survival pin (an explicit show is present, not absent)."
  (let* ((compiled (hl-compile '(:update-form (:fields t :hide-list nil))))
          (form-def (getf compiled :update-form)))
    (is (member :hide-list (u:plist-keys form-def)))
    (is-false (getf form-def :hide-list))))

(test hide-list-omitted-compiles-absent
  "An omitted :hide-list compiles with no key membership."
  (let ((compiled (hl-compile nil)))
    (dolist (form '(:list-form :add-form :update-form :view-form))
      (is-false (member :hide-list (u:plist-keys (getf compiled form)))))))

(test hide-list-on-list-form-signals-t
  ":hide-list t on :list-form signals valid-hide-list's
:list-form message."
  (error-matches
    (compile-model (list :wt (hl-type '(:list-form (:fields t
                                                        :hide-list t)))))
    "hide-list is not legal on :list-form"
    ":hide-list t on :list-form must signal"))

(test hide-list-on-list-form-signals-nil
  "Present-nil :hide-list on :list-form signals too (key
membership on plist keys — even nil is a declaration on the form
that is the list)."
  (error-matches
    (compile-model (list :wt (hl-type '(:list-form (:fields t
                                                        :hide-list nil)))))
    "hide-list is not legal on :list-form"
    ":hide-list nil on :list-form must signal"))

(test hide-list-bad-value-signals-string
  ":hide-list \"yes\" signals the t-or-nil message."
  (error-matches
    (compile-model (list :wt (hl-type '(:view-form (:fields t
                                                        :hide-list "yes")))))
    ":hide-list on :VIEW-FORM of type :WT must be t or nil"
    ":hide-list \"yes\" must signal the value error"))

(test hide-list-bad-value-signals-keyword
  ":hide-list :true signals the t-or-nil message (keywords are
not booleans in the model vocabulary)."
  (error-matches
    (compile-model (list :wt (hl-type '(:add-form (:fields t
                                                        :hide-list :true)))))
    ":hide-list on :ADD-FORM of type :WT must be t or nil"
    ":hide-list :true must signal the value error"))

(test hide-list-unknown-form-key-signals
  "A form key outside *form-keys* signals and the message names
the key (closed set)."
  (error-matches
    (compile-model (list :wt (hl-type '(:view-form (:fields t
                                                        :hidden t)))))
    "Unknown key :HIDDEN on :VIEW-FORM"
    "an unknown form key must signal naming the key"))

(test hide-list-non-plist-view-form-t-signals
  ":view-form t signals — probed live: valid-form-fields runs
first on final-def and its (tree-get type-def :view-form :fields)
dies inside u:tree-get with the generic \"Expected a plist.\"
before valid-hide-list ever sees the shape. Pin that class."
  (error-matches
    (compile-model (list :wt (hl-type '(:view-form t))))
    "Expected a plist"
    ":view-form t must signal a shape error"))

(test hide-list-reserved-field-name-signals-base
  "A hide-list author field on a base type signals the
valid-field-names reservation message."
  (error-matches
    (compile-model
      (list
        :wt (list :table t
              :create :auto :update :auto :delete :auto
              :type-roles '("hl-users")
              :views (list :main (list :tables '(:wt)))
              :fields (list
                        :title (list :type :text :identity t
                                 :ui (list :label "Title"
                                       :widget :textbox)
                                 :source (list :view :main
                                           :column :title
                                           :agg :first)
                                 :column t :not-null t)
                        :hide-list (list :type :text
                                     :ui (list :label "Hide List"
                                           :widget :textbox)
                                     :source (list :view :main
                                               :column :hide-list
                                               :agg :first)
                                     :column t))
              :list-form '(:fields t)
              :add-form '(:fields t)
              :update-form '(:fields t)
              :view-form '(:fields t))))
    "Field key :HIDE-LIST on type :WT is reserved"
    "a hide-list author field must signal the reservation error"))

(test hide-list-rollup-positive-control
  "The known-valid rollup compiles clean — the positive control
every :hide-list rollup variant is built from."
  (is (eq :measure
        (getf (getf (hl-model (list :wt (hl-type nil)
                                 :board (hl-valid-rollup)))
                :board)
          :phase-a-shape))))

(test hide-list-rollup-list-form-signals
  ":hide-list t inside a rollup's :list-form signals
valid-hide-list's :list-form message — on the rollup path this
validator is the only :list-form enforcement (the matrix permits
extra keys inside :list-form)."
  (error-matches
    (hl-model
      (list :wt (hl-type nil)
        :board (hl-with (hl-valid-rollup)
                  (list :list-form '(:fields t :hide-list t)))))
    "hide-list is not legal on :list-form"
    ":hide-list t inside a rollup :list-form must signal"))

(test hide-list-rollup-reserved-field-signals
  "A hide-list author field on a rollup signals the reservation
message through valid-rollup-fields → valid-field-names."
  (error-matches
    (hl-model
      (list
        :wt (hl-type nil)
        :board (hl-with (hl-valid-rollup)
                  (list
                    :fields (append (getf (hl-valid-rollup) :fields)
                              (list
                                (list :hide-list
                                  (list :type :text
                                    :source (list :view :main
                                            :table :wt
                                            :column :title
                                            :agg :first)
                                    :ui (list :label "Hide")))))))))
    "Field key :HIDE-LIST on type :BOARD is reserved"
    "a hide-list rollup field must signal the reservation error"))

(in-suite hide-list-wire)

(defun hl-seed-fixture ()
  "Seed the hide-list-test fixture: 21 posts, 1 draft (by
be-insert as admin; set-model seeds nothing). Returns a plist of
the inserted ids."
  (let ((post-ids
          (loop for a from 1 to 21
            for title = (format nil "Post ~2,'0d" a)
            collect (be-insert :posts
                        (list :title title
                          :body (format nil "Body of ~a" title))
                        "admin")))
        (draft-id (be-insert :drafts '(:title "Draft 1"
                                         :body "Draft body")
                    "admin")))
    (list :posts post-ids :draft draft-id)))

(test hl-seed-sanity
  "Seeding sanity: posts total 21 (20 on page one — pagination
exists to be hidden), drafts total 1."
  (let* ((posts (be-list :posts "admin"))
          (drafts (be-list :drafts "admin")))
    (is (= 21 (getf posts :total)))
    (is (= 20 (length (getf posts :records))))
    (is (= 1 (getf drafts :total)))))

(test hl-fe-fields-emits-declared-keys
  "fe-fields of :posts emits :hide-list t on :view-form /
:add-form (with :title still present beside it), key membership +
nil value on :update-form, nothing on :list-form."
  (let* ((fields (fe-fields :posts "admin"))
          (view (getf fields :view-form))
          (add (getf fields :add-form))
          (update (getf fields :update-form))
          (list (getf fields :list-form)))
    (is-true (getf view :hide-list))
    (is (member :title (u:plist-keys view)))
    (is-true (getf add :hide-list))
    (is (member :title (u:plist-keys add)))
    (is (member :hide-list (u:plist-keys update)))
    (is-false (getf update :hide-list))
    (is (member :title (u:plist-keys update)))
    (is-false (member :hide-list (u:plist-keys list)))
    ;; The hide-list entry rides the form object as a sibling of
    ;; the field entries, not as a field def: value t, not a plist.
    (is-true (equal t (getf view :hide-list)))))

(test hl-fe-fields-json-true
  "The fe-fields JSON carries \"hide-list\":true on the declared
forms (the JSON boolean registration)."
  (let ((json (plist-to-json (fe-fields :posts "admin"))))
    (is (search "\"hide-list\":true" json))))

(test hl-json-boolean-nil-serialization
  "(plist-to-json '(:hide-list nil)) carries \"hide-list\":false
and does not contain the [] nil encoding anywhere for the key."
  (let ((json (plist-to-json (list :hide-list nil))))
    (is (search "\"hide-list\":false" json))
    (is-false (search "hide-list\":\\[\\]" json))))

(test hl-be-list-serves-flag
  "The /api/list contract: list-result's fe-fields splice carries
the flag. be-list defaults to the list form (no :hide-list
there); the emitted :view-form / :update-form / :add-form keys of
the result carry it."
  (let* ((result (be-list :posts "admin"))
          (fields (list :view-form (getf result :view-form)
                        :update-form (getf result :update-form)
                        :add-form (getf result :add-form))))
    (is-true (getf fields :view-form))
    (is (search "\"hide-list\":true"
          (plist-to-json
            (list :view-form (getf result :view-form)
              :update-form (getf result :update-form)
              :add-form (getf result :add-form)))))))

(test hl-be-rec-serves-flag
  "The /api/item contract: be-rec's :fields is fe-fields
verbatim; a :update-form item read carries (:hide-list nil …) —
explicit nil survives the pipeline."
  (let* ((post-id (first (getf *fixture* :posts)))
          (fields (getf (be-rec post-id "admin"
                          :form :update-form :type-key :posts)
                   :fields))
          (update (getf fields :update-form)))
    (is (member :hide-list (u:plist-keys update)))
    (is-false (getf update :hide-list))
    (is (search "\"hide-list\":false" (plist-to-json fields)))))

(test hl-drafts-byte-safety
  ":drafts declares no :hide-list: its fe-fields JSON is free of
the string on every form (the byte-identical payload claim)."
  (is-false
    (search "hide-list"
      (plist-to-json (fe-fields :drafts "admin")))))

(test hl-augment-update-form-preserves-t
  "augment-update-form appends the status key only when the button
field is in the update-form field list; the sibling :hide-list t
survives the add-to-plist rewrite of the :update-form entry."
  (let* ((fields (list
                   :title (list :type :text :identity t
                            :ui (list :label "Title"
                                  :widget :textbox)
                            :source (list :view :main :column :title
                                      :agg :first)
                            :column t :not-null t)
                   :go (list :type :button
                         :ui (list :label "Go" :widget :button)
                         :action '(:test-sync))))
          (type-def (list :fields fields
                      :update-form '(:fields (:title :go)
                                      :hide-list t)))
          (augmented (augment-update-form type-def fields))
          (update (getf augmented :update-form)))
    (is (equal '(:title :go :go-status) (getf update :fields)))
    (is (member :hide-list (u:plist-keys update)))
    (is-true (getf update :hide-list))))

(test hl-augment-update-form-preserves-nil
  "The present-nil variant: :fields gains the status key and
:hide-list stays membership-true with value nil."
  (let* ((fields (list
                   :title (list :type :text :identity t
                            :ui (list :label "Title"
                                  :widget :textbox)
                            :source (list :view :main :column :title
                                      :agg :first)
                            :column t :not-null t)
                   :go (list :type :button
                         :ui (list :label "Go" :widget :button)
                         :action '(:test-sync))))
          (type-def (list :fields fields
                      :update-form '(:fields (:title :go)
                                      :hide-list nil)))
          (augmented (augment-update-form type-def fields))
          (update (getf augmented :update-form)))
    (is (equal '(:title :go :go-status) (getf update :fields)))
    (is (member :hide-list (u:plist-keys update)))
    (is-false (getf update :hide-list))))
