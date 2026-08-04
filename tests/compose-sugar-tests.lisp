(in-package :data-ui)

(def-suite compose-sugar-unit-suite
  :description ":compose sugar compile-error tests (no model needed)")

(def-suite compose-sugar-suite
  :description "Field-level :compose sugar behavioral tests")

(in-suite compose-sugar-unit-suite)

;;; ---------------------------------------------------------------------------
;;; Layer 3: Field-level :compose sugar (compile-error tests run bare)
;;; ---------------------------------------------------------------------------

(test sugar-unknown-placeholder-errors
  "Unknown placeholder in :compose template signals compile error."
  (signals error
    (let ((bad-model
            '(:authors
               (:table t
                 :fields
                 (:name (:type :text :identity t
                          :compose ":first-name :bogus"
                          :column t)
                  :first-name (:type :text :column t))))))
      (stage-1 bad-model))))

(test sugar-self-reference-errors
  "Self-reference in :compose template signals compile error."
  (signals error
    (let ((bad-model
            '(:authors
               (:table t
                 :fields
                 (:name (:type :text :identity t
                          :compose ":name :first-name"
                          :column t)
                  :first-name (:type :text :column t))))))
      (stage-1 bad-model))))

;;; ---------------------------------------------------------------------------
;;; Layer 3: Behavioral tests (run inside shared compose-sugar-test context)
;;; ---------------------------------------------------------------------------

(in-suite compose-sugar-suite)

(test sugar-model-compiles
  "A model with field-level :compose compiles successfully."
  (is-true (u:tree-get *compiled-model* :authors :pre-create))
  (is-true (u:tree-get *compiled-model* :authors :pre-update)))

(test sugar-insert-stores-composed-name
  "Field :compose produces the same insert behavior as type-level hooks."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Donald Roy Cameron")))
    (be-delete :authors id "admin")))

(test sugar-update-recomposes
  "Field :compose recomposes on update."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (be-update :authors id
      '(:first-name "Don" :last-name "Cameron")
      "admin")
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Don Cameron")))
    (be-delete :authors id "admin")))
