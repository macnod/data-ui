(in-package :data-ui)

(def-suite compose-unit-suite
  :description "Compose data-effect and format unit tests (no model needed)")

(def-suite compose-suite
  :description "Compose-string behavioral tests (shared compose-test context)")

(in-suite compose-unit-suite)

;;; ---------------------------------------------------------------------------
;;; Layer 1: Data-effect contract (merge, run-lifecycle-hooks return)
;;; ---------------------------------------------------------------------------
;;; These are pure unit tests — no model or database needed.

(test merge-data-effect-basic
  "merge-data-effect copies effect keys into current, overwriting."
  (let ((merged (merge-data-effect '(:a 1 :b 2) '(:b 3 :c 4))))
    (is (= (getf merged :a) 1))
    (is (= (getf merged :b) 3))
    (is (= (getf merged :c) 4))))

(test merge-data-effect-nil-value
  "merge-data-effect treats nil as an intentional value (unlike add-to-plist)."
  (let ((merged (merge-data-effect '(:a 1 :b 2) '(:b nil))))
    (is (= (getf merged :a) 1))
    (is (null (getf merged :b)))))

(test merge-data-effect-empty-current
  "merge-data-effect works when current is nil."
  (let ((merged (merge-data-effect nil '(:a 1))))
    (is (= (getf merged :a) 1))))

(test run-lifecycle-hooks-nil-return
  "Hook returning nil leaves data unchanged."
  (let ((hooks (list (lambda (type-key data user &key id roles record)
                       (declare (ignore type-key user id roles record))
                       nil))))
    (is (equal '(:name "test")
               (run-lifecycle-hooks hooks :todos '(:name "test") "admin")))))

(test run-lifecycle-hooks-plist-return
  "Hook returning a plist merges into data."
  (let ((hooks (list (lambda (type-key data user &key id roles record)
                       (declare (ignore type-key user id roles record))
                       (list :computed "hello")))))
    (let ((result (run-lifecycle-hooks hooks :todos '(:name "test") "admin")))
      (is (string= (getf result :name) "test"))
      (is (string= (getf result :computed) "hello")))))

(test run-lifecycle-hooks-non-plist-errors
  "Hook returning a non-plist non-nil value signals report-e."
  (let ((hooks (list (lambda (type-key data user &key id roles record)
                       (declare (ignore type-key user id roles record))
                       'not-a-plist))))
    (signals error
      (run-lifecycle-hooks hooks :todos '(:name "test") "admin"))))

(test run-lifecycle-hooks-chained-merge
  "Multiple hooks: later hooks overwrite earlier ones."
  (let ((hooks (list
                 (lambda (tk d u &key id roles record)
                   (declare (ignore tk d u id roles record))
                   (list :x "first"))
                 (lambda (tk d u &key id roles record)
                   (declare (ignore tk d u id roles record))
                   (list :x "second")))))
    (let ((result (run-lifecycle-hooks hooks :todos nil "admin")))
      (is (string= (getf result :x) "second")))))

;;; ---------------------------------------------------------------------------
;;; Layer 2: compose-string-apply unit tests (no model needed)
;;; ---------------------------------------------------------------------------

(test compose-full-name
  "Full name with all parts present."
  (is (string=
        (compose-string-apply ":first-name :middle-name :last-name"
          '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron"))
        "Donald Roy Cameron")))

(test compose-empty-middle
  "Empty middle name collapses to single spaces."
  (is (string=
        (compose-string-apply ":first-name :middle-name :last-name"
          '(:first-name "Donald" :middle-name nil :last-name "Cameron"))
        "Donald Cameron")))

(test compose-all-empty
  "All parts empty yields empty string."
  (is (string=
        (compose-string-apply ":first-name :middle-name :last-name"
          '(:first-name nil :middle-name nil :last-name nil))
        "")))

(test compose-whitespace-collapse
  "Extra internal whitespace is collapsed."
  (is (string=
        (compose-string-apply ":a :b :c"
          '(:a "x" :b "y" :c "z"))
        "x y z")))

(test compose-placeholders-extracted
  "compose-string-placeholders extracts field keys from format."
  (is (equal '(:first-name :middle-name :last-name)
             (compose-string-placeholders
               ":first-name :middle-name :last-name"))))

;;; ---------------------------------------------------------------------------
;;; Layer 2: Compile-time validation (no model needed)
;;; ---------------------------------------------------------------------------

(test compose-missing-format-errors
  "Missing :format parameter signals validation error."
  (signals error
    (resolve-hook-form '(:compose-string :into :name)
      :kind :lifecycle :type-key :authors)))

(test compose-non-string-format-errors
  "Non-string :format parameter signals validation error."
  (signals error
    (resolve-hook-form '(:compose-string :format 123 :into :name)
      :kind :lifecycle :type-key :authors)))

;;; ---------------------------------------------------------------------------
;;; Layer 1+2: Behavioral tests (run inside shared compose-test context)
;;; ---------------------------------------------------------------------------

(in-suite compose-suite)

(test pre-create-fills-required-before-validate
  "A pre-create hook can fill a required field that the client omitted."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (is-true id)
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Donald Roy Cameron")))
    (be-delete :authors id "admin")))

(test pre-update-recomposes-on-edit
  "Updating first-name recomposes :name."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (be-update :authors id
      '(:first-name "Don" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Don Roy Cameron")))
    (be-delete :authors id "admin")))

(test compose-unknown-placeholder-errors
  "Unknown placeholder in :compose-string format signals compile error."
  (signals error
    (let* ((authors-def (getf *base-model* :authors))
           (bad-model
             (list :authors
                   (add-to-plist authors-def
                     (list :pre-create
                           '((:compose-string
                               :format ":first-name :bogus-field"
                               :into :name)))))))
      (compile-lifecycle-hooks bad-model :authors))))

(test compose-unknown-into-errors
  "Unknown :into field signals compile error."
  (signals error
    (let* ((authors-def (getf *base-model* :authors))
           (bad-model
             (list :authors
                   (add-to-plist authors-def
                     (list :pre-create
                           '((:compose-string
                               :format ":first-name"
                               :into :nonexistent)))))))
      (compile-lifecycle-hooks bad-model :authors))))

(test compose-insert-stores-name
  "Insert with F/M/L stores composed :name."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Sarah" :middle-name "Jane" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Sarah Jane Cameron")))
    (be-delete :authors id "admin")))

(test compose-two-authors-same-last-name
  "Two authors sharing a last name both insert (unique identity on :name)."
  (multiple-value-bind (id1 ins1)
    (be-insert :authors
      '(:first-name "Donald" :last-name "Cameron") "admin")
    (is-true ins1)
    (multiple-value-bind (id2 ins2)
      (be-insert :authors
        '(:first-name "Sarah" :last-name "Cameron") "admin")
      (is-true ins2)
      (be-delete :authors id1 "admin")
      (be-delete :authors id2 "admin"))))

(test compose-client-name-overwritten
  "Client-supplied :name is overwritten by compose hook."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :last-name "Cameron"
        :name "HACKED NAME")
      "admin")
    (is-true inserted)
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Donald Cameron")))
    (be-delete :authors id "admin")))

(test compose-required-passes-without-client-name
  ":required on :name passes without client sending :name."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (be-delete :authors id "admin")))

(test compose-empty-names-fail-required
  "Empty F+L yields empty :name, failing :required."
  (signals error
    (be-insert :authors
      '(:first-name nil :last-name nil)
      "admin")))

(test compose-update-changes-name
  "Updating first name updates :name."
  (multiple-value-bind (id inserted)
    (be-insert :authors
      '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (is-true inserted)
    (be-update :authors id
      '(:first-name "Don" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (let ((name (be-val id :name "admin" :type-key :authors)))
      (is (string= name "Don Roy Cameron")))
    (be-delete :authors id "admin")))

(test compose-m2m-labels-show-full-names
  "Book M2M checkbox list shows full composed author names."
  (multiple-value-bind (aid1 ins1)
    (be-insert :authors
      '(:first-name "Donald" :middle-name "Roy" :last-name "Cameron")
      "admin")
    (is-true ins1)
    (multiple-value-bind (aid2 ins2)
      (be-insert :authors
        '(:first-name "Sarah" :last-name "Cameron") "admin")
      (is-true ins2)
      (let ((av (allowed-values-for-field :books :authors "admin")))
        (is-true av)
        (is (member "Donald Roy Cameron" av :test #'string=))
        (is (member "Sarah Cameron" av :test #'string=)))
      (be-delete :authors aid1 "admin")
      (be-delete :authors aid2 "admin"))))
