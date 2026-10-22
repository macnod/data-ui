(in-package :data-ui)

(def-suite page-description-compile
  :description ":page-description / :page-description-open validator
and compile-path probes")

(def-suite page-description-wire
  :description ":page-description wire emission on page-description-test")

(defun pd-base-type-def (&rest extra-keys)
  "Minimal base type-def for valid-page-description probes: a
display-declared table type with one field, plus EXTRA-KEYS
spliced in front."
  (append '(:table t :display t
             :type-roles ("r")
             :views (:main (:tables (:x)))
             :fields (:name (:type :text :identity t
                              :ui (:label "N")
                              :source (:view :main :column :name
                                        :agg :first)
                              :column t :not-null t))
             :list-form (:fields t))
    extra-keys))

(defun pd-minimal-model (&rest types)
  "Types plist for compile-path probes: compile-model takes the
:types value itself (the plist of type-key → def), not the whole
model — TYPES spliced as key/def pairs."
  (apply #'list types))

(in-suite page-description-compile)

(test pd-string-alone-passes
  "A plain string :page-description passes the validator."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :page-description "Hello."))))

(test pd-with-open-t-passes
  ":page-description-open t together with :page-description
passes."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :page-description "Hello."
        :page-description-open t))))

(test pd-with-open-nil-passes
  "An explicit :page-description-open nil is legal (present-nil
companion, collapsed default written out)."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :page-description "Hello."
        :page-description-open nil))))

(test pd-empty-string-signals
  "Empty string :page-description signals (blank counts as
empty)."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description ""))))

(test pd-whitespace-only-signals
  "Whitespace-only :page-description signals (space, tab,
newline, return — whitespace-only renders an empty block)."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description " ")))
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description (coerce
                                            '(#\Tab #\Newline)
                                            'string)))))

(test pd-non-string-signals
  "Non-string :page-description values signal: integer, keyword,
and present-nil."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description 42)))
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description :blue)))
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description nil))))

(test pd-plain-string-passes
  "\"blue\" is a string and passes — only the keyword form
signals."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :page-description "blue"))))

(test pd-open-bad-value-signals
  ":page-description-open \"yes\" signals (t or nil only)."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description "Hello."
        :page-description-open "yes"))))

(test pd-open-alone-signals-naming-type
  ":page-description-open without :page-description signals,
naming the type."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :page-description-open t))))

(test pd-user-setting-signals
  ":user-setting types reject the key: the settings view
auto-opens the single-record form, so the list page — and the
block — never renders there."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :user-setting t
        :page-description "Hello."))))

(test pd-display-nil-signals
  ":display nil rejects the key: be-types collects display types
only, so the description would never reach the wire."
  (signals error
    (valid-page-description :x
      (append '(:table t :display nil
                 :type-roles ("r")
                 :views (:main (:tables (:x)))
                 :fields (:name (:type :text :identity t
                                  :ui (:label "N")
                                  :source (:view :main :column :name
                                            :agg :first)
                                  :column t :not-null t))
                 :list-form (:fields t))
        (list :page-description "Hello.")))))

(test pd-display-omitted-base-signals
  "A base type omitting :display is the same invisible state (no
overlay injects a default on the base path; the compiled def has
no :display key and be-types drops it)."
  (signals error
    (valid-page-description :x
      (append '(:table t
                 :type-roles ("r")
                 :views (:main (:tables (:x)))
                 :fields (:name (:type :text :identity t
                                  :ui (:label "N")
                                  :source (:view :main :column :name
                                            :agg :first)
                                  :column t :not-null t))
                 :list-form (:fields t))
        (list :page-description "Hello.")))))

(test pd-display-t-passes
  ":display t alongside the key passes."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :page-description "Hello."))))

(test pd-rollup-omitting-display-passes
  "A rollup omitting :display passes: the rollup overlay defaults
:display t, so the board reaches be-types."
  (finishes
    (valid-page-description :x
      (list :rollup t
        :page-description "Hello."))))

(test pd-internal-signals
  ":internal t rejects the key: be-types collects non-internal
types only."
  (signals error
    (valid-page-description :x
      (pd-base-type-def :internal t
        :page-description "Hello."))))

(test pd-internal-nil-passes
  "An explicit :internal nil stays visible and passes."
  (finishes
    (valid-page-description :x
      (pd-base-type-def :internal nil
        :page-description "Hello."))))

(test pd-base-compile-path-carries-keys
  "A minimal base type with both keys compiles through
compile-model (stage 3), and the keys ride the compiled def —
including the explicit-nil companion, pinned by the stage-3
re-emission splice (getf alone cannot prove presence of the
nil)."
  (let ((compiled
          (compile-model
            (pd-minimal-model
              :notes
              (pd-base-type-def
                :page-description "Hello **world**."
                :page-description-open t)
              :collapsed
              (pd-base-type-def
                :page-description "Collapsed."
                :page-description-open nil)))))
    (let ((notes (getf compiled :notes))
          (collapsed (getf compiled :collapsed)))
      (is (equal "Hello **world**."
            (getf notes :page-description)))
      (is (eq t (getf notes :page-description-open)))
      (is (equal "Collapsed."
            (getf collapsed :page-description)))
      (is-true (member :page-description-open collapsed)
        ":page-description-open nil must survive compilation as a
present key, not drop.")
      (is (null (getf collapsed :page-description-open))))))

(test pd-rollup-compile-path-carries-keys
  "A minimal rollup with the keys compiles (the rollup path is a
first-class consumer — a rollup board often needs its one
sentence) and the description rides the compiled def. Grain and
fact are distinct types (a real measure may not read the grain)."
  (let* ((base (pd-base-type-def))
          (facts
            (append '(:table t :display t
                       :type-roles ("r")
                       :views (:main (:tables (:facts)))
                       :fields (:name (:type :text :identity t
                                        :ui (:label "N")
                                        :source (:view :main
                                                 :column :name
                                                 :agg :first)
                                        :column t :not-null t)
                                :parent
                                (:type :text
                                  :ui (:label "Parent")
                                  :target :x
                                  :source (:view :main :table :x
                                            :column :name :agg :first)
                                  :column t :not-null t))
                       :list-form (:fields t))
              '()))
          (board
            (list :rollup t
              :grain :x
              :type-roles '("r")
              :views (list :main (list :tables (list :x :facts)))
              :list-form (list :fields t)
              :page-description "Board copy."
              :fields
              (list
                (list :name
                  (list :source (list :view :main :table :x
                                 :column :name :agg :first)
                    :ui (list :label "Name")))
                (list :fact-count
                  (list :type :integer
                    :source (list :view :main :table :facts
                              :column :id :agg :count)
                    :ui (list :label "Facts"))))))
          (compiled
            (compile-model
              (pd-minimal-model :x base :facts facts :board board))))
    (declare (ignore base))
    (is (equal "Board copy."
          (getf (getf compiled :board) :page-description)))))

;;; --- Wire emission (live compiled model) ---

(in-suite page-description-wire)

(defun pd-be-types-entry (type-name)
  "be-types entry (admin) for TYPE-NAME keyword on the
page-description-test fixture."
  (find type-name (be-types "admin") :key
    (lambda (e) (getf e :name)) :test #'eq))

(test pd-be-types-carries-both-keys
  "be-types for admin carries both keys on :notes (getf-gated
description, member-gated companion)."
  (let ((entry (pd-be-types-entry :notes)))
    (is (equal "These are the **notes**."
          (getf entry :page-description)))
    (is-true (member :page-description-open entry))
    (is (eq t (getf entry :page-description-open)))))

(test pd-be-types-explicit-nil-rides
  "The :collapsed entry's explicit :page-description-open nil
rides be-types output as a present key with value nil — the layer
a pipeline regression would actually break."
  (let ((entry (pd-be-types-entry :collapsed)))
    (is-true (member :page-description-open entry)
      "explicit nil must be present, not dropped")
    (is (null (getf entry :page-description-open)))))

(test pd-be-types-plain-type-omits-keys
  "The :plain entry (neither key declared) carries neither key —
minimal payload."
  (let ((entry (pd-be-types-entry :plain)))
    (is-false (member :page-description entry))
    (is-false (member :page-description-open entry))))

(test pd-rollup-entry-carries-description
  "The :board rollup entry (display-less in the fixture; the
rollup overlay defaults :display t) carries the description."
  (let ((entry (pd-be-types-entry :board)))
    (is (equal "Counts per user."
          (getf entry :page-description)))))

(test pd-rest-types-shape-keywords
  "rest-types' mapcar shape: keyword :true/:false encoding,
presence-preserving on the explicit-nil type. Probed through the
same mapcar lambda rest-types runs (be-types output in, entry
out) — the handler itself needs an HTTP request."
  (let* ((be-entry (pd-be-types-entry :collapsed))
          (entry
            (append
              (list :name (format nil "~(~a~)" (getf be-entry :name))
                    :category (getf be-entry :category)
                    :user-setting (if (getf be-entry :user-setting)
                                    :true :false))
              (when (getf be-entry :page-description)
                (list :page-description
                  (getf be-entry :page-description)))
              (when (member :page-description-open be-entry)
                (list :page-description-open
                  (if (getf be-entry :page-description-open)
                    :true
                    :false))))))
    (is (eq :false (getf entry :page-description-open)))))

(test pd-open-nil-serializes-false
  "register-json-boolean-key makes an explicit
:page-description-open nil serialize as false, not [] (which is
truthy in JavaScript)."
  (let ((json (plist-to-json (list :page-description-open nil))))
    (is (search "\"page-description-open\":false" json))))
