(in-package :data-ui)

(def-suite markdown-compile
  :description ":widget :markdown compile-field shape rule probes")

(def-suite markdown-wire
  :description ":markdown / :hide-label wire emission on markdown-test")

(in-suite markdown-compile)

;;; markdown-compile reuses widget-compile (from widget-tests.lisp, loaded
;;; first per data-ui.asd) to probe the compile-field shape rules.

(test markdown-on-text-column-compiles
  ":markdown on :type :text :column t compiles; the compiled :ui
carries :widget :markdown verbatim."
  (let* ((compiled (widget-compile
                     (cons :body
                       '(:type :text
                          :ui (:widget :markdown)
                          :source (:view :main :column :body :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :body)))
    (is (eq :markdown (getf (getf def :ui) :widget)))))

(test markdown-on-omitted-type-compiles
  "Omitted :type (defaults :text) + :column t + :markdown compiles
at the finisher (sql-type still rejects a raw nil :type — an omitted
:type field needs an explicit :type :text, same as :searchable in
practice)."
  (let* ((compiled (widget-compile
                     (cons :body
                       '(:type :text :column t
                          :ui (:widget :markdown)
                          :source (:view :main :column :body :agg :first)))))
         (def (getf (getf compiled :fields) :body)))
    (is (eq :markdown (getf (getf def :ui) :widget)))
    (is (eq :text (getf def :type)))))

(test markdown-rejects-integer
  ":markdown on :type :integer signals, naming the field."
  (signals error
    (widget-compile
      (cons :count
        '(:type :integer
           :ui (:widget :markdown)
           :source (:view :main :column :count :agg :first)
           :column t)))))

(test markdown-requires-column
  ":markdown without :column t signals."
  (signals error
    (widget-compile
      (cons :body
        '(:type :text
           :ui (:widget :markdown)
           :source (:view :main :column :body :agg :first))))))

(test markdown-rejects-target
  ":markdown on a :target field signals: an FK declares :type :text
(display type) while storage is forced to :uuid, so the explicit
clause is required (the :search-select shape)."
  (signals error
    (widget-compile
      (cons :pick
        '(:type :text
           :ui (:widget :markdown)
           :target :users
           :source (:view :main :table :users
                    :column :name :agg :first)
           :source-all (:view :main :table :users
                         :column :name :agg :list)
           :column t)))))

(test markdown-rejects-rollup-column
  ":markdown signals on a rollup field: valid-rollup-fields already
rejects :column t (a rollup has no table), so the column clause covers
rollups; a test proves it."
  (signals error
    (compile-model
      `(:posts
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("md-users")
           :views (:main (:tables (:posts)))
           :fields
           (:title
             (:type :text :identity t
               :ui (:label "Title" :widget :textbox)
               :source (:view :main :column :title :agg :first)
               :column t :not-null t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))
         :post-stats
         (:rollup t
           :grain :posts
           :type-roles ("md-users")
           :views (:main (:tables (:posts :posts)))
           :fields
           ((:body (:type :text
                     :ui (:widget :markdown)
                     :source (:view :main :table :posts
                              :column :title :agg :first)))))))))


(test hide-label-t-compiles-verbatim
  ":hide-label t on a :textarea compiles and survives verbatim
(also proves *ui-keys* accepts it)."
  (let* ((compiled (widget-compile
                     (cons :bio
                       '(:type :text
                          :ui (:widget :textarea :hide-label t)
                          :source (:view :main :column :bio :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :bio)))
    (is (eq t (getf (getf def :ui) :hide-label)))))

(test hide-label-nil-compiles
  ":hide-label nil compiles (explicit shown)."
  (let* ((compiled (widget-compile
                     (cons :bio
                       '(:type :text
                          :ui (:widget :textarea :hide-label nil)
                          :source (:view :main :column :bio :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :bio)))
    (is (null (getf (getf def :ui) :hide-label)))))

(test hide-label-bad-value-signals
  ":hide-label \"yes\" signals (t or nil only)."
  (signals error
    (widget-compile
      (cons :bio
        '(:type :text
           :ui (:widget :textarea :hide-label "yes")
           :source (:view :main :column :bio :agg :first)
           :column t)))))

;;; --- Wire emission (live compiled model) ---

(in-suite markdown-wire)

(defun markdown-fe-fields ()
  "fe-fields output for the markdown-test fixture's :posts type."
  (fe-fields :posts "admin"))

(test markdown-wire-carries-widget-and-hide-label
  "fe-fields emits :widget :markdown and :hide-label t on the
markdown-test fixture (the :ui plist rides verbatim on each form
field entry)."
  (let* ((fields (markdown-fe-fields))
         (body (getf (getf fields :add-form) :body)))
    (is (eq :markdown (getf body :widget)))
    (is (eq t (getf body :hide-label)))))

(test hide-label-nil-serializes-false
  "register-json-boolean-key makes an explicit :hide-label nil
serialize as false, not [] (which is truthy in JavaScript)."
  (let ((json (plist-to-json (list :hide-label nil))))
    (is (search "\"hide-label\":false" json))))
