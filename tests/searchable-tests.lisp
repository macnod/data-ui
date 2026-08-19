(in-package :data-ui)

(def-suite searchable-suite
  :description "Tests for :searchable field attribute and :search behavior.")

(in-suite searchable-suite)

;;; --- Compile helpers (pure, no DB) ---

(defun searchable-test-model (&rest field-plists)
  "Minimal types plist with one type :st whose fields are FIELD-PLISTS."
  `(:st
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("st-users")
       :views (:main (:tables (:st)))
       :fields
       ,(loop
          for (key . def) in field-plists
          appending (list key def))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))

(defun searchable-compile (&rest field-plists)
  "Compile a minimal model; return the compiled :st type def."
  (getf (compile-model (apply #'searchable-test-model field-plists)) :st))

;;; --- Compile / metadata ---

(test searchable-compiles-on-base-column
  ":searchable t on a base-column :text field compiles and appears
in the compiled field plist."
  (is-true
    (u:tree-get *compiled-model* :todos :fields :name :searchable)))

(test searchable-absent-when-not-declared
  "Fields without :searchable t should have :searchable nil."
  (is-false
    (u:tree-get *compiled-model* :todos :fields :done :searchable)))

(test searchable-forwarded-in-fe-fields
  "fe-fields forwards :searchable into the list-form field plist."
  (let ((list-form (getf (fe-fields :todos "admin") :list-form)))
    (is-true (getf (getf list-form :name) :searchable))
    (is-false (getf (getf list-form :done) :searchable))))

(test searchable-fields-drift
  ":searchable-fields on the type is exactly the declared fields'
qualified column names."
  (let* ((cols (u:tree-get *compiled-model* :todos :searchable-fields))
         (name-col (u:tree-get *compiled-model*
                      :todos :fields :name :source :column-name)))
    (is (equal cols (list name-col)))
    (is-true (search "." name-col))))

(test searchable-rejects-non-column
  ":searchable t without :column t fails compile."
  (signals error
    (searchable-compile
      (cons :notes
        '(:type :text :searchable t
           :ui (:label "Notes" :widget :textbox)
           :source (:view :main :column :notes :agg :first))))))

(test searchable-rejects-non-text
  ":searchable t on :integer fails compile."
  (signals error
    (searchable-compile
      (cons :pts
        '(:type :integer :column t :searchable t
           :ui (:label "Pts" :widget :textbox)
           :source (:view :main :column :pts :agg :first))))))

(test searchable-rejects-password
  ":searchable t on :password fails compile."
  (signals error
    (searchable-compile
      (cons :secret
        '(:type :password :column t :searchable t
           :ui (:label "Secret" :widget :password)
           :source (:view :main :column :secret :agg :first))))))

(test searchable-rejects-target
  ":searchable t on a :target FK field fails compile."
  (signals error
    (compile-model
      `(:parent
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("p-users")
           :fields
           (:name
             (:type :text :identity t :column t
               :ui (:label "Name" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :not-null t :unique t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))
        :child
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("c-users")
           :fields
           (:name
             (:type :text :identity t :column t
               :ui (:label "Name" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :not-null t :unique t)
            :parent
             (:type :text :column t :searchable t :target :parent
               :ui (:label "Parent" :widget :select)
               :source (:view :main :column :parent :agg :first)))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))))))

;;; --- Clause builder units ---

(test searchable-clause-blank-is-nil
  "Blank / whitespace search → nil (no clause)."
  (is-false (phase-a-search-clause :todos nil 1))
  (is-false (phase-a-search-clause :todos "" 1))
  (is-false (phase-a-search-clause :todos "   " 1)))

(test searchable-clause-single-field
  "Single searchable field: parenthesized ILIKE, one $n, ESCAPE present."
  (let* ((result (phase-a-search-clause :todos "hello" 3))
         (fragment (car result))
         (pattern (cdr result))
         (name-col (u:tree-get *compiled-model*
                      :todos :fields :name :source :column-name)))
    (is-true result)
    (is (equal pattern "%hello%"))
    (is-true (search "ILIKE $3" fragment))
    (is-true (search "ESCAPE '\\'" fragment))
    (is-true (search name-col fragment))
    (is (char= #\( (char fragment 0)))
    (is (char= #\) (char fragment (1- (length fragment)))))))

(test searchable-clause-escapes-metachars
  "\\, %, _ are escaped in the pattern; result wrapped in %...%."
  (let* ((result (phase-a-search-clause :todos "a%b_c\\d" 1))
         (pattern (cdr result)))
    (is (equal pattern "%a\\%b\\_c\\\\d%"))))

(test searchable-clause-fragment-independent-of-term
  "Two different terms produce the same fragment; term only in bind value."
  (let* ((r1 (phase-a-search-clause :todos "alpha" 2))
         (r2 (phase-a-search-clause :todos "beta" 2)))
    (is (equal (car r1) (car r2)))
    (is (equal (cdr r1) "%alpha%"))
    (is (equal (cdr r2) "%beta%"))))

(test searchable-clause-no-from-substring
  "Fragment contains no \" from\" substring (count-query surgery safety)."
  (let ((fragment (car (phase-a-search-clause :todos "x" 1))))
    (is-false (search " from" fragment :test #'char-equal))))

(test searchable-rejects-nonempty-on-zero-fields
  "Non-empty search on a type with zero searchable fields → validation error."
  (signals error
    (phase-a-search-clause :tags "anything" 1)))

(test searchable-blank-on-zero-fields-ok
  "Blank / whitespace search on zero-searchable type → nil (no error)."
  (is-false (phase-a-search-clause :tags nil 1))
  (is-false (phase-a-search-clause :tags "  " 1)))

;;; --- be-list integration ---

(test searchable-be-list-reduces-total
  "be-list with :search reduces :total."
  (be-insert :todos '(:name "Alpha Task" :points 1) "admin")
  (be-insert :todos '(:name "Beta Task" :points 2) "admin")
  (be-insert :todos '(:name "Gamma Other" :points 3) "admin")
  (let* ((all (be-list :todos "admin"))
         (hit (be-list :todos "admin" :search "Task")))
    (is (>= (getf all :total) 3))
    (is (= (getf hit :total) 2))
    (is (= (length (getf hit :records)) 2)))
  (loop for r in (getf (be-list :todos "admin" :limit 200) :records)
        do (be-delete :todos (getf r :id) "admin")))

(test searchable-composes-with-eq-filter
  "Search ANDs with an :eq filter."
  (be-insert :todos '(:name "Alpha Task" :points 10) "admin")
  (be-insert :todos '(:name "Beta Task" :points 20) "admin")
  (be-insert :todos '(:name "Alpha Other" :points 10) "admin")
  (let* ((result (be-list :todos "admin"
                   :search "Task"
                   :filters '((:todos :points :eq 10))))
         (names (mapcar (lambda (r) (getf r :name))
                  (getf result :records))))
    (is (= (getf result :total) 1))
    (is (equal names '("Alpha Task"))))
  (loop for r in (getf (be-list :todos "admin" :limit 200) :records)
        do (be-delete :todos (getf r :id) "admin")))

(test searchable-composes-with-join-filter
  "Search composes with a join filter (DISTINCT + JOIN Phase A shape)."
  (let ((tag-id (be-insert :tags '(:name "search-tag-x") "admin")))
    (be-insert :todos
      `(:name "Join Alpha" :points 1 :tags ("search-tag-x"))
      "admin")
    (be-insert :todos
      `(:name "Join Beta" :points 2 :tags ("search-tag-x"))
      "admin")
    (be-insert :todos
      '(:name "No Tag Alpha" :points 3)
      "admin")
    (let* ((result (be-list :todos "admin"
                     :search "Alpha"
                     :filters '((:tags :name :eq "search-tag-x"))))
           (names (mapcar (lambda (r) (getf r :name))
                    (getf result :records))))
      (is (= (getf result :total) 1))
      (is (equal names '("Join Alpha"))))
    (loop for r in (getf (be-list :todos "admin" :limit 200) :records)
          do (be-delete :todos (getf r :id) "admin"))
    (be-delete :tags tag-id "admin")))

(test searchable-page-slice-honest
  "limit/offset apply within the searched set; :total is pre-page count."
  (be-insert :todos '(:name "S1 Apple" :points 1) "admin")
  (be-insert :todos '(:name "S2 Apple" :points 2) "admin")
  (be-insert :todos '(:name "S3 Apple" :points 3) "admin")
  (be-insert :todos '(:name "S4 Banana" :points 4) "admin")
  (let* ((page (be-list :todos "admin"
                 :search "Apple"
                 :sort '(:points :asc)
                 :limit 2 :offset 1))
         (names (mapcar (lambda (r) (getf r :name))
                  (getf page :records))))
    (is (= (getf page :total) 3))
    (is (= (length (getf page :records)) 2))
    (is (equal names '("S2 Apple" "S3 Apple"))))
  (loop for r in (getf (be-list :todos "admin" :limit 200) :records)
        do (be-delete :todos (getf r :id) "admin")))

(test searchable-literal-metachars
  "Literal % / _ in input do not act as wildcards."
  (be-insert :todos '(:name "a_b" :points 1) "admin")
  (be-insert :todos '(:name "axb" :points 2) "admin")
  (let* ((exact (be-list :todos "admin" :search "a_b"))
         (wild (be-list :todos "admin" :search "_")))
    (is (= (getf exact :total) 1))
    (is (equal (getf (car (getf exact :records)) :name) "a_b"))
    ;; Searching "_" should only match rows containing a literal underscore,
    ;; not every single-char-wildcard match.
    (is (= (getf wild :total) 1))
    (is (equal (getf (car (getf wild :records)) :name) "a_b")))
  (loop for r in (getf (be-list :todos "admin" :limit 200) :records)
        do (be-delete :todos (getf r :id) "admin")))

(test searchable-blank-be-list-on-tags
  "Blank search on :tags (zero searchable) succeeds with full list;
non-empty search signals."
  (let ((tag-id (be-insert :tags '(:name "zero-search-tag") "admin")))
    (finishes (be-list :tags "admin" :search nil))
    (finishes (be-list :tags "admin" :search "  "))
    (signals error
      (be-list :tags "admin" :search "nope"))
    (be-delete :tags tag-id "admin")))

;;; --- REST parse ---

(test searchable-parse-search-param
  "parse-search-param trims, blanks → nil, clamps at 200."
  (is-false (parse-search-param nil))
  (is-false (parse-search-param ""))
  (is-false (parse-search-param "   "))
  (is (equal (parse-search-param "  hello  ") "hello"))
  (let* ((long (make-string 250 :initial-element #\a))
         (parsed (parse-search-param long)))
    (is (= (length parsed) 200))))

;;; --- JSON boolean serialization ---

(test searchable-json-boolean-keys
  "nil :searchable/:sortable serialize as JSON false, not []."
  (let ((json (plist-to-json '(:searchable nil :sortable nil :read-only nil))))
    (is-true (search "\"searchable\":false" json))
    (is-true (search "\"sortable\":false" json))
    (is-true (search "\"read-only\":false" json))
    (is-false (search "[]" json)))
  (let ((json (plist-to-json '(:searchable t :sortable t))))
    (is-true (search "\"searchable\":true" json))
    (is-true (search "\"sortable\":true" json))))

(test searchable-fe-fields-json-false
  "fe-fields JSON for a non-searchable field emits searchable:false."
  (let* ((list-form (getf (fe-fields :users "admin") :list-form))
         (json (plist-to-json list-form)))
    (is-true (search "\"searchable\":false" json))
    (is-true (search "\"searchable\":true" json))
    (is-false (search "\"searchable\":[]" json))
    (is-false (search "\"sortable\":[]" json))))

;;; --- Base :users ---

(test searchable-users-fields
  "Base :users :name and :email are searchable."
  (is-true (u:tree-get *compiled-model* :users :fields :name :searchable))
  (is-true (u:tree-get *compiled-model* :users :fields :email :searchable))
  (let ((cols (u:tree-get *compiled-model* :users :searchable-fields)))
    (is (= (length cols) 2))))

(test searchable-users-hyphenated-name
  "be-list :users finds a hyphenated username."
  (th-make-user "search-hyphen-user-1")
  (let* ((result (be-list :users "admin" :search "search-hyphen-user-1"))
         (names (mapcar (lambda (r) (getf r :name))
                  (getf result :records))))
    (is (= (getf result :total) 1))
    (is (equal names '("search-hyphen-user-1"))))
  (let ((id (be-value-id :users :name "search-hyphen-user-1" "admin")))
    (when id (be-delete :users id "admin"))))

;;; --- OR suite (search-test fixture) ---

(def-suite search-or-suite
  :description "OR across multiple :searchable fields (search-test fixture).")

(in-suite search-or-suite)

(test search-or-searchable-fields-both
  ":searchable-fields contains both qualified names in field order."
  (let* ((cols (u:tree-get *compiled-model* :items :searchable-fields))
         (name-col (u:tree-get *compiled-model*
                      :items :fields :name :source :column-name))
         (desc-col (u:tree-get *compiled-model*
                      :items :fields :description :source :column-name)))
    (is (= (length cols) 2))
    (is (equal (first cols) name-col))
    (is (equal (second cols) desc-col))))

(test search-or-clause-two-fields
  "Two fields → parenthesized OR, one $n reused."
  (let* ((result (phase-a-search-clause :items "term" 4))
         (fragment (car result)))
    (is-true (search " OR " fragment))
    (is-true (search "ILIKE $4" fragment))
    ;; $4 appears twice (once per column)
    (is (= 2 (length (cl-ppcre:all-matches-as-strings "\\$4" fragment))))))


(test search-or-be-list-description-only
  "Term matching only :description still returns the record (OR)."
  (be-insert :items
    '(:name "Widget" :description "unique-zebra-phrase")
    "admin")
  (be-insert :items
    '(:name "Gadget" :description "ordinary")
    "admin")
  (let* ((result (be-list :items "admin" :search "unique-zebra"))
         (names (mapcar (lambda (r) (getf r :name))
                  (getf result :records))))
    (is (= (getf result :total) 1))
    (is (equal names '("Widget"))))
  (loop for r in (getf (be-list :items "admin" :limit 200) :records)
        do (be-delete :items (getf r :id) "admin")))
