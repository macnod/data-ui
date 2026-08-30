;; :default-sort test fixture. Four types:
;; - :items - base type declaring (:points :desc)
;; - :items-plain - same shape, no declaration (id-ASC regression)
;; - :board - rollup declaring (:total-points :desc), whose
;;   first-sortable-measure policy alone would pick :item-count
;;   (declared earlier) instead - proves the declaration wins
;; - :board-plain - same rollup, no declaration (first-measure
;;   policy regression)
'(:title "Default Sort Test"
   :name "default-sort-test"
   :version "0.1"
   :domain "default-sort.test.data-ui.com"
   :repl nil
   :types
   (:items
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("item-users")
       :default-sort (:points :desc)
       :views (:main (:tables (:items :users)))
       :fields (:name (:type :text :identity t :sortable t
                         :ui (:label "Item" :widget :textbox)
                         :source (:view :main :column :name :agg :first)
                         :column t :not-null t :unique t)
                 :user (:type :text
                         :ui (:label "User" :widget :textbox)
                         :target :users
                         :source (:view :main :table :users :column :name
                                   :agg :first)
                         :column t :not-null t)
                 :points (:type :integer :sortable t
                           :ui (:label "Points" :widget :textbox)
                           :source (:view :main :column :points :agg :first)
                           :column t :not-null t)
                 :notes (:type :text
                          :ui (:label "Notes" :widget :textbox)
                          :source (:view :main :column :notes :agg :first)
                          :column t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :items-plain
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("item-users")
       :views (:main (:tables (:items-plain)))
       :fields (:name (:type :text :identity t :sortable t
                         :ui (:label "Item" :widget :textbox)
                         :source (:view :main :column :name :agg :first)
                         :column t :not-null t :unique t)
                 :points (:type :integer :sortable t
                           :ui (:label "Points" :widget :textbox)
                           :source (:view :main :column :points :agg :first)
                           :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     ;; :item-count is the first sortable measure; the declaration
     ;; points at :total-points instead, so the two orders differ
     ;; under the seed data (bo 3 items / 6 pts, cy 2 / 8, ash 1 / 10).
     :board
     (:rollup t
       :grain :users
       :type-roles ("board-viewers")
       :default-sort (:total-points :desc)
       :views (:main (:tables (:users :items)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
         (:item-count (:type :integer
                        :source (:view :main :table :items :column :id
                                  :agg :count)
                        :sortable t
                        :ui (:label "Items")))
         (:total-points (:type :integer
                          :source (:view :main :table :items :column :points
                                    :agg :sum)
                          :sortable t
                          :ui (:label "Points")))))

     :board-plain
     (:rollup t
       :grain :users
       :type-roles ("board-viewers")
       :views (:main (:tables (:users :items)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
         (:item-count (:type :integer
                        :source (:view :main :table :items :column :id
                                  :agg :count)
                        :sortable t
                        :ui (:label "Items")))
         (:total-points (:type :integer
                          :source (:view :main :table :items :column :points
                                    :agg :sum)
                          :sortable t
                          :ui (:label "Points")))))))

