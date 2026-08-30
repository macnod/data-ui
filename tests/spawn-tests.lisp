(in-package :data-ui)

(def-suite spawn-compile-probes
  :description ":spawn compile-time validation probes (validate-model).")

(def-suite spawn-behavior
  :description ":spawn close-and-respawn behavior on spawn-test.")

(in-suite spawn-compile-probes)

;;; ---------------------------------------------------------------------------
;;; Compile probes (validate-model, no DB). The base model reuses the
;;; spawn-test shape so identity/unique and close-field probes match
;;; the fixture's real fields.
;;; ---------------------------------------------------------------------------

(defun sp-base-model ()
  "Base model for spawn compile probes: the :items type from the
spawn-test fixture shape, built with list/cons forms so overrides
compose."
  (list
    :items
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("item-users")
      :views (list :main (list :tables '(:items)))
      :fields (list
                :name (list :type :text
                         :ui (list :label "Item" :widget :textbox)
                         :validations (list :required)
                         :source (list :view :main :column :name
                                   :agg :first)
                         :column t :not-null t)
                :points (list :type :integer
                          :ui (list :label "Points" :widget :textbox)
                          :source (list :view :main :column :points
                                    :agg :first)
                          :column t :not-null t)
                :notes (list :type :text :default ""
                          :ui (list :label "Notes" :widget :textarea)
                          :source (list :view :main :column :notes
                                    :agg :first)
                          :column t)
                :completed (list :type :boolean :default :false
                             :ui (list :label "Done" :widget :checkbox)
                             :source (list :view :main :column
                                       :completed :agg :first)
                             :column t :not-null t)
                :completed-at (list :type :timestamp
                                :ui (list :label "Completed At"
                                      :widget :textbox)
                                :source (list :view :main :column
                                          :completed-at :agg :first)
                                :column t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun sp-button (close clear)
  "The :complete button field def with CLOSE and CLEAR params."
  (list :type :button
    :ui (list :label "Complete" :widget :button)
    :action (list* :spawn
              (append (when close (list :close close))
                (when clear (list :clear clear))))))

(defun sp-compile (close clear)
  "Compile a model with the :items type carrying a :complete button
built from CLOSE / CLEAR. Returns the compiled :items type-def."
  (let* ((base (sp-base-model))
         (type-def (getf base :items))
         (fields (getf type-def :fields)))
    (setf (getf fields :complete) (sp-button close clear))
    (setf (getf type-def :fields) fields)
    (getf (validate-model base) :items)))

(test spawn-compile-happy-path
  "The fixture's close/clear shape compiles."
  (finishes
    (sp-compile
      (list :completed :true :completed-at :now) (list :notes))))

(test spawn-compile-rejects-unknown-close-field
  "Unknown field in :close is a compile error."
  (signals error
    (sp-compile (list :no-such-field :now) (list :notes))))

(test spawn-compile-rejects-unknown-clear-field
  "Unknown field in :clear is a compile error."
  (signals error
    (sp-compile (list :completed :true) (list :no-such-field))))

(test spawn-compile-rejects-now-on-non-timestamp
  ":now on a non-:timestamp close field is a compile error."
  (signals error
    (sp-compile (list :name :now) (list :notes))))

(test spawn-compile-rejects-literal-type-mismatch
  "A close literal that fails the field's type predicate is a
compile error (:points is :integer)."
  (signals error
    (sp-compile (list :points "not-a-number") (list :notes))))

(test spawn-compile-rejects-field-in-both
  "A field in both :close and :clear is a compile error."
  (signals error
    (sp-compile (list :notes "x") (list :notes))))

(test spawn-compile-rejects-unique-not-cleared
  "A :unique t / :identity t field not in :clear is a compile
error (a copied unique value can only collide)."
  (signals error
    (let* ((base (sp-base-model))
           (type-def (getf base :items))
           (fields (getf type-def :fields))
           (name-def (getf fields :name)))
      (setf (getf name-def :unique) t)
      (setf (getf fields :name) name-def)
      (setf (getf fields :complete) (sp-button nil (list :notes)))
      (setf (getf type-def :fields) fields)
      (validate-model base))))

(test spawn-compile-rejects-button-status-in-close
  "The button's own status companion is not a close/clear target."
  (signals error
    (sp-compile (list :complete-status "x") (list :notes))))

(test spawn-factory-rejects-missing-close
  "A :spawn action form without :close params signals at factory
time (missing-param / non-empty plist guard)."
  (signals error
    (resolve-hook-form '(:spawn) :kind :action
      :type-key :items :field-key :complete)))

;;; ---------------------------------------------------------------------------
;;; Behavior (with-model "spawn-test")
;;; ---------------------------------------------------------------------------

(in-suite spawn-behavior)

(defun th-sp-fresh-id ()
  "The id of the open (uncompleted) item, regardless of history."
  (be-id :items '((:items :name :eq "mow-the-lawn")
                  (:items :completed :eq :false))
    "admin"))

;; The open item id (used both before and after actions).
(defun th-sp-item-id ()
  (th-sp-fresh-id))

(defun th-sp-reset ()
  "Delete every item row and re-seed one open item so each test
starts from the same state (order-independent)."
  (dolist (r (getf (be-list :items "admin" :limit 100) :records))
    (be-delete :items (getf r :id) "admin"))
  (be-insert :items
    (list :name "mow-the-lawn"
      :points 3
      :tags '("home" "yard")
      :notes "prior-round-notes")
    "worker"
    :roles '("item-users")))

(test spawn-closes-and-respawns
  "be-action on the :complete button closes the row and inserts a
fresh open successor."
  (th-sp-reset)
  (let ((id (th-sp-item-id)))
    (is-true id "Precondition: seeded item exists")
    (let ((result (be-action :items id :complete "worker")))
      (is (equal (getf result :status) "complete")
        "Sync hook should report complete"))
    ;; Old row: closed, close values written
    (let* ((old (getf (rec id "admin" :type-key :items) :record)))
      (is (equal (getf old :completed) :true)
        "Old row is completed")
      (is-true (timestamp-p (getf old :completed-at))
        "completed-at is a timestamp string: ~a"
        (getf old :completed-at))
      (is (equal (getf old :completed-by) '("worker"))
        "completed-by is the acting user (M2M list): ~a"
        (getf old :completed-by))
      (is (equal (getf old :complete-status) "complete")
        "Status column is complete"))
    ;; New row: open, definition fields copied, state reset
    (let* ((new-id (th-sp-fresh-id))
           (new (and new-id
                     (getf (rec new-id "admin" :type-key :items)
                       :record))))
      (is-true new-id "A fresh open successor exists")
      (is (equal (getf new :name) "mow-the-lawn")
        "Name copied to successor")
      (is (equal (getf new :points) 3)
        "Points copied to successor")
      (is (equal (getf new :tags) '("home" "yard"))
        "Tags copied through the join table: ~a" (getf new :tags))
      (is (equal (getf new :notes) "")
        "Notes cleared back to default")
      (is (equal (getf new :completed) :false)
        "Successor is open")
      (is-false (getf new :completed-at)
        "Successor completed-at is NULL")
      (is (null (getf new :completed-by))
        "Successor completed-by is empty")
      ;; RBAC: successor visible to the acting user (create path)
      (is-true (user-allowed-resource "worker" new-id "read")
        "Spawned row visible to worker")
      (when new-id (be-delete :items new-id "admin")))))

(test spawn-list-separates-open-from-history
  "be-list with a :completed filter separates open from history."
  (th-sp-reset)
  (let ((id (th-sp-item-id)))
    (is-true id "Precondition: seeded item exists")
    (be-action :items id :complete "worker")
    (let* ((open (getf (be-list :items "admin"
                          :filters '((:items :completed :eq :false)))
                     :records))
           (history (getf (be-list :items "admin"
                            :filters '((:items :completed :eq :true)))
                        :records)))
      (is (= 1 (length open)) "Exactly one open row")
      (is (= 1 (length history)) "Exactly one history row")
      (is-false (getf (first open) :completed-at)
        "Open row has no completed-at")
      (is-true (timestamp-p (getf (first history) :completed-at))
        "History row has a timestamp"))
    ;; Cleanup: delete both rows
    (let ((open-id (th-sp-fresh-id)))
      (when open-id (be-delete :items open-id "admin"))
      (be-delete :items id "admin"))))

(test spawn-rerun-creates-second-history-row
  "Running the action on the already-completed row again yields a
second history row (and restamps completed-at)."
  (th-sp-reset)
  (let ((id (th-sp-item-id)))
    (is-true id "Precondition: seeded item exists")
    (be-action :items id :complete "worker")
    ;; Run again on the fresh open successor: closes it too, so two
    ;; history rows and one new open successor.
    (let* ((open-id (th-sp-fresh-id))
           (result (be-action :items open-id :complete "worker")))
      (is (equal (getf result :status) "complete")
        "Second run also completes"))
    (let* ((history (getf (be-list :items "admin"
                            :filters '((:items :completed :eq :true)))
                        :records))
           (open (getf (be-list :items "admin"
                          :filters '((:items :completed :eq :false)))
                     :records)))
      (is (= 2 (length history)) "Two history rows")
      (is (= 1 (length open)) "One open row"))
    ;; Cleanup
    (dolist (r (getf (be-list :items "admin" :limit 100) :records))
      (be-delete :items (getf r :id) "admin"))))
