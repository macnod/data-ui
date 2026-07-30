(in-package :data-ui)

(def-suite bi-m2m-suite
  :description "Bidirectional M2M — compile, no-overflow, allowed-values,
CRUD both directions, skip-flag behavior.")

(def-suite bi-m2m-one-way-regression-suite
  :description "One-way / multi-joiner regression under m2m-test fixture.")

(in-suite bi-m2m-suite)

;;; ---------------------------------------------------------------------------
;;; Group A — Compile / structure
;;; ---------------------------------------------------------------------------

(test bi-m2m-model-compiles
  "Compiled model has :books, :authors, :book-authors."
  (is-true (u:tree-get *compiled-model* :books))
  (is-true (u:tree-get *compiled-model* :authors))
  (is-true (u:tree-get *compiled-model* :book-authors)))

(test bi-m2m-both-list-fields-share-joiner
  "Both list fields' :join-table is :book-authors."
  (is (eq (u:tree-get *compiled-model* :books :fields :authors
             :join-table)
          :book-authors))
  (is (eq (u:tree-get *compiled-model* :authors :fields :books
             :join-table)
          :book-authors)))

(test bi-m2m-both-sides-have-source-all
  "Both sides have :source-all pointing at the other type."
  (let ((books-sa (u:tree-get *compiled-model* :books :fields
                     :authors :source-all))
        (auth-sa (u:tree-get *compiled-model* :authors :fields
                     :books :source-all)))
    (is-true books-sa "Books :authors must have :source-all")
    (is-true auth-sa "Authors :books must have :source-all")
    (is (eq (getf books-sa :table) :authors))
    (is (eq (getf auth-sa :table) :books))))

(test bi-m2m-insert-sql-shape
  "Each side's insert-sql for its list field references book_authors
with exactly the two FK keys."
  (let ((ins (u:tree-get *compiled-model* :books :insert-sql)))
    (let ((entry (getf ins :authors)))
      (is-true entry "Expected :authors in books insert-sql")
      (is (= 2 (length (cdr entry)))
          "Expected 2 param keys, got ~a" (cdr entry))
      (is (search "book_authors" (car entry)))))
  (let ((ins (u:tree-get *compiled-model* :authors :insert-sql)))
    (let ((entry (getf ins :books)))
      (is-true entry "Expected :books in authors insert-sql")
      (is (= 2 (length (cdr entry)))
          "Expected 2 param keys, got ~a" (cdr entry))
      (is (search "book_authors" (car entry))))))

(test bi-m2m-update-sql-shape
  "Each side's update-sql has :insert and :delete for its list field
referencing book_authors."
  (let ((upd (u:tree-get *compiled-model* :books :update-sql)))
    (let ((entry (getf upd :authors)))
      (is-true entry "Expected :authors in books update-sql")
      (is-true (getf entry :insert))
      (is-true (getf entry :delete))
      (is (search "book_authors" (car (getf entry :insert))))
      (is (search "book_authors" (car (getf entry :delete))))))
  (let ((upd (u:tree-get *compiled-model* :authors :update-sql)))
    (let ((entry (getf upd :books)))
      (is-true entry "Expected :books in authors update-sql")
      (is-true (getf entry :insert))
      (is-true (getf entry :delete))
      (is (search "book_authors" (car (getf entry :insert))))
      (is (search "book_authors" (car (getf entry :delete)))))))

;;; ---------------------------------------------------------------------------
;;; Group B — No stack overflow (core fix)
;;; ---------------------------------------------------------------------------

(test bi-m2m-be-list-no-overflow
  "be-list on both ends of bidirectional M2M must not stack-overflow,
with or without data, in alternating calls."
  ;; Populated tables
  (finishes (be-list :books "admin"))
  (finishes (be-list :authors "admin"))
  ;; Alternating
  (finishes (be-list :books "admin"))
  (finishes (be-list :authors "admin"))
  ;; Empty tables: delete all seeded rows, test, re-seed
  (let ((book-ids (mapcar (lambda (r) (getf r :id))
                   (getf (be-list :books "admin") :records)))
        (auth-ids (mapcar (lambda (r) (getf r :id))
                   (getf (be-list :authors "admin") :records))))
    (unwind-protect
      (progn
        (dolist (id book-ids) (be-delete :books id "admin"))
        (dolist (id auth-ids) (be-delete :authors id "admin"))
        ;; Empty tables — still must not overflow
        (finishes (be-list :books "admin"))
        (finishes (be-list :authors "admin")))
      ;; Re-seed
      (be-insert :authors '(:name "A1") "admin")
      (be-insert :authors '(:name "A2") "admin")
      (be-insert :authors '(:name "A3") "admin")
      (be-insert :books '(:title "B1") "admin")
      (be-insert :books '(:title "B2") "admin")
      (be-insert :books '(:title "B3") "admin"))))

;;; ---------------------------------------------------------------------------
;;; Group C — Allowed-values correctness
;;; ---------------------------------------------------------------------------

(test bi-m2m-allowed-values-books-lists-authors
  "Books' allowed-values :authors contains seeded author names."
  (let ((av (getf (be-list :books "admin") :allowed-values)))
    (let ((authors (getf av :authors)))
      (is-true authors "Expected :authors in allowed-values")
      (is (member "A1" authors :test 'equal))
      (is (member "A2" authors :test 'equal))
      (is (member "A3" authors :test 'equal)))))

(test bi-m2m-allowed-values-authors-lists-books
  "Authors' allowed-values :books contains seeded book titles."
  (let ((av (getf (be-list :authors "admin") :allowed-values)))
    (let ((books (getf av :books)))
      (is-true books "Expected :books in allowed-values")
      (is (member "B1" books :test 'equal))
      (is (member "B2" books :test 'equal))
      (is (member "B3" books :test 'equal)))))

(test bi-m2m-allowed-values-empty-options
  "When no rows exist, list-field allowed-values is empty."
  (let ((book-ids (mapcar (lambda (r) (getf r :id))
                   (getf (be-list :books "admin") :records)))
        (auth-ids (mapcar (lambda (r) (getf r :id))
                   (getf (be-list :authors "admin") :records))))
    (unwind-protect
      (progn
        (dolist (id book-ids) (be-delete :books id "admin"))
        (dolist (id auth-ids) (be-delete :authors id "admin"))
        (let ((books-av (getf (be-list :books "admin") :allowed-values))
              (auth-av (getf (be-list :authors "admin") :allowed-values)))
          (is (null (getf books-av :authors))
              "Books :authors options should be empty")
          (is (null (getf auth-av :books))
              "Authors :books options should be empty")))
      ;; Re-seed
      (be-insert :authors '(:name "A1") "admin")
      (be-insert :authors '(:name "A2") "admin")
      (be-insert :authors '(:name "A3") "admin")
      (be-insert :books '(:title "B1") "admin")
      (be-insert :books '(:title "B2") "admin")
      (be-insert :books '(:title "B3") "admin"))))

(test bi-m2m-allowed-values-roles-still-present
  "Non-base types still get :roles in allowed-values (skip flag
must not strip outer list-result metadata)."
  (let ((books-av (getf (be-list :books "admin") :allowed-values))
        (auth-av (getf (be-list :authors "admin") :allowed-values)))
    (is-true (getf books-av :roles)
             "Books allowed-values must include :roles")
    (is-true (getf auth-av :roles)
             "Authors allowed-values must include :roles")))

;;; ---------------------------------------------------------------------------
;;; Group D — CRUD both directions
;;; ---------------------------------------------------------------------------

(test bi-m2m-insert-book-with-authors
  "Insert a book with authors; join rows exist; list shows associations."
  (let ((id (be-insert :books
              '(:title "IB-Test" :authors ("A1" "A2")) "admin")))
    (is-true id "Insert should return an ID")
    (unwind-protect
      (let* ((records (getf (be-list :books "admin") :records))
             (rec (find id records :key (lambda (r) (getf r :id))
                        :test 'equal)))
        (is-true rec "Inserted book should appear in list")
        (is (equal (sort (copy-list (getf rec :authors)) #'string<)
                   '("A1" "A2"))
            "Book should show both authors"))
      (be-delete :books id "admin"))))

(test bi-m2m-insert-author-with-books
  "Insert an author with books; reverse direction."
  (let ((id (be-insert :authors
              '(:name "IA-Test" :books ("B1" "B2")) "admin")))
    (is-true id "Insert should return an ID")
    (unwind-protect
      (let* ((records (getf (be-list :authors "admin") :records))
             (rec (find id records :key (lambda (r) (getf r :id))
                        :test 'equal)))
        (is-true rec "Inserted author should appear in list")
        (is (equal (sort (copy-list (getf rec :books)) #'string<)
                   '("B1" "B2"))
            "Author should show both books"))
      (be-delete :authors id "admin"))))

(test bi-m2m-update-book-authors-add-remove
  "Update a book's authors: add then remove."
  (let ((id (be-insert :books '(:title "UB-Test") "admin")))
    (unwind-protect
      (progn
        ;; Add A1, A2
        (be-update :books id '(:authors ("A1" "A2")) "admin")
        (let ((rec (find id (getf (be-list :books "admin") :records)
                         :key (lambda (r) (getf r :id)) :test 'equal)))
          (is (equal (sort (copy-list (getf rec :authors)) #'string<)
                     '("A1" "A2"))))
        ;; Remove A1, keep A2, add A3
        (be-update :books id '(:authors ("A2" "A3")) "admin")
        (let ((rec (find id (getf (be-list :books "admin") :records)
                         :key (lambda (r) (getf r :id)) :test 'equal)))
          (is (equal (sort (copy-list (getf rec :authors)) #'string<)
                     '("A2" "A3"))
              "After update should have A2 and A3 only")))
      (be-delete :books id "admin"))))

(test bi-m2m-update-author-books-add-remove
  "Update an author's books: add then remove."
  (let ((id (be-insert :authors '(:name "UA-Test") "admin")))
    (unwind-protect
      (progn
        ;; Add B1, B2
        (be-update :authors id '(:books ("B1" "B2")) "admin")
        (let ((rec (find id (getf (be-list :authors "admin") :records)
                         :key (lambda (r) (getf r :id)) :test 'equal)))
          (is (equal (sort (copy-list (getf rec :books)) #'string<)
                     '("B1" "B2"))))
        ;; Remove B1, keep B2, add B3
        (be-update :authors id '(:books ("B2" "B3")) "admin")
        (let ((rec (find id (getf (be-list :authors "admin") :records)
                         :key (lambda (r) (getf r :id)) :test 'equal)))
          (is (equal (sort (copy-list (getf rec :books)) #'string<)
                     '("B2" "B3"))
              "After update should have B2 and B3 only")))
      (be-delete :authors id "admin"))))

(test bi-m2m-list-symmetric-associations
  "After linking book to authors, both sides show the association."
  (let ((book-id (be-insert :books
                   '(:title "SYM-Book" :authors ("A1" "A2")) "admin")))
    (unwind-protect
      (let* ((book-rec (find book-id
                      (getf (be-list :books "admin") :records)
                      :key (lambda (r) (getf r :id)) :test 'equal))
             (auth-recs (getf (be-list :authors "admin") :records))
             (a1-rec (find "A1" auth-recs
                      :key (lambda (r) (getf r :name)) :test 'equal))
             (a2-rec (find "A2" auth-recs
                      :key (lambda (r) (getf r :name)) :test 'equal)))
        ;; Book shows authors
        (is (member "A1" (getf book-rec :authors) :test 'equal))
        (is (member "A2" (getf book-rec :authors) :test 'equal))
        ;; Authors show book
        (is (member "SYM-Book" (getf a1-rec :books) :test 'equal))
        (is (member "SYM-Book" (getf a2-rec :books) :test 'equal)))
      (be-delete :books book-id "admin"))))

(test bi-m2m-delete-book-removes-join-rows
  "Deleting a book removes join rows; authors remain."
  (let ((book-id (be-insert :books
                   '(:title "DEL-Book" :authors ("A1")) "admin")))
    ;; Confirm link exists
    (let ((a1-rec (find "A1" (getf (be-list :authors "admin") :records)
                    :key (lambda (r) (getf r :name)) :test 'equal)))
      (is (member "DEL-Book" (getf a1-rec :books) :test 'equal)))
    ;; Delete the book
    (be-delete :books book-id "admin")
    ;; Author still exists, book gone from their list
    (let ((a1-rec (find "A1" (getf (be-list :authors "admin") :records)
                    :key (lambda (r) (getf r :name)) :test 'equal)))
      (is-true a1-rec "Author A1 should still exist")
      (is (not (member "DEL-Book" (getf a1-rec :books) :test 'equal))
          "Deleted book should not appear in author's list"))))

(test bi-m2m-delete-author-removes-join-rows
  "Deleting an author removes join rows; books remain."
  (let ((auth-id (be-insert :authors
                   '(:name "DEL-Auth" :books ("B1")) "admin")))
    ;; Confirm link exists
    (let ((b1-rec (find "B1" (getf (be-list :books "admin") :records)
                    :key (lambda (r) (getf r :title)) :test 'equal)))
      (is (member "DEL-Auth" (getf b1-rec :authors) :test 'equal)))
    ;; Delete the author
    (be-delete :authors auth-id "admin")
    ;; Book still exists, author gone from its list
    (let ((b1-rec (find "B1" (getf (be-list :books "admin") :records)
                    :key (lambda (r) (getf r :title)) :test 'equal)))
      (is-true b1-rec "Book B1 should still exist")
      (is (not (member "DEL-Auth" (getf b1-rec :authors) :test 'equal))
          "Deleted author should not appear in book's list"))))

;;; ---------------------------------------------------------------------------
;;; Group E — Skip-flag unit behavior
;;; ---------------------------------------------------------------------------

(test bi-m2m-be-list-skip-flag-nil-allowed-values
  "Top-level be-list has non-nil :allowed-values."
  (let ((result (be-list :books "admin")))
    (is-true (getf result :allowed-values))
    (is-true (getf result :records))))

(test bi-m2m-be-list-skip-flag-t-omits-allowed-values
  "be-list with :skip-allowed-values t returns nil allowed-values
but records are still present."
  (let ((result (be-list :books "admin" :skip-allowed-values t)))
    (is (null (getf result :allowed-values)))
    (is-true (getf result :records))))

(test bi-m2m-be-list-column-values-with-skip
  "be-list-column with :skip-allowed-values t returns correct
:values and does not overflow."
  (let ((result (be-list-column :authors :name "admin"
                  :skip-allowed-values t)))
    (is (eq (getf result :type) :authors))
    (is-true (getf result :values))
    (is (member "A1" (getf result :values) :test 'equal))))

;;; ---------------------------------------------------------------------------
;;; Group G — Edge cases
;;; ---------------------------------------------------------------------------

(test bi-m2m-self-cycle-three-calls
  "After inner skip, a fresh outer be-list still has full allowed-values."
  ;; This call triggers the inner skip path (allowed-values-for-field
  ;; → be-list-column → be-list with skip)
  (finishes (be-list :books "admin"))
  ;; Fresh outer call must still compute full allowed-values
  (let ((result (be-list :authors "admin")))
    (is-true (getf result :allowed-values))
    (is-true (getf (getf result :allowed-values) :books))))

;;; ---------------------------------------------------------------------------
;;; One-way regression suite (m2m-test fixture)
;;; ---------------------------------------------------------------------------

(in-suite bi-m2m-one-way-regression-suite)

(test bi-m2m-regression-items-be-list
  "One-way M2M: be-list on owner type still works."
  (finishes (be-list :items "admin"))
  (let ((av (getf (be-list :items "admin") :allowed-values)))
    (is-true (getf av :tags) "Expected :tags in allowed-values")
    (is-true (getf av :assignees) "Expected :assignees in allowed-values")))

(test bi-m2m-regression-tags-be-list
  "One-way M2M: be-list on target type (no reverse list) works."
  (finishes (be-list :tags "admin")))

(test bi-m2m-regression-multi-joiner-allowed-values
  "Multi-joiner: both list fields get non-empty option lists."
  (let ((av (getf (be-list :items "admin") :allowed-values)))
    (is-true (getf av :tags) "Tags options should be present")
    (is (member "red" (getf av :tags) :test 'equal))
    (is (member "blue" (getf av :tags) :test 'equal))))
