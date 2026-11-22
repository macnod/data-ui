(in-package :data-ui)

;; Render-cost suite (item "Optimize rendering"). Characterization
;; tests written against today's code — they pin palette contents and
;; order, suppression, and freshness, and must pass unchanged after
;; every engine step. The palette fast path, path selection, call
;; counts, and permitted-row-ids tests land here too as the plan's
;; steps 9, 14, and 18 add them.

(def-suite render-cost-palette-suite
  :description "Characterization and fast-path tests for the
palette read path and permitted-row-ids on the render-cost-test
fixture")

(in-suite render-cost-palette-suite)

;;; ---------------------------------------------------------------------------
;;; Seed (rc-seed-fixture): a non-admin reader, stages whose position
;;; order differs from name order (some readable by rc-reader, one
;;; admin-only), labels, teams, vault rows, and one task.
;;; ---------------------------------------------------------------------------

(defun rc-seed-fixture ()
  "Seed the render-cost-test fixture."
  (th-make-user "rc-reader" :roles '("rc-users"))
  (th-make-user "rc-nobody" :roles '())
  ;; stages: positions order them differently from names; s3 is
  ;; admin-only (no rc-users role on its resource)
  (be-insert :stages '(:name "alpha" :position 3) "admin"
    :roles '("rc-users" "admin-only"))
  (be-insert :stages '(:name "bravo" :position 1) "admin"
    :roles '("rc-users" "admin-only"))
  (be-insert :stages '(:name "charlie" :position 2) "admin"
    :roles '("rc-users" "admin-only"))
  (be-insert :stages '(:name "delta" :position 4) "admin") ; admin-only
  ;; labels (M2M target, identity :title)
  (be-insert :labels '(:title "urgent") "admin" :roles '("rc-users"))
  (be-insert :labels '(:title "later") "admin" :roles '("rc-users"))
  ;; teams (joined-sort fallback shape)
  (be-insert :teams '(:name "team-a" :owner "admin") "admin"
    :roles '("rc-users"))
  (be-insert :teams '(:name "team-b" :owner "rc-reader") "admin"
    :roles '("rc-users"))
  ;; vaults (view-scope fallback shape; one row per user)
  (be-insert :vaults '(:name "vault-admin") "admin"
    :roles '("rc-users"))
  (be-insert :vaults '(:name "vault-reader") "rc-reader"
    :roles '("rc-users"))
  ;; cross rows: boolean and timestamp values behind :text fields
  (be-insert :cross '(:name "c-one" :flag :true
                      :stamp "2026-01-02T03:04:05") "admin"
    :roles '("rc-users"))
  (be-insert :cross '(:name "c-two" :flag :false) "admin"
    :roles '("rc-users"))
  nil)

(defun rc-stage-names (values)
  "The stage palette in name order, given the position-ordered
palette — for the relative-order assertion."
  (sort (copy-list values) #'string<))

;;; ---------------------------------------------------------------------------
;;; Characterization: palette contents and order (step 5)
;;; ---------------------------------------------------------------------------

(test rc-stage-palette-position-order-admin
  "The :stage palette for admin lists every stage in position
order (1 2 3 4 = bravo charlie alpha delta) — the documented
:default-sort behavior for allowed-values palettes."
  (let ((palette (getf (allowed-values :tasks "admin") :stage)))
    (is (equal '("bravo" "charlie" "alpha" "delta") palette))))

(test rc-stage-palette-readable-subset-same-order
  "For rc-reader the :stage palette is the readable subset (delta
excluded) in the same relative position order."
  (let ((palette (getf (allowed-values :tasks "rc-reader") :stage)))
    (is (equal '("bravo" "charlie" "alpha") palette))))

(test rc-labels-team_vault-empty-palettes
  "Exact contents of the :labels (M2M), :team (joined sort), and
:vault (view scope) palettes (labels have no :default-sort, so
their palette follows the be-list id order — random uuids — and
only the set is pinned); the :empty palette is NIL; the :ui
:options palette is the literal list."
  (let ((av (allowed-values :tasks "admin")))
    (is (equal '("later" "urgent")
            (sort (copy-list (getf av :labels)) #'string<)))
    (is (equal '("team-a" "team-b") (getf av :team)))
    (is (equal '("vault-admin") (getf av :vault)))
    (is (null (getf av :empty)))
    (is (equal '("low" "high") (getf av :priority)))))

(test rc-reader-vault-palette-scoped
  "The :vault palette for rc-reader shows only that user's rows
(view-level :scope :user)."
  (let ((av (allowed-values :tasks "rc-reader")))
    (is (equal '("vault-reader") (getf av :vault)))))

(test rc-owner-palette-suppressed-for-reader
  "Suppression contract: :users is admin-only
(allowed-values-test shape), so for rc-reader the :owner key is
present with value NIL and nothing signals; for admin it lists
user names."
  (let ((reader-av (allowed-values :tasks "rc-reader"))
        (admin-av (allowed-values :tasks "admin")))
    (is-true (member :owner reader-av))
    (is-false (getf reader-av :owner))
    (is (listp (getf admin-av :owner)))
    (is (member "admin" (getf admin-av :owner) :test 'equal))))

(test rc-palette-freshness
  "Freshness: removing a stage row's role drops its value from
rc-reader's next allowed-values call; re-adding restores it. No
caching across calls."
  (let ((delta-id (be-id :stages
                   '((:stages :name :eq "delta")) "admin")))
    (is-true delta-id)
    (unwind-protect
        (progn
          (a:add-resource-role *rbac*
            (id-to-resource-name delta-id) "rc-users")
          (is (equal '("bravo" "charlie" "alpha" "delta")
                  (getf (allowed-values :tasks "rc-reader") :stage)))
          (a:remove-resource-role *rbac*
            (id-to-resource-name delta-id) "rc-users")
          (is (equal '("bravo" "charlie" "alpha")
                  (getf (allowed-values :tasks "rc-reader") :stage))))
      ;; restore: delta ends admin-only again either way
      (a:remove-resource-role *rbac*
        (id-to-resource-name delta-id) "rc-users"))))

;;; ---------------------------------------------------------------------------
;;; Fast path: parity, selection, call counts, memo (plan steps 10-14)
;;; ---------------------------------------------------------------------------

(defun rc-palette-fields (type-key)
  "The allowed-values selection rule for TYPE-KEY: every field
with a :join-table, :target, or :ui :options (never :id)."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (and (or (getf field-def :join-table)
                (getf field-def :target)
                (u:tree-get field-def :ui :options))
           (not (equal field-key :id)))
    collect field-key))

(defun rc-parity-sweep (users)
  "For every type in *compiled-model*, every palette field, and
each user in USERS: whenever palette-fast-path-p holds,
fast-palette-values and legacy-palette-values are equal — order
included — or both signal validation-error."
  (loop for type-key in (u:plist-keys *compiled-model*)
    append (rc-parity-sweep-type type-key users)))

(defun rc-parity-sweep-type (type-key users)
  (loop for field-key in (rc-palette-fields type-key)
    append (loop for user in users
             collect (cons (list type-key field-key user)
                     (rc-parity-one type-key field-key user)))))

(defun rc-parity-one (type-key field-key user)
  "One parity comparison. Returns T when the two paths agree (or
are not comparable because the fast path does not apply); the
fiveam is lives in the caller so failures name the field."
  (let* ((source (or (u:tree-get *compiled-model* type-key :fields
                      field-key :source-all)
                   (u:tree-get *compiled-model* type-key :fields
                     field-key :source)))
         (x-type-key (getf source :table))
         (x-field-key (getf source :column)))
    (if (and x-type-key x-field-key
          (palette-fast-path-p x-type-key x-field-key))
      (multiple-value-bind (fast fast-err)
          (ignore-errors (fast-palette-values x-type-key x-field-key
                           user))
        (multiple-value-bind (legacy legacy-err)
            (ignore-errors (legacy-palette-values x-type-key
                             x-field-key user))
          (or (and (null fast-err) (null legacy-err)
                (equal fast legacy))
            (and fast-err legacy-err))))
      t)))

(test rc-parity-sweep-render-cost
  "The parity sweep on the render-cost fixture: every palette
field of every type, for admin, rc-reader, and guest — wherever
the fast path applies, it returns what the legacy path returns,
order included, or both deny."
  (let ((failures
          (loop for (spec . ok) in (rc-parity-sweep
                                     '("admin" "rc-reader" "guest"))
            unless ok collect spec)))
    (is (null failures)
      "fast/legacy divergence at: ~s" failures)))

(test rc-path-selection
  "palette-fast-path-p selects exactly the guarded shapes: true
for :stages :name and :labels :title; false for :teams :name
(joined default sort), :vaults :name (view scope), :users
:password (type whitelist), :stages :note (display field left
out of the target's :list-form), and both :cross display fields
(cross-sourced boolean / timestamp columns)."
  (is-true (palette-fast-path-p :stages :name))
  (is-true (palette-fast-path-p :labels :title))
  (is-false (palette-fast-path-p :teams :name))
  (is-false (palette-fast-path-p :vaults :name))
  (is-false (palette-fast-path-p :users :password))
  (is-false (palette-fast-path-p :stages :note))
  (is-false (palette-fast-path-p :cross :flag-text))
  (is-false (palette-fast-path-p :cross :stamp-text)))

(test rc-cross-palettes-carry-converted-values
  "The cross-source guard's payoff: the :cross-flag and
:cross-stamp palettes show the legacy path's converted values —
keyword booleans and ISO timestamp strings — never the raw
column text the fast path would have read."
  (let ((av (allowed-values :tasks "admin")))
    (is (equal '(:false :true)
            (sort (copy-list (getf av :cross-flag))
              #'string< :key #'princ-to-string)))
    ;; the NULL stamp of c-two reads as a NIL element (the legacy
    ;; record value); the set holds it plus the one ISO string
    (is (equal '("2026-01-02T03:04:05")
            (remove nil (getf av :cross-stamp))))
    (is (member nil (getf av :cross-stamp)))))

(test rc-be-list-column-called-only-for-legacy
  "Call counts: with be-list-column wrapped in a counting stub
that records its arguments, allowed-values for :tasks (admin)
enters it only for the legacy palettes — :teams, :vaults, and
the two :cross display columns — never for :stages, :labels,
:empties, or :users."
  (let ((calls nil)
        (real-blc (symbol-function 'data-ui::be-list-column)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::be-list-column
            (lambda (type-key field-key user &rest args)
              (push (list type-key field-key) calls)
              (apply real-blc type-key field-key user args))))
      (let ((av (allowed-values :tasks "admin")))
        (declare (ignore av))
        (is (equal '((:teams :name) (:vaults :name)
                     (:cross :flag-text) (:cross :stamp-text))
                (nreverse calls)))))))

(test rc-memo-shares-read-same-target
  "Memo: two fields with one target and display column share one
read — palette-values runs once for :empty/:empty-2 (both
(:empties :name)) and once for :owner/:owner-2 (both
(:users :name)) in one allowed-values call."
  (let ((calls nil)
        (real-pv (symbol-function 'data-ui::palette-values)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::palette-values
            (lambda (x-type-key x-field-key user)
              (push (list x-type-key x-field-key) calls)
              (funcall real-pv x-type-key x-field-key user))))
      (let ((av (allowed-values :tasks "admin")))
        (declare (ignore av))
        (is (= 1 (count '(:empties :name) calls :test #'equal)))
        (is (= 1 (count '(:users :name) calls :test #'equal)))
        (is (= 1 (count '(:stages :name) calls :test #'equal)))
        (is (= 1 (count '(:labels :title) calls :test #'equal)))))))

(test rc-memo-empty-palette-still-memoized
  "An empty palette is a successful read and is memoized: the
:empty-2 field triggers no second read (tested by the single
(:empties :name) call count above); this test pins the mechanism
directly — the second gethash finds the stored NIL."
  (let ((memo (make-hash-table :test #'equal))
        (calls 0)
        (real-pv (symbol-function 'data-ui::palette-values)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::palette-values
            (lambda (x-type-key x-field-key user)
              (incf calls)
              (funcall real-pv x-type-key x-field-key user))))
      (is (null (allowed-values-for-field :tasks :empty "admin"
                    memo)))
      (is (= 1 calls))
      (is (null (allowed-values-for-field :tasks :empty-2 "admin"
                    memo)))
      (is (= 1 calls)))))

(test rc-memo-denied-palette-not-memoized
  "A denied palette is never memoized: for rc-reader both
:owner and :owner-2 run their own read and log their own
suppression (each signals validation-error; the handler returns
NIL without storing)."
  (let ((calls nil)
        (real-pv (symbol-function 'data-ui::palette-values)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::palette-values
            (lambda (x-type-key x-field-key user)
              (push (list x-type-key x-field-key) calls)
              (funcall real-pv x-type-key x-field-key user))))
      (let ((av (allowed-values :tasks "rc-reader")))
        (is-true (member :owner av))
        (is-false (getf av :owner))
        (is-true (member :owner-2 av))
        (is-false (getf av :owner-2))
        ;; only the denied :users reads count here — the other
        ;; palette fields run their own (successful or empty)
        ;; reads, but (:users :name) appears exactly twice: once
        ;; per denied field, never shared through the memo
        (is (= 2 (count '(:users :name) calls :test #'equal)))))))



;;; ---------------------------------------------------------------------------
;;; permitted-row-ids: the narrow harvest (plan steps 15-18)
;;; ---------------------------------------------------------------------------

(test rc-permitted-row-ids-readable-sets
  "permitted-row-ids returns exactly the readable stage ids for
rc-reader (admin-only delta excluded) and all of them for admin;
the 'update' and 'delete' answers differ from 'read' where the
fixture's roles do (rc-users carries read only)."
  (let* ((readable (remove nil
                     (mapcar
                       (lambda (name)
                         (be-id :stages
                           `((:stages :name :eq ,name)) "admin"))
                       '("alpha" "bravo" "charlie"))))
         (all (append readable
                  (list (be-id :stages
                          '((:stages :name :eq "delta")) "admin"))))
         (reader-read (permitted-row-ids :stages "rc-reader" "read"))
         (admin-read (permitted-row-ids :stages "admin" "read")))
    (is (equal (u:safe-sort readable) (u:safe-sort reader-read)))
    (is (equal (u:safe-sort all) (u:safe-sort admin-read)))
    ;; rc-users (ensure-declared-roles default) carries all four
    ;; permissions, so update and delete answer the same set; a
    ;; user with no roles answers NIL for all three
    (is (equal (u:safe-sort readable)
            (u:safe-sort (permitted-row-ids :stages "rc-reader"
                            "update"))))
    (is (equal (u:safe-sort readable)
            (u:safe-sort (permitted-row-ids :stages "rc-reader"
                            "delete"))))
    (is (null (permitted-row-ids :stages "rc-nobody" "read")))
    (is (null (permitted-row-ids :stages "rc-nobody" "update")))))

(test rc-permitted-row-ids-distinct-and-paged
  "A row reachable through two of the user's roles appears once,
and binding a:*default-page-size* to 2 does not change the
result (the loop is correct at any page size)."
  (let* ((id (be-id :stages '((:stages :name :eq "alpha"))
                  "admin"))
         (resource (id-to-resource-name id))
         (full (permitted-row-ids :stages "rc-reader" "read")))
    (unwind-protect
        (progn
          ;; a second role on the same row: rc-reader already holds
          ;; rc-users via the insert; add a second role that also
          ;; carries read
          (a:add-role *rbac* "rc-second"
            :permissions '("read"))
          (a:add-user-role *rbac* "rc-reader" "rc-second")
          (a:add-resource-role *rbac* resource "rc-second")
          (is (equal (u:safe-sort full)
                  (u:safe-sort (permitted-row-ids :stages
                                  "rc-reader" "read"))))
          (let ((a:*default-page-size* 2))
            (is (equal (u:safe-sort full)
                    (u:safe-sort (permitted-row-ids :stages
                                    "rc-reader" "read"))))))
      (a:remove-resource-role *rbac* resource "rc-second")
      (a:remove-user-role *rbac* "rc-reader" "rc-second"))))

;;; ---------------------------------------------------------------------------
;;; Public decoration suite (public-pages-test fixture; the batch
;;; decoration and call-count tests of plan step 9 land here)
;;; ---------------------------------------------------------------------------

(def-suite render-cost-public-suite
  :description "Batch :public-url decoration tests on the
public-pages-test fixture (list reads decorate once, be-rec keeps
its fresh single-row check)")

(in-suite render-cost-public-suite)

(test rc-parity-sweep-public-pages
  "The parity sweep on the public-pages fixture (admin and
guest): every palette field of every type — wherever the fast
path applies, it matches the legacy path."
  (let ((failures
          (loop for (spec . ok) in (rc-parity-sweep '("admin" "guest"))
            unless ok collect spec)))
    (is (null failures)
      "fast/legacy divergence at: ~s" failures)))

(test rc-links-palette-one-read-two-fields
  "On :links, whose two fields both target :posts through the
same display column: one allowed-values call runs the palette
reader once and both fields receive equal values."
  (let ((calls 0)
        (real-pv (symbol-function 'data-ui::palette-values)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::palette-values
            (lambda (x-type-key x-field-key user)
              (incf calls)
              (funcall real-pv x-type-key x-field-key user))))
      (let ((av (allowed-values :links "admin")))
        (is (= 1 calls))
        (is (equal (getf av :first) (getf av :second)))
        (is (plusp (length (getf av :first))))))))

(test rc-public-type-p-called-once-per-list
  "Batch decoration: with public-type-p wrapped in a counting stub
that delegates to the real function, a be-list of :posts calls it
exactly once whatever the record count, and user-allowed-resource
is never called from the list path."
  (let ((calls 0) (uar-calls 0)
        (real-ptp (symbol-function 'data-ui::public-type-p))
        (real-uar (symbol-function 'data-ui::user-allowed-resource)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::public-type-p
            (lambda (type-key)
              (incf calls)
              (funcall real-ptp type-key)))
          (list 'data-ui::user-allowed-resource
            (lambda (&rest args)
              (incf uar-calls)
              (apply real-uar args))))
      (let ((result (be-list :posts "admin" :form :list-form
                      :limit 100 :skip-allowed-values t)))
        (declare (ignore result))
        (is (= 1 calls))
        (is (= 0 uar-calls))))))

(test rc-public-url-on-public-records-only
  "Public posts carry :public-url and the draft does not — key
membership through u:plist-keys, never getf."
  (let ((*public-origin* *pp-origin*))
    (let* ((ids (pp-ids))
           (listing (getf (be-list :posts "admin" :form :list-form
                            :limit 100)
                       :records))
           (public (find (getf ids :commented) listing
                      :key (lambda (r) (getf r :id)) :test #'equal))
           (draft (find (getf ids :draft) listing
                     :key (lambda (r) (getf r :id)) :test #'equal)))
      (is (member :public-url (u:plist-keys public)))
      (is (equal (format nil "~a/p/posts/~a" *pp-origin*
                    (getf ids :commented))
            (getf public :public-url)))
      (is (null (member :public-url (u:plist-keys draft)))))))

(test rc-empty-id-set-omits-key-no-uar
  "Supplied-but-empty: with permitted-row-ids stubbed to return NIL
for the guest, no record carries the key and
user-allowed-resource is never called (the batch function treats
an empty id set as 'decorate nobody', not 'ask per row')."
  (let ((uar-calls 0)
        (real-pri (symbol-function 'data-ui::permitted-row-ids))
        (real-uar (symbol-function 'data-ui::user-allowed-resource)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::permitted-row-ids
            (lambda (type-key user permission)
              ;; stub only the GUEST harvest the decorator makes;
              ;; the acting user's own row filter passes through
              (if (equal user *guest-user*)
                nil
                (funcall real-pri type-key user permission))))
          (list 'data-ui::user-allowed-resource
            (lambda (&rest args)
              (incf uar-calls)
              (apply real-uar args))))
      (let ((*public-origin* *pp-origin*)
            (listing (getf (be-list :posts "admin" :form :list-form
                              :limit 100 :skip-allowed-values t)
                       :records)))
        (is (plusp (length listing)))
        (is (null (find :public-url
                     (mapcar #'u:plist-keys listing)
                     :test #'member)))
        (is (= 0 uar-calls))))))

(test rc-non-public-type-records-unchanged
  "A list read of a type for which public-type-p is false returns
its records unchanged, with one public-type-p call."
  (let ((calls 0)
        (real-ptp (symbol-function 'data-ui::public-type-p)))
    (th-atr-with-stubs
        (list
          (list 'data-ui::public-type-p
            (lambda (type-key)
              (incf calls)
              (funcall real-ptp type-key))))
      (let ((note-id (be-insert :notes '(:note "rc-note") "admin"
                        :roles '("public"))))
        (unwind-protect
            (let ((listing (be-list :notes "admin" :form :list-form
                             :limit 100)))
              (is (plusp (length (getf listing :records))))
              (is (null (find :public-url
                           (mapcar #'u:plist-keys
                             (getf listing :records))
                           :test #'member)))
              (is (= 1 calls)))
          (be-delete :notes note-id "admin"))))))

(test rc-wire-suite-regression
  "The existing public-pages-wire-suite runs unmodified as the
regression bar: list and item agreement, row-level and type-level
revocation, the invalid-origin case."
  (is (fiveam:run 'public-pages-wire-suite)))

