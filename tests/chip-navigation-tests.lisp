(in-package :data-ui)

(def-suite chip-navigation-suite
  :description "Chip-to-record navigation: the fe-fields :record-lookup
wire key on join-table fields (the /api/value-id call that resolves one
chip value to its target row) and the resolution legs the frontend's
highlighted-chip lookup consumes — resolve, miss, RBAC fail-closed, and
more-than-one-match ambiguity — pinned at the backend (be-value-id) and
on the wire (/api/value-id and /api/id: the ambiguity 500 and the empty
miss, through the direct-handler harness from
api-type-roles-access-tests.lisp).")

(in-suite chip-navigation-suite)

(defun cn-delete-user (name)
  "Delete the user NAME when present (a nil id would signal in
be-delete) — suite-local cleanup, safe in an unwind-protect cleanup
form whether or not the user was created."
  (let ((uid (be-value-id :users :name name "admin")))
    (when uid (be-delete :users uid "admin"))))

;;; ---------------------------------------------------------------------------
;;; Wire emission: the :record-lookup key (fe-fields)
;;; ---------------------------------------------------------------------------

(test chip-nav-record-lookup-emission
  "fe-fields emits :record-lookup on a join-table list field: the
joined target type (:type, a keyword) and the target-side display
column (:field, a downcased string) — the exact /api/value-id
parameters that resolve one chip value. getf on the nested plist,
never equal on the whole wire entry (the entry carries unrelated
keys)."
  (let* ((fe (fe-fields :items "admin"))
         (tags-lookup (u:tree-get fe :list-form :tags :record-lookup))
         (asg-lookup (u:tree-get fe :list-form :assignees
                       :record-lookup)))
    (is (eq :tags (getf tags-lookup :type)))
    (is (equal "name" (getf tags-lookup :field)))
    (is (eq :users (getf asg-lookup :type)))
    (is (equal "name" (getf asg-lookup :field)))))

(test chip-nav-record-lookup-absent
  "A non-relation field carries no :record-lookup key at all — the
emission is join-table-gated, and a present nil would serialize as
\"record-lookup\":[] (truthy in JS). member, not getf (the
reference-chip-wire-column-emission idiom)."
  (let ((name-entry (u:tree-get (fe-fields :items "admin")
                     :list-form :name)))
    (is-false (member :record-lookup name-entry)
      "non-relation wire entry must not carry :record-lookup at all")))

(test chip-nav-record-lookup-base-roles
  "The base model's real :users :roles field (the join through
:role-users) carries the lookup too — the emission needs nothing
beyond the base model, so this leg runs on any fixture."
  (let ((roles-lookup (u:tree-get (fe-fields :users "admin")
                       :list-form :roles :record-lookup)))
    (is (eq :roles (getf roles-lookup :type)))
    (is (equal "name" (getf roles-lookup :field)))))

(test chip-nav-no-target-key-on-m2m
  "This suite's own guard that the new key did not become the
reference family's key: a join-table field still carries no :target
at all (duplicating m2m-field-no-sibling-no-wire-key on purpose)."
  (let ((tags-entry (u:tree-get (fe-fields :items "admin")
                     :list-form :tags)))
    (is-false (member :target tags-entry)
      "M2M wire entry must not carry :target at all")))

;;; ---------------------------------------------------------------------------
;;; Resolution: be-value-id (the /api/value-id backend)
;;; ---------------------------------------------------------------------------

(test chip-nav-value-id-resolves
  "The lookup a highlighted M2M chip issues: a seeded tag resolves to
its UUID string as admin; an unknown value is NIL (a miss on the wire
is result:[], never an error — the frontend renders no arrow)."
  (let ((red (be-value-id :tags :name "red" "admin")))
    (is-true (uuid-p red) "expected a UUID string for the seeded tag")
    (is-false (be-value-id :tags :name "nope" "admin")
      "unknown value resolves to NIL, not an error")))

(test chip-nav-value-id-fails-closed-rbac
  "The row gate in be-id: a roleless reader gets NIL on a tag row
private to admin (an unreadable row leaks nothing), while a member of
the row's granted role resolves it. Cleanup runs in unwind-protect —
a signal mid-test must not leak the tag or the cn-* users into later
tests (a leaked user breaks th-make-user on the next run)."
  (let ((open-id nil))
    (unwind-protect
        (progn
          (setf open-id (be-insert :tags '(:name "cn-open") "admin"
                            :roles '("m2m-users")))
          (th-make-user "cn-reader")
          (is-false (be-value-id :tags :name "red" "cn-reader")
            "private row invisible to a roleless reader")
          (th-make-user "cn-m2m" :roles '("m2m-users"))
          (is (equal open-id
                (be-value-id :tags :name "cn-open" "cn-m2m"))
            "row granted to m2m-users resolves for a member"))
      (when open-id (be-delete :tags open-id "admin"))
      (cn-delete-user "cn-reader")
      (cn-delete-user "cn-m2m"))))

(test chip-nav-value-id-ambiguous-signals
  "Two records sharing a display value fail closed as an error — the
more-than-one-match contract the frontend's ambiguity rule relies on
(the 500 is read as a miss, no arrow). Both users pass the shared
email explicitly: an invalid address would signal inside th-make-user
before the assertion runs, and the default \"no-email\" already
belongs to the fixture's own users. Cleanup in unwind-protect so a
signal cannot leak the cn-* users into later tests."
  (unwind-protect
      (progn
        (th-make-user "cn-a" :email "cn-shared@example.com")
        (th-make-user "cn-b" :email "cn-shared@example.com")
        (signals error
          (be-value-id :users :email "cn-shared@example.com"
            "admin")))
    (cn-delete-user "cn-a")
    (cn-delete-user "cn-b")))

;;; ---------------------------------------------------------------------------
;;; REST wire contract (direct-handler harness from
;;; api-type-roles-access-tests.lisp): the ambiguity 500 and the
;;; empty miss the chip lookup reads
;;; ---------------------------------------------------------------------------

(test chip-nav-rest-value-id-ambiguous-500
  "/api/value-id pins the ambiguity half of the wire contract: two
records sharing the lookup value abort with 500 and \"More than one
match.\" — the response the frontend reads as a miss (no arrow),
never a wrong record. The harness runs the real handler with only
the transport stubbed; the no-abort case fails the test, not just
the code checks inside the handler-case."
  (unwind-protect
      (progn
        (th-make-user "cn-a" :email "cn-shared@example.com")
        (th-make-user "cn-b" :email "cn-shared@example.com")
        (let ((abort
                (handler-case
                    (progn
                      (th-atr-rest-value-id :type "users"
                        :field "email"
                        :value "cn-shared@example.com")
                      nil)
                  (atr-aborted (e)
                    (cons (atr-aborted-code e)
                      (atr-aborted-message e))))))
          (is-true abort
            "expected /api/value-id to abort on ambiguity")
          (when abort
            (is (= h:+http-internal-server-error+ (car abort))
              "ambiguity is a 500")
            (is (search "More than one match." (cdr abort))
              "message names the ambiguity"))))
    (cn-delete-user "cn-a")
    (cn-delete-user "cn-b")))

(test chip-nav-rest-id-ambiguous-500
  "/api/id pins the same ambiguity contract through its filter form:
JSON filters selecting both shared-email users abort with 500 and
\"More than one match.\" (be-id under the handler — the same
backend, the other route)."
  (let ((filters
          "[[\"users\",\"email\",\"eq\",\"cn-shared@example.com\"]]"))
    (unwind-protect
        (progn
          (th-make-user "cn-a" :email "cn-shared@example.com")
          (th-make-user "cn-b" :email "cn-shared@example.com")
          (let ((abort
                  (handler-case
                      (progn
                        (th-atr-rest-id :type "users"
                          :filters filters)
                        nil)
                    (atr-aborted (e)
                      (cons (atr-aborted-code e)
                        (atr-aborted-message e))))))
            (is-true abort
              "expected /api/id to abort on ambiguity")
            (when abort
              (is (= h:+http-internal-server-error+ (car abort))
                "ambiguity is a 500")
              (is (search "More than one match." (cdr abort))
                "message names the ambiguity"))))
      (cn-delete-user "cn-a")
      (cn-delete-user "cn-b"))))

(test chip-nav-rest-value-id-miss-and-unreadable-empty
  "/api/value-id pins the empty half of the wire contract: no match,
and a row the caller cannot read, both answer a success envelope
with an empty result — never an error, never a leak. This is the
exact shape the chip lookup parses (nil serializes as [], which is
truthy in JS — the frontend type-checks result); render-json t, so
the assertions pin the actual JSON output. The seeded tags are
admin-private (seed-m2m-fixture inserts them without roles) and
guest holds only public / guest:exclusive, so \"red\" exercises the
row gate."
  (let ((json (th-atr-rest-value-id :type "tags" :field "name"
                 :value "nope" :render-json t)))
    (is (search "\"status\":\"success\"" json)
      "a miss is a success envelope")
    (is (search "\"result\":[]" json)
      "a miss is an empty result"))
  (let ((json (th-atr-rest-value-id :type "tags" :field "name"
                 :value "red" :token "atr-token-guest"
                 :render-json t)))
    (is (search "\"status\":\"success\"" json)
      "an unreadable row is a success envelope")
    (is (search "\"result\":[]" json)
      "an unreadable row is an empty result")))
