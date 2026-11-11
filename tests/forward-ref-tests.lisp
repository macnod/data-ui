(in-package :data-ui)

(def-suite forward-ref-suite
  :description "Forward references — dependency-ordered table
creation (create-tables / type-ddl-order): a type may target a type
declared later; a dependency cycle is a named report-e.")

(in-suite forward-ref-suite)

;;; ---------------------------------------------------------------------------
;;; End-to-end: the fixture loads (the regression pin)
;;; ---------------------------------------------------------------------------

(test forward-ref-model-loads
  "The fixture declares its referrers first — an ordinary FK type and
an internal joiner, both targeting later-declared types — so
with-model's set-model exercises dependency-ordered creation end to
end. All four tables (the joiner's included) must exist; without the
ordering, the suite's own fixture load dies on Postgres 42P01 at the
first forward CREATE TABLE."
  (is-true (table-exists "rt_assignments"))
  (is-true (table-exists "rt_work_tasks"))
  (is-true (table-exists "rt_workers"))
  (is-true (table-exists "rt_tasks")))

;;; ---------------------------------------------------------------------------
;;; Pure unit tests: type-ddl-order on synthetic plists
;;; ---------------------------------------------------------------------------

(test forward-ref-order-unit
  "type-ddl-order is pure and declaration-stable: an absent target
table creates an edge (target created first); a present target
creates none (declaration order unchanged); a no-edge model returns
declaration order as-is."
  (let ((model '(:a (:table-name "rt_a"
                    :fields (:ref (:target :b)))
                  :b (:table-name "rt_b"
                    :fields (:name (:type :text))))))
    ;; B's table absent: A's edge defers it — target first.
    (is (equal '(:b :a) (type-ddl-order model nil)))
    ;; B's table present: the edge vanishes — declaration order.
    (is (equal '(:a :b) (type-ddl-order model '("rt_b")))))
  ;; No edges anywhere: declaration order.
  (is (equal '(:a :b)
    (type-ddl-order
      '(:a (:fields (:name (:type :text)))
        :b (:fields (:name (:type :text))))
      nil))))

(test forward-ref-cycle-signals
  "A dependency cycle (A targets B, B targets A) signals a named
report-e before any DDL runs — not a mystery 42P01 partway through
the walk."
  (error-matches
    (type-ddl-order
      '(:a (:table-name "rt_a"
             :fields (:ref (:target :b)))
        :b (:table-name "rt_b"
             :fields (:ref (:target :a))))
      nil)
    "cycle.*:A.*:B"
    "type-ddl-order must signal on a dependency cycle, naming the types"))

(test forward-ref-self-target-ok
  "A self-targeting field (e.g. a tree :parent-type edge) is legal
DDL in one CREATE statement — the self-edge is dropped, no cycle."
  (is (equal '(:a)
    (type-ddl-order
      '(:a (:table-name "rt_a"
             :fields (:parent (:target :a))))
      nil))))
