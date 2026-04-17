(def-suite predicate-tests :description "Predicate tests")

(in-suite predicate-tests)

(test uuid-p
  (is-true (uuid-p "b94f1905-464d-4da6-b6c8-878b9d014fc9")))
