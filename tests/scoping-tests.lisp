(in-package :data-ui)

(def-suite scoping-suite :description "Scoping tests")

(in-suite scoping-suite)

(test view-scope
  (is (equal
        ':user
        (u:tree-get *compiled-model* :images :views :main :scope))))

