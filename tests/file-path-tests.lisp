(in-package :data-ui)

(def-suite file-path-suite
  :description "Compile-time tests for the :file-path widget")

(def-suite file-path-compile
  :in file-path-suite
  :description "Pure compile-model probes (no DB)")

(def-suite file-path-wire
  :in file-path-suite
  :description "fe-fields wire emission (needs a live model)")

(in-suite file-path-compile)

(defun fp-compile (&rest types)
  "Compile a model whose :types plist is TYPES and return the result."
  (apply #'compile-model types))

(defun fp-file-types (&key (name-ui '(:label "File" :widget :textbox))
                        (dir-type :missing))
  "Build a two-type model (:d directories + :f files). NAME-UI is the
:ui plist for the :f :name field. DIR-TYPE is the :directory-type value
for that field's :ui (:missing leaves the key out entirely)."
  (flet ((with-dir-type (ui)
           (if (eq dir-type :missing)
             ui
             (add-to-plist ui (list :directory-type dir-type)))))
    `(:d
       (:table t
         :create :auto :update :auto :delete :auto
         :tree t :is-leaf nil :parent-type :d :fs-backed t
         :type-roles ("fp-users")
         :views (:main (:tables (:d)))
         :fields
         (:name (:type :text :identity t :path t
                  :ui (:label "Directory" :widget :textbox)
                  :source (:view :main :column :name :agg :first)
                  :column t :not-null t :unique t))
         :list-form (:fields t)
         :update-form (:fields t)
         :add-form (:fields t))
       :f
       (:table t
         :create :auto :update :auto :delete :auto
         :tree t :is-leaf t :parent-type :d :fs-backed t
         :type-roles ("fp-users")
         :views (:main (:tables (:f)))
         :fields
         (:name (:type :text :identity t :path t
                  :ui ,(with-dir-type name-ui)
                  :validations (:required)
                  :source (:view :main :column :name :agg :first)
                  :column t :not-null t :unique t)
           :file (:type :file
                   :ui (:label "Select File" :widget :file)
                   :validations (:required)))
         :list-form (:fields t)
         :update-form (:fields t)
         :add-form (:fields t)))))

(test file-path-compiles
  ":widget :file-path with :directory-type :d compiles, and the
compiled :ui carries both keys."
  (let* ((compiled (fp-compile
                     (fp-file-types
                       :name-ui '(:label "File" :widget :file-path)
                       :dir-type :d)))
          (ui (u:tree-get compiled :f :fields :name :ui)))
    (is (eq :file-path (getf ui :widget)))
    (is (eq :d (getf ui :directory-type)))))

(test file-path-missing-directory-type
  ":widget :file-path without :directory-type fails compile."
  (signals error
    (fp-compile
      (fp-file-types :name-ui '(:label "File" :widget :file-path)))))

(test file-path-unknown-directory-type
  ":directory-type naming a type not in the model fails compile."
  (signals error
    (fp-compile
      (fp-file-types
        :name-ui '(:label "File" :widget :file-path)
        :dir-type :no-such-type))))

(test directory-type-on-textbox
  ":directory-type with :widget :textbox fails compile."
  (signals error
    (fp-compile
      (fp-file-types
        :name-ui '(:label "File" :widget :textbox)
        :dir-type :d))))

(test directory-type-non-keyword
  ":directory-type not a keyword fails compile."
  (signals error
    (fp-compile
      (fp-file-types
        :name-ui '(:label "File" :widget :file-path)
        :dir-type "directories"))))

(in-suite file-path-wire)

(test file-path-fe-fields-emission
  "fe-fields passes :directory-type through on the wire (as part of
the verbatim :ui plist). Runs on the file-path-test fixture."
  (let* ((fields (fe-fields :files "admin"))
          (name-ui (getf (getf fields :add-form) :name)))
    (is (eq :file-path (getf name-ui :widget)))
    (is (eq :directories (getf name-ui :directory-type)))))
