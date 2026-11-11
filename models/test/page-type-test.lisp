;; Purpose: Page type fixture — the worked :pages / :about pair from
;;   the page-type plan, plus the sibling pages (:other-page,
;;   :draft-page, :guide) that pin the boolean truthiness forms and
;;   the clock ride-along, and :blurb, the public-eligible ordinary
;;   control for the public-type-p discrimination probe. Top level
;;   carries :guest-allowed t and the widened :api-roles so a guest
;;   browser smoke (and the de-vacuated public-type-p test) can run
;;   — the guest lacks the immutable logged-in role that is the
;;   default gate on /api/types.
;; Author: GLM (via implement-data-ui-plan)
;; Created: [2026-11-10 Tue]
;; Prompt: (see ~/workbench/page-type-plan.org)
'(:title "Page Type Test"
  :name "page-type-test"
  :version "0.1"
  :domain "page-type-test.demo.data-ui.com"
  :repl t
  :guest-allowed t
  :api-roles ("logged-in" "public")
  :landing-page :about
  :types
  (:pages
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("editor")
      :fields
      (:slug
        (:type :text :identity t :not-null t
          :ui (:label "Slug" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :slug :agg :first)
          :column t)
       :title
        (:type :text :not-null t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t)
       :body
        (:type :text
          :ui (:label "Body" :widget :textarea)
          :source (:view :main :column :body :agg :first)
          :column t)
       ;; Nullable on purpose: the NULL-valued row is the
       ;; bare-form NULL-exclusion pin (:other-page reads 0 though
       ;; the row exists).
       :published
        (:type :boolean
          :ui (:label "Published" :widget :checkbox)
          :source (:view :main :column :published :agg :first)
          :column t))
      :list-form (:fields t)
      :add-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields (:title :body)))

  ;; The canonical page. :published :eq t is the boolean ride-along
  ;; (a bare-column truthiness form, no bind); :slug :eq is the
  ;; identity pin. The page's :fields deliberately declares :slug
  ;; while :view-form omits it — the projection pin asserts the
  ;; record does not carry :slug. The :title heading is the
  ;; canonical :widget :heading pairing (:hide-label t — the
  ;; markdown-body idiom; the widget is what removes the
  ;; property-sheet row, the hide-label keeps the label out of
  ;; other surfaces).
  :about
    (:page t
      :of :pages
      :filter ((:pages :slug :eq "about")
               (:pages :published :eq t))
      :type-roles ("public")
      :view-form (:fields (:title :body))
      :fields (:title (:ui (:label "Title" :widget :heading
                           :hide-label t))
               :body (:ui (:widget :markdown :hide-label t))
               :slug (:ui (:label "Slug"))))

  ;; The NULL-exclusion row: "other" exists with published SQL NULL,
  ;; so the bare-column form excludes it — total 0 though the row
  ;; exists (three-valued logic, the discriminating assertion).
  :other-page
    (:page t
      :of :pages
      :filter ((:pages :slug :eq "other")
               (:pages :published :eq t))
      :type-roles ("public")
      :view-form (:fields (:title :body))
      :fields (:title (:ui (:label "Title"))
               :body (:ui (:widget :markdown :hide-label t))))

  ;; The not-form row: "draft" exists with published false, so
  ;; (:published :eq nil) — the not-column form — selects it.
  :draft-page
    (:page t
      :of :pages
      :filter ((:pages :slug :eq "draft")
               (:pages :published :eq nil))
      :type-roles ("public")
      :view-form (:fields (:title :body))
      :fields (:title (:ui (:label "Title"))
               :body (:ui (:widget :markdown :hide-label t))))

  ;; The clock ride-along on an unprojected default field, and the
  ;; empty state: no "guide" row is seeded. 1825 is the validator's
  ;; maximum window (1..1825); any seeded row is inside it.
  :guide
    (:page t
      :of :pages
      :filter ((:pages :slug :eq "guide")
               (:pages :created-at :last-days 1825))
      :type-roles ("public")
      :view-form (:fields (:title :body))
      :fields (:title (:ui (:label "Title"))
               :body (:ui (:widget :markdown :hide-label t))))

  ;; The public-eligible control: an ordinary table type a page is
  ;; not. public-type-p must answer true here and false on :about
  ;; under the same :guest-allowed t — the exclusion is the only
  ;; failing leg.
  :blurb
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :fields
      (:text
        (:type :text :identity t
          :ui (:label "Text" :widget :textbox)
          :source (:view :main :column :text :agg :first)
          :column t :not-null t))
      :list-form (:fields t)
      :add-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t))))
