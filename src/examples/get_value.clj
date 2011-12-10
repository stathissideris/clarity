(ns examples.get-value
  (:require [clarity.component :as c]
            [clarity.dev :as dev]
            [clarity.layout :as layout]))

(def panel
  (layout/mig
   (c/make :panel)
   :layout [:wrap 2]
   :column "[left][grow,fill]"

   (c/make :label "Name")
   (c/make :text-field "Stathis" (:id :name))

   (c/make :label "Surname")
   (c/make :text-field "Sideris" (:id :surname))

   (c/make :label "Sub-panel")
   (c/make :panel (:id :sub-panel)
           (.add (c/make :button "Test button")))
   
   (c/make :label "Gender")
   (c/make :combo-box
           (:init (into-array String ["male" "female"]))
           (:id :gender))))

(dev/show-comp panel)

(c/value panel)
