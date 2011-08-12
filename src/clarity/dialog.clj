(ns clarity.dialog
  (:require [clarity.component :as c])
  (:import [java.awt BorderLayout FlowLayout]))

(defn make-button [dialog button]
  (let [text (if (string? button)
               button
               button)] ;;TODO handle keywords
    (c/make :button button
            [:category :dialog-button]
            [:on-click (.dispose dialog)])))

(defn make-button-panel [dialog buttons]
  (let [panel (c/make :panel [:category :dialog-button-panel])]
    (.setLayout panel (FlowLayout.))
    (doall (map #(.add panel (make-button dialog %)) buttons))
    panel))

(defn dialog [content buttons]
  {:pre [(instance? java.awt.Component content)
         (or (vector? buttons)
             (map? buttons))]}
  (if (instance? clarity.style.Styleable content)
    (dosync (.addCategory content :dialog-content-panel)))
  (let [dialog (c/make :dialog [:category :dialog])]
    (doto dialog
      (.setLayout (BorderLayout.))
      (.add content BorderLayout/CENTER)
      (.add (make-button-panel dialog buttons) BorderLayout/SOUTH)
      (.pack))))

(defn show-dialog [content buttons]
  (.setVisible (dialog content buttons) true))

;;example
#_(show-dialog
   (clarity.form/form [:header "The form"]
                      [:text "Please fill in the **entire** form." :rich]
                      :first-name ""
                      :surname ""
                      :gender ["male" "female"])
   ["OK" "Cancel"])