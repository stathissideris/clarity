(ns clarity.dialog
  (:require [clarity.component :as c])
  (:import [java.awt BorderLayout FlowLayout]))

(def response-map
     {:ok "OK"
      :cancel "Cancel"
      :yes "Yes"
      :no "No"
      :submit "Submit"})

(def ok [:ok])
(def submit [:submit])
(def ok-cancel [:ok :cancel])
(def yes-no [:yes :no])
(def yes-no-cancel [:yes :no :cancel])

(defn make-button [dialog button response]
  (let [text (if (string? button)
               button
               (get response-map button))
        id (if (keyword? button) button nil)]
    (if (nil? id)
      (c/make :button text
              [:category :dialog-button]
              [:on-click
               (reset! response (.getText this))
               (.dispose dialog)])
      (c/make :button text
              [:id id]
              [:category :dialog-button]
              [:on-click
               (reset! response id)
               (.dispose dialog)]))))

(defn make-button-panel [dialog buttons response]
  (let [panel (c/make :panel [:category :dialog-button-panel])]
    (.setLayout panel (FlowLayout.))
    (doall (map #(.add panel (make-button dialog % response)) buttons))
    panel))

(defn dialog [content buttons response]
  {:pre [(instance? java.awt.Component content)
         (or (vector? buttons)
             (map? buttons))]}
  (if (instance? clarity.style.Styleable content)
    (dosync (.addCategory content :dialog-content-panel)))
  (let [dialog (c/make :dialog [:category :dialog])]
    (doto dialog
      (.setLayout (BorderLayout.))
      (.add content BorderLayout/CENTER)
      (.add (make-button-panel dialog buttons response) BorderLayout/SOUTH)
      (.setModal true)
      (.pack))))

(defn show-dialog
  ([content]
     (show-dialog content [:ok]))
  ([content buttons]
     (let [response (atom :closed)]
       (.setVisible (dialog content buttons response) true)
       {:value (c/value content) :response @response})))

;;example
#_(show-dialog
   (clarity.form/form [:header "The form"]
                      [:text "Please fill in the **entire** form." :rich]
                      :first-name ""
                      :surname ""
                      :gender ["male" "female"])
   ["OK" "Cancel"])