(ns examples.reactive-form
  (:require [clarity.component :as c]
            [clarity.structure :as s]
            [clarity.event :as event]
            [clarity.style :as style]
            [clarity.dev :as dev]
            [clarity.form :as form]))

(def the-form (form/form
               :first-name ""
               :surname ""
               :gender ["Male" "Female" "Other"]
               :employed false))

(def back-map
  (into {}
        (map (fn [c] [c (.getBackground c)])
             (s/$ the-form .field))))

(def normal-font (style/font))
(def bold-font (style/font :style :bold))

(doseq [component (s/$ the-form .field)]
  (c/do-component
   component
   (:on-mouse-over
    (.setBackground this (style/color 0xE3EDFA))
    (.setFont (s/previous-sibling this) bold-font))
   (:on-mouse-out
    (.setBackground this (get back-map this))
    (.setFont (s/previous-sibling this) normal-font))))

(dev/show-comp the-form)
