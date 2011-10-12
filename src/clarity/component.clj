(ns
    ^{:doc "The core of the clarity library. The two main functions
    are make and do-component."
      :author "Stathis Sideris"}
  clarity.component
  (:require [clojure.string :as str]
            [clojure.contrib.str-utils2 :as str2]
            [clarity.event :as event]
            [clarity.style :as style])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane JEditorPane JFileChooser]
           [javax.swing.text.html HTMLEditorKit]
           [com.petebevin.markdown MarkdownProcessor]))

(defprotocol Component
  (get-id [this])
  (categories [this])
  (add-category [this c])
  (remove-category [this c]))

(defn component-mixin []
  ;;TODO really ref?
  (let [cat (ref #{})]
    {"categories" (fn [this] (deref cat))
     "add_category" (fn [this c] (alter cat conj c))
     "remove_category" (fn [this c] (alter cat disj c))}))

(defn component?
  "Tests whether x satisfies the clarity.component.Component
  protocol."
  [x] (satisfies? Component x))

(defn id
  "Get the identifier of the passed component. Returns nil if there is
  an exception."
  [c]
  (try
    (get-id c)
    (catch Exception e nil)))

(def special-setters #{:init :id :category :categories})

(defn has-category
  "Check whether the component has the passed category."
  [comp category]
  (try
    (contains? (categories comp) category)
    (catch Exception e false)))

;;the rest of the namespace has been split over the following files
(load "component_protocols")
(load "component_make")
(load "component_widgets")
