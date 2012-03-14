(ns
    ^{:doc "The core of the clarity library. The two main functions
    are make and do-component."
      :author "Stathis Sideris"}
  clarity.component
  (:require [clarity.util :as util]
            [clarity.event :as event]))

(cond (util/clojure-1-2?) (require '[clojure.contrib.str-utils2 :as str])
      (util/clojure-1-3?) (require '[clojure.string :as str]))

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

(defn iobj-mixin []
  (let [m (atom {})]
    {"meta" (fn [this] @m)
     "withMeta" (fn [this new-meta] (do (reset! m new-meta) this))}))

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

(defn debug-category [component]
  (if (has-category component)
    (apply str )
    nil))

(defn debug-name [component]
  (if-let
      [i (id component)] (str "$" (name i))
      (if (not (empty? (categories component)))
        (str "." (str/join "." (map name (categories component))))
        (str (.getName (class component))
             "@" (Integer/toHexString (.hashCode component))))))

;;the rest of the namespace has been split over the following files
(load "component_protocols")
(load "component_make")
