(ns clarity.list
  (:require [clarity.component :as c])
  (:import [javax.swing AbstractListModel]))

(defprotocol CanProvideListModel
  (^javax.swing.ListModel list-model [this]))

(defn immutable-list-model
  "Constructs a list model of immutable data."
  [^clojure.lang.Indexed data]
  (let [c (count data)]
    (proxy [javax.swing.AbstractListModel] []
      (getElementAt [index] (nth data index))
      (getSize [] c))))

(extend-type clojure.lang.Indexed
  CanProvideListModel
  (list-model [this] (immutable-list-model this)))

(defn mutable-list-model
  "Constructs a list model that wraps the passed agent/atom/var/ref
  reference. Also adds a watch to the data so that any changes cause
  the fireContentsChanged() method of the model to be called. If a
  function is passed, it is applied to the reference before a
  particular element being returned. This works OK for short lists,
  but if the list will likely be large (or change very often), you
  should consider writing a customised model."
  ([^clojure.lang.ARef data]
     (let [watch-key (gensym "list-watch-")
           model (proxy [javax.swing.AbstractListModel] []
                   (getElementAt [index] (nth @data index))
                   (getSize [] (count @data)))]
       (add-watch data watch-key
                  (fn [k r old new]
                    (.fireContentsChanged model model 0 (dec (count new)))))
       model))
  ([^clojure.lang.ARef data fun]
     (let [cache (atom (fun @data))
           watch-key (gensym "list-watch-")
           model (proxy [javax.swing.AbstractListModel] []
                   (getElementAt [index] (nth @cache index))
                   (getSize [] (count @cache)))]
       (add-watch data watch-key
                  (fn [k r old new]
                    (reset! cache (fun new))
                    (.fireContentsChanged model model 0 (dec (count new)))))
       model)))

(extend-type clojure.lang.ARef
  CanProvideListModel
  (list-model [this] (mutable-list-model this)))

(defprotocol ListModel
  (append [this item])
  (insert [this index item])
  (delete [this index]))

(defn list-model-from-functions
  [fmap]
  (proxy [javax.swing.AbstractListModel
          clarity.list.ListModel] []
    (getElementAt [index] ((:get fmap) index))
    (getSize [] ((:count fmap)))
    (append [item]
      (when (:append fmap)
        ((:append fmap) item)
        (let [i (dec (.getSize this))]
          (proxy-super fireIntervalAdded this i i))))
    (insert [index item]
      (when (:insert fmap)
        ((:insert fmap) index item)
        (proxy-super fireIntervalAdded this index index)))
    (delete [index item]
      (when (:delete fmap)
        ((:delete fmap) index item)
        (proxy-super fireIntervalRemoved this index index)))))

(extend-type clojure.lang.APersistentMap
  CanProvideListModel
  (list-model [this] (list-model-from-functions this)))

(defn list-model-seq
  [^javax.swing.ListModel model]
  (let
      [lms
       (fn lms [^javax.swing.ListModel model pointer]
         (proxy [clojure.lang.ASeq
                 clojure.lang.Counted
                 clojure.lang.Indexed] []
           (first [] (.getElementAt model pointer))
           (next []
             (if (< (inc pointer) (.getSize model))
               (lms model (inc pointer))
               nil))
           (count [] (- (.getSize model) pointer))
           (nth
             ([index]
                (.getElementAt model (+ pointer index)))
             ([index not-found]
                (if (or (< index 0)
                        (>= index (count this)))
                  not-found
                  (.getElementAt model (+ pointer index)))))))]
    (lms model 0)))

(defn- index-tuples [coll]
  (partition 2 (interleave (range (count coll)) coll)))

(defn selections
  [^javax.swing.JList lst]
  (seq (.getSelectedValues lst)))

(defn first-selection
  [^javax.swing.JList lst]
  (.getSelectedValue lst))

(defn selected-indices
  [^javax.swing.JList lst]
  (seq (.getSelectedIndices lst)))

(defn selected-tuples
  [^javax.swing.JList lst]
  (filter #(.isSelectedIndex lst (first %))
          (index-tuples (list-model-seq (.getModel lst)))))

(defn selected-index
  [^javax.swing.JList lst]
  (.getSelectedIndex lst))

(defn set-selected-index
  [^javax.swing.JList lst index]
  (.setSelectedIndex lst index))

(defn set-selected-indices
  [^javax.swing.JList lst indices]
  (.setSelectedIndices lst (int-array indices)))

(defn set-selected-value
  ([^javax.swing.JList lst value] (set-selected-value lst value true))
  ([^javax.swing.JList lst value scroll] (.setSelectedValue lst value scroll)))

(defn select-by
  [lst fn]
  (let [model (list-model-seq (.getModel lst))]
    (set-selected-indices
     lst
     (map first (filter #(fn (second %))
                        (index-tuples model))))))

(extend-type javax.swing.JList
  c/HasValue
  (value [this] (list-model-seq (.getModel this)))
  (set-value [this value] (.setModel this value)))

(let [data (atom ["a" "b" "c" "d" "e" "f"])]
    (def m
      (list-model
       {:get (fn [index] (nth @data index))
        :append (fn [item] (swap! data conj item))
        :count (fn [] (count @data))})))
