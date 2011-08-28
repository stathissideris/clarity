(ns clarity.list
  (:import [javax.swing AbstractListModel]))

(defn immutable-list-model [data]
  (let [c (count data)]
    (proxy [javax.swing.AbstractListModel] []
      (getElementAt [index] (nth data index))
      (getSize [] c))))

(defn mutable-list-model
  "Constructs a list model that wraps the passed agent/atom/var/ref
  reference. Also adds a watch to the data so that any changes cause
  the fireContentsChanged() method of the model to be called. If a
  function is passed, it is applied to the reference before a
  particular element being returned. This works OK for short lists,
  but if the list will likely be large (or change very often), you
  should consider writing a customised model."
  ([data]
     (let [watch-key (gensym "list-watch-")
           model (proxy [javax.swing.AbstractListModel] []
                   (getElementAt [index] (nth @data index))
                   (getSize [] (count @data)))]
       (add-watch data watch-key
                  (fn [k r old new]
                    (.fireContentsChanged model model 0 (dec (count new)))))
       model))
  ([data fun]
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
