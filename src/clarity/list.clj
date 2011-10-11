(ns clarity.list
  (:import [javax.swing AbstractListModel]))

(defn immutable-list-model
  "Constructs a list model of immutable data."
  [data]
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

(defprotocol ListModel
  (append [this item])
  (insert [this index item]))


;;(def a (to-array [1 2 3 4]))
;;(def s (seq a)
;;(aset a 2 15)
;;s
(defn list-model
  ([fmap] (list-model fmap 0))
  ([fmap pointer]
     (proxy [javax.swing.AbstractListModel
             clarity.list.ListModel
             clojure.lang.ISeq
             clojure.lang.Counted
             clojure.lang.Indexed] []
       (getElementAt [index] ((:get fmap) index))
       (getSize [] ((:count fmap)))
       (append [item]
         (do ((:add fmap) item)
             (let [i (dec (count this))]
               (proxy-super fireIntervalAdded this i i))))
       (insert [index item]
         (do ((:insert fmap) index item)
             (proxy-super fireIntervalAdded this index index)))
       (first [] (.getElementAt this pointer))
       (next []
         (if (< (inc pointer) (.getSize this))
           (list-model fmap (inc pointer))
           nil))
       (more [] (let [n (next this)]
                  (if n n
                      clojure.lang.PersistentList/EMPTY)))
       (cons [x] (cons x this))
       (empty [] clojure.lang.PersistentList/EMPTY)
       (equiv [x] false) ;;TODO
       (seq [] this)
       (nth
         ([index]
            (.getElementAt this (+ pointer index)))
         ([index not-found]
            (if (or (< index 0)
                    (>= index (count this)))
              not-found
              (.getElementAt this (+ pointer index)))))
       (count []
         (- (.getSize this) pointer)))))

(let [data (atom [1 2 3 4 5 6 7 8])]
    (def m
      (list-model
       {:get (fn [index] (nth @data index))
        :add (fn [item] (swap! data conj item))
        :insert (fn [index item])
        :count (fn [] (count @data))})))

