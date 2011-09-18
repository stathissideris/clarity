(ns clarity.structure
  (:require [clarity.component :as c]))

(defn comp-seq
  "Walks the contents of the passed java.awt.Container depth-first and
  returns the results as a lazy sequence."
  [root]
  {:pre [(instance? java.awt.Container root)]}
  (tree-seq (fn [node] (not (zero? (count (.getComponents node)))))
            (fn [node] (.getComponents node))
            root))

(extend-type java.awt.Container
  c/HasValue
  (value [this]
         (into {}
               (map (fn [x] [(c/id x) (c/value x)])
                    (filter
                     #(and (c/id %)
                           (satisfies? c/HasValue %)) (.getComponents this)))))
  (set-value [this value]
             (let [components (comp-seq this)]
               (for [c components]
                 (if (and (not (nil? (c/id c)))
                          (satisfies? c/HasValue c)
                          (contains? value (c/id c)))
                   (c/set-value c (get value (c/id c))))))))

(defn filter-by-category
  [category coll]
  (filter #(c/has-category % category) coll))

(defn find-by-category
  "Finds recursively all the children of root that have the requested
  category. Root itself is included in the search."
  [root category]
  (filter-by-category category (comp-seq root)))

;;example
#_(map #(.getText %) (find-by-category (clarity.form/form :a 6 :b 8) :form-label))

(defn children-with-category
  "Returns the direct descendants of parent with the matching category (if any)."
  [parent category]
  (filter-by-category category (.getComponents parent)))

(defn find-by-id
  "Finds recursively the child of root (or root itself) that has the
  passed ID."
  [root id]
  (first (filter #(= id (c/id %)) (comp-seq root))))

(defn $
  "With a single parameter, it applies (find-by-id) to the last opened
  java.awt.Frame. With 2 parameters it's just a synonym
  of (find-by-id)."
  ([id]
     (let [frames (java.awt.Frame/getFrames)]
       ($ (last frames) id)))
  ([root id] (find-by-id root id)))

(defn child-with-id
  "Returns a direct descendant of parent with a matching id (if any)."
  [parent id]
  (first (filter #(= id (c/id %)) (.getComponents parent))))

;;example
#_(.getText (find-by-id (clarity.form/form :a 6 :b 7) :a))

(defn path
  "Returns a list representing the path of components from the
  top-most parent to comp."
  [comp]
  (loop [c comp p (list c)]
    (let [parent (.getParent c)]
      (if (nil? parent) p
        (recur parent (conj p parent))))))

;;;selectors

;; (category :form
;;           category :form-header
;; 
;; (filter-by-category :form-header
;;                     (mapcat () (find-by-category :form))
