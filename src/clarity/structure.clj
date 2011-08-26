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

(def $ find-by-id)

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
