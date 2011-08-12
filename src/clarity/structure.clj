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

(defn find-by-category
  "Finds recursively all the children of root that have the requested
  category. Root itself is included in the search."
  [root category]
  (filter #(c/has-category % category) (comp-seq root)))

;;example
#_(map #(.getText %) (find-by-category (clarity.form/form :a 6 :b 8) :form-label))

(defn find-by-id
  "Finds recursively the child of root (or root itself) that has the
  passed ID."
  [root id]
  (first (filter #(= id (c/id %)) (comp-seq root))))

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
