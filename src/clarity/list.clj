(ns clarity.list
  (import [javax.swing AbstractListModel]))

(defn immutable-list-model [data]
  (let [c (count data)]
    (proxy [javax.swing.AbstractListModel] []
      (getElementAt [index] (nth data index))
      (getSize [] c))))
