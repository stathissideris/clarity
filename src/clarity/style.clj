(ns clarity.style)

(defn categories [comp] (.getCategories comp))
(defn add-category [comp s] (.addCategory comp s))
(defn remove-category [comp s] (.removeCategory comp s))

(definterface Styleable
  (getCategories [])
  (addCategory [s])
  (removeCategory [s]))
