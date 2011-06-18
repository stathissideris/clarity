(ns clarity.style)

(definterface Styleable
  (getCategories [])
  (addCategory [s])
  (removeCategory [s]))
