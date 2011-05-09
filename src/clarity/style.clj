(ns clarity.style)

(defn styles [comp] (.getStyles comp))
(defn add-style [comp s] (.addStyle comp s))
(defn remove-style [comp s] (.removeStyle comp s))

(definterface Styleable
  (getStyles [])
  (addStyle [s])
  (removeStyle [s]))
