(ns clarity.animation)

(defprotocol interpolatable
  (interpolate [start end t]))
;;  (interpolate [start end t function]))

(extend-type java.lang.Number
  interpolatable
  (interpolate [start end t]
               (+ start (* t (- end start)))))

(extend-type clojure.lang.PersistentVector
  interpolatable
  (interpolate [start end t]
               (vec (map #(interpolate %1 %2 t) start end))))

(extend-type java.awt.Color
  interpolatable
  (interpolate [start end d]
               (new java.awt.Color
                    (int (interpolate (.getRed start)   (.getRed end)   t))
                    (int (interpolate (.getGreen start) (.getGreen end) t))
                    (int (interpolate (.getBlue start)  (.getBlue end)  t))
                    (int (interpolate (.getAlpha start) (.getAlpha end) t)))))