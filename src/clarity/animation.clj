(ns clarity.animation)

(defprotocol interpolatable
  (interpolate [start end d]))
;;  (interpolate [start end d function]))

(extend-type java.lang.Number
  interpolatable
  (interpolate [start end d]
               (+ start (* d (- end start)))))

(extend-type clojure.lang.PersistentVector
  interpolatable
  (interpolate [start end d]
               (vec (map #(interpolate %1 %2 d) start end))))

(extend-type java.awt.Color
  interpolatable
  (interpolate [start end d]
               (new java.awt.Color
                    (int (interpolate (.getRed start) (.getRed end) d))
                    (int (interpolate (.getGreen start) (.getGreen end) d))
                    (int (interpolate (.getBlue start) (.getBlue end) d))
                    (int (interpolate (.getAlpha start) (.getAlpha end) d)))))