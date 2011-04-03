(ns clarity.debug)

(defmacro debug-values [& args]
  (let* [labels (map (fn [x] (str x ": ")) args)
         everything (interleave labels args (repeat "\n"))]
    `(print (str ~@everything))))
