(ns clarity.debug)

(defmacro debug-values [& args]
  `(do ~@(map (fn [x]
                `(print (str ~(str x) ": " (pr-str ~x) "\n"))) args)))
