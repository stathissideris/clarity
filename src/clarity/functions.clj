(ns clarity.functions)

(defn normalize-mixed-params [m flags]
   (let [values (map
                  #(if (keyword? %) (symbol (name %)) %)
                  (remove flags m))
         true-flags (apply concat
                      (for [f (filter flags m)] 
                           [(symbol (name f)) true]))
         false-flags (apply concat
                        (for [f (remove (into #{} (filter flags m)) flags)]
                             [(symbol (name f)) false]))]
         (into [] (concat values true-flags false-flags))))

(defmacro let-flags [m flags & body]
  `(let ~(normalize-mixed-params m flags) ~@body))
