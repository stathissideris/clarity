(ns ^{:doc "Functions to \"pipe\" functions together by passing the
	output of one to the input of the next."}
  clarity.chain)

(defn chain-vetoable
  "Chains the passed functions. Stops and returns :veto if one of the
   functions in the chain returns :veto."
  [functions & args]
  (loop [fns functions, params args]
    (if (seq fns)
      (let [f (first fns)
            results (if (coll? params) (apply f params) (f params))]
        (if (= :veto results) :veto
            (recur (rest fns) results)))
      params)))

;;this looks A LOT like comp, see if you can simplify it
;;https://github.com/clojure/clojure/blob/f86db9cc68773dd3e4a166c1ff7b81e4a98aa602/src/clj/clojure/core.clj#L1936
(defn chain
  "Chains the passed functions by calling the first with the passed
   arguments and applying the result(s) to the second etc."
  [functions & args]
  (loop [fns functions, params args]
    (if (seq fns)
      (let [f (first fns)
             results (if (coll? params) (apply f params) (f params))]
        (recur (rest fns) results))
      params)))
