(ns ^{:doc "Functions to \"pipe\" functions together by passing the
	output of one to the input of the next."}
  clarity.chains)

(defn chain-vetoable
  "Chains the passed functions. Stops and returns :veto if one of the
  functions in the chain returns :veto."
  [functions & args]
  (loop [fns functions, params args]
	(if (not fns) params
		(let* [f (first fns)
			   results (if (coll? params) (apply f params) (f params))]
			  (if (= :veto results) :veto
				  (recur (next fns) results))))))

(defn chain
  "Chains the passed functions by calling the first with the passed
  arguments and applying the result(s) of the the first function to the
  second etc."
  [functions & args]
  (loop [fns functions, params args]
	(if (not fns) params
		(let* [f (first fns)
			   results (if (coll? params) (apply f params) (f params))]
			  (recur (next fns) results)))))