(ns ^{:doc "Functions to \"pipe\" functions together by passing the
	output of one to the input of the next."}
  clarity.chain
  (:require [clarity.debug :as d]))

(defn chain-vetoable
  "Chains the passed functions. Stops and returns :veto if one of the
   functions in the chain returns :veto."
  [functions & args]
  (d/debug-values functions args)
  (loop [fns functions, params args]
    (if (seq fns)
      (let* [f (first fns)
             results (if (coll? params) (apply f params) (f params))]
            (if (= :veto results) :veto
                (recur (rest fns) results)))
      params)))
  
(defn chain
  "Chains the passed functions by calling the first with the passed
   arguments and applying the result(s) of the the first function to the
   second etc."
  [functions & args]
  (loop [fns functions, params args]
    (if (seq fns)
      (let* [f (first fns)
             results (if (coll? params) (apply f params) (f params))]
            (recur (rest fns) results))
      params)))
