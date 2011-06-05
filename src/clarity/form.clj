(ns clarity.form)

(defn interpose-every [sep coll every]
  (let [piece (fn piece [sep coll every]
                (when (and coll (not (empty? coll)))
                  (concat (take every coll)
                          [sep]
                          (piece sep (drop every coll) every))))]
    (butlast (piece sep coll every))))

(defn make-form [& components]
  (let [pairs (interpose-every :wrap components 2)]
    pairs))