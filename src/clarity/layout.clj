(ns clarity.layout
  (:require [artem.miglayout :as artem]))

(def mig artem/miglayout)

(defmacro grid-bag
  "This macro was developed by Stuart Sierra and was first published
  here: http://stuartsierra.com/2010/01/05/taming-the-gridbaglayout

  It is used here with permission from the author."

  [container & body]
  (let [c (gensym "c")
        cntr (gensym "cntr")]
    `(let [~c (new java.awt.GridBagConstraints)
           ~cntr ~container]
       ~@(loop [result '() body body]
           (if (empty? body)
             (reverse result)
             (let [expr (first body)]
               (if (keyword? expr)
                 (recur (cons `(set-grid! ~c ~expr
                                          ~(second body))
                              result)
                        (next (next body)))
                 (recur (cons `(.add ~cntr ~expr ~c)
                              result)
                        (next body)))))))))
