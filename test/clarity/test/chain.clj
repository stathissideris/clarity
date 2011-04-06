(ns clarity.test.chain
  (:use [clarity.chain] :reload)
  (:use [clojure.test]))

(deftest addition
  (is (= 4 (+ 2 2))))
