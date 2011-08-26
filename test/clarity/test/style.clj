(ns clarity.test.style
  (:use [clarity.style] :reload)
  (:use [clojure.test]))

(deftest test-derive-size
  (is (= 220 (derive-size 200 "+10%")))
  (is (= 220 (derive-size 200 "110%")))
  (is (= 160 (derive-size 200 "-20%")))
  (is (= 180 (derive-size 200 "90%")))
  (is (= 12 (derive-size 10 "+2")))
  (is (= 8 (derive-size 10 "-2")))
  (is (= 50 (derive-size 25 "x2")))
  (is (= 75 (derive-size 50 "x1.5")))
  (is (= 25 (derive-size 50 "x0.5"))))
