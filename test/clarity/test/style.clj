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
  (is (= 75.0 (derive-size 50 "x1.5")))
  (is (= 25.0 (derive-size 50 "x0.5"))))

(deftest test-font-creation
  (is (= "Dialog" (.getName (font :name :dialog))))
  (is (= 14 (.getSize (font :size 14))))
  (is (= (:bold font-styles) (.getStyle (font :style :bold))))
  (let [f (font :size 13 :style :bold)]
    (is (= 13 (.getSize f)))
    (is (= (:bold font-styles) (.getStyle f))))
  (let [f (font :size 13 :name :dialog)]
    (is (= 13 (.getSize f)))
    (is (= "Dialog" (.getName f))))
  (let [f (font :size 13 :name :dialog :style :bold)]
    (is (= 13 (.getSize f)))
    (is (= (:bold font-styles) (.getStyle f)))
    (is (= "Dialog" (.getName f)))))

(deftest test-font-derivation
  (let [f (font :name :dialog :style :bold :size 50)]
    (let [f2 (derive-font f :name :mono)]
      (is (= 50 (.getSize f2)))
      (is (= (:bold font-styles) (.getStyle f2)))
      (is (= "Monospaced" (.getName f2))))
    (let [f2 (derive-font f :style :italic)]
      (is (= 50 (.getSize f2)))
      (is (= (:italic font-styles) (.getStyle f2)))
      (is (= "Dialog" (.getName f2))))
    (let [f2 (derive-font f :size 20)]
      (is (= 20 (.getSize f2)))
      (is (= (:bold font-styles) (.getStyle f2)))
      (is (= "Dialog" (.getName f2))))
    (let [f2 (derive-font f :style :italic :size 20)]
      (is (= 20 (.getSize f2)))
      (is (= (:italic font-styles) (.getStyle f2)))
      (is (= "Dialog" (.getName f2))))))