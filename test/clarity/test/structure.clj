(ns clarity.test.structure
  (:use [clarity.structure] :reload)
  (:use [clojure.test])
  (:require clarity.form)
  (:require clarity.component))

(deftest by-category
  (is (= '("A" "B")
         (map #(.getText %)
              (find-by-category (clarity.form/form :a 6 :b 8) :form-label)))))

(deftest by-id
  (is (= "6"
         (.getText (find-by-id (clarity.form/form :a 6 :b 7) :a)))))

(deftest test-path
  (let [panel1 (clarity.component/make :panel)
        panel2 (clarity.component/make :panel)
        button (clarity.component/make :button)]
    (.add panel2 button)
    (.add panel1 panel2)
    (is (= (list panel1 panel2 button)
           (path button)))))

