(ns clarity.test.component
  (:use [clarity.component] :reload)
  (:use [clojure.test]))

(deftest value-of-single-field
  (let [text-field (make :text-field (:text "clarity"))
        check-box (make :check-box (:selected true))]
    (is (= "clarity" (value text-field)))
    (is (= true (value check-box)))))

(deftest value-of-panel
  (let [panel (doto (make :panel
                          (.add (make :text-field
                                      (:text "clojure") (:id "field1")))
                          (.add (make :text-field
                                      (:text "rocks") (:id "field2")))))]
    (is (= {"field1" "clojure"
            "field2" "rocks"} (value panel)))))

(deftest value-of-nested-panel
  (let [panel (make :panel
                    (.add (make :panel
                                (:id :panel1)
                                (.add (make :text-field
                                            (:text "nested")
                                            (:id :nested)))))
                    (.add (make :text-field
                                (:text "clojure")
                                (:id :field1)))
                    (.add (make :text-field
                                (:text "rocks")
                                (:id :field2))))]
    (is (= {:panel1 {:nested "nested"}
            :field1 "clojure"
            :field2 "rocks"} (value panel)))))
