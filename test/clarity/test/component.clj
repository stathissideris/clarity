(ns clarity.test.component
  (:use [clarity.component] :reload)
  (:use [clojure.test]))

(deftest make-simple
  (let [button (make :button)]
    (is (.isAssignableFrom javax.swing.JButton (class button)))
    (is (.isAssignableFrom clarity.style.Styleable (class button)))
    (is (.isAssignableFrom clarity.component.Component (class button)))))

(deftest make-simple-awt
  (let [button (make :awt/button)]
    (is (.isAssignableFrom java.awt.Button (class button)))
    (is (.isAssignableFrom clarity.style.Styleable (class button)))
    (is (.isAssignableFrom clarity.component.Component (class button)))))

(deftest make-with-const-args
  (let [button (make :button "the button")]
    (is (= "the button" (.getText button)))))

(deftest make-with-special-setters
  (let [button (make :button
                     (:text "the button")
                     (:border nil))]
    (is (= "the button" (.getText button)))
    (is (nil? (.getBorder button)))))

(deftest make-with-normal-setters
  (let [button (make :button
                     (.setText "the button")
                     (.setBorder nil))]
    (is (= "the button" (.getText button)))
    (is (nil? (.getBorder button)))))

(deftest make-with-special-setters-and-id
  (let [button (make :button
                     (:id :my-button)
                     (:text "the button")
                     (:border nil))]
    (is (= :my-button (id button)))
    (is (= "the button" (.getText button)))
    (is (nil? (.getBorder button)))))

(deftest make-with-init
  (let [button (make :button (:init "the button"))]
    (is (= "the button" (.getText button)))))

(deftest make-with-categories
  (let [button1 (make :button (:category :a :b))
        button2 (make :button (:categories :c :d))]
    (is (= #{:a :b} (.getCategories button1)))
    (is (= #{:c :d} (.getCategories button2)))))

;;;;

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
