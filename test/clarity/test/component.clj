(ns clarity.test.component
  (:use [clarity.component] :reload)
  (:use [clarity.structure])
  (:use [clojure.test]))

(deftest make-simple
  (let [button (make :button)]
    (is (instance? javax.swing.JButton button))
    (is (.isAssignableFrom clarity.component.Component (class button)))))

(deftest make-simple-awt
  (let [button (make :awt/button)]
    (is (instance? java.awt.Button button))
    (is (.isAssignableFrom clarity.component.Component (class button)))))

(deftest make-with-const-args
  (let [button (make :button "the button")]
    (is (= "the button" (.getText button)))))

(deftest make-with-keyword-do
  (let [button (make :button
                     (:text "the button")
                     (:border nil))]
    (is (= "the button" (.getText button)))
    (is (nil? (.getBorder button)))))

(deftest make-with-interface
  (let [button (make [:button :has-value])]
    (is (satisfies? HasValue button))
    (is (instance? javax.swing.JButton button))))

(deftest make-with-interface-and-implementation
  (let [v (atom 10)
        button
        (make [:button :has-value]
              (:impl
               (value [] @v)
               (set-value [x] (reset! v x))
               (getText [] "correct")))]
    (is (satisfies? HasValue button))
    (is (instance? javax.swing.JButton button))
    (is (= 10 (value button)))
    (set-value button 20)
    (is (= 20 (value button)))
    (is (= "correct" (.getText button)))))

(deftest make-with-interface-and-implementation2
  (let [button
        (make [:button java.lang.Readable]
              (:impl
               (read [buffer] -1)))]
    (is (instance? java.lang.Readable button))
    (is (instance? javax.swing.JButton button))
    (is (= -1 (.read button nil)))))

(deftest make-with-normal-do
  (let [button (make :button
                     (.setText "the button")
                     (.setBorder nil))]
    (is (= "the button" (.getText button)))
    (is (nil? (.getBorder button)))))

(deftest make-with-keyword-do-and-id
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
    (is (= #{:a :b} (categories button1)))
    (is (= #{:c :d} (categories button2)))))

(deftest make-with-events-one-listener
  (let [label (make :label "testing events"
                    (:on-mouse-exited (.setText this "exited"))
                    (:on-mouse-over (.setText this "over")))]
    (is (= 1 (count (.getMouseListeners label))))))

(deftest make-with-events-two-listeners
  (let [label (make :label "testing events"
                    (:on-component-resized (.setText this "resized"))
                    (:on-mouse-exited (.setText this "exited"))
                    (:on-mouse-over (.setText this "over")))]
    (is (= 1 (count (.getMouseListeners label))))
    (is (= 1 (count (.getComponentListeners label))))))

(deftest do-component-simple
  (let [button (make :button)]
    (do-component button
                  (.setText "lala"))
    (is (= "lala" (.getText button)))))

(deftest do-component-keyword
  (let [button (make :button)]
    (do-component button
                  [:text "lala"])
    (is (= "lala" (.getText button)))))

(deftest do-component-with-events-one-listener
  (let [label (make :label)]
    (do-component label
                  (:on-mouse-exited (.setText this "exited"))
                  (:on-mouse-over (.setText this "over")))
    (is (= 1 (count (.getMouseListeners label))))))

(deftest do-component-with-events-two-listeners
  (let [label (make :label)]
    (do-component label
                  (:on-component-resized (.setText this "resized"))
                  (:on-mouse-exited (.setText this "exited"))
                  (:on-mouse-over (.setText this "over")))
    (is (= 1 (count (.getMouseListeners label))))
    (is (= 1 (count (.getComponentListeners label))))))

(deftest has-category-true
  (is (= true
         (has-category (make :button [:categories :a :b]) :b))))

(deftest has-category-false
  (is (= false
         (has-category (make :button [:categories :a :b]) :c))))

(deftest has-category-forgiving
  (is (= false
         (has-category 1 :b))))

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
  (let [panel (make
               :panel
               (.add (make
                      :panel
                      (:id :panel1)
                      (.add (make
                             :text-field
                             (:text "nested")
                             (:id :nested)))))
               (.add (make
                      :text-field
                      (:text "clojure")
                      (:id :field1)))
               (.add (make
                      :text-field
                      (:text "rocks")
                      (:id :field2))))]
    (is (= {:panel1 {:nested "nested"}
            :field1 "clojure"
            :field2 "rocks"} (value panel)))))

(deftest is-component-true
  (is (= true (component? (make :button)))))

(deftest id-component-false
  (is (= false (component? 1))))

(deftest id-getter
  (is (= :the-test (id (make :button [:id :the-test])))))

(deftest id-getter-nil
  (is (= nil (id 1))))
