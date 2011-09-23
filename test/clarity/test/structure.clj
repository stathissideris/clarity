(ns clarity.test.structure
  (:use [clarity.structure] :reload)
  (:use [clojure.test])
  (:require clarity.form)
  (:require [clarity.component :as c]))

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

(deftest test-id-matcher
  (is (= true ((id-matcher :test) (c/make :button (:id :test)))))
  (is (= false ((id-matcher :test) (c/make :button (:id :not-test))))))


(deftest test-category-matcher
  (let [m (category-matcher :cat2)]
    (is (= true (m (c/make :button (:categories :cat1 :cat2)))))
    (is (= false (m (c/make :button (:categories :cat3 :cat1)))))))

(deftest test-type-matcher
  (is (= true ((type-matcher :button) (c/make :button))))
  (is (= false ((type-matcher :text-field) (c/make :button))))
  (is (= true ((type-matcher javax.swing.JButton) (c/make :button))))
  (is (= false ((type-matcher javax.swing.JButton) (c/make :text-field)))))

(deftest test-and-matcher
  (is ((and-matcher (id-matcher :la) (category-matcher :lo))
       (c/make :button (:id :la) (:category :lo))))
  (is (not ((and-matcher (id-matcher :la) (category-matcher :lo))
            (c/make :button (:id :la) (:category :lolo)))))
  (is ((and-matcher (category-matcher :la) (category-matcher :lo))
       (c/make :button (:category :la :lo))))
  (is (not ((and-matcher (category-matcher :la) (category-matcher :lo))
            (c/make :button (:category :la :lolo))))))

(deftest test-or-matcher
  (is ((or-matcher (id-matcher :la) (category-matcher :lo))
       (c/make :button (:id :la))))
  (is (not ((or-matcher (id-matcher :la) (category-matcher :lo))
            (c/make :button (:id :laX) (:category :lolo)))))
  (is ((or-matcher (category-matcher :la) (category-matcher :lo))
       (c/make :button (:category :la))))
  (is (not ((or-matcher (category-matcher :la) (category-matcher :lo))
            (c/make :button (:category :laX :lolo))))))

(deftest test-direct-parent
  (let [panel1 (c/make :panel (:id :panel1))
        panel2 (c/make :panel (:id :panel2))
        button (c/make :button (:id :the-button))]
    (.add panel2 button)
    (.add panel1 panel2)
    (is (= true ((direct-parent-matcher
                  (id-matcher :panel2)
                  (id-matcher :the-button)) button)))
    (is (= true ((direct-parent-matcher
                  (id-matcher :panel1)
                  (id-matcher :panel2)) panel2)))
    (is (= false ((direct-parent-matcher
                   (id-matcher :panel1)
                   (id-matcher :the-button)) button)))
    (is (= false ((direct-parent-matcher
                   (id-matcher :lalala)
                   (id-matcher :panel2)) panel2)))))

(deftest test-direct-parent-mixed-selectors
  (let [panel1 (c/make :panel)
        panel2 (c/make :panel (:category :cat))
        button (c/make :button (:id :the-button))]
    (.add panel2 button)
    (.add panel1 panel2)
    (is ((direct-parent-matcher
          (category-matcher :cat)
          (id-matcher :the-button)) button))
    (is (not ((direct-parent-matcher
               (category-matcher :cat2)
               (id-matcher :the-button)) button)))
    (is ((direct-parent-matcher
          (type-matcher :panel)
          (category-matcher :cat)) panel2))
    (is (not ((direct-parent-matcher
               (type-matcher :button)
               (category-matcher :cat)) panel2)))))

;;this test makes sure that 2 direct-parent matchers that
;;differ in that the one is parent-expensive and the other
;;is child-expensive will both run correctly
(deftest test-direct-parent-assymetrical-cost
  (let [panel1 (c/make :panel)
        panel2 (c/make :panel (:category :cat))
        button (c/make :button (:id :the-button))
        m (direct-parent-matcher (category-matcher :cat)
                                 (or-matcher (id-matcher :the-button)
                                             (type-matcher :panel)
                                             (id-matcher :the-other-button)
                                             (category-matcher :lala)))
        m2 (direct-parent-matcher (or-matcher (id-matcher :the-panel)
                                              (type-matcher :panel)
                                              (category-matcher :cat))
                                  (id-matcher :the-button))]
    (.add panel2 button)
    (.add panel1 panel2)
    (is (= 8 (get (meta m) :clarity.structure/cost)))
    (is (= :clarity.structure/test-parent-first
           (get (meta m) :clarity.structure/priority)))
    (is (m button))    
    (is (= 6 (get (meta m2) :clarity.structure/cost)))
    (is (= :clarity.structure/test-child-first
           (get (meta m2) :clarity.structure/priority)))
    (is (m2 button))))

(deftest test-direct-parent-nested
  (let [matcher (direct-parent-matcher
                 (direct-parent-matcher
                  (id-matcher :panel1)
                  (category-matcher :cat1))
                 (type-matcher :button))
        b (c/make :button)
        panel (c/make :panel
                      (:id :panel1)
                      (.add (c/make :panel
                                    (:category :cat1)
                                    (.add b))))]
    (is (matcher b))))

(deftest test-any-parent-mixed-selectors
  (let [panel1 (c/make :panel)
        panel2 (c/make :panel (:category :cat))
        button (c/make :button (:id :the-button))]
    (.add panel2 button)
    (.add panel1 panel2)

    ;; same tests as direct parent (should behave the same)

    (is ((any-parent-matcher
          (category-matcher :cat)
          (id-matcher :the-button)) button))
    (is (not ((any-parent-matcher
               (category-matcher :cat2)
               (id-matcher :the-button)) button)))
    (is ((any-parent-matcher
          (type-matcher :panel)
          (category-matcher :cat)) panel2))
    (is (not ((any-parent-matcher
               (type-matcher :button)
               (category-matcher :cat)) panel2)))
    
    ;; true indirect tests
    
    (is ((any-parent-matcher
          (type-matcher :panel)
          (id-matcher :the-button)) button))
    (is (not ((any-parent-matcher
               (type-matcher :button)
               (id-matcher :the-button)) button)))
    (let [button (c/make :button)
          p (c/make
             :panel
             (:id :panel)
             (:category :fancy-panel)
             (.add (c/make
                    :panel
                    (:id :panel2)
                    (.add (c/make
                           :panel
                           (:category :cat1)
                           (.add button))))))]
      (is ((any-parent-matcher (id-matcher :panel)
                               (type-matcher :button)) button)))))

(deftest test-path-matcher-direct-only
  (let [button (c/make :button)
        matcher (path-matcher*
                 (id-matcher :panel1)
                 (category-matcher :cat1)
                 (type-matcher :button))
        panel (c/make :panel
                      (:id :panel1)
                      (.add (c/make :panel
                                    (:category :cat1)
                                    (.add button))))]
    (is (matcher button)))
  (let [button (c/make :button)
        matcher (path-matcher*
                 (id-matcher :panel1)
                 (category-matcher :cat1)
                 (type-matcher :button))
        panel (c/make :panel
                      (:id :panelNOT)
                      (.add (c/make :panel
                                    (:category :cat1)
                                    (.add button))))]
    (is (not (matcher button)))))

(deftest test-path-matcher-indirect
  (let [button (c/make :button)
        matcher (path-matcher*
                 (id-matcher :panel1)
                 ...
                 (type-matcher :button))
        panel (c/make
               :panel
               (:id :panel1)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (matcher button)))
  (let [button (c/make :button)
        matcher (path-matcher*
                 (id-matcher :panel1)
                 ...
                 (type-matcher :button))
        panel (c/make
               :panel
               (:id :panelNOT)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (not (matcher button))))
  (let [button (c/make :button)
        p (c/make
           :panel
           (:id :panel)
           (:category :fancy-panel)
           (.add (c/make
                  :panel
                  (:id :panel2)
                  (.add (c/make
                         :panel
                         (:category :cat1)
                         (.add button))))))]
    (is ((path-matcher* (id-matcher :panel)
                        ...
                        (type-matcher :button)) button))))

(deftest test-path-matcher-macro
  (let [button (c/make :button)
        matcher (matcher
                 (id :panel1) ... (type :button))
        panel (c/make
               :panel
               (:id :panel1)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (matcher button)))
  (let [button (c/make :button)
        matcher (matcher
                 (id :panel1) ... (or (type :label) (type :button)))
        panel (c/make
               :panel
               (:id :panel1)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (matcher button)))
  (let [label (c/make :label)
        matcher (matcher
                 (id :panel1) ... (or (type :label) (type :button)))
        panel (c/make
               :panel
               (:id :panel1)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add label))))))]
    (is (matcher label)))
  (let [button (c/make :button)
        matcher (matcher
                 (id :panel1) ... (type :button))
        panel (c/make
               :panel
               (:id :panelNOT)
               (.add (c/make
                      :panel
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (not (matcher button))))
  (let [button (c/make :button)
        p (c/make
           :panel
           (:id :panel)
           (:category :fancy-panel)
           (.add (c/make
                  :panel
                  (:id :panel2)
                  (.add (c/make
                         :panel
                         (:category :cat1)
                         (.add button))))))]
    (is ((matcher (id :panel) ... (type :button)) button))
    (is ((matcher (category :fancy-panel)
                  ...
                  (category :cat1)
                  (type :button)) button)))
  (let [button (c/make :button)
        matcher (matcher
                 (and
                  (path (id :panel) ... (type :button))
                  (path (category :fancy-panel) ... (category :cat1) (type :button))))
        panel (c/make
               :panel
               (:id :panel)
               (:category :fancy-panel)
               (.add (c/make
                      :panel
                      (:id :panel2)
                      (.add (c/make
                             :panel
                             (:category :cat1)
                             (.add button))))))]
    (is (matcher button))))
