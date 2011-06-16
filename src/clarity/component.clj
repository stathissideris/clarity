(ns clarity.component
  (:require [clojure.string :as str])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane]))

(def special-keywords #{:category})

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn scroll-pane [comp]
  (JScrollPane. comp))

(defprotocol HasValue
  (value [this])
  (set-value [this value]))

(extend-type javax.swing.JTextField
  HasValue
  (value [this] (.trim (.getText this)))
  (set-value [this value] (.setText this value)))

(extend-type javax.swing.JComboBox
  HasValue
  (value [this] (.getSelectedItem this))
  (set-value [this value] (.setSelectedItem this value)))

(extend-type javax.swing.JCheckBox
  HasValue
  (value [this] (.isSelected this))
  (set-value [this value] (.setSelected this value)))

(extend-type java.awt.Container
  HasValue
  (value [this]
    (map #(value %)
         (filter
          #(satisfies? HasValue %) (.getComponents this))))
  (set-value [this value]))
  
(defprotocol HasSelection
  (selection [this])
  (set-selection [this selection]))

(extend-type javax.swing.JComboBox
  HasSelection
  (selection [this] (.getSelectedItem this))
  (set-selection [this selection] (.setSelectedItem this value)))

(extend-type javax.swing.JList
  HasSelection
  (selection [this] (.getSelectedValues this))
  (set-selection [this selection] (.setSelectedValues this selection)))

(extend-type javax.swing.JTree
  HasSelection
  (selection [this] (.getSelectionPaths this))
  (set-selection [this selection] (.setSelectionPaths this selection)))

                                        ;not used
;(defn make-method-name [name]
;  (let [[first & rest] (str/split name #"-")]
;    (symbol (apply str "." first (map str/capitalize rest)))))

(defn make-setter-name [name]
  (let [pieces (str/split name #"-")]
    (symbol (apply str ".set" (map str/capitalize pieces)))))

(defn do-special-keyword [expression]
  (cond (= :category (first expression))
        `(.addCategory ~(second expression))))

(defmacro do-component [component & expressions]
  `(~'doto ~component
     ~@(map (fn [exp]
              (if (contains? special-keywords (first exp))
                (do-special-keyword exp)
                (conj
                 (drop 1 exp)
                 (if (keyword? (first exp))
                   (make-setter-name (name (first exp)))
                   (first exp)))))
            expressions)))

(defn make-class-name [component & flags]
  (let [awt (some #{:awt} flags)
        name (name component)
        prefix (if (namespace component)
                 (if awt
                   (str "java.awt." (namespace component) ".")
                   (str "javax.swing." (namespace component) ".J"))
                 (if awt "java.awt." "javax.swing.J"))]
  (apply str prefix
         (map str/capitalize
              (str/split name #"-")))))

(defmacro make-component [component & args]
  (let [clazz (if (keyword? component)
                (symbol (apply make-class-name component args))
                component)
        params (remove #{:awt} args)]
    ;;TODO: really ref?
    `(let [~'st (ref #{})]
       (proxy [~clazz clarity.style.Styleable] [~@params]
         (~'.getCategories [] (deref ~'st))
         (~'.addCategory [~'s] (alter ~'st conj ~'s))
         (~'.removeCategory [~'s] (alter ~'st disj ~'s))))))

(defmacro make
  "Creates a Swing component which also implements the
  clarity.style.Styleable interface. The first parameter is a
  lisp-ified version of the normal names of swing components."
  [& args]
  (if (list? (first args))
    (let [[[component & args] & expressions] args]
      `(do-component (make-component ~component ~@args)
                     ~@expressions))
    (let [[component & args] args]
      `(make-component ~component ~@args))))
