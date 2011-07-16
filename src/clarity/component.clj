(ns clarity.component
  (:require [clojure.string :as str])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane]))

(definterface Component
  (getId []))
(defn id [c] (.getId c))

(def special-setters #{:id :category})

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn scroll-pane [comp]
  (JScrollPane. comp))

(defn categories [comp] (.getCategories comp))
(defn add-category [comp s] (.addCategory comp s))
(defn remove-category [comp s] (.removeCategory comp s))

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

(defn component-name [component]
  (if (.getName component)
    (.getName component)
    (str (.getName (.getClass component)) "#" (.hashCode component))))

(extend-type java.awt.Container
  HasValue
  (value [this]
    (apply hash-map
           (apply concat
                  (map (fn [x] [(id x) (value x)])
                       (filter
                        #(and (id %)
                              (satisfies? HasValue %)) (.getComponents this))))))
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

(defn special-setter-form? [[k v]]
  (contains? special-setters k))

(defn pairs-to-map [p]
  (into {} (for [[k v] p] [k v])))

(defn parse-component-params [params]
  (let [const-params (remove list? params)
        special-setter-forms (pairs-to-map
                              (filter special-setter-form?
                                      (filter list? params)))
        setter-forms (remove special-setter-form?
                             (filter list? params))]
    {:constructor const-params
     :special-setter-forms special-setter-forms
     :setter-forms setter-forms}))

(defn make-setter-name [name]
  (let [pieces (str/split name #"-")]
    (symbol (apply str ".set" (map str/capitalize pieces)))))

(defn do-special-keyword [expression]
  (cond (= :category (first expression))
        `(.addCategory ~(second expression))))

(defmacro do-component [component & expressions]
  `(~'doto ~component
     ~@(map (fn [exp]
              (conj
               (drop 1 exp)
               (if (keyword? (first exp))
                 (make-setter-name (name (first exp)))
                 (first exp))))
            expressions)))

(defn make-class-name [component]
  (let [name (name component)
        ns (namespace component)
        prefix (if ns
                 (if (.startsWith ns "awt")
                   (str "java." ns ".")
                   (str "javax.swing." ns ".J"))
                 "javax.swing.J")]
  (apply str prefix
         (map str/capitalize
              (str/split name #"-")))))

(defmacro make-component [component const-params special-setters]
  (let [clazz (if (keyword? component)
                (symbol (make-class-name component))
                component)]
    ;;TODO: really ref?
    `(let [~'id ~(if (contains? special-setters :id)
                   (:id special-setters))
           ~'cat (ref #{})]
       (proxy [~clazz Component clarity.style.Styleable] [~@const-params]
         (~'getId [] ~'id)
         (~'getCategories [] (deref ~'cat))
         (~'addCategory [~'s] (alter ~'cat conj ~'s))
         (~'removeCategory [~'s] (alter ~'cat disj ~'s))))))

(defmacro make
  "Creates a Swing component which also implements the
  clarity.style.Styleable interface. The first parameter is a
  lisp-ified version of the normal names of swing components."
  [& args]
  (let [{:keys [constructor
                special-setter-forms
                setter-forms]} (parse-component-params args)]
    (if (empty? setter-forms)
      `(make-component ~(first constructor)
                       ~(rest constructor)
                       ~special-setter-forms)
      `(do-component
        (make-component ~(first constructor)
                        ~(rest constructor)
                        ~special-setter-forms)
        ~@setter-forms))))