(ns clarity.component
  (:require [clojure.string :as str])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane]))

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn scroll-pane [comp]
  (JScrollPane. comp))

(defn make-class-name [component & flags]
  (let [awt (= :awt (first flags))
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
  (let [clazz (symbol (make-class-name component))]
    ;;TODO: really ref?
    `(let [~'st (ref #{})]
       (proxy [~clazz clarity.style.Styleable] [~@args]
         (~'getStyles [] (deref ~'st))
         (~'addStyle [~'s] (alter ~'st conj ~'s))
         (~'removeStyle [~'s] (alter ~'st disj ~'s))))))

(defmacro make
  "Creates a Swing component which also implements the
  clarity.style.Styleable interface. The first parameter is a
  lisp-ified version of the normal names of swing components."
  [& args]
  (if (list? (first args))
    (let [[[component & args] & expressions] args]
      `(doto (make-component ~component ~@args)
         ~@expressions))
    (let [[component & args] args]
      `(make-component ~component ~@args))))
