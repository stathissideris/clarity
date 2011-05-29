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

(defn make-class-name [component]
  (let [name (name component)
        prefix (if (namespace component)
                 (str "javax.swing."
                      (namespace component)
                      ".J")
                 "javax.swing.J")]
  (apply str prefix
         (map str/capitalize
              (str/split name #"-")))))

(defmacro make
  "Creates a Swing component which also implements the
  clarity.style.Styleable interface. The first parameter is a
  lisp-ified version of the normal names of swing components."
  
  [component & args]
  ;;TODO: really ref?
  (let [clazz (symbol (make-class-name component))]
    `(let [~'st (ref #{})]
       (proxy [~clazz clarity.style.Styleable] [~@args]
         (~'getStyles [] (deref ~'st))
         (~'addStyle [~'s] (alter ~'st conj ~'s))
         (~'removeStyle [~'s] (alter ~'st disj ~'s))))))