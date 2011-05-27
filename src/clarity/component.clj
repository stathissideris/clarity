(ns clarity.component
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane]))

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn scroll-pane [comp]
  (JScrollPane. comp))

(defmacro make [clazz & args]
  ;;TODO: really ref?
  `(let [~'st (ref #{})]
     (proxy [~clazz clarity.style.Styleable] [~@args]
     (~'getStyles [] (deref ~'st))
     (~'addStyle [~'s] (alter ~'st conj ~'s))
     (~'removeStyle [~'s] (alter ~'st disj ~'s)))))
