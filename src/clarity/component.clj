(ns clarity.component
  (:import clarity.style.Styleable))

(defmacro make [clazz & args]
  ;;TODO: really ref?
  `(let [~'st (ref #{})]
     (proxy [~clazz clarity.style.Styleable] [~@args]
     (~'getStyles [] (deref ~'st))
     (~'addStyle [~'s] (alter ~'st conj ~'s))
     (~'removeStyle [~'s] (alter ~'st disj ~'s)))))
