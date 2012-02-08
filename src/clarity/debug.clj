(ns clarity.debug
  (:import [java.awt Container]))

(defn dump [component]
  (let [classname (.getName (.getClass component))]
    (if (instance? java.awt.Container component)
      [classname (into [] (map dump (.getComponents component)))]
      classname)))

(defmacro debug-values [& args]
  `(do
     ;;(print (str "ns: " ~(str *ns*) "\n"))
     ~@(map (fn [x]
                `(print (str "  " ~(str x) ": " (pr-str ~x) "\n"))) args)))
