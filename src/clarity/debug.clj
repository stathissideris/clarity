(ns clarity.debug
  (:use [clojure.contrib.str-utils :only (re-sub)]))

(defn- unmangle
  "Given the name of a class that implements a Clojure function,
  returns the function's name in Clojure. Note: If the true Clojure
  function name contains any underscores (a rare occurrence), the
  unmangled name will contain hyphens at those locations instead. See
  http://www.mail-archive.com/clojure@googlegroups.com/msg13018.html"
  [class-name]
  (.replace
   (re-sub #"^(.+)\$(.+)__\d+$" "$1/$2" class-name)
   \_ \-))

(defmacro current-function-name []
  "Returns a string, the name of the current Clojure function. See http://www.mail-archive.com/clojure@googlegroups.com/msg13018.html"
  `(-> (Throwable.) .getStackTrace first .getClassName unmangle))

(defmacro calling-function-name []
  "Returns a string, the name of the current Clojure function"
  `(-> (Throwable.) .getStackTrace second .getClassName unmangle))

(defmacro debug-values [& args]
  `(do
     ;;(print (str "ns: " ~(str *ns*) "\n"))
     ~@(map (fn [x]
                `(print (str "  " ~(str x) ": " (pr-str ~x) "\n"))) args)))
