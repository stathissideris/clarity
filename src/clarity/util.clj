(ns clarity.util
  (:import [javax.swing SwingUtilities]
           [javax.imageio ImageIO]))

(defn load-resource
  [name]
  (let [thr (Thread/currentThread)
        ldr (.getContextClassLoader thr)]
    (.getResourceAsStream ldr name)))

(defn load-image-resource
  [name]
  (ImageIO/read (load-resource name)))

(defn resolve-value
  "Tests the symbol passed to see if it resolves to a var and if it
  does, it returns its value, or nil otherwise. Supports an optional
  second parameter which is the missing value to be returned instead
  of nil."
  ([sym] (resolve-value sym nil))
  ([sym missing]
     (let [v (resolve sym)]
       (if v (var-get v) missing))))

(defn do-swing*
  "Runs thunk in the Swing event thread according to schedule:
    - :later => schedule the execution and return immediately
    - :now   => wait until the execution completes."
  [schedule thunk]
  (cond
   (= schedule :later) (SwingUtilities/invokeLater thunk)
   (= schedule :now) (if (SwingUtilities/isEventDispatchThread)
                       (thunk)
                       (SwingUtilities/invokeAndWait thunk)))
  nil)

(defmacro do-swing
  "Executes body in the Swing event thread asynchronously. Returns
  immediately after scheduling the execution."
  [& body]
  `(do-swing* :later (fn [] ~@body)))

(defmacro do-swing-and-wait
  "Executes body in the Swing event thread synchronously. Returns
  after the execution is complete."
  [& body]
  `(do-swing* :now (fn [] ~@body)))

(defn clojure-1-2? []
  (and (= 1 (:major *clojure-version*))
       (= 2 (:minor *clojure-version*))))

(defn clojure-1-3? []
  (and (= 1 (:major *clojure-version*))
       (= 3 (:minor *clojure-version*))))
