(ns clarity.util
  (:import [javax.imageio ImageIO]))

(defn load-resource
  [name]
  (let [thr (Thread/currentThread)
        ldr (.getContextClassLoader thr)]
    (.getResourceAsStream ldr name)))

(defn load-image-resource
  [name]
  (ImageIO/read (load-resource name)))

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
