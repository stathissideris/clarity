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

(defn resolve-value
  "Tests the symbol passed to see if it resolves to a var and if it
  does, it returns its value, or nil otherwise. Supports an optional
  second parameter which is the missing value to be returned instead
  of nil."
  ([sym] (resolve-value sym nil))
  ([sym missing]
     (let [v (resolve sym)]
       (if v (var-get v) missing))))
