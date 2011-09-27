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
