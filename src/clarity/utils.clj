(ns clarity.utils
  (:require [clarity.component :as c]
            [clojure.contrib.swing-utils :as swing])
  (:import [javax.swing UIManager JFrame]
           [java.awt.image BufferedImage]))

(defn show-comp [comp]
  (doto (JFrame.)
    (.add comp)
    (.pack)
    (.setVisible true)))

(defn- error-image [msg]
  (let [i (BufferedImage. 600 300 BufferedImage/TYPE_INT_RGB)
        g (.getGraphics i)]
    (.drawString g msg 10 20)
    i))

(defn watch-image
  ;; "Shows the passed java.awt.Image in a frame, and re-paints at 15
  ;; FPS (or the specified FPS). You can also pass a reference to an
  ;; Image, which will be dereferenced at every frame, or an
  ;; image-returning function, which will be called at every frame.  The
  ;; function returns a future which can be cancelled to stop the
  ;; re-painting. Of course the re-painting stops automatically when the
  ;; frame is closed."
  ([image] (watch-image image 15))
  ([image fps]
     (let [get-image (fn [] (cond (instance? clojure.lang.IDeref image) @image
                                  (fn? image) (image)
                                  #_(try (image)
                                         (catch Exception e
                                           (error-image
                                            (str (class e) ": " (.getMessage e)))))
                                  :otherwise image))
           cached-image (ref nil)
           panel (proxy [javax.swing.JPanel] []
                   (paintComponent [g]
                                   (dosync (ref-set cached-image (get-image)))
                                   (if @cached-image
                                         (.drawImage g @cached-image 0 0 this)))
                   (getPreferredSize[] (if @cached-image
                                         (java.awt.Dimension.
                                          (.getWidth @cached-image)
                                          (.getHeight @cached-image))
                                         (java.awt.Dimension. 100 100))))
           updater (future
                    (while true
                      (Thread/sleep (/ 1000 fps))
                      (swing/do-swing (.repaint panel))))]
       (c/make :frame
               (.add panel)
               (.pack)
               [:visible true]
               [:on-window-closing
                (future-cancel updater)])
       updater)))
       
(defn dispose-all-frames []
  (doseq [frame (java.awt.Frame/getFrames)]
    (.dispose frame)))
