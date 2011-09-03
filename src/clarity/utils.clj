(ns clarity.utils
  (:require [clarity.component :as c])
  (:import [javax.swing UIManager JFrame]))

(defmacro qw
  "Constructs a vector of the names (strings) of the passed symbols.
  This is to save you typing unneccesary quotes. Stolen from Perl.

  Example: (qw \"first name\" surname address)"
  [& words]
  `(vector ~@(map name words)))

(defn show-comp [comp]
  (doto (JFrame.)
    (.add comp)
    (.pack)
    (.setVisible true)))

(defn watch-image
  "Shows the passed java.awt.Image in a frame, and re-paints at 15
  FPS (or the specified FPS). You can also pass a reference to an
  Image, which will be dereferenced at every frame. The function
  returns a future which can be cancelled to stop the re-painting. Of
  course the re-painting stops automatically when the frame is
  closed."
  ([image] (watch-image image 15))
  ([image fps]
     (let [get-image (fn [] (if (instance? clojure.lang.IDeref image)
                              @image image))
           panel (proxy [javax.swing.JPanel] []
                   (paintComponent [g] (if (get-image)
                                         (.drawImage g (get-image) 0 0 this)))
                   (getPreferredSize[] (if (get-image)
                                         (java.awt.Dimension.
                                          (.getWidth (get-image))
                                          (.getHeight (get-image)))
                                         (java.awt.Dimension. 100 100))))
           updater (future
                    (while true
                      (Thread/sleep (/ 1000 fps))
                      (.repaint panel)))]
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

(defn set-system-laf []
  (javax.swing.UIManager/setLookAndFeel
   (javax.swing.UIManager/getCrossPlatformLookAndFeelClassName)))

(defn get-laf-properties
  ([] (get-laf-properties nil))
  ([regex]
     (let [defaults (javax.swing.UIManager/getLookAndFeelDefaults)]
       (if regex
         (filter #(re-seq regex (.toString (key %))) defaults)
         defaults))))

(defn find-laf-properties
  ([] (find-laf-properties nil))
  ([regex]
     (let [matches (get-laf-properties regex)]
       (doseq [entry matches]
         (print (key entry) ": " (val entry) "\n")))))

(defn get-laf-property
  [key]
  (javax.swing.UIManager/get key))