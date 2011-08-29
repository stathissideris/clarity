(ns clarity.utils
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