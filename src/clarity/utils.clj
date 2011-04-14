(ns clarity.utils)


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