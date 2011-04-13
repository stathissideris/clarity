(ns clarity.utils)


(defn get-properties
  ([] (get-properties nil))
  ([regex]
     (let [defaults (javax.swing.UIManager/getLookAndFeelDefaults)]
       (if regex
         (filter #(re-find regex (.toString (key %))) defaults)
         defaults))))

(defn show-properties
  ([] (show-properties nil))
  ([regex]
     (let [matches (get-properties regex)]
       (doseq [entry matches]
         (print (key entry) ": " (val entry) "\n")))))