(ns clarity.form
  (:require [clojure.string :as str]
            [clojure.contrib.miglayout :as mig]
            [clarity.component :as component]
            clarity.list))

(defn interpose-every [every sep coll]
  (let [piece (fn piece [sep coll every]
                (when (and coll (not (empty? coll)))
                  (concat (take every coll)
                          [sep]
                          (piece sep (drop every coll) every))))]
    (butlast (piece sep coll every))))

(defn destructure-flags [flags & args]
  {:flags (apply hash-set (filter flags args))
   :args (apply hash-map (remove flags args))})

(defn header? [x] (and (sequential? x) (some #(= :header %) x)))
(defn group? [x] (and (sequential? x) (some #(= :group %) x)))
(defn end-group? [x] (and (sequential? x) (some #(= :end-group %) x)))
(defn text? [x] (and (sequential? x) (some #(= :text %) x)))

(defn special-tag? [x]
  (or (header? x)
      (group? x)
      (end-group? x)
      (text? x)))

(defn tokens [form]
  (loop [f form
         tok ()]
    (cond (not (seq f)) (apply vector (reverse tok))
          (special-tag? (first f)) (recur (next f) (conj tok (first f)))
          (and (> (count f) 2)
               (sequential? (nth f 2))
               (not (special-tag? (nth f 2))))
          (recur (next (nnext f)) (conj tok (apply vector (take 3 f))))
          :else (recur (nnext f) (conj tok (apply vector (take 2 f)))))))

(defn make-field [param]
  (cond (keyword? param)
        (cond (= :number param) (component/make :text-field)
              (= :string param) (component/make :text-field))
        (string? param) (component/make (:text-field param))
        (number? param) (component/make (:text-field (str param)))
        (sequential? param) (component/make
                             (:combo-box (to-array param)))))

(defn make-label [l]
  (component/make (:label
                   (str/capitalize (str/replace l "-" " ")))))

(defn form [& components]
  (apply mig/miglayout (component/make :panel)
         :layout "wrap 2"
         :column "[left][grow,fill]"
         (reduce concat
                 (map #(list (make-label (name (first %)))
                             (make-field (second %))
                             :sg) ;;to achieve equal heights
                      (partition 2 components)))))
