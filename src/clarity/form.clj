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

(defn field [param]
  (cond (keyword? param)
        (cond (= :number param) (component/make :text-field)
              (= :string param) (component/make :text-field))
        (string? param) (component/make (:text-field param))
        (number? param) (component/make (:text-field (str param)))
        (sequential? param) (component/make
                             (:combo-box (to-array param)))))

(defn label [l]
  (component/make (:label
                   (str/capitalize (str/replace l "-" " ")))))

(defn simple-form [& components]
  (apply mig/miglayout (component/make :panel)
         :layout "wrap 2"
         :column "[left][grow,fill]"
         (reduce concat
                 (map #(list (label (name (first %)))
                             (field (second %))
                             :sg) ;;to achieve equal heights
                      (partition 2 components)))))
