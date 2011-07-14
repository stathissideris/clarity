(ns clarity.form
  (:require [clojure.string :as str]
            [clojure.contrib.miglayout :as mig]
            [clarity.component :as component]
            clarity.list))

(def field-flags #{:full-width})
(def header-flags #{})
(def group-flags #{})
(def text-flags #{:rich})

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

(defn boolean? [x] (= java.lang.Boolean (class x)))

(defn extra-params? [token]
  (= 3 (count token)))

(defn destructure-token-flags [flags token]
  (apply destructure-flags flags (nth token 2)))

(defn make-label-text [token]
  (if (extra-params? token)
    (let [{:keys [args]} (destructure-token-flags field-flags token)]
      (if (contains? args :label)
        (:label args)
        (make-label-text (butlast token))))
    (str/capitalize (str/replace (name (first token)) "-" " "))))

(defn make-field [token]
  (let [[id param] token
        field
        (cond (keyword? param)
              (cond (= :number param) (component/make :text-field (:id id))
                    (= :string param) (component/make :text-field (:id id)))
              (string? param) (component/make :text-field
                                              (:text param) (:id id))
              (number? param) (component/make :text-field
                                              (:text (str param)) (:id id))
              (boolean? param) (component/make :check-box
                                               (:selected param)
                                               (:id id))
              (sequential? param) (let [data (to-array param)]
                                    (component/make
                                     :combo-box data (:id id))))] ;;let necessary because of syntax limitation
    (if (nil? (.getName field)) (.setName field (make-label-text token)))
    field))

(defn make-label [text]
  (component/make :label text))

(defn log [x] (print x) x)

(defn form [& components]
  (apply mig/miglayout (component/make :panel)
         :layout "wrap 2"
         :column "[left][grow,fill]"
         (reduce concat
                 (map #(list (make-label (make-label-text %))
                             (make-field %)
                             :sg) ;;to achieve equal heights
                      (tokens components)))))
