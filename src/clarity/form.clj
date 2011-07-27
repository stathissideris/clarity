(ns clarity.form
  (:require [clojure.string :as str]
            [clojure.contrib.miglayout :as mig]
            [clarity.component :as c]
            [clarity.functions :as fun]
            clarity.list)
  (:import [javax.swing BorderFactory]))

(def special-tag-keys #{:header :group :end-group :text})
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

(defn make-header [& args]
  (let [[s & {level :level}] (remove #{:header} args)]
    (if level
      (list (make :label s [:category (keyword (str "header-" level))]) :span 2)
      (list (make :label s [:category :header]) :span 2))))

;;TODO
(defn start-group [& args]
  (let [title (first (remove #{:group} args))]
    (c/make :panel
            [:border (BorderFactory/createTitledBorder title)])))

;;TODO
(defn make-text [& args]
  (let [text (first (remove #{:text} args))]
    (list (c/make :label text) :span 2)))

(defn handle-special-tag [tag]
  (cond (header? tag) (apply make-header tag)
        (group? tag) (apply start-group tag)
        (text? tag) (apply make-text tag)))

(defn tokens [form]
  (loop [f form
         tok ()]
    (cond (not (seq f)) ;;finished, reverse and return
          (apply vector (reverse tok))
          (special-tag? (first f)) ;;special, take one, recur with the rest
          (recur (next f) (conj tok (first f)))
          (and (> (count f) 2) ;;normal case where there are extra params, take 2
               (sequential? (nth f 2))
               (not (special-tag? (nth f 2))))
          (recur (next (nnext f)) (conj tok (apply vector (take 3 f))))
          :else ;;normal case, no params,keep 2
          (recur (nnext f) (conj tok (apply vector (take 2 f)))))))

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
                (cond (= :number param) (c/make :text-field (:id id))
                      (= :string param) (c/make :text-field (:id id)))
                (string? param) (c/make :text-field
                                        (:text param) (:id id))
                (number? param) (c/make :text-field
                                        (:text (str param)) (:id id))
                (boolean? param) (c/make :check-box
                                         (:selected param)
                                         (:id id))
                (sequential? param) (c/make
                                     :combo-box data
                                     (:init (to-array param)) (:id id)))]
      (if (nil? (.getName field)) (.setName field (make-label-text token)))
      field))

(defn make-label [text]
  (c/make :label text))

(defn handle-form-token [token]
  (if (special-tag? token)
    (handle-special-tag token)
    (list (make-label (make-label-text token))
          (make-field token)
          :sg))) ;;to achieve equal heights

(defn make-form-panel [mig-params]
  (apply mig/miglayout (c/make :panel)
         :layout "wrap 2"
         :column "[left][grow,fill]"
         mig-params))

(defn params-to-mig-params [params]
  (reduce concat
          (map handle-form-token
               (tokens params))))

(defn form [& components]
  (make-form-panel (params-to-mig-params components)))
