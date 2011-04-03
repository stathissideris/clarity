(ns clarity.document
  (:require [clojure.string :as str]
            [clarity.chain :as chain]))


(def all-upper
  {:insert (fn [component offset string attr]
             [component offset (str/upper-case string) attr])})

(def all-lower
  {:insert (fn [component offset string attr]
             [component offset (str/lower-case string) attr])})

(defn max-len [max]
  {:insert (fn [component offset string attr]
             (if (< max (+ (count (.getText component)) (count string)))
               :veto
               [component offset string attr]))})

(defn min-len [min]
  {:remove (fn [component offset length]
             (if (> min (- (count (.getText component)) length))
               :veto
               [component offset length]))})

(defn matches [regex]
  {:insert (fn [component offset string attr]
             (prn string)
             (if (re-matches regex string)
               [component offset string attr]
               :veto))
   :replace (fn [component offset length string attributes]
              (if (re-matches regex string)
                [component offset length string attributes]
                :veto))})

(defn build-document [component & fnmaps]
  (let [inserts  (remove nil? (map #(:insert %1) fnmaps))
        replaces (remove nil? (map #(:replace %1) fnmaps))
        removes  (remove nil? (map #(:remove %1) fnmaps))]
    (proxy [javax.swing.text.PlainDocument] []
      (insertString [offset string attributes]
                    (let [results (chain/chain-vetoable inserts
                                                        component offset string attributes)]
                      (if (not= :veto results)
                        (proxy-super insertString
                                     (nth results 1)
                                     (nth results 2)
                                     (nth results 3)))))
      (remove [offset length]
              (let [results (chain/chain-vetoable removes
                                                  component offset length)]
                (if (not= :veto results)
                  (proxy-super remove
                               (nth results 1)
                               (nth results 2)))))
      (replace [offset length text attributes]
               (let [results (chain/chain-vetoable replaces
                                                   component offset length text attributes)]
                 (if (not= :veto results)
                   (proxy-super replace
                                (nth results 1)
                                (nth results 2)
                                (nth results 3)
                                (nth results 4))))))))
