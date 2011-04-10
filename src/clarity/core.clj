(ns clarity.core
  (:require [clojure.contrib.pprint :as pp]
            [clojure.string :as str]
            [clojure.contrib.swing-utils :as sw]
            [clarity.chain :as chain]
            [clarity.document :as doc])
  (:import (javax.swing JFrame JButton JTextField)
           (java.awt.event ActionListener)))

(defmacro frame [& args] `(new JFrame ~@args))
(defmacro button [& args] `(new JButton ~@args))
(defmacro text [& args] `(new JTextField ~@args))

(defn test-document []
  (let [text-component (text)]
    (sw/do-swing (.setText text-component "pre-"))
    (.setDocument text-component
                  (doc/build-document text-component
                                      (doc/max-len 10)
                                      (doc/min-len 4)
                                      doc/all-upper))
    (doto (frame)
      (.add text-component)
      (.pack)
      (.setVisible true))))

(defn test-document []
  (let [text-component (text)]
    (.setText text-component "0x")
    (.setDocument text-component
                  (doc/build-document text-component
                                      doc/all-upper
                                      ))
    (doto (frame)
      (.add text-component)
      (.pack)
      (.setVisible true))))


(defn test-document []
  (let [text-component (text)]
    (.setDocument text-component
                  (doc/build-document text-component
                                      doc/all-upper
                                      (doc/matches #"[A-F0-9]*")))
    (.setText text-component "0X")
    (doto (frame)
      (.add text-component)
      (.pack)
      (.setVisible true))))

(defn -main []
  (test-document))

(defmacro on-action [component & body]
  `(. ~component ~'addActionListener
      (proxy [java.awt.event.ActionListener] []
        (~'actionPerformed [~'event] ~@body))))

(defmacro on-mouse-over [component & body]
  `(. ~component ~'addMouseListener
      (proxy [java.awt.event.MouseAdapter] []
        (mouseEntered [~'event] ~@body))))

(defmacro on-mouse-out [component & body]
  `(. ~component ~'addMouseListener
      (proxy [java.awt.event.MouseAdapter] []
        (mouseExited [~'event] ~@body))))

(def mouse-listener-map
  {:on-mouse-over    :mouseEntered
   :on-mouse-out     :mouseExited
   :on-click         :mouseClicked
   :on-mouse-press   :mousePressed
   :on-mouse-release :mouseReleased})

(def f {:on-click '(prn "wdw")
        :on-mouse-out '(prn "out")})

(defn- make-proxy-fn [pair]
  (list
   (symbol (name ((first pair) mouse-listener-map)))
   ['event]
   (second pair)))

(defmacro make-listeners [fnmap]
  `(proxy [java.awt.event.MouseListener] []
     ~@(map make-proxy-fn fnmap)))

;;(defn make-mouse-listener [fnmap]
;;  (proxy [MouseListener] [] 
;;	(mouseClicked [e] (when-let [f (:clicked fnmap)] (f e))) 
;;	(mousePressed [e] (when-let [f (:pressed fnmap)] (f e))) ...))
;;

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

;;(def-watched x (* 12 2))
