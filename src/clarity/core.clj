(ns clarity.core
  (:require [clarity.chain :as chain]
            [clarity.document :as doc]
            [clarity.renderer :as render]
            [clarity.list :as l])
  (:import (javax.swing JFrame JButton JTextField JList JScrollPane)
           (java.awt.event ActionListener)))

(defmacro frame [& args] `(new JFrame ~@args))
(defmacro button [& args] `(new JButton ~@args))
(defmacro text [& args] `(new JTextField ~@args))
(defmacro jlist [& args] `(new JList ~@args))

#_(defn test-document []
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

(defn test-list-render []
  (let [list-comp (jlist (l/immutable-list-model
                          ["ena" "dyo" "tria" "tessara" "pede"]))]
    (.setCellRenderer list-comp
                      (render/make-list-renderer
                       render/stripy
                       render/stripy-selection))
    (doto (frame)
      (.add (new JScrollPane list-comp))
      (.pack)
      (.setVisible true))))

(def ldata (ref ["ena" "dyo" "tria" "tessara" "pede"]))
(defn test-list-render2 []
  (let [list-comp (jlist (l/mutable-list-model ldata))]
    (.setCellRenderer list-comp
                      (render/make-list-renderer
                       render/stripy
                       render/stripy-selection))
    (doto (frame)
      (.add (new JScrollPane list-comp))
      (.pack)
      (.setVisible true))))

(defn -main []
  (test-list-render2))

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
