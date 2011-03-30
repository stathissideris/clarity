(ns clarity.core
  (:require [clojure.contrib.pprint :as pp]
			[clojure.string :as str])
  (:import (javax.swing JFrame JButton JTextField)
		   (java.awt.event ActionListener)))

(defmacro frame [& args] `(new JFrame ~@args))
(defmacro button [& args] `(new JButton ~@args))
(defmacro text [& args] `(new JTextField ~@args))

(def frame1
  (doto (frame)
	(.add (doto (button "test2")
			(on-action (prn "clicked2"))
			(on-mouse-over (prn "mouse over"))))
										;   (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
	(.pack)
	(.setVisible true)))

(def frame1
  (doto (frame)
	(.add (doto (text "test2")
			(.setDocument doc)))
	(.pack)
	(.setVisible true)))

(.dispose frame1)

(defmacro make-document [& fnmaps]
  (let [inserts (remove nil? (map #(:insert %1) fnmaps))
		updates (remove nil? (map #(:update %1) fnmaps))
		removes (remove nil? (map #(:remove %1) fnmaps))]
	`(proxy [javax.swing.text.PlainDocument] []
	   (~'insertString [~'offset ~'string ~'attributes] ~@inserts)
	   (~'insertUpdate [~'event ~'attributes] ~@updates)
	   (~'removeUpdate [~'change] ~@removes))))

(def default-document {:insert #(proxy-super insertString offset string attributes)
					   :update #(proxy-super insertUpdate event attributes)
					   :remove #(proxy-super removeUpdate change)})

(def doc (proxy [javax.swing.text.PlainDocument] []
		   (insertString [offset string attributes]
						 (proxy-super insertString offset (str/capitalize string) attributes))))
						 
(defn make-document [& fnmaps]
  (let [inserts (remove nil? (map :insert fnmaps))
		updates (remove nil? (map :update fnmaps))
		removes (remove nil? (map :remove fnmaps))]
	(proxy [javax.swing.text.PlainDocument] []
	  (

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


;;how to compose the string-"mutating" functions of the document
(def f #(if (< %1 10)
		  (do (print %1) (+ 2 %1)) :stop))

(loop [fns [f f f f f f], x 0]
  (let [r ((first fns) x)]
	(if (= :stop r) :stop (recur (next fns) r))))

(loop [xs [1 2 3 4 5 6]]
  (if xs (do (prn xs) (recur (next xs))) :stop))

(defn- apply-recursively-stop [value functions]
  (loop [fns functions, x value]
	(if (not fns) x
		(let* [f (first fns)
			   r (f x)]
			  (if (= :stop r) :stop
				  (recur (next fns) r))))))
;;;



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

(def-watched x (* 12 2))
