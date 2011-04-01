(ns clarity.core
  (:require [clojure.contrib.pprint :as pp]
			[clojure.string :as str]
			[clarity.chains :as chains])
  (:import (javax.swing JFrame JButton JTextField)
		   (java.awt.event ActionListener)))

(defmacro frame [& args] `(new JFrame ~@args))
(defmacro button [& args] `(new JButton ~@args))
(defmacro text [& args] `(new JTextField ~@args))

;;(def frame1
;;  (doto (frame)
;;	(.add (doto (button "test2")
;;			(on-action (prn "clicked2"))
;;			(on-mouse-over (prn "mouse over"))))
										;   (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
;;	(.pack)
;;	(.setVisible true)))

;;(def frame1
;;  (doto (frame)
;;	(.add (doto (text "test2")
;;			(.setDocument doc)))
;;	(.pack)
;;	(.setVisible true)))

;;(.dispose frame1)

(def capitalize-document
  {:insert (fn [component offset str attr]
			 [component offset (str/upper-case str) attr])})
(defn max-len-document [max]
  {:insert (fn [component offset str attr]
			 (if (< max (+ (count (.getText component)) (count str)))
			   :veto
			   [component offset str attr]))})
(defn min-len-document [min]
  {:remove (fn [component offset length]
			(if (> min (- (count (.getText component)) length))
			  :veto
			  [component offset length]))})

(defn test-document []
  (let [text-component (text)]
	(.setText text-component "pre-")
	(.setDocument text-component
				  (build-document text-component
								  (max-len-document 10)
								  (min-len-document 4)
								  capitalize-document))
	(doto (frame)
	  (.add text-component)
	  (.pack)
	  (.setVisible true))))

;;works!!
(defn build-document [component & fnmaps]
  (let [inserts  (remove nil? (map #(:insert %1) fnmaps))
		replaces (remove nil? (map #(:replace %1) fnmaps))
		removes  (remove nil? (map #(:remove %1) fnmaps))]
	(proxy [javax.swing.text.PlainDocument] []
	  (insertString [offset string attributes]
					(let [results (chains/chain-vetoable inserts
														 component offset string attributes)]
					  (if (not= :veto results)
						(proxy-super insertString
									 (second results)
									 (nth results 2)
									 (nth results 3)))))
	  (remove [offset length]
			  (let [results (chains/chain-vetoable removes
												   component offset length)]
				(if (not= :veto results)
				  (proxy-super remove
							   (second results)
							   (nth results 2)))))
	  (replace [offset length text attributes]
			   (proxy-super replace offset length text attributes)))))


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
