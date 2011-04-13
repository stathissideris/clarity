(ns clarity.renderer
  (require [clarity.chain :as chain]
           [clojure.contrib.string :as str])
  (import [java.awt Color]))

(def background1 java.awt.Color/white)
(def background2 (new Color 236 241 245))
(def selection-background1 (new Color 145 119 227))
(def selection-background2 (new Color 104 66 227))
(def selection-foreground java.awt.Color/white)

(defn stripy [component list-component value index selected focused]
  (if (even? index)
    (.setBackground component background1)
    (.setBackground component background2))
  [component list-component value index selected focused])

(defn stripy-selection [component list-component value index selected focused]
  (when selected
    (if (even? index)
      (.setBackground component selection-background1)
      (.setBackground component selection-background2))
    (.setForeground component selection-foreground))
  [component list-component value index selected focused])

(defn make-stripy [color1 color2]
  (fn [component list-component value index selected focused]
    (if (even? index)
      (.setBackground component color1)
      (.setBackground component color2))
    [component list-component value index selected focused]))

;;(defn make-bold [str-ref]
;;  (fn [component list-component value index selected focused]
;;    (if (string? value)      
;;      [component list-component value index selected focused])

(defn make-list-renderer [& renderers]
  (proxy [javax.swing.DefaultListCellRenderer] []
    (getListCellRendererComponent [list-component value index selected focused]
                                  (let [component
                                        (proxy-super getListCellRendererComponent
                                                     list-component value index selected focused)]
                                    (chain/chain renderers
                                                 component
                                                 list-component
                                                 value index
                                                 selected
                                                 focused) component))))
