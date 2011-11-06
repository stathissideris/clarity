(ns clarity.renderer
  (require [clarity.chain :as chain])
  (import [java.awt Color]))

(def background1 java.awt.Color/white)
(def background2 (new Color 240 240 240))

(def selection-foreground java.awt.Color/white)
;;(def selection-background2 (utils/get-laf-property "List.selectionBackground"))
(def selection-background2 (new Color 49 106 197))
(def selection-background1 (.brighter selection-background2))

(def success-background1 (new Color 80 217 80))
(def success-background2 (.brighter success-background1))

(def error-background1 (new Color 255 192 203))
(def error-background2 (new Color 255 220 226))

(def selection-error-background1 (new Color 150 148 199))
(def selection-error-background2 (new Color 150 162 211))


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

(defn make-label [f]
  (fn [component list-component value index selected focused]
    (.setText component (f value index selected focused))
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
