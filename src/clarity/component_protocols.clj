(in-ns 'clarity.component)

;;; this part of the namespace implements some common behaviours of the
;;; components concerning their values and selections

;;;;;; value

(defprotocol HasValue
  "A component that has a value that can be retrieved and set."
  (value [this])
  (set-value [this value]))

(extend-type javax.swing.JTextField
  HasValue
  (value [this] (.trim (.getText this)))
  (set-value [this value] (.setText this value)))

(extend-type javax.swing.JLabel
  HasValue
  (value [this] (.trim (.getText this)))
  (set-value [this value] (.setText this value)))

(extend-type javax.swing.JComboBox
  HasValue
  (value [this] (.getSelectedItem this))
  (set-value [this value] (.setSelectedItem this value)))

(extend-type javax.swing.JCheckBox
  HasValue
  (value [this] (.isSelected this))
  (set-value [this value] (.setSelected this value)))

(extend-type javax.swing.JSpinner
  HasValue
  (value [this] (.getValue this))
  (set-value [this value] (.setValue this value)))

(extend-type javax.swing.JSlider
  HasValue
  (value [this] (.getValue this))
  (set-value [this value] (.setValue this value)))

(defn component-name [component]
  (if (.getName component)
    (.getName component)
    (str (.getName (.getClass component)) "#" (.hashCode component))))

;;; selection

(defprotocol HasSelection
  (selection [this])
  (set-selection [this selection]))

(extend-type javax.swing.text.JTextComponent
  HasSelection
  (selection [this] [(.getSelectedText this)
                     (.getSelectionStart this)
                     (.getSelectionEnd this)])
  (set-selection [this selection] (.select this ;;TODO set doesn't work
                                           (first selection)
                                           (second selection))))

(extend-type javax.swing.JComboBox
  HasSelection
  (selection [this] (.getSelectedItem this))
  (set-selection [this selection] (.setSelectedItem this value)))

(extend-type javax.swing.JList
  HasSelection
  (selection [this] (.getSelectedValues this))
  (set-selection [this selection] (.setSelectedValues this selection)))

(extend-type javax.swing.JTree
  HasSelection
  (selection [this] (.getSelectionPaths this))
  (set-selection [this selection] (.setSelectionPaths this selection)))

;;; basic component structure

(defprotocol HasChildren
  (children [this])
  (count-children [this]))

(extend-type java.awt.Container
  HasChildren
  (children [this] (seq (.getComponents this)))
  (count-children [this] (alength (.getComponents this))))
