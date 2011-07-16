(ns clarity.event
  (:require [clarity.component :as component]
            [clarity.utils :as utils]
            clojure.set
            [clojure.contrib.str-utils2 :as str2]))

(def awt-listeners
  (map (partial str "java.awt.event.")
       (utils/qw
        ActionListener
        AdjustmentListener
        AWTEventListener
        ComponentListener
        ContainerListener
        FocusListener
        HierarchyBoundsListener
        HierarchyListener
        InputMethodListener
        ItemListener
        KeyListener
        MouseListener
        MouseMotionListener
        MouseWheelListener
        TextListener
        WindowFocusListener
        WindowListener
        WindowStateListener)))

(def swing-listeners
  (map (partial str "javax.swing.event.")
       (utils/qw
        AncestorListener
        CaretListener
        CellEditorListener
        ChangeListener
        DocumentListener
        HyperlinkListener
        InternalFrameListener
        ListDataListener
        ListSelectionListener
        MenuDragMouseListener
        MenuKeyListener
        MenuListener
        ;;MouseInputListener ;;too much overlap with MouseListener
        PopupMenuListener
        TableColumnModelListener
        TableModelListener
        TreeExpansionListener
        TreeModelListener
        TreeSelectionListener
        TreeWillExpandListener
        UndoableEditListener)))

(def event-synonyms
  {:on-action
   ["java.awt.event.ActionListener" "actionPerformed"]
   :on-click
   ["javax.swing.event.MouseListener" "mouseClicked"]
   :on-mouse-over
   ["javax.swing.event.MouseListener" "mouseEntered"]
   :on-mouse-out
   ["javax.swing.event.MouseListener" "mouseExited"]})
;;TODO on double click
;;TODO on right click

(defn split-camel
  "Seperate camel-case with spaces."
  [s]
  (-> s
      (str2/replace #"[A-Z]" #(str " " %))
      (str2/trim)
      (.toLowerCase)
      (str2/split #" ")))

(defn first-lower [s]
  (str (str2/lower-case (str2/take s 1))
       (str2/drop s 1)))

(defn make-listener-keyword [clazz]
  (keyword
   (str2/lower-case
    (str2/join
     "-"
     (-> clazz
         (str2/replace "Listener" "")
         (str2/replace "java.awt.event." "")
         (str2/replace "javax.swing.event." "")
         (split-camel))))))

(defn listener-keyword-to-adder [key]
  (symbol
   (str ".add"
        (apply str
               (map str2/capitalize
                    (str2/split (name key) #"-"))) "Listener")))

(defn make-event-keyword
  "Make the event keyword used in the components special setters from
  a method object."
  [method]
  (keyword
   (str "on-"
        (str2/join "-"
                   (split-camel (.getName method))))))

;;(defn parse-event-keyword [key]
;;  (symbol
;;   (first-lower
;;    (apply str (map str2/capitalize
;;                    (drop 1 (str2/split (name key) #"-")))))))

(def listener-map
  (into {}
        (map (fn [clazz]
               [(make-listener-keyword clazz)
                {:classname (symbol clazz)
                 :methods
                 (map #(.getName %) (.getMethods (Class/forName clazz)))}])
             (concat awt-listeners swing-listeners))))

(def event-map
  (merge
   event-synonyms
   (into {}
         (apply
          concat
          (map (fn [name]
                 (let [clazz (Class/forName name)]
                   (for [method (.getMethods clazz)]
                     [(make-event-keyword method)
                      [(.getName clazz)
                       (.getName method)]])))
               (concat awt-listeners swing-listeners))))))

(defn lookup-event-keyword [key]
  (symbol
   (second (get event-map key))))

(reify java.awt.event.KeyListener
  (keyPressed [this event] (print "pressed"))
  (keyReleased [this event] (print "released"))
  (keyTyped [this event] (print "typed")))

(defmacro listener
  "(listener :key
     (:on-key-typed (print \"typed\"))
     (:on-key-pressed (print \"pressed\"))
     (:on-key-released (print \"released\")))"
  [listener & handlers]
  (let [interface (:classname (get listener-map listener))
        required-methods (into
                          #{}
                          (map #(symbol %)
                               (:methods (get listener-map listener))))
        passed-methods (into
                        #{}
                        (map #(lookup-event-keyword (first %)) handlers))
        missing-methods (clojure.set/difference
                         required-methods passed-methods)]

    `(reify ~interface
       ~@(map (fn [method] `(~method [~'_ ~'_])) missing-methods)
       ~@(map (fn [method body] `(~method [~'listener ~'event] ~@body))
           (map #(lookup-event-keyword (first %)) handlers)
           (map rest handlers)))))

;;;; example ;;;;
;; (let [button (component/make :button "click")]
;;  (doto button
;;    (.addActionListener
;;     (listener :action (:on-action (.setText button "ok"))))
;;    (.addMouseListener
;;     (listener :mouse
;;               (:on-mouse-over (.setText button "on"))
;;               (:on-mouse-out (.setText button "out"))))
;;    (show-comp)))
