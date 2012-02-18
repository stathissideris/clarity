(ns clarity.event
  (:require clojure.set)
  (:use clarity.util))

(cond (clojure-1-2?) (require '[clojure.contrib.str-utils2 :as str])
      (clojure-1-3?) (require '[clojure.string :as str]))

(defmacro qw
  "Constructs a vector of the names (strings) of the passed symbols.
  This is to save you typing unneccesary quotes. Stolen from Perl.

  Example: (qw \"first name\" surname address)"
  [& words]
  `(vector ~@(map name words)))

(def awt-listeners
  (map (partial str "java.awt.event.")
       (qw
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
       (qw
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
      (str/replace #"[A-Z]" #(str " " %))
      (str/trim)
      (.toLowerCase)
      (str/split #" ")))

(defn first-lower [s]
  (str (str/lower-case (apply str (take 1 s)))
       (apply str (drop 1 s))))

(defn make-listener-keyword [clazz]
  (keyword
   (str/lower-case
    (str/join
     "-"
     (-> clazz
         (str/replace "Listener" "")
         (str/replace "java.awt.event." "")
         (str/replace "javax.swing.event." "")
         (split-camel))))))

(defn listener-keyword-to-adder [key]
  (symbol
   (str ".add"
        (apply str
               (map str/capitalize
                    (str/split (name key) #"-"))) "Listener")))

(defn make-event-keyword
  "Make the event keyword used in the components special setters from
  a method object."
  [method]
  (keyword
   (str "on-"
        (str/join "-"
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

(defmacro add-window-shortcut
  "Adds a keyboard shortcut to the component. This is detected when
  the window that contains the component is in focus. The expressions
  in body are executed when the shortcut is pressed. Binds 'this to
  the passed component."
  [component key-event key-modifiers & body]
  (let [action (name (gensym "ACTION_"))]
    `(let [~'this ~component]
       (.put (.getInputMap
              ~'this
              javax.swing.JComponent/WHEN_IN_FOCUSED_WINDOW)
             (javax.swing.KeyStroke/getKeyStroke ~key-event ~key-modifiers)
             ~action)
       (.put (.getActionMap
              ~'this)
              ~action
              (proxy [javax.swing.AbstractAction] []
                (~'actionPerformed [~'event] ~@body))))))

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

(defmacro timer
  "Creates a javax.swing.Timer with the specified delay, which
  executes the code in body at the specified intervals. Delay is in
  milliseconds. The timer waits the specified amount of time before
  executing the code for the first time, and after that, it continues
  executing the code every time the delay time has elapsed."
  [delay & body]
  `(javax.swing.Timer.
     ~delay
     (event/listener
      :action
      (:on-action-performed ~@body))))
