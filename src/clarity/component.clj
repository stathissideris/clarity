(ns clarity.component
  "This namespace is the core of the clarity library. The two main
  functions are make and do-component."
  (:require [clojure.string :as str]
            [clojure.contrib.str-utils2 :as str2]
            [clarity.event :as event]
            [clarity.style :as style])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane JEditorPane JFileChooser]
           [javax.swing.text.html HTMLEditorKit]
           [com.petebevin.markdown MarkdownProcessor]))

(definterface Component
  (getId []))

;;TODO does not work!
(defn component? [x] (instance? clarity.component.Component x))

(defn id
  "Get the identifier of the passed component. Returns nil if there is
  an exception."
  [c]
  (try
    (.getId c)
    (catch Exception e nil)))

(def special-setters #{:init :id :category :categories})

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn categories
  "Get the categories of the component."
  [comp] (.getCategories comp))

(defn add-category
  "Add a category to the component."
  [comp s] (.addCategory comp s))

(defn remove-category
  "Remove a category from the component"
  [comp s] (.removeCategory comp s))

(defn has-category
  "Check whether the component has the passed category."
  [comp category]
  (try
    (contains? (categories comp) category)
    (catch Exception e false)))

(defprotocol HasValue
  "A component that has a value that can be retrieved and set."
  (value [this])
  (set-value [this value]))

(extend-type javax.swing.JTextField
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

(defn component-name [component]
  (if (.getName component)
    (.getName component)
    (str (.getName (.getClass component)) "#" (.hashCode component))))

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

;;;;;;;;;;;;

(defn special-setter-form? [form]
  (if (sequential? form)
    (let [[k v] form]
      (contains? special-setters k))))

(defn event-form? [form]
  (if (sequential? form)
    (let [[k v] form]
      (contains? (into #{} (keys clarity.event/event-map)) k))))
;;TODO optimize?

(defn event-form-listener [[k v]]
  (event/make-listener-keyword
   (first (get event/event-map k))))

(defn simple-setter-form? [form]
  (and (sequential? form)
       (not (special-setter-form? form))
       (not (event-form? form))))

(defn pairs-to-map [p]
  (into {} (for [[k & v] p] [k (apply vector v)])))

(defn- parse-component-params
  "Expects a form like (class-type const-params* [setters]*) and
  splits them into const-params, special-setter forms (:id etc) event-
  forms and setter-forms."
  [params]
  (let [const-params (remove sequential? params)
        special-setter-forms (pairs-to-map
                              (filter special-setter-form? params))
        event-forms (filter event-form? params)
        setter-forms (filter simple-setter-form? params)]
    {:constructor const-params
     :special-setter-forms special-setter-forms
     :event-forms event-forms
     :setter-forms setter-forms}))

(defn make-setter-name [name]
  (let [pieces (str/split name #"-")]
    (symbol (apply str ".set" (map str/capitalize pieces)))))

(defn- process-event-form
  "Expects a form like (:listener-name [(:on-event (code))+]) and
  produces a bit of code that is suitable to be used in a doto macro
  to add the listener to the component."
  [[key handler-forms]]
  `(~(event/listener-keyword-to-adder key)
    (event/listener ~key ~@handler-forms)))

(defmacro do-component
  "This macro is similar to clojure.core/doto, and in fact it supports
  an identical syntax. If an expression starts with a keyword, the
  name of the keyword is used to derive the equivalent setter name and
  the setter is invoked instead. For example, the following are
  equivalent:

     (do-component frame
       (.setVisible true))

     (do-component frame
       (:visible true))

  Dashes in the keywords are translated to camel case,
  i.e. :focus-painted results in .setFocusPainted to be called.

  This function also supports easily adding event handling to the
  passed component. For example you can say:

     (do-component button
       (:on-click (println \"clicked!\"))
       (:on-mouse-over (.setText this \"mouse over\")))

  This will create a listener using clarity.event/listener and adds it
  to the component. See the documentation of the event namespace for
  more details.

  do-component binds \"this\" to the component that was passed. Also,
  the event expressions are grouped together so that the above example
  produces a single javax.swing.event.MouseListener instead of two.

  The expressions can be lists or vectors, either will work."
  
  [component & expressions]
  (let [{:keys [event-forms
                setter-forms]} (parse-component-params (conj expressions :dummy))
                event-forms (group-by event-form-listener event-forms)]
    `(let [~'this ~component]
       (~'doto ~'this
         ~@(map (fn [exp]
                  (conj
                   (drop 1 exp)
                   (if (keyword? (first exp))
                     (make-setter-name (name (first exp)))
                     (first exp)))) setter-forms)
         ~@(map process-event-form event-forms)))))

(defn make-class-name [component]
  (let [name (name component)
        ns (namespace component)
        prefix (if ns
                 (if (.startsWith ns "awt")
                   (str "java." ns ".")
                   (str "javax.swing." ns ".J"))
                 "javax.swing.J")]
  (apply str prefix
         (map str/capitalize
              (str/split name #"-")))))

(defn- process-special-setter [[key [& params]]]
  (cond (or (= :category key) (= :categories key))
        `(dosync ~@(map (fn [cat] `(.addCategory ~'result ~cat)) params))))

(defmacro make-component [component
                          const-params
                          special-setters]
  {:pre [(sequential? const-params)
         (map? special-setters)]}
  (let [clazz (if (keyword? component)
                (symbol (make-class-name component))
                component)
        init-params (if (contains? special-setters :init)
                      (:init special-setters)
                      const-params)]
    ;;TODO: really ref?
    `(let [~'id ~(if (contains? special-setters :id)
                   (first (:id special-setters)))
           ~'cat (ref #{})
           ~'result
           (proxy [~clazz Component clarity.style.Styleable] [~@init-params]
             (~'getId [] ~'id)
             ~@(style/styleable-mixin))]
       ~@(map process-special-setter special-setters)
       ~'result)))

(defmacro make
  "Usage:

     (make :button \"Hello\"
           (:id :hello-button)
           (:category :plain-button)
           (:border nil))
 
  Creates a proxy to a Swing component that is also a
  clarity.component.Component and a clarity.styleable.Styleable.

  The first parameter is either a Java class or a keyword. If a Java
  class is passed, it is used as is in the created proxy. Keywords are
  traslated to Swing class names (:button becomes
  javax.swing.JButton). Namespace-qualified symbols are translated to
  the respective Swing sub-namespace (:table/table-header becomes
  javax.swing.table.JTreeHeader). If you really need to use AWT
  components, then prefix the keyword with the awt
  namespace (:awt/button becomes java.awt.Button).

  The parameters after the first one are interpreted as parameters to
  the constructor of the Swing component, until a list is
  encountered. This means that you can say

    (make :button \"Hello\")

  but (make :button (str \"Hello \" name)) will *not* work because the
  the list (str \" Hello \" name) is not interpreted as a parameter to
  the constructor. You can achieve the above by using the :init
  special form, as in:

    (make :button
          (:init (str \"Hello \" name)))

  This complication is necessary because the macro passes the lists to
  a call to (do-component). There are 3 exceptions to that. The :init
  form which passes its parameters the constructor of the component,
  and the :id and :category forms. The :id form is used to define an
  identifier for the component and the :categories form is used to
  assign a number of categories to the component. These are useful for
  selection and styling purposes. The categories are analogous to CSS
  classes in HTML. You can use :category as a synomym.

  Any remaining forms are passed to (do-component) which uses them to
  call setters of the component or create event handlers to attach to
  the component (see the relevant documentation).

  So a let's have a closer look at the above example:

     (make :button \"Hello\"
             ;;(javax.swing.JButton \"Hello\")

           (:id :hello-button)
             ;;sets the ID to :hello-button

           (:category :plain-button)
             ;;sets the categories to :plain-button

           (:border nil))
             ;;passed to (do-com0ponent), calls .setBorder"

  [& args]
  (let [{:keys [constructor
                special-setter-forms
                event-forms
                setter-forms]} (parse-component-params args)]
    (if (and (empty? setter-forms) (empty? event-forms))
      `(make-component ~(first constructor)
                       ~(rest constructor)
                       ~special-setter-forms)
      `(do-component
        (make-component ~(first constructor)
                        ~(rest constructor)
                        ~special-setter-forms)
        ~@(concat setter-forms
                  event-forms)))))

(defn para
  "Creates a paragraph of text (using JEditorPane) that wraps
  according to the available width. It is also possible to copy the
  text and the input can be HTML. If the :rich flag is passed, the
  text is processed as markdown."
  [s & flags]
  (let [font (style/get-laf-property "Label.font")
        rich? (some #{:rich} flags)
        text (if rich?
               (str2/replace
                (.markdown (MarkdownProcessor.) s)
                "\n" "<br>")
               s)
        rule (str "body { font-family: "
                  (.getFamily font)
                  "; "
                  "font-size: "
                  (.getSize font)
                  "pt; }")
        pane (doto (make :editor-pane
                         [:init (.getContentType (HTMLEditorKit.)) text])
               (.setText text)
               (.setOpaque false)
               (.setBorder nil)
               (.setEditable false))]
    (.addRule (.getStyleSheet (.getDocument pane)) rule)
    pane))

(defn scroll-pane [comp]
  (make :scroll-pane comp))

(defn choose-file
  "Opens a file selection dialog and returns the absolute path of the
  selected file. The initial path can be passed, and if the second
  parameter is :save, the dialog is opened in save mode."
  ([] (choose-file nil))
  ([path] (choose-file path :load))
  ([path flag]
     (let [chooser (JFileChooser. path)
           option (if (= :save flag)
                    (.showSaveDialog chooser nil)
                    (.showOpenDialog chooser nil))]
       (if (= JFileChooser/APPROVE_OPTION option)
         (.getAbsolutePath (.getSelectedFile chooser))))))

;;example with events
#_(show-comp (make :button "testing events"
                 (:on-mouse-exited (.setText this "exited"))
                 (:on-mouse-over (.setText this "over"))))