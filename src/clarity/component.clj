(ns clarity.component
  (:require [clojure.string :as str]
            [clojure.contrib.str-utils2 :as str2]
            [clarity.event :as event]
            [clarity.style :as style]
            [clarity.utils :as utils])
  (:import [clarity.style.Styleable]
           [javax.swing JSplitPane JScrollPane JEditorPane JFileChooser]
           [javax.swing.text.html HTMLEditorKit]
           [com.petebevin.markdown MarkdownProcessor]))

(definterface Component
  (getId []))

;;TODO does not work!
(defn component? [x] (instance? clarity.component.Component x))

(defn id [c]
  (try
    (.getId c)
    (catch Exception e nil)))

(def special-setters #{:init :id :category :categories})

(defn split-pane [orientation one two]
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn categories [comp] (.getCategories comp))
(defn add-category [comp s] (.addCategory comp s))
(defn remove-category [comp s] (.removeCategory comp s))
(defn has-category [comp category]
  (try
    (contains? (categories comp) category)
    (catch Exception e false)))

(defprotocol HasValue
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

(extend-type java.awt.Container
  HasValue
  (value [this]
    (apply hash-map
           (apply concat
                  (map (fn [x] [(id x) (value x)])
                       (filter
                        #(and (id %)
                              (satisfies? HasValue %)) (.getComponents this))))))
  (set-value [this value]))

(defprotocol HasSelection
  (selection [this])
  (set-selection [this selection]))

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

(defn parse-component-params
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

(defn process-event-form
  "Expects a form like (:listener-name [(:on-event (code))+]) and
  produces a bit of code that is suitable to be used in a doto macro
  to add the listener to the component."
  [[key handler-forms]]
  `(~(event/listener-keyword-to-adder key)
    (event/listener ~key ~@handler-forms)))

(defmacro do-component [component & expressions]
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

(defn process-special-setter [[key [& params]]]
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
  "Creates a Swing component which also implements the
  clarity.style.Styleable interface. The first parameter is a
  lisp-ified version of the normal names of swing components."
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
  (let [font (utils/get-laf-property "Label.font")
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