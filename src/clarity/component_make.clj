(in-ns 'clarity.component)

(ns clarity.style)
(defn apply-stylesheet [_ _])
(in-ns 'clarity.component)

;;; this part of the namespace implements the construction and
;;; manipulation of components

(def ^{:dynamic true} *default-stylesheet* nil)

(def special-setters #{:init :id :category :categories})
(def class-name-map
  {:has-value 'clarity.component.HasValue
   :has-selection 'clarity.component.HasSelection
   :has-children 'clarity.component.HasChildren})

(defn implementation-form? [form]
  (and (sequential? form) (= :impl (first form))))

(defn special-setter-form? [form]
  (and (not (implementation-form? form))
       (if (sequential? form)
         (let [[k v] form]
           (contains? special-setters k)))))

(defn event-form? [form]
  (if (sequential? form)
    (let [[k v] form]
      (contains? (into #{} (keys clarity.event/event-map)) k))))
;;TODO optimize?

(defn event-form-listener [[k v]]
  (event/make-listener-keyword
   (first (get event/event-map k))))

(defn simple-setter-form? [form]
  (and (not (implementation-form? form))
       (sequential? form)
       (not (special-setter-form? form))
       (not (event-form? form))))

(defn pairs-to-map [p]
  (into {} (for [[k & v] p] [k (apply vector v)])))

(defn- parse-component-params
  "Expects a form like (class-type const-params* [setters]*) and
  splits them into const-params, special-setter forms (:id etc) event-
  forms, setter-forms and the implementation form (if any)."
  [params]
  (let [type (first params)
        params (drop 1 params)
        const-params (remove sequential? params)
        special-setter-forms (pairs-to-map
                              (filter special-setter-form? params))
        event-forms (filter event-form? params)
        setter-forms (filter simple-setter-form? params)
        implementation-forms (rest (first
                                   (filter implementation-form? params)))]
    {:constructor (cons type const-params)
     :special-setter-forms special-setter-forms
     :event-forms event-forms
     :setter-forms setter-forms
     :implementation-forms implementation-forms}))

(defn make-setter-name [s]
  (if (keyword? s)
    (let [pieces (str/split (name s) #"-")]
      (symbol (apply str ".set" (map str/capitalize pieces))))
    s))

(defn- process-event-form
  "Expects a form like (:listener-name [(:on-event (code))+]) and
  produces a bit of code that is suitable to be used in a doto macro
  to add the listener to the component."
  [[key handler-forms]]
  `(~(event/listener-keyword-to-adder key)
    (event/listener ~key ~@handler-forms)))

(defn- proxy-fn-to-update-proxy-key-value
  "Converts this

    (setText [text]
      (.setText this text))

  to this:

    [\"setText\"
      (fn [this text]
        (.setText this text))]"
  [[fn-name args & body]]
  (let [method-name (name (make-setter-name fn-name))
        method-name (if (.startsWith method-name ".")
                      (apply str (drop 1 method-name))
                      method-name)]
    [method-name
     `(fn [~@(concat ['this] args)] ~@body)]))

(defmacro proxy-updater
  "Produces a function that can update a proxy with a new
  implementation. The input is a series of forms that look like the
  function forms of (proxy). These are converted into a map compatible
  with (update-proxy)."
  [& fn-forms]
  `(fn [~'proxy]
     (update-proxy
      ~'proxy
      ~(into {} (map proxy-fn-to-update-proxy-key-value fn-forms)))))

(defmacro mutator
  "Generates a function with one parameter which should be a
  component. When the function is invoked, the expressions are called
  on the component, using the same syntax as (do-component)."
  [& expressions]
  (let [{:keys [event-forms
                setter-forms
                implementation-forms]}
        (parse-component-params (concat [:dummy] expressions))
        event-forms (group-by event-form-listener event-forms)]
    `(fn [~'this]
       ~(if (not (empty? implementation-forms))
          `((proxy-updater ~@implementation-forms) ~'this))
       ~(if (or (not (empty? setter-forms))
                (not (empty? event-forms)))
          `(~'doto ~'this
             ~@(map (fn [exp]
                      (conj
                       (drop 1 exp)
                       (make-setter-name (first exp))))
                    setter-forms)
             ~@(map process-event-form event-forms)))
       ~'this)))

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

  The expressions can be lists or vectors, either will work.

  Finally, it is possible to override/implement methods of the
  component class within an optional form that starts with :impl. This
  is possible after component construction for proxies (which is
  anything created using (make)). The contents of the :impl form have
  an identical syntax to the method definitions in the proxy
  macro. For example, here is a button whose value is always returned
  as 10 (it is assumed that it implements the HasValue interface) and
  its text is always \"foo\":

    (do-component my-button
      (:impl (.getText [] \"foo\")
             (value [] 10)))

  Note that any :impl forms are evaluated before the setter calls in
  the same (make), irrespective of the actual order that they appear
  in the code."
  
  [component & expressions]
  `((mutator ~@expressions) ~component))

(defn make-class-name [component]
  (cond (symbol? component)
        component
        
        (contains? class-name-map component)
        (get class-name-map component)

        :else
        (let [name (name component)
              ns (namespace component)
              prefix (if ns
                       (if (.startsWith ns "awt")
                         (str "java." ns ".")
                         (str "javax.swing." ns ".J"))
                       "javax.swing.J")]
          (apply str prefix
                 (map str/capitalize
                      (str/split name #"-"))))))

(defn make-class [name]
  (java.lang.Class/forName (make-class-name name)))

(defn- process-special-setter [[key [& params]]]
  (cond (or (= :category key) (= :categories key))
        `(dosync ~@(map (fn [cat] `(add-category ~'result ~cat)) params))))

(defmacro make-component [component
                          const-params
                          special-setters]
  {:pre [(sequential? const-params)
         (map? special-setters)]}
  (let [clazz (concat (cond (keyword? component)
                            [(symbol (make-class-name component))]
                            (sequential? component)
                            (map (comp symbol make-class-name) component)
                            :else
                            [component])
                      ['clarity.component.Component])
        init-params (if (contains? special-setters :init)
                      (:init special-setters)
                      const-params)]
    `(let [~'id ~(if (contains? special-setters :id)
                   (first (:id special-setters)))
           ~'result
           (proxy [~@clazz] [~@init-params]
             (get_id [] ~'id))]
       (update-proxy ~'result (component-mixin))
       ~@(map process-special-setter special-setters)
       
       ;;apply default style
       (let [~'style (util/resolve-value ~''*stylesheet*)]
         (when ~'style
           (clarity.style/apply-stylesheet ~'result ~'style)
           (do-component
            ~'result
            (:on-component-added
             (clarity.style/apply-stylesheet (.getChild ~'event) ~'style)))))

       ~'result)))

(defmacro make
  "Example:

     (make :button \"Hello\"
           (:id :hello-button)
           (:category :plain-button)
           (:border nil))
 
  Creates a proxy to a Swing component that is also a
  clarity.component.Component.

  The first parameter is either a Java class, a keyword or a
  vector. If a Java class is passed, it is used as is in the created
  proxy. Keywords are traslated to Swing class names (:button becomes
  javax.swing.JButton). Namespace-qualified symbols are translated to
  the respective Swing sub-namespace (:table/table-header becomes
  javax.swing.table.JTreeHeader). If you really need to use AWT
  components, then prefix the keyword with the awt
  namespace (:awt/button becomes java.awt.Button).

  If a vector is passed as the first parameter, each element of it
  should follow the same rules as described above to indicate one
  class and a number of interfaces implemented by the component. You
  can also use :has-value to implement the clarity.component.HasValue
  interface, and also :has-selection and :has-children for the other
  similarly-named interfaces of Clarity.

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
             ;;passed to (do-component), calls .setBorder

  Finally, it is possible to override/implement methods of the base
  class or the interfaces within an optional form that starts
  with :impl. The contents of this form have an identical syntax to
  the method definitions in the proxy macro. For example, here is a
  button whose value is always 10 (implements the HasValue interface)
  and its text is always \"foo\":

    (make [:button :has-value]
      (:impl (.getText [] \"foo\")
             (value [] 10)))

  Note that any :impl forms are evaluated before the setter calls in
  the same (make), irrespective of the actual order that they appear
  in the code.

  If your namespace contains a var called *stylesheet* then its value
  will be as the default stylesheet to be applied to all the
  components constructed using this macro."

  [& args]
  (let [{:keys [constructor
                special-setter-forms
                event-forms
                setter-forms
                implementation-forms]} (parse-component-params args)]
    #_(println (str "make: " args))
    #_(println (str "make: " (parse-component-params args)))
    (if (every? empty? [setter-forms
                        event-forms
                        implementation-forms])
      `(make-component ~(first constructor)
                       ~(rest constructor)
                       ~special-setter-forms)
      `(do-component
        (make-component ~(first constructor)
                        ~(rest constructor)
                        ~special-setter-forms)
        ~@(concat setter-forms
                  event-forms
                  [(concat [:impl] implementation-forms)])))))
