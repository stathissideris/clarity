(ns
    ^{:doc "Easy creation of forms."
      :author "Stathis Sideris"}
  clarity.form
  (:require [clojure.string :as str]
            [clojure.contrib.miglayout :as mig]
            [clarity.component :as c]
            [clarity.style :as style]
            [clarity.functions :as fun]
            clarity.list)
  (:import [javax.swing BorderFactory]))

(def special-tag-keys #{:header :group :end-group :text})
(def field-flags #{:full-width})
(def header-flags #{})
(def group-flags #{})
(def text-flags #{:rich})

(defn- interpose-every [every sep coll]
  (let [piece (fn piece [sep coll every]
                (when (and coll (not (empty? coll)))
                  (concat (take every coll)
                          [sep]
                          (piece sep (drop every coll) every))))]
    (butlast (piece sep coll every))))

(defn- destructure-flags [flags & args]
  {:flags (apply hash-set (filter flags args))
   :args (apply hash-map (remove flags args))})

(defn- header? [x] (and (sequential? x) (= (first x) :header)))
(defn- group? [x] (and (sequential? x) (= (first x) :group)))
(defn- end-group? [x] (and (sequential? x) (= (first x) :end-group)))
(defn- text? [x] (and (sequential? x) (= (first x) :text)))

(defn- special-tag? [x]
  (or (header? x)
      (group? x)
      (end-group? x)
      (text? x)))

(defn- make-header [_ s & {level :level}]
  (list
   (c/do-component
    (if level
      (c/make :label s [:category (keyword (str "header-" level))])
      (c/make :label s [:category :header]))
    (:font (style/font :size "x1.5" :style :bold)))
   [:span 2]))

;;TODO under development
(defn- start-group [[_ title] args]
  (c/make :panel
          [:border (BorderFactory/createTitledBorder title)]))

(defn- make-text [_ text & flags]
  (let [para (apply c/para text flags)]
    (dosync (.addCategory para :text))
    (list para [:span 2])))

(defn- handle-special-tag [tag]
  (cond (header? tag) (apply make-header tag)
        (group? tag) (apply start-group tag)
        (text? tag) (apply make-text tag)))

(defn- tokens [form]
  (loop [f form
         tok ()]
    (cond (not (seq f)) ;;finished, reverse and return
          (apply vector (reverse tok))
          (special-tag? (first f)) ;;special, take one, recur with the rest
          (recur (next f) (conj tok (first f)))
          (and (> (count f) 2) ;;normal case where there are extra params, take 2
               (sequential? (nth f 2))
               (not (special-tag? (nth f 2))))
          (recur (next (nnext f)) (conj tok (apply vector (take 3 f))))
          :else ;;normal case, no params,keep 2
          (recur (nnext f) (conj tok (apply vector (take 2 f)))))))

(defn- boolean? [x] (= java.lang.Boolean (class x)))

(defn- extra-params? [token]
  (= 3 (count token)))

(defn- destructure-token-flags [flags token]
  (apply destructure-flags flags (nth token 2)))

(defn- make-label-text [token]
  (if (extra-params? token)
    (let [{:keys [args]} (destructure-token-flags field-flags token)]
      (if (contains? args :label)
        (:label args)
        (make-label-text (butlast token))))
    (str/capitalize (str/replace (name (first token)) "-" " "))))

;;TODO allow the combo boxes to have an initial value
(defn- make-field [token]
    (let [[id param] token
          field
          (cond (keyword? param)
                (cond (= :number param) (c/make :text-field [:id id])
                      (= :string param) (c/make :text-field [:id id]))
                (string? param) (c/make :text-field
                                        [:text param]
                                        [:id id])
                (number? param) (c/make :text-field
                                        [:text (str param)]
                                        [:id id])
                (boolean? param) (c/make :check-box
                                         [:selected param]
                                         [:id id])
                (sequential? param) (c/make
                                     :combo-box data
                                     [:init (to-array param)]
                                     [:id id])
                (instance? java.awt.Component param) param)]
      (if (nil? (.getName field)) (.setName field (make-label-text token)))
      (dosync (c/add-category field :field))
      field))

(defn- make-label [text]
  (c/make :label text (:category :label)))

(defn- handle-form-token [token]
  (if (special-tag? token)
    (handle-special-tag token)
    (list (make-label (make-label-text token))
          (make-field token)
          :sg))) ;;to achieve equal heights

(defn- make-form-panel [mig-params]
  (apply mig/miglayout (c/make :panel (:category :form))
         :layout "wrap 2"
         :column "[left][grow,fill]"
         mig-params))

(defn- params-to-mig-params [params]
  (reduce concat
          (map handle-form-token
               (tokens params))))

(defn form
  "Simple usage:

    (clarity.dev/show-comp
      (form :first-name \"\"
            :surname \"\"
            :age 10
            :sex [\"Male\" \"Female\"]))

  Advanced usage:

    (def f
      (form [:header \"Personal details\"]
            [:text \"Use the form *below* to enter your details\" :rich]
            :first-name \"\"
            :surname \"\"
            :age (make :spinner (:id :age)) [:label \"Your age\"]
            :married false 
            :sex [\"male\" \"female\"]))

    (clarity.dev/show-comp f)
    (value f)

  This function contructs a form based on the parameters passed. In
  the simplest case, the parameters are pairs of keys and values. The
  keys are used as IDs for the corresponding form fields and for
  inferring the form field labels, and the values are used in order to
  infer what form fields are going to be and their initial values. For
  example, a pair of :first-name \"Bob\" results in the creation of a
  JTextField with the contents of \"Bob\" and before it a JLabel with
  \"First Name\" as its text. Text fields are also constructed for
  number values. If the value is boolean, a JCheckbox is
  inserted (selected on not, depending on the value). If the value is
  a vector, a JComboBox is added, with the passed strings as its
  options (there is currently no way to control which option is
  initially selected -- it's always the first one).

  Note that because each field is given an ID, it is then possible to
  call (clarity.component/value) on the whole form and collect the
  values of the fields in a map that looks very similar to the
  original parameters. Similarly, you can call set-value and pass a
  map with values for each component (or just some of them).

  In its more advanced usage, the key/value pairs are interrupted by
  vectors, which either add special elements to the form or somehow
  affect the way the field is constructed.

  [:header \"My header\"] results in a header being added to the form,
  which can be used to seperate different sections. You can use
  [:header \"My sub-header\" :level 2] to make the header have a
  different header category (see below).

  [:text \"Help *text*.\" :rich] results in a rich para to be inserted
  in the form (see documentation of clarity.component/para).

  The default generated label can be overriden by passing a vector
  after the value which looks like [:label \"My field\"].

  If a java.awt.Component is passed as a value, then it is used as it
  is in the form. Note that in such cases you will usually want to
  have assigned an ID to the passed component from before, as IDs are
  currently immutable, so the function cannot assign a new ID to the
  component to match the key.

  The constructed form uses categories heavily to allow styling. The
  form JPanel is assigned the :form category, all fields are assigned
  the :field category, and their labels the :label category. Headers
  are assigned the :header category or :header-X where X is the level
  that was passed with the :level flag while constructing the
  form. Text paragraphs are assigned the :text category."

  [& components]
  
  (make-form-panel (params-to-mig-params components)))
