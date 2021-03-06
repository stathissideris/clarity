(ns clarity.structure
  (:use clojure.walk)
  (:require clarity
            [clarity.component :as c]))

(defn comp-seq
  "Walks the contents of the passed clarity.component.HasChildren
  depth-first and returns the results as a lazy sequence."
  [root]
  {:pre [(satisfies? c/HasChildren root)]}
  (tree-seq (fn [node] (not (zero? (c/count-children node))))
            (fn [node] (c/children node))
            root))

(extend-type java.awt.Container
  c/HasValue
  (value [this]
    (into {}
          (map (fn [x] [(c/id x) (c/value x)])
               (filter
                #(and (c/id %)
                      (satisfies? c/HasValue %)) (.getComponents this)))))
  (set-value [this value]
    (let [components (comp-seq this)]
      (doseq [c components]
        (when (and (not (nil? (c/id c)))
                   (satisfies? c/HasValue c)
                   (contains? value (c/id c)))
          (c/set-value c (get value (c/id c))))))))

(defn find-by-id
  "Finds recursively the child of root (or root itself) that has the
  passed ID."
  [root id]
  (first (filter #(= id (c/id %)) (comp-seq root))))

(defn path
  "Returns a list representing the path of components from the
  top-most parent to comp."
  [comp]
  (loop [c comp p (list c)]
    (let [parent (.getParent c)]
      (if (nil? parent) p
        (recur parent (conj p parent))))))

(defn parent
  "Returns the parent of the component."
  [component]
  (.getParent component))

(defn previous-sibling
  "Returns the previous sibling of the component, nil if there is no
  parent or if component is the first child."
  [component]
  (let [p (parent component)]
    (if p
      (let [c (c/children p)
            index (.indexOf c component)]
        (if (not (zero? index))
          (nth c (dec index)))))))

(defn next-sibling
  "Returns the next sibling of the component, nil if there is no
  parent or if component is the last child."
  [component]
  (let [p (parent component)]
    (if p
      (let [c (c/children p)
            index (.indexOf c component)]
        (if (not= index (dec (count c)))
          (nth c (inc index)))))))

;; various matchers that can be composed to make complex selectors

(defn- get-cost
  "Get the cost associated with a matcher in order to decide whether
  to run the parent or the child matcher first in the
  direct-parent-matcher and the matching order for the
  or-matcher. This is a small optimisation and probably not worth it,
  but it was so easy and fun to develop that I couldn't resist."
  [matcher]
  (let [c (get (meta matcher) ::cost)]
    (if (nil? c) 1 c))) ;;assume 1 if not present

(defn- get-debug [matcher]
  (let [d (get (meta matcher) ::debug)]
    (if (nil? d) :no-debug d)))

(defn
  id-matcher
  "Produces a matcher function that accepts a component and tests
  whether it has the passed id."
  [id]
  (with-meta 
    (fn [component]
      (= id (c/id component)))
    {::cost 1
     ::debug [:id id]}))

(defn category-matcher
  "Produces a matcher function that accepts a component and tests
  whether it has the passed category."
  [category]
  (with-meta
    (fn [component]
      (c/has-category component category))
    {::cost 2
     ::debug [:category category]}))

(defn type-matcher
  "Produces a matcher function that accepts a component and tests
  whether it is an instance of tha passed class. If type is a keyword,
  the rules that are used in the component/make macro are also used to
  translate it to a Swing (or AWT) class."
  [type]
  (let [type (if (keyword? type)
               (c/make-class type)
               type)]
    (with-meta
      (fn [component]
        (instance? type component))
      {::cost 2
       ::debug [:type type]})))

(defn and-matcher
  [& matchers]
  (with-meta
    (fn [component]
      (every? #(= % true) (map #(% component) matchers)))
    {::cost (apply + (map get-cost matchers))
     ::debug (into ["and"] (map get-debug matchers))}))

(defn or-matcher
  [& matchers]
  (let [matchers (sort-by get-cost matchers)]
    (with-meta
      (fn [component]
        (loop [ms matchers]
          (if ms (if ((first ms) component)
                   true
                   (recur (next ms)))
              false)))
      {::cost (apply + (map get-cost matchers))
       ::debug (into ["or"] (map get-debug matchers))})))

(defn not-matcher
  [matcher]
  (with-meta
    (fn [component]
      (not (matcher component)))
    {::cost (get-cost matcher)
     ::debug ["not" (get-debug matcher)]}))

(defn any-matcher []
  (with-meta
    (fn [component] true)
    {::cost 0
     ::debug '*}))

(defn after-matcher
  "Produces a matcher function that matches if the passed component
  has a sibling that comes before it in the layout, the sibling
  matches before-m and the component itself matches this-m."
  [before-m this-m]
  (with-meta
    (fn [component]
      (let [sibling (previous-sibling component)]
        (and sibling
             (before-m sibling)
             (this-m component))))
    {::cost (+ (get-cost before-m) (get-cost this-m))
     ::debug ["after" (get-debug before-m) (get-debug this-m)]}))

(defn before-matcher
  "Produces a matcher function that matches if the passed component
  has a sibling that comes after it in the layout, the sibling
  matches before-m and the component itself matches this-m."
  [this-m after-m]
  (with-meta
    (fn [component]
      (let [sibling (next-sibling component)]
        (and sibling
             (this-m component)
             (after-m sibling))))
    {::cost (+ (get-cost this-m) (get-cost after-m))
     ::debug ["before" (get-debug this-m) (get-debug after-m)]}))

(defn direct-parent-matcher
  "Produces a matcher function that accepts a component and tests
  whether the direct parent of the component matches the passed
  parent-matcher and the component itself matches the
  child-matcher. If the component does not have a parent, the matcher
  does not match."
  [parent-matcher child-matcher]
  (let [child-first (> (get-cost parent-matcher) (get-cost child-matcher))
        m
        ;; produce a different function depending on which matcher
        ;; is more expensive
        (if child-first
          (fn [component]
            (let [parent (.getParent component)]
              (if (nil? parent) false
                  (and (child-matcher component) (parent-matcher parent)))))
          (fn [component]
            (let [parent (.getParent component)]
              (if (nil? parent) false
                  (and (parent-matcher parent) (child-matcher component))))))]
    (with-meta
      m
      {::cost (+ (get-cost parent-matcher)
                 (get-cost child-matcher))
       ::debug [:direct-parent (get-debug parent-matcher) (get-debug child-matcher)]
       ::priority
       (if child-first ::test-child-first ::test-parent-first)})))

(defn any-parent-matcher
  "Produces a matcher function that accepts a component and tests
  whether any of the ancestors of the component matches the passed
  parent-matcher and the component itself matches the
  child-matcher. If the component does not have a parent, the matcher
  does not match."
  [parent-matcher child-matcher]
  (with-meta
    (fn [component]
      (if (child-matcher component)
        (loop [parent (.getParent component)]
          (if (nil? parent) false
              (if (parent-matcher parent) true
                  (recur (.getParent parent)))))
        false))
    {::cost (+ (get-cost parent-matcher)
               (get-cost child-matcher))
     ::debug [:any-parent (get-debug parent-matcher) (get-debug child-matcher)]}))

(def ... "...")

(defn path-matcher*
  "A matcher that combines multiple matchers so that the direct
  parents (or indirect ancestors) of a component can be matched. The
  matchers are chained in the same order as in CSS (from parent to
  child). If you separate matchers with the ... symbol (or the \"...\"
  string), then a indirect matcher will combine the two
  matchers. Otherwise the matchers are always direct.

  Example:

    (path-matcher* (id-matcher :panel1)
                   ...
                   (category-matcher :cat1)
                   (type-matcher :button))

  This will match a button whose direct parent has the :cat1 category
  and one of the ancestors of the parent has an ID of :panel1."
  
  [& args]
  (loop [matcher-so-far (first args)
         matchers (next args)
         is-indirect false]
    (cond (nil? matchers) matcher-so-far
          (= ... (first matchers)) (recur matcher-so-far (next matchers) true)
          is-indirect (recur (any-parent-matcher
                              matcher-so-far
                              (first matchers))
                             (next matchers)
                             false)
          :else (recur (direct-parent-matcher
                        matcher-so-far
                        (first matchers))
                       (next matchers)
                       false))))

(defn- expand-matcher-shorthand
  "Converts symbols whose names match one of the following patterns:

  $button.important
  .important
  $button

  ...to forms appropriate for feeding into the (matcher) macro, like
  so:

  (and (id :button) (category :important))
  (category :important)
  (id :button)"
  [symbol]
  (if (or (not (symbol? symbol)) (= '... symbol))
    symbol
    (let [s (name symbol)
          [match id category] (re-matches #"^\$(.+)\.(.+)$" s)]
      (if match
        `(~'and
          (~'id ~(keyword id))
          (~'category ~(keyword category)))
        (let [[match category] (re-matches #"^\.(.+)$" s)]
          (if match
            `(~'category ~(keyword category))
            (let [[match id] (re-matches #"^\$(.+)$" s)]
              (if match
                `(~'id ~(keyword id))
                symbol))))))))

(defmacro matcher
  "This is a macro that makes the syntax of (path-matcher*) a bit
  lighter. It makes the following replaces to the first element of all
  passed expressions:

    id        id-matcher
    category  category-matcher
    type      type-matcher
    *         any-matcher
    or        or-matcher
    and       and-matcher
    not       not-matcher
    before    before-matcher
    after     after-matcher
    ...       clarity.structure/...
    path      path-matcher*

  The produced matcher combines multiple matchers so that the direct
  parents (or indirect ancestors) of a component can be matched. The
  matchers are chained in the same order as in CSS (from parent to
  child). If you separate matchers with the ... symbol (or the \"...\"
  string), then a indirect matcher will combine the two
  matchers. Otherwise the matchers are always direct.

  Examples:

    (matcher (id :panel1)
              ...
              (category :cat1)
              (type :button))

    (matcher
      (and
        (path (id :panel) ... (type :button))
        (path (category :fancy-panel)
              ... (category :cat1) (type :button))))

  This will match a button whose direct parent has the :cat1 category
  and one of the ancestors of the parent has an ID of :panel1."
  [& args]
  (let [lookup {'id 'clarity.structure/id-matcher
                'category 'clarity.structure/category-matcher
                'type 'clarity.structure/type-matcher
                '* 'clarity.structure/any-matcher
                'or 'clarity.structure/or-matcher
                'and 'clarity.structure/and-matcher
                'not 'clarity.structure/not-matcher
                'before 'clarity.structure/before-matcher
                'after 'clarity.structure/after-matcher
                '... 'clarity.structure/...
                'path 'path-matcher*}
        
        replace-first
        (fn [x]
          (if (sequential? x)
            (if (contains? lookup (first x))
              (conj (drop 1 x) (get lookup (first x))) x) x))
        
        p (postwalk
           replace-first
           (postwalk expand-matcher-shorthand args))]
    `(path-matcher* ~@p)))

(defn select
  "Filter root and the component tree below it and return all the
  components that match the matcher."
  [root matcher]
  (filter matcher (comp-seq root)))

(defmacro $
  "Convenience macro. If passed a keyword, it runs find-by-id on the
  root with the keyword as the ID. Otherwise, it calls (select) on the
  root with a matcher constructed by passing matchers to the (matcher)
  macro."
  ([root & matchers]
     (if (keyword? (first matchers))
       `(find-by-id ~root ~(first matchers))
       `(select ~root (matcher ~@matchers)))))

(defmacro $-last-frame
  "Like ($) but uses the last-opened frame as root. Useful for
  debugging."
  [& matchers]
  `(let [frames (java.awt.Frame/getFrames)]
     `($ (last frames) ~@matchers)))

(defmacro with-component
  "Macro for \"wiring\" components with IDs within root. In the passed
  forms, all symbols that start with a $ sign are converted into the
  equivalent keywords, which are then used to look up children of the
  root by ID, and the results are bound to variables within a let. All
  occurrences of the dollar-symbols are then replaced by the
  variables.

  For example, if you have a panel with 2 components (ignore the
  absent layout):

    (def panel
      (make :panel
            (.add (make :button \"button\" (:id :button)))
            (.add (make :text-field (:id :text))))    

  You can then say:

    (with-component panel
      (do-component $button
                    (:on-mouse-over (.setText $text \"over\"))
                    (:on-mouse-out (.setText $text \"out\"))))

  Which (approximately) expands to:

    (let*
     [comp18079 (clarity.structure/find-by-id f :button)
      comp18080 (clarity.structure/find-by-id f :text)]
     (do-component comp18079
      (:on-mouse-over (.setText comp18080 \"over\"))
      (:on-mouse-out (.setText comp18080 \"out\"))))

  (The exact variable names may differ, as they are generated to be
  unique depending on the context)."
  [root & forms]
  (let [dollar?
        (fn [x] (and (symbol? x)
                     (.startsWith (name x) "$")))
        
        collect-dollars
        (fn [f] (filter dollar? (flatten f)))
        
        dollars
        (into #{} (flatten (map #(collect-dollars %) forms)))
        
        dollars-to-symbols
        (into {} (map (fn [x] [x (gensym "comp")]) dollars))

        bindings
        (apply concat (map (fn [[n s]]
                             [s `(clarity.structure/find-by-id
                                  ~root
                                  ~(keyword (.substring (name n) 1)))])
                           dollars-to-symbols))
        
        forms
        (postwalk (fn [x] (if (dollar? x)
                            (x dollars-to-symbols) x)) forms)]
        
    `(let [~@bindings] ~@forms)))
