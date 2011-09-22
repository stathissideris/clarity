(ns clarity.structure
  (:require [clarity.component :as c]))

(defn comp-seq
  "Walks the contents of the passed java.awt.Container depth-first and
  returns the results as a lazy sequence."
  [root]
  {:pre [(instance? java.awt.Container root)]}
  (tree-seq (fn [node] (not (zero? (count (.getComponents node)))))
            (fn [node] (.getComponents node))
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
               (for [c components]
                 (if (and (not (nil? (c/id c)))
                          (satisfies? c/HasValue c)
                          (contains? value (c/id c)))
                   (c/set-value c (get value (c/id c))))))))

(defn find-by-id
  "Finds recursively the child of root (or root itself) that has the
  passed ID."
  [root id]
  (first (filter #(= id (c/id %)) (comp-seq root))))

;;TODO redefine to use selectors
(defn $
  "With a single parameter, it applies (find-by-id) to the last opened
  java.awt.Frame. With 2 parameters it's just a synonym
  of (find-by-id)."
  ([id]
     (let [frames (java.awt.Frame/getFrames)]
       ($ (last frames) id)))
  ([root id] (find-by-id root id)))

(defn path
  "Returns a list representing the path of components from the
  top-most parent to comp."
  [comp]
  (loop [c comp p (list c)]
    (let [parent (.getParent c)]
      (if (nil? parent) p
        (recur parent (conj p parent))))))

;; various matchers that can be composed to make complex selectors

(defn id-matcher
  "Produces a matcher function that accepts a component and tests
  whether it has the passed id."
  [id]
  (fn [component]
    (= id (c/id component))))

(defn category-matcher
  "Produces a matcher function that accepts a component and tests
  whether it has the passed category."
  [category]
  (fn [component]
    (c/has-category component category)))

(defn type-matcher
  "Produces a matcher function that accepts a component and tests
  whether it is an instance of tha passed class. If type is a keyword,
  the rules that are used in the component/make macro are also used to
  translate it to a Swing (or AWT) class."
  [type]
  (let [type (if (keyword? type)
               (c/make-class type)
               type)]
    (fn [component]
      (instance? type component))))

(defn and-matcher
  [& matchers]
  (fn [component]
    (every? #(= % true) (map #(% component) matchers))))

(defn or-matcher
  [& matchers]
  (fn [component]
    (some #(= % true) (map #(% component) matchers))))

(defn any-matcher []
  (fn [component] true))

(defn direct-parent-matcher
  "Produces a matcher function that accepts a component and tests
  whether the direct parent of the component matches the passed
  parent-matcher and the component itself matches the
  child-matcher. If the component does not have a parent, the matcher
  does not match."
  [parent-matcher child-matcher]
  (fn [component]
    (let [parent (.getParent component)]
      (if (nil? parent) false
          (and (parent-matcher parent) (child-matcher component))))))

(defn any-parent-matcher
  "Produces a matcher function that accepts a component and tests
  whether any of the ancestors of the component matches the passed
  parent-matcher and the component itself matches the
  child-matcher. If the component does not have a parent, the matcher
  does not match."
  [parent-matcher child-matcher]
  (fn [component]
    (if (child-matcher component)
      (loop [parent (.getParent component)]
        (if (nil? parent) false
            (if (parent-matcher parent) true
                (recur (.getParent parent)))))
      false)))

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

(let [lookup {'id 'id-matcher
              'category 'category-matcher
              'type 'type-matcher
              '* 'any-matcher
              'or 'or-matcher
              'and 'and-matcher
              '... 'clarity.structure/...}]
  (defmacro path-matcher
    "This is a macro that makes the syntax of (path-matcher*) a bit
  lighter. It makes the following replaces to the first element of all
  passed expressions:

    id        id-matcher
    category  category-matcher
    type      type-matcher
    *         any-matcher
    or        or-matcher
    and       and-matcher
    ...       clarity.structure/...

  The produced matcher combines multiple matchers so that the direct
  parents (or indirect ancestors) of a component can be matched. The
  matchers are chained in the same order as in CSS (from parent to
  child). If you separate matchers with the ... symbol (or the \"...\"
  string), then a indirect matcher will combine the two
  matchers. Otherwise the matchers are always direct.

  Example:

    (path-matcher (id :panel1)
                  ...
                  (category :cat1)
                  (type :button))

  This will match a button whose direct parent has the :cat1 category
  and one of the ancestors of the parent has an ID of :panel1."
    [& args]

    
    (let [replace-firsts (fn replace-firsts [exp]
                           (if (not (sequential? exp))
                             (if (contains? lookup exp)
                               (get lookup exp)
                               exp)
                             (map replace-firsts exp)))
          p (replace-firsts args)]
      `(path-matcher* ~@p))))
 