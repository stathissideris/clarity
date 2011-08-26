(ns clarity.style
  (require [clojure.contrib.str-utils2 :as str2]
           [clarity.utils :as utils]))

(def default-font (utils/get-laf-property "TextField.font"))

(def font-styles {:plain java.awt.Font/PLAIN
                  :italic java.awt.Font/ITALIC
                  :bold java.awt.Font/BOLD})

(def font-families {:dialog java.awt.Font/DIALOG
                    :dialog-input java.awt.Font/DIALOG_INPUT
                    :mono java.awt.Font/MONOSPACED
                    :monospaced java.awt.Font/MONOSPACED
                    :sans java.awt.Font/SANS_SERIF
                    :sans-serif java.awt.Font/SANS_SERIF})

(definterface Styleable
  (getCategories [])
  (addCategory [s])
  (removeCategory [s])
  (setFont [& args]))

(defn styleable-mixin []
  `((getCategories [] (deref ~'cat))
    (addCategory [~'s] (alter ~'cat ~'conj ~'s))
    (removeCategory [~'s] (alter ~'cat ~'disj ~'s))
    (setFont [& ~'args] (if (instance? ~'java.awt.Font (first ~'args))
                          (proxy-super ~'setFont (first ~'args))
                          (proxy-super ~'setFont (apply font ~'args))))))

;;this now works: (show-comp (make :button "Testing" [:font :size "x6"]))

;;TODO does not work!
(defn styleable?
  [x]
  (instance? Styleable x))

(defn derive-size
  "Given a numerical size and a size-spec, derive a new value. The
  size-spec can be a number, in which case the size-spec is
  returned. If the size-spec is a string it can be of the format
  \"+10%\" or \"10%\" or \"-6\" in which case the appropriate
  conversion is applied to the original value. Limitation: do not use
  leading zeroes in the size-spec, unless you it's a decimal (0.1)."
  [original-size size-spec]
  {:pre [(number? original-size)
         (or (number? size-spec) (string? size-spec))]}
  (if (number? size-spec) size-spec
      (let [[_ sign amount-str percent] (re-find #"([x+-])?([\d\.]+)(%)?" size-spec)
            amount (read-string amount-str)]
        (if percent
          (cond (nil? sign)
                (* original-size (/ amount 100))
                (= "+" sign)
                (+ original-size (* original-size (/ amount 100)))
                (= "-" sign)
                (- original-size (* original-size (/ amount 100))))
          (cond (nil? sign)
                amount
                (= "+" sign)
                (+ original-size amount)
                (= "-" sign)
                (- original-size amount)
                (= "x" sign)
                (* original-size amount))))))

;;; font

(defn ^{:private true} interpret-font-style [style]
  (cond (keyword? style) (get font-styles style)
        (vector? style) (bit-or
                         (:bold font-styles)
                         (:italic font-styles)) ;;this is the only case
        :else style))

(defn derive-font-size [f size-spec]
  {:pre [(instance? java.awt.Font f)]}
  (max 1 (if (string? size-spec)
           (derive-size (.getSize f) size-spec)
           size-spec)))

(defn font
  "Constructs a font out of three optional named parameters, :name
  :size :style. Style can be :plain :bold :italic or a vector
  containing :bold and :italic."
  [&{name :name style :style size :size
     :or {style (.getStyle default-font)
          size (.getSize default-font)}}]
  (let [the-style (interpret-font-style style)
        the-name (if (keyword? name)
                   (get font-families name)
                   name)
        size (if (string? size) (derive-font-size default-font size)
                 size)]
    (java.awt.Font. the-name the-style size)))

(defn derive-font
  "Given a font, derive a new font, by overwriting its parameters with
  the passed ones. The parameters are the same as the (font) function,
  but the size can be derived by passing a size-spec as in
  the (derive-size) function."
  [f &{name :name style :style size :size
       :or {name (.getName f)
            style (.getStyle f)
            size (.getSize f)}}]
  {:pre [(isa? (class f) java.awt.Font)]}
  (font :name name
        :style style
        :size (derive-font-size f size)))

;;; color

(defn color
  ([c]
     (if (keyword? c)
       (eval `(. java.awt.Color ~(symbol (name c))))
       (java.awt.Color. c))) ;;hex
  ([r g b]
     (if (and (float? r) (float? g) (float? b))
       (java.awt.Color. (float r) (float g) (float b))
       (java.awt.Color. r g b)))
  ([r g b a]
     (if (and (float? r) (float? g) (float? b) (float a))
       (java.awt.Color. (float r) (float g) (float b))
       (java.awt.Color. r g b))))

(defn mix-colors
  ([c1 c2]
     (color (min 255 (+ (.getRed c1) (.getRed c2)))
            (min 255 (+ (.getGreen c1) (.getGreen c2)))
            (min 255 (+ (.getBlue c1) (.getBlue c2)))
            (min 255 (+ (.getAlpha c1) (.getAlpha c2)))))
  ([c1 c2 & colors]
     (reduce mix-colors (conj colors c2 c1))))

;;; applying styles

;;(defn apply-style [component style-forms]
;;  (do-component component style-forms))