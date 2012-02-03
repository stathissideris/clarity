(ns
    ^{:doc "Various functions concerning the look of the
    GUI. Currently covers fonts, colors, gradients, some border
    functionality, and defining and applying stylesheets."
      :author "Stathis Sideris"}

  clarity.style

  (require [clojure.java.io :as io]
           [clarity.component :as c]
           [clarity.structure :as s])
  (use clojure.contrib.apply-macro)
  (import [java.awt Color Paint Stroke BasicStroke GradientPaint
           LinearGradientPaint RadialGradientPaint
           MultipleGradientPaint]
          [java.awt.geom Point2D$Float Point2D$Double]
          [javax.swing.border AbstractBorder]))

;;; look and feel
(defn set-system-laf []
  (javax.swing.UIManager/setLookAndFeel
   (javax.swing.UIManager/getCrossPlatformLookAndFeelClassName)))

(defn get-laf-properties
  ([] (get-laf-properties nil))
  ([regex]
     (let [defaults (javax.swing.UIManager/getLookAndFeelDefaults)]
       (if regex
         (filter #(re-seq regex (.toString (key %))) defaults)
         defaults))))

(defn find-laf-properties
  ([] (find-laf-properties nil))
  ([regex]
     (let [matches (get-laf-properties regex)]
       (doseq [entry matches]
         (print (key entry) ": " (val entry) "\n")))))

(defn get-laf-property
  [key]
  (javax.swing.UIManager/get key))

;;; font defaults

(def default-font (get-laf-property "TextField.font"))

(def font-styles {:plain java.awt.Font/PLAIN
                  :italic java.awt.Font/ITALIC
                  :bold java.awt.Font/BOLD})

(def font-families {:dialog java.awt.Font/DIALOG
                    :dialog-input java.awt.Font/DIALOG_INPUT
                    :mono java.awt.Font/MONOSPACED
                    :monospaced java.awt.Font/MONOSPACED
                    :sans java.awt.Font/SANS_SERIF
                    :sans-serif java.awt.Font/SANS_SERIF})

(def font-formats {:truetype java.awt.Font/TRUETYPE_FONT
                   :tt java.awt.Font/TRUETYPE_FONT
                   :type1 java.awt.Font/TYPE1_FONT})

;;; sizes

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

(defn dimension [w h]
  (java.awt.Dimension. w h))

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

(defn derive-font
  "Given a font, derive a new font, by making subsequent calls to the
  various java.awt.Font/deriveFont methods depending on the presence
  and the values of the various parameters. All parameters are
  optional."
  ;;TODO add support for derive-size-like size definitions
  [^java.awt.Font f &{style :style size :size transform :transform}]
  (let [f (if size (.deriveFont f (float size)) f)
        f (if style (.deriveFont f (get font-styles style)) f)
        f (if transform (.deriveFont f transform) f)]
    f))

(defn font-from-file
  "Create a Font from a file. The format parameter can be :tt
  or :truetype to load TrueType fonts, or :type1 for Type1 fonts. The
  file argument is passed to clojure.java.io/file, so it is coerced
  into a file and therefore can be a String, a File, or a URL/URI
  pointing to a file. The resulting font has the same size as the
  default-font."
  [format file]
  (let [format (get font-formats format)]
    (derive-font
     (java.awt.Font/createFont format (io/file file))
     :size (.getSize default-font))))

(defn font-from-resource
  [format resource]
  (let [format (get font-formats format)])
  ) ;;TODO

(defn font
  "Constructs a font out of three optional named parameters, :name
  :size :style. The name can be any valid font name, or a keyword
  corresponding to one of the static font families (:dialog,
  :dialog-input, :mono, :monospaced, :sans, :sans-serif). Style
  can be :plain :bold :italic or a vector containing :bold
  and :italic."
  [&{name :name style :style size :size
     file :file format :format
     :or {style (.getStyle default-font)
          size (.getSize default-font)}}]
  (let [the-style (interpret-font-style style)
        the-name (if (keyword? name)
                   (get font-families name)
                   name)
        size (if (string? size) (derive-font-size default-font size)
                 size)]
    (cond file (derive-font
                (font-from-file format file)
                :size size
                :style style)
          :else (java.awt.Font. the-name the-style size))))

;;; color

(defn color
  "Create a java.awt.Color. The single-parameter form accepts either
  keywords which it then maps to the static pre-defined colors (so you
  can say things like (color :white)) or numbers, so that you can pass
  a color as a hexadecimal.

  The other two versions for RGB and RGBA colors either accept
  integers from 0 to 255 or floats from 0 to 1. You cannot mix the
  two."
  
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
       (java.awt.Color. (float r) (float g) (float b) (float a))
       (java.awt.Color. r g b a))))

(defn mix-colors
  ([c1 c2]
     (color (int (/ (+ (.getRed c1) (.getRed c2)) 2))
            (int (/ (+ (.getGreen c1) (.getGreen c2)) 2))
            (int (/ (+ (.getBlue c1) (.getBlue c2)) 2))
            (int (/ (+ (.getAlpha c1) (.getAlpha c2)) 2))))
  ([c1 c2 & colors]
     (reduce mix-colors (conj colors c2 c1))))

;;; geometry

(defn point-float [x y] (Point2D$Float. x y))
(defn point-double [x y] (Point2D$Double. x y))

(def point point-float)

;;; borders

(defprotocol Border
  "Protocol to be implemented by borders that can tell you what shape
  they are."
  (shape [this component]))

(defn install-border
  "Set the border as the border of the component and if the border is
  a clarity.style.Border, modify the component proxy mapping of the
  paintComponent function to draw the background of the component
  clipped using the shape of the border. Sets the opaque flag of the
  component to false to prevent the background being painted
  twice. Assumes that component is a proxy."
  
  [^clarity.component.Component component
   ^AbstractBorder border]
  
  (.setBorder component border)
  (when (satisfies? Border border)
    (c/do-component
     component
     (:impl (isOpaque [] false)
            (paintComponent [g]
                           (let [p (.getPaint g)
                           b (.getBorder this)]
                       (when (satisfies? Border b)
                         (.setPaint g (.getBackground this))
                         (.fill g (shape b this))
                         (.setPaint g p))
                       (proxy-super paintComponent g)))))
    
    (update-proxy component
                  {"paintComponent"
                   (fn [this g]
                     (let [p (.getPaint g)
                           b (.getBorder this)]
                       (when (satisfies? Border b)
                         (.setPaint g (.getBackground this))
                         (.fill g (shape b this))
                         (.setPaint g p))
                       (proxy-super paintComponent g)))})))

(defn rounded-border
  "Create a rounded border with the passed corner size in pixels, and
  the stroke instance that defines the line. Since this is a
  clarity.style.Border, for an opaque component it must be installed
  on it using (install-border) to ensure that the component's
  background is painted correctly."
  
  [corner-size ^Stroke stroke]
  
  (let [arc (* 2 corner-size)]
    (proxy [AbstractBorder clarity.style.Border] []
      (shape [component]
        (let [line-width (.getLineWidth stroke)
              x (.getX component)
              y (.getY component)
              w (.getWidth component)
              h (.getHeight component)]
          (java.awt.geom.RoundRectangle2D$Float.
           line-width
           line-width
           (- w (* 2 line-width))
           (- h (* 2 line-width))
           arc arc)))
      (getBorderInsets [component]
        (let [inset (+ (/ arc 2)
                       (/ (.getLineWidth stroke) 2))]
          (java.awt.Insets. inset inset inset inset)))
      (paintBorder [component g x y w h]
        (let [rect (shape this component)]
          (doto g
            (.setRenderingHint java.awt.RenderingHints/KEY_ANTIALIASING
                               java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
            (.setStroke stroke)
            (.draw rect)))))))

;;to try the rounded corners border:
#_(use 'clarity.dev 'clarity.style 'clarity.component 'clarity.dev)
#_(show-comp
   (make :panel
         (.add
          (make :panel
                (install-border
                 (rounded-border 25 (stroke 3)))
                (:background (color :yellow))
                (.add (make :label "Hello World! Borderz!!!"))))))

#_(show-comp
   (make :panel
         (.add
          (make :button "hello"
                (install-border
                 (rounded-border 25 (stroke 1)))))))

;;stroke

(let [cap-map {:butt java.awt.BasicStroke/CAP_BUTT
               :round java.awt.BasicStroke/CAP_ROUND
               :square java.awt.BasicStroke/CAP_SQUARE}
      join-map {:miter java.awt.BasicStroke/JOIN_MITER
                :round java.awt.BasicStroke/JOIN_ROUND
                :bevel java.awt.BasicStroke/JOIN_BEVEL}]
  (defn stroke
    "Creates a basic stroke (java.awt.BasicStroke). The first
  parameter is the width of the stroke, and it can be customised by
  passing a number of extra key/value pairs.

  The cap of the stroke can be defined by passing :cap
  and :butt, :round or :square (default is :round).

  The join of stroke can be defined by passing :join
  and :miter, :round or :bevel (default is :round).

  The dash pattern can be defined by passing :dash and a vector of
  numbers. Finally, the dash phase can be defined by
  passing :dash-phase and a number.

  Examples:

  (stroke 2)

  (stroke 3 :join :miter)

  (stroke 3, :join :round, :cap :square)

  (stroke 5 :dash [10 20 5])"

    [width
     & {:keys [cap join miter-limit dash dash-phase]
        :or {cap :round
             join :round
             miter-limit 10.0
             dash nil
             dash-phase 0}}]
    (let [cap (if cap (get cap-map cap) nil)
          join (if join (get join-map join) nil)
          dash (if dash (into-array Float/TYPE dash) nil)]
      (BasicStroke. width cap join miter-limit dash dash-phase))))

;; gradients

(defn gradient
  [[x1 y1] c1, [x2 y2] c2 & cyclic?]
  (let [cyclic? (if cyclic? true false)]
    (GradientPaint. x1 y1 c1, x2 y2 c2, cyclic?)))

;;example
#_(gradient [10 10] (color :white)
            [20 20] (color :black)
            :cyclic)

(let [cycle-method-map
      {:no-cycle java.awt.MultipleGradientPaint$CycleMethod/NO_CYCLE
       :repeat java.awt.MultipleGradientPaint$CycleMethod/REPEAT
       :reflect java.awt.MultipleGradientPaint$CycleMethod/REFLECT}]

  (defn- parse-gradient-stops [stops]
    (loop [s stops
           fractions []
           colors []]

      (if (or (empty? s) (keyword? (first s)))
        {:fractions (into-array Float/TYPE fractions)
         :colors (into-array java.awt.Color colors)
         :cycle-method (if (first s) (first s) :no-cycle)}
        (if (not (number? (first s)))
          (throw (IllegalArgumentException.
                  (str "Expected number instead of " (first s))))
          (if (not (instance? java.awt.Color (second s)))
            (throw (IllegalArgumentException.
                    (str "Expected color instead of " (second s))))
            (recur (drop 2 s)
                   (conj fractions (float (first s)))
                   (conj colors (second s))))))))
            
  (defn linear-gradient
    "Construct a linear gradient with multiple stops
  (java.awt.LinearGradientPaint). The first two vectors define the
  start and end points of the gradient. The rest of the parameters
  define the stops of the gradient as pairs of numbers (between 0 and
  1) and colors. Optionally, as a last parameter you can pass a
  keyword defining the cycle method of the gradient (:no-cycle,
  :repeat or :reflect, with :no-cycle being the default).

  Example:

  (linear-gradient [10 10]
                   [100 10]
                   0.1 (color :white)
                   0.2 (color :green)
                   0.5 (color :red)
		           0.9 (color :black))"
    
    [[x1 y1] [x2 y2] & stops]
    {:pre [(>= (count stops) 4)]}
    (let [params (parse-gradient-stops stops)]
      (LinearGradientPaint.
       (point x1 y1)
       (point x2 y2)
       (:fractions params)
       (:colors params)
       (get cycle-method-map (:cycle-method params)))))

  (defn radial-gradient
    "Construct a radial gradient with multiple stops
  (java.awt.RadialGradientPaint). The first vector parameter defines
  the center of the gradient and the second is the radius. If the
  third parameter is also a 2-element vector, it defines an off-center
  focus point for the gradient.

  The rest of the parameters define the stops of the gradient as pairs
  of numbers (between 0 and 1) and colors. Optionally, as a last
  parameter you can pass a keyword defining the cycle method of the
  gradient (:no-cycle, :repeat or :reflect, with :no-cycle being the
  default).

  Example without a custom focus point:

  (radial-gradient [10 10] 100
                   0.1 (color :white)
                   0.2 (color :green)
                   0.5 (color :red)
                   0.9 (color :black))

  Example with a custom focus point:

  (radial-gradient [10 10] 100 [0 0]
                   0.1 (color :white)
                   0.2 (color :green)
                   0.5 (color :red)
                   0.9 (color :black))"

    [[cx cy] radius & stops]
    (let [focus
          (if (vector? (first stops))
            (apply point (first stops))
            nil)
          
          params
          (if focus
            (parse-gradient-stops (rest stops))
            (parse-gradient-stops stops))]
      (if focus
        (RadialGradientPaint. ;;with off-center focus
         (point cx cy)
         (float radius)
         focus
         (:fractions params)
         (:colors params)
         (get cycle-method-map (:cycle-method params)))
        (RadialGradientPaint. ;;no off-center focus
         (point cx cy)
         (float radius)
         (:fractions params)
         (:colors params)
         (get cycle-method-map (:cycle-method params)))))))


;;; making stylesheets

(defn- make-style-form
  [[_ matcher & look-forms]]
  {:matcher `(s/matcher ~matcher)
   :mutator `(c/mutator ~@look-forms)})

(defmacro defstylesheet
  "Defines a stylesheet. The syntax is:

  (defstylesheet
    stylesheet-name
    (style matcher & mutator-forms)
    (style matcher & mutator-forms))

  Where the matchers are defined using the same syntax as the forms
  passed to the clarity.structure/matcher macro. The mutator forms
  follow the syntax of clarity.component/do-component. Here is a more
  concrete example:

  (defstylesheet
    test-stylesheet
    (style .important
           (:color (color :red))
           (:font (font :style :bold)))
    (style $title.header
           (:font (font :size \"200%\"))))

  The name is def-ed as a Var in the current namespace. The name is
  also added as a string to the metadata of the stylesheet."
  [name & styles]
  `(def ~name
     (with-meta
       (vector ~@(map make-style-form styles))
       {:name ~(str name)})))

;;; applying stylesheets
(defn apply-stylesheet
  "Applies a stylesheet to the root component. The component and its
  descendants are each tested against the rules of the stylesheet, and
  any matches have the corresponding look apply.

  Please note: re-applying a stylesheet on a component that already
  has one, may have unpredictable results, since clarity does not
  attempt to reverse the side-effects of the previous stylesheet
  before applying the new one. This is especially important if your
  stylesheets add listeners to the matching components."
  [root stylesheet]
  (doseq [component (s/comp-seq root)]
    (doseq [style stylesheet]
      (if ((:matcher style) component)
        ((:mutator style) component))))
  root)
