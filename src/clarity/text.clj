(ns clarity.text
  (:require [clarity.component :as c])
  (:import [java.text AttributedString]
           [java.awt.font LineBreakMeasurer TextAttribute]))

(def text-attributes
  (atom
   {:family [TextAttribute/FAMILY clarity.style/font-families]
    :weight [TextAttribute/WEIGHT
             {:regular TextAttribute/WEIGHT_REGULAR
              :bold TextAttribute/WEIGHT_BOLD}]
    :width [TextAttribute/WIDTH
            {:condensed TextAttribute/WIDTH_CONDENSED
             :regular TextAttribute/WIDTH_REGULAR
             :extended TextAttribute/WIDTH_EXTENDED}]
    :posture [TextAttribute/POSTURE
              {:regular TextAttribute/POSTURE_REGULAR
               :oblique TextAttribute/POSTURE_OBLIQUE}]
    :size [TextAttribute/SIZE]
    :transform [TextAttribute/TRANSFORM
                (fn [x] x)]
    :superscript [TextAttribute/SUPERSCRIPT
                  {true TextAttribute/SUPERSCRIPT_SUPER}]
    :subscript [TextAttribute/SUPERSCRIPT
                {true TextAttribute/SUPERSCRIPT_SUB}]
    :font [TextAttribute/FONT]
    ;; :char-replacement
    :foreground [TextAttribute/FOREGROUND]
    :color      [TextAttribute/FOREGROUND]
    :background [TextAttribute/BACKGROUND]
    :underline [TextAttribute/UNDERLINE
                {true TextAttribute/UNDERLINE_ON}]
    :strikethrough [TextAttribute/STRIKETHROUGH
                    {true TextAttribute/STRIKETHROUGH_ON}]
    :run-direction [TextAttribute/RUN_DIRECTION
                    {:ltr TextAttribute/RUN_DIRECTION_LTR
                     :left-to-right TextAttribute/RUN_DIRECTION_LTR
                     :rtl TextAttribute/RUN_DIRECTION_RTL
                     :right-to-left TextAttribute/RUN_DIRECTION_RTL}]

    ;;;synonyms
    :bold [TextAttribute/WEIGHT
           {false TextAttribute/WEIGHT_REGULAR
            true TextAttribute/WEIGHT_BOLD}]
    :italic [TextAttribute/POSTURE
             {false TextAttribute/POSTURE_REGULAR
              true TextAttribute/POSTURE_OBLIQUE}]}))

(def text-tags
  (atom
   {:span {}
    :u {:underline true}
    :b {:weight :bold}
    :i {:posture :oblique}
    :strike {:strikethrough true}}))

(defn add-text-attribute
  "Adds a text attribute to an attributed string (a-str). attr should
  be a key from the text-attributes map. For some attributes, it is
  possible to provide values in the form of keywords (also contained
  in the text-attributes map). The start and end parameters are
  optional, and if not present, the attribute is applied to the whole
  string.

  Examples:

  (add-text-attribute a-str :weight :bold 4 8)
  (add-text-attribute a-str :size 40)
  (add-text-attribute a-str :strikethrough true)"  
  [a-str attr value & [start end]]
  (let [value-map (second (get @text-attributes attr))
        mapped-value (if value-map
                       (value-map value))
        mapped-value (if (nil? mapped-value)
                       value
                       mapped-value)
        attr (first (get @text-attributes attr))]
    (when attr
      (if (and start end)
        (.addAttribute a-str attr mapped-value start end)
        (.addAttribute a-str attr mapped-value)))))

(defn add-text-attributes
  "Adds multiple attributes to an attributed string. The attrs
  argument should be of the form:

  [attr-name1 [value1 start1? end1?]
   attr-name2 [value2 start2? end2?]
   ...]

  Example:

  (add-text-attributes a-str
    [:underline [true 19 22]
     :posture [:oblique 15 22]
     :underline [true 5 7]
     :size [40 0 23]])"
  [a-str attrs]
  (doseq [[key value] (partition 2 attrs)]
    (if (sequential? value)
      (let [[value start end] value]
        (add-text-attribute a-str key value start end))
      (add-text-attribute a-str key value))))

(defn attributed-string
  "Convience function that creates an AttributedString instance and
  adds a number of attributes to it by calling add-text-attributes
  using the new instance and attrs as parameters."
  [s attrs]
  (doto (AttributedString. s)
    (add-text-attributes attrs)))

(defn- text-tag?
  "Is tag a text tag? Should be sequential and start with a keyword
  which is a key in the text-tags map."
  [tag]
  (and (sequential? tag)
       (contains? @text-tags (first tag))))

;;[:span {:weight :bold, :posture :oblique} "bobo"]

;;["The start " [:span {:weight :bold, :posture :oblique} "bobo" [:span "lala"]] " the end."]

(defn- extract-text
  "Recursively extracts the text contained within a tag. Numbers are
  coerced into strings."
  [tags]
  (cond (string? tags) tags
        (keyword? tags) ""
        (number? tags) (str tags)
        :else
        (apply str
               (flatten
                (map #(if (text-tag? %)
                        (extract-text %) %)
                     (filter
                      #(or (string? %)
                           (number? %)
                           (text-tag? %)) tags))))))

(defn- parse-text-tag
  [[tag-name attr-map] start end]
  (let [attr-map (when (map? attr-map) attr-map)
        attrs (merge (get @text-tags tag-name)
                     attr-map)]
    (apply concat
           (map (fn [[k v]] [k [v start end]]) attrs))))

(defn- calculate-ranges
  [offset text]
  (let [element (first text)
        end (+ offset (count (extract-text element)))]
    (concat 
     (when (text-tag? element)
       (calculate-ranges offset element))
     (when (next text)
       (calculate-ranges end (next text)))
     (when (text-tag? element)
       (parse-text-tag element offset end)))))

(defn parse-tagged-text
  "Creates an attributes string from marked-up text. The tags used
  follow the format of prxml. Each tag is a sequential of the format:

  [:tag-name {attribute-map}? content+]

  The tag name keyword is one of the keys in the text-tags map. The
  attribute map is optional and can contain keys and values from the
  text-attributes map (or you can provide the literal values from the
  TextAttribute class). The content can be any number of strings,
  numbers and text tags.

  Example:

  (parse-tagged-text
    [:span {:size 40} \"this \" [:u \"is\"] \" a cat. \"
      [:i \"The \" [:u \"end.\"]]]"
  [& content]
  (attributed-string
   (extract-text content)
   (calculate-ranges 0 content)))

#_[:span {:size 40} "this " [:u "is"] " a cat. " [:i "The " [:u "end"]] ")"]

(defn text
  ([{:keys [wrap]} & s]
     (let [a-str (apply parse-tagged-text s)
           styled-text (.getIterator a-str)
           #_(attributed-string
            s [:weight [:bold 0 2]
               :superscript [true 7 8]
               :posture :oblique
               :size 40])]
       (if wrap
         (c/make
          :panel
          (:opaque false)
          (:impl
           (paint [g]
                  )))
         (c/make
          :panel
          (:id :no-wrap)
          (:opaque false)
          (:impl
           (paint [g]
                  (let [frc (.getFontRenderContext g)
                        measurer (LineBreakMeasurer. styled-text frc)])
                  (.drawString g (.getIterator a-str) 0 0))))))))
