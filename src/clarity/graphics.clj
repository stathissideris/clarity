(ns clarity.graphics
  (:use [clarity.style :only [color]])
  (:import [java.awt Rectangle TexturePaint]
           [java.awt.image BufferedImage]
           [javax.swing ImageIcon]))

(defn icon
  "Make an ImageIcon from an image."
  [image]
  (ImageIcon. image))

(defn buffered-image-type [type]
  (if (keyword? type)
    (eval `(. java.awt.image.BufferedImage
              ~(symbol
                (str "TYPE_"
                     (-> type
                         (name)
                         (.replace \- \_)
                         (.toUpperCase))))))
    type))

(defn buffered-image
  ([[width height]] (buffered-image [width height] :int-rgb)) ;;TODO make compatible
  ([[width height] type]
     (BufferedImage. width height (buffered-image-type type))))

(defn graphics [image]
  (.getGraphics image))

(defn rect [x y w h]
  (Rectangle. x y w h))

(defn rect-seq [r]
  [(. r x) (. r y) (. r width) (. r height)])

(defn draw [gfx o]
  (cond (instance? Rectangle o)
        (let [[x y w h] (rect-seq o)]
          (.drawRect gfx x y w h))))

(defn fill [gfx o]
  (cond (instance? Rectangle o)
        (let [[x y w h] (rect-seq o)]
          (.fillRect gfx x y w h))))

(defmacro paint-image [img & paint-statements]
  `(let [image# ~img]
     (doto (graphics image#) ~@paint-statements) image#))

(defn texture-paint
  ([img] (texture-paint img (rect 0 0 (.getWidth img) (.getHeight img))))
  ([img rect] (TexturePaint. img rect)))
