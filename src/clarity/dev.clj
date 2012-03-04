(ns  
    ^{:doc
  "A collection of utilities indented to be used during
  development. The functionality provided makes interactive
  development of GUIs easier."
      :author "Stathis Sideris"}
  clarity.dev
  (:require [clarity.component :as c]
            [clarity.widgets :as w]
            [clarity.style :as style]
            [clarity.util :as util]
            [clarity.layout :as layout])
  (:use [clarity.structure :only [$ comp-seq]])
  (:import [javax.swing SwingUtilities UIManager JFrame ImageIcon]
           [java.awt Frame MouseInfo]
           [java.awt.image BufferedImage]))

(def *error-icon* (style/get-laf-property "OptionPane.errorIcon"))

(defn show-comp
  "Show the passed component in a JFrame."
  [comp & {on-top :on-top
           :or {on-top false}}]
  (if (nil? comp)
    (throw (IllegalArgumentException.
            "nil component passed to clarity.dev.show-comp"))
    (c/do-component
     (JFrame.)
     (.add comp)
     (.pack)
     (:visible true)
     (:always-on-top on-top))))
  
(defn- error-image [msg]
  (let [i (BufferedImage. 600 300 BufferedImage/TYPE_INT_RGB)
        g (.getGraphics i)
        icon-width (.getIconWidth *error-icon*)]
    (.paintIcon *error-icon* nil g 0 0)
    (.drawString g msg (+ 10 icon-width) 20)
    i))

(defn watch-image
  "Shows the passed java.awt.Image in a frame, and re-paints at 15
  FPS (or the specified FPS). You can also pass a reference to an
  Image, which will be dereferenced at every frame, or an
  image-returning function, which will be called at every frame.  The
  function returns a future which can be cancelled to stop the
  re-painting. Of course the re-painting stops automatically when the
  frame is closed."
  ([image] (watch-image image 15))
  ([image fps]
     (let [get-image (fn [] (cond (instance? clojure.lang.IDeref image) @image
                                  (fn? image)
                                  (try (image)
                                       (catch Exception e
                                         (do
                                           (.printStackTrace e)
                                           (error-image (str (.getName (class e))
                                                             ", check your console")))))
                                  :otherwise image))
           cached-image (ref nil)
           panel (proxy [javax.swing.JPanel] []
                   (paintComponent [g]
                                   (dosync (ref-set cached-image (get-image)))
                                   (if @cached-image
                                         (.drawImage g @cached-image 0 0 this)))
                   (getPreferredSize[] (if @cached-image
                                         (java.awt.Dimension.
                                          (.getWidth @cached-image)
                                          (.getHeight @cached-image))
                                         (java.awt.Dimension. 100 100))))
           updater (future
                    (while true
                      (Thread/sleep (/ 1000 fps))
                      (util/do-swing (.repaint panel))))]
       (c/make :frame
               (.add panel)
               (.pack)
               [:visible true]
               [:on-window-closing
                (future-cancel updater)])
       updater)))

;;; watch component

(def watcher-backgrounds
  (cycle [:black :chequered :white]))

(def green-led-off-icon
  (ImageIcon. (util/load-image-resource "resources/green_led_off.png")))

(def green-led-on-icon
  (ImageIcon. (util/load-image-resource "resources/green_led_on.png")))

(defn component-watcher-gui []
  (let [panel
        (layout/mig
         (c/make :panel (:id :panel))
         :layout :nogrid :fillx ;;:debug
         :row "[][grow][]"
         (c/make :label green-led-off-icon (:id :indicator))
         (c/make :button "stop" (:id :start-button))

         (c/make :panel) :growx
         (c/make :check-box "Always on top"
                 (:id :on-top)
                 (:selected true))
         (c/make :button "back" (:id :background))
         (c/make :label "Update every")
         [:gap :unrelated]
         (c/make :spinner
                 (:init (c/make javax.swing.SpinnerNumberModel
                                (:init 0.5 0.2 nil 0.2)))
                 (:id :delay-spinner))
         (c/make :label "sec") :wrap
         (w/scroll-pane
          (c/make :panel
                  (:id :main-panel)
                  (:background (style/color :black))))
         :grow :span :wrap
         (c/make :label "Not started" (:id :time-label))
         :span)]
    (c/make :frame
            (:title "Clarity Component Watcher")
            (:always-on-top true)
            (.add panel))))

(defn periodic-caller [t fun]
  (agent {:fn fun
          :running false
          :date nil
          :delay t}))

(defn call-action [state]
  (if (:running state)
    (do (send-off *agent* call-action)
        ((:fn state))
        (Thread/sleep (:delay state))
        (assoc state :date (java.util.Date.)))
    state))

(defn start-action [state]
  (send-off *agent* call-action)
  (assoc state :running true))

(defn stop-action [state]
  (assoc state :running false))

(defn change-period-action [state period]
  (assoc state :delay period))

(defn update-indicator [indicator]
  (if (= green-led-off-icon (.getIcon indicator))
    (.setIcon indicator green-led-on-icon)
    (.setIcon indicator green-led-off-icon)))

(defn watch-component
  [component]
  (let [frame (component-watcher-gui)
        time-label ($ frame :time-label)
        button-play ($ frame :start-button)
        spinner ($ frame :delay-spinner)
        panel ($ frame :main-panel)
        indicator ($ frame :indicator)
        on-top ($ frame :on-top)

        get-component
        (cond (symbol? component)
              #((eval component))
               
              (instance? clojure.lang.IDeref component)
              #(@component)
                     
              (fn? component)
              #(try (component)
                    (catch Exception e
                      (do
                        (.printStackTrace e)
                        nil)))
              :otherwise component)
        
        updater (periodic-caller
                 500
                 (fn []
                   (util/do-swing
                    (.removeAll panel)
                    (.add panel (get-component) java.awt.BorderLayout/CENTER)
                    (update-indicator indicator)
                    (.setText time-label (str "Updated at: "
                                              (str (java.util.Date.))))
                    #_(.validate frame)
                    (.pack frame))))]
    (c/do-component
     spinner
     (:on-state-changed
      (let [value (c/value spinner)]
        (if (> value 0.2)
          (send updater change-period-action (* 1000 value))))))
    (c/do-component
     on-top
     (:on-click
      (.setAlwaysOnTop frame (c/value on-top))))
    (c/do-component
     button-play
     (:on-click
      (if (:running @updater)
        (do
          (send updater stop-action)
          (.setText button-play "start")
          (.setIcon indicator green-led-off-icon))
        (do
          (send updater start-action)
          (.setText button-play "stop")))))
    (c/do-component frame
                    (.pack)
                    (:visible true)
                    (:size (style/dimension 550 400))
                    (:on-window-closing
                     (send updater stop-action)))
    (util/do-swing (.toFront frame)
                    (.repaint frame))
    (send updater start-action)
    updater))


#_(use 'clarity.form)
#_(defn f [] (c/make :panel
                     (.add
                      (form [:header "Personal info"]
                            [:header "Main" :level 3]
                            :first-name "Stathis"
                            :surname "Sideris"
                            :gender ["male" "female"]
                            [:header "Address" :level 3]
                            :line1 "50 Essex Road" [:label "Number and street"]
                            :line2 "" [:label "Line 2"]
                            :postcode ""
                            :city ""
                            :country ""
                            ))))
#_(defn f2 [] (c/make :panel
                     (:layout (java.awt.FlowLayout.))
                     (.add (c/make :label "ddd"))
                     (.add (c/make :button "lalalala"))))
#_(def p (watch-component #'f))
#_(def p (watch-component #(f)))
#_(def p (watch-component #($ (component-watcher-gui) :panel)))
#_(send p inject-fn f)
#_(send p start-action)
#_(send p stop-action)

(defn all-frames
  "Get all open frames."
  []
  (Frame/getFrames))

(defn frame-titles
  "Get the titles of all open frames."
  []
  (map #(.getTitle %) (all-frames)))

(defn mouse-location
  "Get the location of the mouse on the screen."
  []
  (.getLocation (MouseInfo/getPointerInfo)))

(defn point-from-screen
  "Convert a screen point to a point relative to the top-left corner
  of the component."
  [p component]
  (let [p (mouse-location)]
    (SwingUtilities/convertPointFromScreen p component)
    p))

(defn component-at-screen-point
  "Returns the deepest component at the screen point p."
  [p component]
  (.findComponentAt component (point-from-screen p component)))

(defn component-at-mouse
  "Returns the deepest component at the position of the mouse,
  contained within container."
  [container]
  (component-at-screen-point (mouse-location) container))

(defn dispose-all-frames
  "Call .dispose on all frames."
  []
  (doseq [frame (java.awt.Frame/getFrames)]
    (.dispose frame)))

;;(defmulti explain (fn [x] (first x)))
;;(defmethod explain 'apply-stylesheet [[_ root stylesheet]])

(defmacro explain
  "Attempts to explain the passed code. Currently only applicable to
  applying stylesheets: it acts as a delegate to the
  clarity.style/explain-stylesheet function. For example:

    (use 'clarity.style)
    (use 'clarity.component)
    (use 'clarity.dev)

    (defstylesheet the-style
      (style (type :button)
         (:background (color :red))))

    (def panel
      (make :panel (:id :panel)
        (.add (make :button \"Test\" (:id :button1)))
        (.add (make :button \"Test 2\" (:id :button2)))))

    (explain (apply-stylesheet panel the-style))

  ...prints:

    --- Showing matches only ---
    1 out of 1 styles match

    ---

    Style: (type :button)
    Mutator:
      (:background (color :red))
    Matches (2):
      $panel/$button1
      $panel/$button2

  You can also pass extra parameters to determine whether non-matches
  are also shown. Possible values for :show are :all :matched
  :not-matched. For example:

    (explain (apply-stylesheet panel the-style) :show :all)"
  
  [x]
  (if (= #'clarity.style/apply-stylesheet (resolve (first x)))
    (let [[_ root stylesheet & rest] x
          root (eval root)
          stylesheet (eval stylesheet)]
        (apply style/explain-stylesheet root stylesheet rest))))
