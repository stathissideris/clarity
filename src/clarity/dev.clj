(ns  
    ^{:doc
  "A collection of utilities indented to be used during
  development. The functionality provided makes interactive
  development of GUIs easier."
      :author "Stathis Sideris"}
  clarity.dev
  (:require [clarity.component :as c]
            [clarity.style :as style]
            [clojure.contrib.swing-utils :as swing]
            [clojure.contrib.miglayout :as mig])
  (:use [clarity.structure :only [$]])
  (:import [javax.swing UIManager JFrame]
           [java.awt.image BufferedImage]))

(def *error-icon* (style/get-laf-property "OptionPane.errorIcon"))

(defn show-comp
  "Show the passed component in a JFrame."
  [comp]
  (doto (JFrame.)
    (.add comp)
    (.pack)
    (.setVisible true)))

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
                      (swing/do-swing (.repaint panel))))]
       (c/make :frame
               (.add panel)
               (.pack)
               [:visible true]
               [:on-window-closing
                (future-cancel updater)])
       updater)))

;;; watch component

(defn component-watcher-gui []
  (let [panel
        (mig/miglayout
         (c/make :panel)
         (c/make :button "stop" (:id :start-button))
         (c/make :label "[ ]" (:id :indicator))

         [:gap :unrelated]
         (c/make :label "Update every ")
         (c/make :spinner (:id :delay-spinner))
         (c/make :label " sec")

         :wrap
         (c/make :panel (:id :main-panel)) :span

         :wrap
         (c/make :label "Not started" (:id :time-label)))]
    (c/make :frame
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

(defn update-indicator [indicator]
  (if (= "[ ]" (.getText indicator))
    (.setText indicator "[*]")
    (.setText indicator "[ ]")))

(defn watch-component
  [component]
  (let [frame (component-watcher-gui)
        time-label ($ frame :time-label)
        button-play ($ frame :start-button)
        spinner ($ frame :delay-spinner)
        panel ($ frame :main-panel)
        indicator ($ frame :indicator)

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
                   (swing/do-swing
                    (.removeAll panel)
                    (.add panel (get-component) java.awt.BorderLayout/CENTER)
                    (update-indicator indicator)
                    (.setText time-label (str (java.util.Date.)))
                    (.revalidate frame)
                    (.pack frame))))]
    (c/do-component button-play
                    (:on-click
                     (if (:running @updater)
                       (do
                         (send updater stop-action)
                         (.setText button-play "start"))
                       (do
                         (send updater start-action)
                         (.setText button-play "stop")))))
    (doto frame
      (.pack)
      (:visible true)
      (:on-window-closing
       (send updater stop-action)))
    ;;(send updater start-action)
    updater))


#_(defn f [] (c/make :panel
                     (.add
                      (form [:header "Personal info"]
                            :first-name "Stathis"
                            :surnane "Sideris"
                            :text3 "text"
                            :text4 "wewew"
                            :sex ["male" "female"]))))
#_(defn f2 [] (c/make :panel
                     (:layout (java.awt.FlowLayout.))
                     (.add (c/make :label "ddd"))
                     (.add (c/make :button "lalalala"))))
#_(def p (watch-component 'f))
#_(def p (watch-component #(f)))
#_(send p inject-fn f)
#_(send p start-action)
#_(send p stop-action)



(defn dispose-all-frames
  "Call .dispose on all frames."
  []
  (doseq [frame (java.awt.Frame/getFrames)]
    (.dispose frame)))
