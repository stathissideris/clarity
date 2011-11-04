(ns clarity.dialog
  (:require [clarity.component :as c])
  (:import [java.awt BorderLayout FlowLayout]))

(def ^{:doc
 "The map of built-in responses. :ok, :cancel, :yes, :no and :submit
 are supported and replaced by English labels."}
     response-map
     {:ok "OK"
      :cancel "Cancel"
      :yes "Yes"
      :no "No"
      :submit "Submit"})

(def ^{:doc "Single OK button"} ok [:ok])
(def ^{:doc "Single submit button"} submit [:submit])
(def ^{:doc "OK and Cancel buttons"} ok-cancel [:ok :cancel])
(def ^{:doc "Yes and No buttons"} yes-no [:yes :no])
(def ^{:doc "Yes, No and Cancel buttons"} yes-no-cancel [:yes :no :cancel])

(defn- make-button [dialog button response]
  (let [text (if (string? button)
               button
               (get response-map button))
        id (if (keyword? button) button nil)]
    (if (nil? id)
      (c/make :button text
              [:category :dialog-button]
              [:on-click
               (reset! response (.getText this))
               (.dispose dialog)])
      (c/make :button text
              [:id id]
              [:category :dialog-button]
              [:on-click
               (reset! response id)
               (.dispose dialog)]))))

(defn- make-button-panel [dialog buttons response]
  (let [panel (c/make :panel [:category :dialog-button-panel])]
    (.setLayout panel (FlowLayout.))
    (doall (map #(.add panel (make-button dialog % response)) buttons))
    panel))

(defn dialog
  "Example:

    (let [response (atom nil)
          f (form :name \"\" :surname \"\")
          d (dialog f [:ok :cancel] response)]
      (.setModal d true)
      (.setVisible d true)
      (println @response)
      (println (value f)))

  Constructs a dialog that has content as its main component, and a
  series of buttons at the bottom. The buttons parameter is a vector
  of strings or keywords. Keywords are translated to strings using the
  response-map. When a button is clicked, the dialog is closed and the
  response atom is reset to the value that corresponds to the clicked
  button. If the passed button element was a keyword, then the same
  keyword is placed in the response atom. If the button element was a
  string, then the same string is placed in the response.

  If you don't mind using a blocking function, show-dialog is much
  easier to use."

  [content buttons response]
  {:pre [(instance? java.awt.Component content)
         (or (vector? buttons)
             (map? buttons))]}
  (if (c/component? content)
    (dosync (.addCategory content :dialog-content-panel)))
  (let [dialog (c/make :dialog [:category :dialog])]
    (doto dialog
      (.setLayout (BorderLayout.))
      (.add content BorderLayout/CENTER)
      (.add (make-button-panel dialog buttons response) BorderLayout/SOUTH)
      (.pack))))

(defn show-dialog
  "Easily show a modal dialog and get the user response after the
  dialog has been closed. This is a blocking function.

  Example:

    (show-dialog (form :first-name \"\" :surname \"\") [:ok :cancel])

    ... user enters details and presses the OK button ...

    {:value {:first-name \"Joe\", :surname \"Smith\"}, :response :ok}

  A modal dialog showing the component in content and the relevant
  buttons is created and the thread blocks. After the dialog is
  closed, a map is returned, with two keys, :value and :response. The
  value of :value is the result of calling clarity.component/value on
  the main contents of the dialog (see HasValue protocol) and
  the :response key represents the button that was clicked. This can
  be the text of the button or its ID, depending on what was passed in
  the buttons parameter. If no buttons are passed, the dialog will
  only have an \"OK\" button. If the dialog is closed by clicking on
  the close button of the window title bar, the :response will
  be :closed."
  ([content]
     (show-dialog content [:ok]))
  ([content buttons]
     (let [response (atom :closed)
           d (dialog content buttons response)]
       (.setModal d true)
       (.setVisible d true)
       {:value (c/value content) :response @response})))

;;TODO dialogs that allow custom actions when clicking on buttons?

;;example
#_(show-dialog
   (clarity.form/form [:header "The form"]
                      [:text "Please fill in the **entire** form." :rich]
                      :first-name ""
                      :surname ""
                      :gender ["male" "female"])
   ["OK" "Cancel"])
