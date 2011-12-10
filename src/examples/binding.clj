(ns examples.binding
  (:require [clojure.contrib.swing-utils :as swing]
            [clarity.component :as c]
            [clarity.form :as form]
            [clarity.dev :as dev]))

;;Example of how to make a form to automatically update according to
;;changes to a record contained in a ref.


;;define a record. A record is also a map, therefore it can be used
;;with Clarity's "external wiring" capabilities
(defrecord Person [name address])

;;if you need to change the values of the person record on the fly, it
;;has to be a ref or an atom. Let's make it an atom
(def person (ref (Person. "Felix" "66 Main Str")))

;;define the form
(def the-form
  (form/form
   :name ""
   :address ""))

;;This is the actual "magic" binding. What happens here is that when
;;the value of the person atom is modified, the form is populated with
;;the new value. set-value makes sure that all the fields of the form
;;are updated.
(add-watch
 person
 :form-watch
 (fn [key reference old new]
   (swing/do-swing
    (c/set-value the-form new))))

;;Show the form. Stays on top of all your other windows.
(dev/show-comp the-form :on-top true)

;;initialise the form with the contents of person
(swing/do-swing
 (c/set-value the-form @person))

;;Once you see the form, eval the following in the REPL:
;;(dosync (alter person assoc :name "Stathis"))

;;This will change the name of the person ref and the form will update
;;automatically.
