<img src="https://github.com/stathissideris/clarity/raw/master/src/resources/logo.png">

Clojure GUI library, based on Swing.

Example of the form facilities:

````clojure
    (dialog
     (form [:header "The form"]
           [:text "Please fill in the **entire** form." :rich]
           :first-name ""
           :surname ""
           :gender ["male" "female"])
     ["OK" "Cancel"])
`````
