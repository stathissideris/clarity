<img src="https://github.com/stathissideris/clarity/raw/master/src/resources/logo.png">

Clojure GUI library, based on Swing.

If you are using leiningen, add the following to your `project.clj`:

`[clarity "0.5.0-SNAPSHOT"]`

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
