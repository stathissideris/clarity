<img src="https://github.com/stathissideris/clarity/raw/master/src/resources/logo.png">

Clojure GUI library, based on Swing. See the project wiki for details on how to use it.

If you are using leiningen, add the following dependency to your
`project.clj`:

`[clarity "0.5.0-SNAPSHOT"]`

Example of the form facilities:

````clojure
    (show-dialog
     (form [:header "The form"]
           [:text "Please fill in the **entire** form." :rich]
           :first-name ""
           :surname ""
           :gender ["male" "female" "other"])
     ["OK" "Cancel"])
`````
