<img src="https://github.com/stathissideris/clarity/raw/master/src/resources/logo.png">

Clojure GUI library, based on Swing. See the project wiki for details on how to use it. You can also watch a [30 minute presentation](http://skillsmatter.com/podcast/scala/lightening-talk-clarity-a-wrapper-for-swing) on the library.

If you are using leiningen, add the following dependency to your
`project.clj`:

`[clarity "0.5.6"]`

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
