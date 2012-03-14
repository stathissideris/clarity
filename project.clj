(defproject clarity "0.5.6"
  :description "Clojure GUI library, based on Swing."
  :url "http://stathissideris.github.com/clarity/"
  :autodoc {:name "Clarity"
            :page-title "Clarity API docs"
            :web-src-dir "https://github.com/stathissideris/clarity/blob/"
            :web-home "http://stathissideris.github.com/clarity/"
            :output-path "autodoc"
            :trim-prefix "clarity."
            :copyright "Copyright 2011-2012 by Stathis Sideris"
            :load-except-list [#"test/" #"project\.clj"
                               #"autodoc/" #"src/examples/"]}
  :dependencies      [[org.clojure/clojure "1.3.0"]
                      [artem "0.5"]
                      [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
  :dev-dependencies  [[lein-clojars "0.6.0"]
                      [lein-multi "1.1.0-SNAPSHOT"]
                      [org.clojars.weavejester/autodoc "0.9.0"]
                      [org.apache.commons/commons-exec "1.1"]]
  :multi-deps {"1.2.0" [[org.clojure/clojure "1.2.0"]
                        [org.clojure/clojure-contrib "1.2.0"]
                        [artem "0.5"]
                        [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
               "1.2.1" [[org.clojure/clojure "1.2.1"]
                        [org.clojure/clojure-contrib "1.2.0"]
                        [artem "0.5"]
                        [org.markdownj/markdownj "0.3.0-1.0.2b4"]]}
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
