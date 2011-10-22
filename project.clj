(defproject clarity "0.5.0-SNAPSHOT"
  :description "Clojure GUI library, based on Swing."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojars.nakkaya/miglayout "3.7.3.1"]
                 [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
  :dev-dependencies [[lein-clojars "0.6.0"]
                     #_[autodoc "0.7.1"
                        :exclusions [org.clojure/clojure-contrib 
                                     org.clojure/clojure]]]
  :main clarity.core
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
