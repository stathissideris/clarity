(defproject clarity "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
				 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.nakkaya/miglayout "3.7.3.1"]
                 [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
  :dev-dependencies [[autodoc "0.7.1"
                      :exclusions [org.clojure/clojure-contrib 
                                   org.clojure/clojure]]]
  :main clarity.core
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
