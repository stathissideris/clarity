(defproject clarity "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
				 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :main clarity.core
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
