(defproject clarity "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.2.0"]
				 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[marginalia "0.5.1"]]
  :main clarity.core
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
