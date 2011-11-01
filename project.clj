(defproject clarity "0.5.0-SNAPSHOT"
  :description "Clojure GUI library, based on Swing."
  :autodoc {:name "Clarity"
            :page-title "Clarity API docs"
            :web-src-dir "https://github.com/myfreeweb/ringfinger/blob/"
            :web-home "http://myfreeweb.github.com/ringfinger/"
            :output-path "autodoc"
            :trim-prefix "clarity."
            :namespaces-to-document ["clarity"]
            :load-except-list [#"test/" #"project\.clj" #"autodoc/"]}
  :dependencies [[org.clojure/clojure "1.2.0"]
				         [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.nakkaya/miglayout "3.7.3.1"]
                 [org.markdownj/markdownj "0.3.0-1.0.2b4"]]
  :dev-dependencies [[lein-clojars "0.6.0"]
                     [org.clojars.weavejester/autodoc "0.7.1"]]
  :main clarity.core
  :jvm-opts ["-Xdebug" "-Xrunjdwp:transport=dt_socket,server=y,suspend=n"])
