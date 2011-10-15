(ns clarity.widgets
  (:require [clarity.component :as c]
            [clarity.style :as style]
            [clojure.contrib.str-utils2 :as str2])
  (:import [javax.swing JSplitPane JScrollPane JEditorPane JFileChooser]
           [javax.swing.text.html HTMLEditorKit]
           [com.petebevin.markdown MarkdownProcessor]))

;;; this part of the namespace wraps some Swing components to make them more
;;; clojure-y and implements some other components for common tasks

(defn split-pane [orientation one two]
  "Creates a JSplitPane with contents. Pass :horizontal for horizontal
  orientation. one and two are the components added." 
  (JSplitPane. (if (= :horizontal orientation)
                 JSplitPane/HORIZONTAL_SPLIT
                 JSplitPane/VERTICAL_SPLIT)
               one two))

(defn para
  "Creates a paragraph of text (using JEditorPane) that wraps
  according to the available width. It is also possible to copy the
  text and the input can be HTML. If the :rich flag is passed, the
  text is processed as markdown."
  [s & flags]
  (let [font (style/get-laf-property "Label.font")
        rich? (some #{:rich} flags)
        text (if rich?
               (str2/replace
                (.markdown (MarkdownProcessor.) s)
                "\n" "<br>")
               s)
        rule (str "body { font-family: "
                  (.getFamily font)
                  "; "
                  "font-size: "
                  (.getSize font)
                  "pt; }")
        pane (doto (c/make :editor-pane
                           [:init (.getContentType (HTMLEditorKit.)) text])
               (.setText text)
               (.setOpaque false)
               (.setBorder nil)
               (.setEditable false))]
    (.addRule (.getStyleSheet (.getDocument pane)) rule)
    pane))

(defn scroll-pane [comp]
  (c/make :scroll-pane comp))

(defn choose-file
  "Opens a file selection dialog and returns the absolute path of the
  selected file. The initial path can be passed, and if the second
  parameter is :save, the dialog is opened in save mode."
  ([] (choose-file nil))
  ([path] (choose-file path :load))
  ([path flag]
     (let [chooser (JFileChooser. path)
           option (if (= :save flag)
                    (.showSaveDialog chooser nil)
                    (.showOpenDialog chooser nil))]
       (if (= JFileChooser/APPROVE_OPTION option)
         (.getAbsolutePath (.getSelectedFile chooser))))))
