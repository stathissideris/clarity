(ns clarity.tree
  (:import [javax.swing JTree]
           [javax.swing.event TreeModelEvent]))

(defn get-selection [^JTree tree]
  (let [path (.getSelectionPath tree)]
    (if path
      (.getLastPathComponent path))))

(defn get-selections [^JTree tree]
  (let [paths (.getSelectionPaths tree)]
    (if paths
      (map #(.getLastPathComponent %) paths))))

(defn event
  ([^Tree tree path]
     (TreeModelEvent. source path))
  ([^JTree tree path indices objects]
     (TreeModelEvent. source path indices objects)))

(defn fire-event
  "The type can be :change :insert :remove :structure."
  ([listeners event type]
     (case type
           :change (doall (map #(.treeNodesChanged % event) listeners))
           :insert (doall (map #(.treeNodesInserted % event) listeners))
           :remove (doall (map #(.treeNodesRemoved % event) listeners))
           :structure (doall (map #(.treeStructuredChanged % event) listeners)))))
