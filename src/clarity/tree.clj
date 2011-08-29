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

(defn- coerce-to-array [type content]
  (if (sequential? content)
    (into-array type content)
    (into-array type [content])))

(defn event
  ([tree path]
     (let [path (coerce-to-array Object path)]
       (TreeModelEvent. tree path)))
  ([tree path indices objects]
     (let [path (coerce-to-array Object path)
           indices (coerce-to-array Integer/TYPE indices)
           objects (coerce-to-array Object objects)]
       (TreeModelEvent. tree path indices objects))))

(defn fire-event
  "The type can be :change :insert :remove :structure."
  ([listeners type event]
     (case type
           :change (doall (map #(.treeNodesChanged % event) listeners))
           :insert (doall (map #(.treeNodesInserted % event) listeners))
           :remove (doall (map #(.treeNodesRemoved % event) listeners))
           :structure (doall (map #(.treeStructuredChanged % event) listeners)))))
