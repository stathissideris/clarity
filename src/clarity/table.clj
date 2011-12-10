(ns clarity.table
  (:import [javax.swing.table TableModel AbstractTableModel]))

(defn immutable-table-model
  "Create an immutable table model from a seq of seqs in data and the
  passed column names."
  [data column-names]
  (let [row-count (count data)
        column-count (count column-names)]
    (proxy [AbstractTableModel] []
      (getColumnName [index] (nth column-names index))
      (getColumnCount [] column-count)
      (getRowCount [] row-count)
      (getValueAt [row column] (nth (nth data row) column)))))

(defn column-names
  "Seq of the column names of a table."
  [^TableModel model]
  (map #(.getColumnName model %)
       (range (.getColumnCount model))))

(defn row-count
  "Get the row count."
  [^TableModel model]
  (.getRowCount model))

(defn column-count
  "Get the column count."
  [^TableModel model]
  (.getColumnCount model))

(defn value-at
  "Get the value at a cell."
  [^TableModel model row column]
  (.getValueAt model row column))

(defn row-seq
  "Seq containing all the values of a row."
  [^TableModel model row]
  (map #(.getValueAt model row %)
       (range (column-count model))))

(defn column-seq
  "Seq containing all the values of a column."
  [^TableModel model column]
  (map #(.getValueAt model % column)
       (range (row-count model))))

(defn row-as-map
  "Map of column names to the values of a specific row."
  [^TableModel model row]
  (zipmap (column-names model) (row-seq model row)))

(defn rows-as-map
  "Seq of maps of rows."
  [^TableModel model]
  (map #(row-as-map model %) (range (row-count model))))
