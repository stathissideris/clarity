(ns clarity.menu
  (:import [javax.swing JMenuBar JMenu JMenuItem]))

(defn menu-item [item]
  (if (string? item)
    (JMenuItem. item)
    (item)))

(defn menu [title items]
  (let [m (if (string? title)
            (JMenu. title)
            title)
        items (map menu-item items)]
    (map #(.add m %) items)
    m))

(defn menu-bar [& menus]
  (let [bar (JMenuBar.)
        menus (map #(apply menu %) (partition 2 menus))]
    (map #(.add bar %) menus)
    bar))
