#_(defstylesheet
    test-stylesheet
    (style (category :important)
           (:color (color :red))
           (:font (font :style :bold)))
     
    (style (or (id :title)
               (category :header))
           (:font (font :size "200%"))))
