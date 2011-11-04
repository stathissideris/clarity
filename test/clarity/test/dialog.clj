(ns clarity.test.dialog
  (:require [clarity.form :as form])
  (:use [clarity.dialog] :reload)
  (:use [clojure.test]))

(deftest test-dialog-with-form
  (dialog (form/form :a 5 :b 2) [:ok :cancel] nil))
