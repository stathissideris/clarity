(ns clarity.test.chain
  (:require [clojure.string :as str])
  (:use [clarity.chain] :reload)
  (:use [clojure.test]))

;;simple chain

(deftest chain-simple
  (let [appender (fn [x] (fn [s] (str s x)))]
  (is (= "tseT" (chain [str/capitalize str/reverse] "test")))
  (is (= "Tset" (chain [str/reverse str/capitalize] "test")))
  (is (= "Sabc" (chain [(appender "a") (appender "b") (appender "c")] "S")))))

(deftest chain-multiple-args
  (let [cap (fn [a1 a2 a3 a4] [a1 a2 (str/capitalize a3) a4])
        rev (fn [a1 a2 a3 a4] [a1 a2 (str/reverse a3) a4])]
    (is (= ["a" "b" "tseT" "d"]
             (chain [cap rev] "a" "b" "test" "d")))
    (is (= ["a" "b" "Tset" "d"]
             (chain [rev cap] "a" "b" "test" "d")))))

(deftest chain-multiple-args-separate
  (let [cap (fn [a1 a2 a3 a4] [a1 a2 (str/capitalize a3) a4])
        rev (fn [a1 a2 a3 a4] [a1 (str/reverse a2) a3 a4])]
    (is (= ["a" "kcik" "Test" "d"]
             (chain [cap rev] "a" "kick" "test" "d")))
    (is (= ["a" "kcik" "Test" "d"]
             (chain [rev cap] "a" "kick" "test" "d")))))

;;vetoable chain - no veto

(deftest chain-vetoable-simple
  (is (= "tseT" (chain-vetoable [str/capitalize str/reverse] "test")))
  (is (= "Tset" (chain-vetoable [str/reverse str/capitalize] "test"))))

(deftest chain-vetoable-multiple-args
  (let [cap (fn [a1 a2 a3 a4] [a1 a2 (str/capitalize a3) a4])
        rev (fn [a1 a2 a3 a4] [a1 a2 (str/reverse a3) a4])]
    (is (= ["a" "b" "tseT" "d"]
             (chain-vetoable [cap rev] "a" "b" "test" "d")))
    (is (= ["a" "b" "Tset" "d"]
             (chain-vetoable [rev cap] "a" "b" "test" "d")))))

(deftest chain-vetoable-multiple-args-separate
  (let [cap (fn [a1 a2 a3 a4] [a1 a2 (str/capitalize a3) a4])
        rev (fn [a1 a2 a3 a4] [a1 (str/reverse a2) a3 a4])]
    (is (= ["a" "kcik" "Test" "d"]
             (chain-vetoable [cap rev] "a" "kick" "test" "d")))
    (is (= ["a" "kcik" "Test" "d"]
             (chain-vetoable [rev cap] "a" "kick" "test" "d")))))

;;vetoable chain - with veto

(deftest chain-vetoable-simple-with-veto-numbers
  (let [adder (fn [x] (partial + x))
        ensure-more-than-10 (fn [x] (if (< x 10) :veto x))]
    (is (= :veto
           (chain-vetoable [(adder 1) (adder 2) (adder 3) ensure-more-than-10] 1)))
    (is (= 56
           (chain-vetoable [(adder 1) (adder 2) (adder 3) ensure-more-than-10] 50)))
    (is (= :veto
           (chain-vetoable [(adder 1) ensure-more-than-10 (adder 2) (adder 50)] 1)))
    (is (= 13
           (chain-vetoable [(adder 1) ensure-more-than-10 (adder 1) (adder 1)] 10)))))

(deftest chain-vetoable-simple-with-veto-strings
  (let [appender (fn [x] (fn [s] (str s x)))
        finder (fn [regex] (fn [s] (if (re-find regex s) s :veto)))]
    (is (= :veto
           (chain-vetoable [(appender "a")
                            (appender "b")
                            (appender "c")
                            (finder #"^test")]
                            "S")))
    (is (= :veto
           (chain-vetoable [(appender "a")
                            (appender "b")
                            (finder #"^test")
                            (appender "c")]
                            "S")))
    (is (= "Sabc"
           (chain-vetoable [(appender "a")
                            (appender "b")
                            (finder #"^Sa")
                            (appender "c")]
                            "S")))
    (is (= :veto
           (chain-vetoable [(appender "a")
                            (appender "b")
                            (finder #"^a")
                            (appender "c")]
                            "S")))
    (is (= "Sabc"
           (chain-vetoable [(appender "a")
                            (appender "b")
                            (finder #"ab")
                            (appender "c")]
                            "S")))))

(deftest chain-vetoable-with-hex
  (is (= :veto
         (chain-vetoable [(fn [a b s] [a b "s"])
                          (fn [a b s] (if (re-matches #"[A-F]+" s) [a b s] :veto))]
                         "_" "_" "_")))
  (is (= ["a" "b" "F"]
         (chain-vetoable [(fn [a b s] [a b (str/upper-case s)])
                          (fn [a b s] (if (re-matches #"[A-F]+" s) [a b s] :veto))]
                         "a" "b" "f"))))
                         