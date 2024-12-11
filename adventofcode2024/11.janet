(import judge)
(use ./util)

(def real-input (slurp "./input/11.txt"))
(def test-input "125 17")

(defn read [str] (peg/match '(split :s+ (+ -1 ':d+)) str))

(judge/test (read test-input) @["125" "17"])

(defn split [stone]
  (def mid (/ (length stone) 2))
  (map |(as-> $ _ (string/triml _ "0") (if (= "" _) "0" _))
       [(string/slice stone 0 mid)
        (string/slice stone mid)]))

(judge/test (split "1000") @["10" "0"])

(var blink-many nil)

(defmemo blink-one [stone times]
  (when (= times 0)
    (break 1))

  (def next-stones
    (cond
      (= "0" stone) ["1"]
      (even? (length stone)) (split stone)
      [(-> stone scan-number (* 2024) string)]))
  (blink-many next-stones (dec times)))

(varfn blink-many [stones times]
  (sum (map |(blink-one $ times) stones)))

(defn run [str num]
  (blink-many (read str) num))

(judge/test (run test-input 25) 55312)
(judge/test (run real-input 25) 189092)
(judge/test (run real-input 75) 224869647102559)
