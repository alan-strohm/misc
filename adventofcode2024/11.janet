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

(defn blink [stones]
  (var result @[])
  (loop [stone :in stones]
    (cond
      (= "0" stone) (array/push result "1")
      (even? (length stone)) (array/push result ;(split stone))
      (array/push result (-> stone scan-number (* 2024) string))))
  result)

(judge/test (blink ["0" "1" "10" "99" "999"]) @["1" "2024" "1" "0" "9" "9" "2021976"])
(judge/test (blink ["1000"]) @["10" "0"])

(defn part1 [str num]
  (var stones (read str))
  (loop [:repeat num] (set stones (blink stones)))
  (length stones))
  #stones)


(judge/test (part1 test-input 2) 4)
(judge/test (part1 test-input 25) 55312)
(judge/test (part1 real-input 25) 189092)
