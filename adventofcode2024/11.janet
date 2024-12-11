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

(defn run [str num]
  (var stones (read str))
  (loop [:before (var i 0) :repeat num :after (++ i)] (pp i) (set stones (blink stones)))
  (length stones))
  #stones)

(def memo @{})

(defn blink-n [stone n]
  (if-let [res (memo [stone n])]
    (break res))

  (when (= n 0)
    (break 1))

  (def next-stones
    (cond
      (= "0" stone) ["1"]
      (even? (length stone)) (split stone)
      [(-> stone scan-number (* 2024) string)]))
  (def res (sum (map |(blink-n $ (dec n)) next-stones)))
  (put memo [stone n] res)
  res)

(defn run [str num]
  (sum (map |(blink-n $ num) (read str))))

(judge/test (run test-input 2) 4)
(judge/test (run test-input 25) 55312)
(judge/test (run real-input 25) 189092)
(judge/test (run real-input 75) 224869647102559)
