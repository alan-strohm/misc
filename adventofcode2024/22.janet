(use judge)
(use ./util)

(def real-input (slurp "./input/22.txt"))
(def test-input `
1
10
100
2024
`)

(defn next-num [num]
  (var res num)
  (set res (xor (* res 64) res))
  (%= res 16777216)
  (set res (xor (div res 32) res))
  (%= res 16777216)
  (set res (xor (* res 2048) res))
  (%= res 16777216)
  res)

(test (bxor 15 42) 37)
(test (next-num 123) 15887950)
(test (* 15887950 64) 1016828800)
(test (next-num 15887950) 16495136)

(defn nums [start times]
  (seq [:before (var cur start)
        :repeat (inc times)
        :after (set cur (next-num cur))]
    cur))

(test (last (nums 123 10)) 5908254)

(defn read [str]
  (peg/match '(split "\n" (+ -1 (number :d+))) str))

(test (read test-input) @[1 10 100 2024])

(defn part1 [str]
  (sum-loop [num :in (read str)]
            (last (nums num 2000))))

(test (part1 test-input) 37327623)
(test (part1 real-input) 19847565303)

(defn prices [start times]
  (map |(% $ 10) (nums start times)))

(test (prices 123 10) @[3 0 6 5 4 4 6 4 4 2 4])

(defn get-changes [prices]
  (seq [[i p2] :pairs (array/slice prices 1)]
    (- p2 (in prices i))))

(test (get-changes (prices 123 10))
  @[-3 6 -1 -1 0 2 -2 0 -2 2])

(defn possible-sales [num]
  (def result @{})
  (def prices (prices num 2000))
  (def changes (get-changes prices))
  (loop [i :range [3 (length changes)]
         :let [sig [(changes (- i 3)) (changes (- i 2))
                    (changes (- i 1)) (changes i)]]
         :unless (result sig)]
      (put result sig (prices (+ i 1))))
  result)

(test (in (possible-sales 1) [-2 1 -1 3]) 7)
(test (in (possible-sales 2) [-2 1 -1 3]) 7)
(test (in (possible-sales 3) [-2 1 -1 3]) nil)
(test (in (possible-sales 2024) [-2 1 -1 3]) 9)

(def test-input `
1
2
3
2024
`)

(defn part2 [str]
  (def totals @{})
  (loop [start :in (read str)
         :before (var seen @{})
         :let [prices (prices start 2000)
               changes (get-changes prices)]
         i :range [3 (length changes)]
         :let [sig [(changes (- i 3)) (changes (- i 2))
                    (changes (- i 1)) (changes i)]]
         :unless (seen sig)]
    (put seen sig true)
    (def price (prices (+ i 1)))
    (update totals sig |(+ (or $ 0) price)))
  (max-of totals))

(test (part2 test-input) 23)
# 7.4s
(test (part2 real-input) 2250)
