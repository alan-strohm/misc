(import judge)

(use ../util)

(def test-input `
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
`)

(def real-input (string/trim (slurp "./input.txt")))

(def peg1
  '{:main (split "\n" (group :line))
    :line (some (+ (number :d) 1))})

(judge/test (peg/match peg1 test-input) @[@[1 2] @[3 8] @[1 2 3 4 5] @[7]])

(defn solve [peg str]
  (sum-loop [line :in (peg/match peg str)]
            (+ (* 10 (first line)) (last line))))

(judge/test (solve peg1 test-input) 142)
(judge/test (solve peg1 real-input) 54304)

(def peg2
  '{:main (split "\n" (group :line))
    :line (some (+ :number 1))
    :number (if (+ (/ "one" 1)
                   (/ "two" 2)
                   (/ "three" 3)
                   (/ "four" 4)
                   (/ "five" 5)
                   (/ "six" 6)
                   (/ "seven" 7)
                   (/ "eight" 8)
                   (/ "nine" 9)
                   (number :d))
              1)})

(def test-input `
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`)

(judge/test (solve peg2 test-input) 281)
(judge/test (solve peg2 real-input) 54418)
