(import judge)

(use ./util)

(def test-input `
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen`)

(def real-input (slurp "./input/01.txt"))

(defn run [number-peg str]
  (def lines (peg/match (sep ~(group ,number-peg) "\n") str))
  (sum
    (seq [line :in lines
          :let [tens (first line)
                ones (last line)]]
      (+ (* tens 10) ones))))

(defn part1 [str]
  (run ~(some (+ (number :d) :a)) str))

(judge/test (part1 real-input) 54304)

(def number-peg-part2
  ~(some
     (+
      (/ (if "one" 1) 1)
      (/ (if "two" 1) 2)
      (/ (if "three" 1) 3)
      (/ (if "four" 1) 4)
      (/ (if "five" 1) 5)
      (/ (if "six" 1) 6)
      (/ (if "seven" 1) 7)
      (/ (if "eight" 1) 8)
      (/ (if "nine" 1) 9)
      (number :d)
      :a)))

(defn part2 [str]
  (run number-peg-part2 str))

(judge/test (part2 real-input) 54418)
(judge/test (part2 test-input) 281)
