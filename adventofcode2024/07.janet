(import judge)

(use ./util)

(def real-input (slurp "./input/07.txt"))

(def test-input `
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
`)

(defn read [str]
  (def peg ~{:main (split "\n" (+ -1 (group :line)))
             :line (* (number :d+) ":" :s+ :ins)
             :ins (/ (split :s+ (number :d+)) ,|(make-list $&))})
  (peg/match peg str))

(judge/test (read test-input)
  @[@[190 [10 [19 []]]]
    @[3267 [81 [40 [27 []]]]]
    @[83 [17 [5 []]]]
    @[156 [15 [6 []]]]
    @[7290 [6 [8 [6 [15 []]]]]]
    @[161011 [16 [10 [13 []]]]]
    @[192 [17 [8 [14 []]]]]
    @[21037 [9 [7 [18 [13 []]]]]]
    @[292 [11 [6 [16 [20 []]]]]]])

(defn concat-nums [a b]
  (var [a rem] [a b])
  (while (> (set rem (div rem 10)) 0)
    (*= a 10))
  (+ (* 10 a) b))

(judge/test (concat-nums 6 15) 615)
(judge/test (concat-nums 15 62) 1562)

(defn solvable? [ops out ins]
  (match ins
    ([a rest] (> a out)) (break false)
    [a [b rest]] (some 
                   |(solvable? ops out (cons ($ a b) rest))
                   ops)
    [a []] (break (= out a))
    (errorf "unexpected input: %n" ins)))

(judge/test (solvable? [concat-nums] 156 [15 [6 []]]) true)
(judge/test (solvable? [* concat-nums] 7290 (make-list [6 8 6 15])) true)

(defn run [str ops]
  (sum-loop [[out ins] :in (read str)
             :when (solvable? ops out ins)]
            out))

(defn part1 [str] (run str [* +]))

(judge/test (part1 test-input) 3749)
(judge/test (part1 real-input) 3119088655389)

(defn part2 [str] (run str [concat-nums * +]))

(judge/test (part2 test-input) 11387)
# 6.5s
(judge/test (part2 real-input) 264184041398847)
