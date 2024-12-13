(import judge)
(use ./util)

(def real-input (slurp "./input/13.txt"))

(def test-input `
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
`)

(defn read [str]
  (def peg ~{:main (split "\n\n" (+ -1 (group :machine)))
             :machine (* (group (2 (group :button))) (group :prize))
             :button (* "Button " (+ "A" "B") ": X+" (number :d+) ", Y+" (number :d+) "\n")
             :prize (* "Prize: X=" (number :d+) ", Y=" (number :d+))})
  (peg/match peg str))

(judge/test (read test-input)
  @[@[@[@[94 34] @[22 67]] @[8400 5400]]
    @[@[@[26 66] @[67 21]] @[12748 12176]]
    @[@[@[17 86] @[84 37]] @[7870 6450]]
    @[@[@[69 23] @[27 71]] @[18641 10279]]])

# 94A + 22B = 8400
# 34A + 67B = 5400

(judge/test (solve2d [[94 34] [22 67]] [8400 5400]) [80 40])
(judge/test (solve2d [[69 27] [23 71]] [18641 10279]) [254.11266947171575 48.140252454417954])
(judge/test (mod 2 1) 0)

(defn part1 [str]
  (sum-loop [[A y] :in (read str)
             :let [[a b] (solve2d A y)]
             :when (all |(zero? (mod $ 1)) [a b])]
            (+ (* a 3) b)))

(judge/test (part1 test-input) 480)
(judge/test (part1 real-input) 36571)
