(import judge)

(use ./util)

(def test-input `
3   4
4   3
2   5
1   3
3   9
3   3`)

(def real-input (slurp "./input/01.txt"))

(defn read [str]
  (def peg ~{:main (split "\n" (+ -1 :line))
             :line (group (split :s+ (number :d+)))})
  (def lines (peg/match peg str))
  [(map first lines)
   (map last lines)])

(judge/test (read test-input)
  [@[3 4 2 1 3 3] @[4 3 5 3 9 3]])

(judge/test (map length (read real-input)) @[1000 1000])

(defn part1 [str]
  (defn dist [x y] (math/abs (- x y)))
  (def lists (map sort (read str)))
  (sum (map dist ;lists)))

(judge/test (part1 test-input) 11)
(judge/test (part1 real-input) 2430334)

(defn part2 [str]
  (def [left right] (read str))
  (def right-frequencies (frequencies right))
  (sum-loop [num :in left
             :let [freq (in right-frequencies num 0)]]
            (* num freq)))

(judge/test (part2 test-input) 31)
(judge/test (part2 real-input) 28786472)
