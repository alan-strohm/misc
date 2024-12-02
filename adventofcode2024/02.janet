(import judge)

(use ./util)

(def test-input `
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9`)

(def real-input (slurp "./input/02.txt"))

(defn read [str]
  (def peg ~{:main (split "\n" (+ -1 (group :line)))
             :line (split :s+ (number :d+))})
  (peg/match peg str))

(judge/test (read test-input)
  @[@[7 6 4 2 1]
    @[1 2 7 8 9]
    @[9 7 6 2 1]
    @[1 3 2 4 5]
    @[8 6 4 4 1]
    @[1 3 6 7 9]])

(defn safe [report]
  (def diffs (seq [[i other] :pairs (array/slice report 1)
                    :let [this (in report i)]]
                (- this other)))
  (def dists (map math/abs diffs))
  (def dirs (map / diffs dists))
  (and
    (or (all |(= 1 $) dirs)
        (all |(= -1 $) dirs))
    (all |(and (>= $ 1) (<= $ 3)) dists)))

(judge/test (safe @[7 6 4 2 1])
  true)

(defn part1 [str]
  (sum-loop [report :in (read str)
             :when (safe report)]
            1))

(judge/test (part1 test-input) 2)
(judge/test (part1 real-input) 369)
