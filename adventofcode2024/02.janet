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

(defn safe-v1 [report]
  (def diffs (seq [[i other] :pairs (array/slice report 1)
                    :let [this (in report i)]]
                (- this other)))
  (def dists (map math/abs diffs))
  (def dirs (map / diffs dists))
  (and
    (let [num-one (count |(= 1 $) dirs)]
      (or (= num-one 0)
          (= num-one (length dirs))))
    (all |(and (>= $ 1) (<= $ 3)) dists)))

(judge/test (safe-v1 @[7 6 4 2 1])
  true)

(defn solve [str safe]
  (sum-loop [report :in (read str)
             :when (safe report)]
            1))

(defn part1 [str]
  (solve str safe-v1))

(judge/test (part1 test-input) 2)
(judge/test (part1 real-input) 369)

(defn safe-v2 [report]
  (var safe (safe-v1 report))
  (or
    (safe-v1 report)
    (some |(safe-v1 (array/remove (array ;report) $))
          (keys report))))

(judge/test (safe-v2 @[1 3 2 4 5]) true)
(judge/test (safe-v2 @[7 6 4 2 3]) true)

(defn part2 [str]
  (solve str safe-v2))

(judge/test (part2 test-input) 4)
(judge/test (part2 real-input) 428)
