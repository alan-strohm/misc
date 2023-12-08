(import judge)

(def example `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45`)

(def peg
  (peg/compile
    '{:number (number (* (? "-") :d+))
      :row (group (some (+ " " :number)))
      :main (some (+ :row "\n"))}))

(judge/test (peg/match peg example)
  @[@[0 3 6 9 12 15]
    @[1 3 6 10 15 21]
    @[10 13 16 21 30 45]])

(defn diff-list [list]
  (var result @[])
  (loop [i :range [1 (length list)]]
    (let [r (get list i)
          l (get list (- i 1))]
      (array/push result (- r l))))
  result)

(judge/test (diff-list @[0 3 6 9 12 15]) @[3 3 3 3 3])

(defn next-entry [list]
  (if (all |(= $ 0) list) 0
    (+ (last list) (next-entry (diff-list list)))))

(judge/test (next-entry @[0 3 6 9 12 15]) 18)

(defn part1 [file]
  (sum (map next-entry (peg/match peg (slurp file)))))

(judge/test (part1 "./day09/input.txt") 2043183816)

(defn prev-entry [list]
  (if (all |(= $ 0) list) 0
    (- (first list) (prev-entry (diff-list list)))))

(judge/test (prev-entry @[0 3 6 9 12 15]) -3)

(defn part2 [file]
  (sum (map prev-entry (peg/match peg (slurp file)))))

(judge/test (part2 "./day09/input.txt") 1118)
