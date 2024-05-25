(import judge)

(def test-input `
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`)

(def real-input (string/trim (slurp "./day09/input.txt")))

(def peg '{:main (split "\n" (group :row))
           :row (split " " (number (some 1)))})

(judge/test (peg/match peg test-input)
  @[@[0 3 6 9 12 15]
    @[1 3 6 10 15 21]
    @[10 13 16 21 30 45]])

(defn diff-list [list]
  (seq [[i r] :pairs (array/slice list 1)
        :let [l (in list i)]]
      (- r l)))

(judge/test (diff-list @[0 3 6 9 12 15]) @[3 3 3 3 3])

(defn next-entry [list]
  (if (all |(= $ 0) list) 0
    (+ (last list) (next-entry (diff-list list)))))

(judge/test (next-entry @[0 3 6 9 12 15]) 18)

(defn part1 [str] (sum (map next-entry (peg/match peg str))))

(judge/test (part1 real-input) 2043183816)

(defn prev-entry [list]
  (if (all |(= $ 0) list) 0
    (- (first list) (prev-entry (diff-list list)))))

(judge/test (prev-entry @[0 3 6 9 12 15]) -3)

(defn part2 [str] (sum (map prev-entry (peg/match peg str))))

(judge/test (part2 real-input) 1118)
