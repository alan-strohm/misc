(import judge)
(use ./util)

(def real-input (slurp "./input/14.txt"))
(def test-input `
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
`)

(defn read [str]
  (def peg ~{:main (split "\n" (+ -1 (group :line)))
             :line (* "p=" :p2d " v=" :p2d)
             :p2d (group (* :num "," :num))
             :num (number (* (? "-") :d+))})
  (peg/match peg str))

(judge/test (read test-input)
  @[@[@[0 4] @[3 -3]]
    @[@[6 3] @[-1 -3]]
    @[@[10 3] @[-1 2]]
    @[@[2 0] @[2 -1]]
    @[@[0 0] @[1 3]]
    @[@[3 0] @[-2 -2]]
    @[@[7 6] @[-1 -3]]
    @[@[3 0] @[-1 -2]]
    @[@[9 3] @[2 3]]
    @[@[7 3] @[-1 2]]
    @[@[2 4] @[2 -3]]
    @[@[9 5] @[-3 -3]]])

(defn move [dims p v t]
  (def next-p (p2+ p (vec*n v t)))
  (tuple/slice
    (map mod next-p dims)))

(judge/test (move [11 7] [2 4] [2 -3] 1) [4 1])
(judge/test (move [11 7] [2 4] [2 -3] 2) [6 5])
(judge/test (move [11 7] [2 4] [2 -3] 3) [8 2])
(judge/test (move [11 7] [2 4] [2 -3] 4) [10 6])
(judge/test (move [11 7] [2 4] [2 -3] 5) [1 3])

(defn quad [dims p]
  (defn coord [dim v]
    (def mid (div dim 2))
    (cond
      (< v mid) 0
      (> v mid) 1
      nil))
  (def result (tuple/slice (map coord dims p)))
  (if (some nil? result) nil result))

(judge/test (quad [11 7] [5 0]) nil)
(judge/test (quad [11 7] [4 0]) [0 0])
(judge/test (quad [11 7] [6 0]) [1 0])
(judge/test (quad [11 7] [6 3]) nil)
(judge/test (quad [11 7] [6 4]) [1 1])
(judge/test (quad [11 7] [4 4]) [0 1])

(defn part1 [dims str]
  (def num-per-quad @{})
  (loop [[p v] :in (read str)
         :let [p (move dims p v 100)
               quad (quad dims p)]
         :when quad]
    (update num-per-quad quad |(inc (or $ 0))))
  (product num-per-quad))

(judge/test (part1 [11 7] test-input) 12)
(judge/test (part1 [101 103] real-input) 222901875)
