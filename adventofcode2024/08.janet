(import judge)

(use ./util)

(def real-input (slurp "./input/08.txt"))

(def test-input `
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
`)

(defn read [str]
  (def ants @{})
  (def grid (grid/parse str))
  (loop [[p v] :in (grid/pairs grid)
         :when (not= v ".")]
    (update ants v |(array/push (or $ @[]) p)))
  [ants grid])

(defn node [a b] (p2+ b (p2- b a)))

(judge/test (node [4 3] [5 5]) [6 7])
(judge/test (node [5 5] [4 3]) [3 1])

(defn part1 [str]
  (def [by-ant grid] (read str))
  (def nodes @{})
  (loop [[_ ants] :pairs by-ant
         a :in ants
         b :in ants
         :let [n (node a b)]
         :when (and
                 (not= a b)
                 (grid/contains grid n))]
    (put nodes n true))
(length nodes))

(judge/test (part1 test-input) 14)
(judge/test (part1 real-input) 369)
