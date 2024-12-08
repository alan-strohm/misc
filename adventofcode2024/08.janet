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

(varfn nodes [grid a b]
  (def n (p2+ b (p2- b a)))
  (if (and (not= a b)
           (grid/contains grid n))
    [n]
    []))

(defn run [str]
  (def [by-ant grid] (read str))
  (length
    (tabseq [[_ ants] :pairs by-ant
             a :in ants
             b :in ants
             n :in (nodes grid a b)]
            n true)))

(judge/test (run test-input) 14)
(judge/test (run real-input) 369)

(varfn nodes [grid a b]
  (seq [:when (not= a b)
        :let [dist (p2- b a)]
        :before (var cursor b)
        _ :iterate (grid/contains grid cursor)
        :after (p2+= cursor dist)]
    cursor))

(judge/test (run test-input) 34)
(judge/test (run real-input) 1169)
