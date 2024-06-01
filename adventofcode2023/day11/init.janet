(import judge)
(use ../util)

(def test-input `
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
`)
(def real-input (slurp "./day11/input.txt"))

(defn find-galaxies [grid]
  (seq [[p v] :in (grid/pairs grid)
        :when (= v "#")]
    p))

(defn expand [by grid]
  (def galaxies (find-galaxies grid))

  (def has-galaxy (let [[w h] (grid :dims)]
                    [(array/new-filled w false)
                     (array/new-filled h false)]))

  (loop [p :in galaxies
         [dim i] :pairs p]
    (put-in has-galaxy [dim i] true))

  (def expanded (tabseq [g :in galaxies] g (array ;g)))

  (loop [[dim dim-has-galaxy] :pairs has-galaxy
         [i i-has-galaxy] :pairs dim-has-galaxy
         :unless i-has-galaxy
         g :in galaxies :when (> (in g dim) i)]
    (update-in expanded [g dim] |(+ by $)))
  (values expanded))

(judge/test (expand 1 (grid/parse test-input))
  @[@[9 1]
    @[12 7]
    @[4 0]
    @[0 11]
    @[1 6]
    @[9 10]
    @[0 2]
    @[8 5]
    @[5 11]])

(defn sum-distances [galaxies]
  (sum-loop [[i g1] :pairs galaxies
             g2 :in (array/slice galaxies (inc i))
             [dim g1dim] :pairs g1
             :let [g2dim (in g2 dim)]]
            (math/abs (- g1dim g2dim))))

(defn run [str by]
  (->> str grid/parse (expand by) sum-distances))

(judge/test (run test-input 1) 374)
(judge/test (run real-input 1) 9627977)

(judge/test (run test-input 9) 1030)
(judge/test (run test-input 99) 8410)

(judge/test (run real-input 999999) 644248339497)

