(import judge)

(def example `
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

(defn find-galaxies [lines]
  (var galaxies @[])
  (loop [y :range [0 (length lines)]
         :let [line (get lines y)]
         x :range [0 (length line)]
         :let [v (get line x)]]
    (if (= v (chr "#"))
      (array/push galaxies [x y])))
  galaxies)

(defn expand [by lines]
  (def has-galaxy
    [(array/new-filled (length (get lines 0)) false)
     (array/new-filled (length lines) false)])

  (def galaxies (find-galaxies lines))

  (loop [p :in galaxies
         dim :range [0 2]]
    (put-in has-galaxy [dim (get p dim)] true))

  (def galaxy-mapping (zipcoll galaxies (map |(array ;$) galaxies)))

  (loop [dim :range [0 2]
         i :range [0 (length (get has-galaxy dim))]
         :when (not (get-in has-galaxy [dim i]))
         g :in galaxies :when (> (get g dim) i)]
    (update-in galaxy-mapping [g dim] |(+ by $)))
  (values galaxy-mapping))

(judge/test (expand 1 (string/split "\n" example))
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
  (var dist 0)
  (loop [gi :range [0 (length galaxies)]
         gj :range [(+ gi 1) (length galaxies)]
         dim :range [0 2]
         :let [g1dim (get-in galaxies [gi dim])
               g2dim (get-in galaxies [gj dim])]]
    (+= dist (math/abs (- g1dim g2dim))))
  dist)

(defn run [str by]
  (->> str (string/trim) (string/split "\n") (expand by) (sum-distances)))

(judge/test (run example 1) 374)
(judge/test (run (slurp "./day11/input.txt") 1) 9627977)

(judge/test (run example 9) 1030)
(judge/test (run example 99) 8410)

(judge/test (run (slurp "./day11/input.txt") 999999) 644248339497)

