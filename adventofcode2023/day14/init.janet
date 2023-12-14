(import judge)

(def example `
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....`)

(def types
  {(chr ".") :.
   (chr "#") :$
   (chr "O") :O})

(defn load [str]
  (->> str (string/trim) (string/split "\n") (map |(map types $))))

(judge/test (load example)
  @[@[:O :. :. :. :. :$ :. :. :. :.]
    @[:O :. :O :O :$ :. :. :. :. :$]
    @[:. :. :. :. :. :$ :$ :. :. :.]
    @[:O :O :. :$ :O :. :. :. :. :O]
    @[:. :O :. :. :. :. :. :O :$ :.]
    @[:O :. :$ :. :. :O :. :$ :. :$]
    @[:. :. :O :. :. :$ :O :. :. :O]
    @[:. :. :. :. :. :. :. :O :. :.]
    @[:$ :. :. :. :. :$ :$ :$ :. :.]
    @[:$ :O :O :. :. :$ :. :. :. :.]])

(defn tilt [grid]
  (loop [y :range [0 (length grid)]
         :let [line (y grid)]
         x :range [0 (length line)]
         :when (= :O (x line))]
    (var finaly y)
    (loop [off :range [0 y]
           :let [oy (- y off 1)
                 oitem (get-in grid [oy x])]]
      (if (= :. oitem) (set finaly oy) (break)))
    (if (not= finaly y)
      (do
        (put-in grid [finaly x] :O)
        (put-in grid [y x] :.))))
  grid)


(judge/test (tilt (load example))
  @[@[:O :O :O :O :. :$ :. :O :. :.]
    @[:O :O :. :. :$ :. :. :. :. :$]
    @[:O :O :. :. :O :$ :$ :. :. :O]
    @[:O :. :. :$ :. :O :O :. :. :.]
    @[:. :. :. :. :. :. :. :. :$ :.]
    @[:. :. :$ :. :. :. :. :$ :. :$]
    @[:. :. :O :. :. :$ :. :O :. :O]
    @[:. :. :O :. :. :. :. :. :. :.]
    @[:$ :. :. :. :. :$ :$ :$ :. :.]
    @[:$ :. :. :. :. :$ :. :. :. :.]])

(defn calc-load [grid]
  (var result 0)
  (loop [y :range [0 (length grid)]
         :let [line (y grid)]
         x :range [0 (length line)]
         :when (= :O (x line))
         :let [load (- (length grid) y)]]
    (+= result load))
  result)

(defn part1 [str] (-> str (load) (tilt) (calc-load)))

(judge/test (part1 example) 136)
(judge/test (part1 (slurp "./day14/input.txt")) 108889)
