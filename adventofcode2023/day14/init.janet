(import judge)

(def test-input `
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....

`)

(def real-input (slurp "./day14/input.txt"))

(def types
  {(chr ".") :.
   (chr "#") :$
   (chr "O") :O})

(defn load [str]
  (->> str (string/trim) (string/split "\n") (map |(map types $))))

(judge/test (load test-input)
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

(defn p+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(def dirs [[0 -1] [-1 0] [0 1] [1 0]])
(def [N W S E] dirs)

(defn tilt [grid dir]
  (def yt (if (= dir S) reverse identity))
  (def xt (if (= dir E) reverse identity))
  (loop [[y line] :in (yt (pairs grid))
         [x item] :in (xt (pairs line))
         :when (= :O item)]
    (var p [x y])
    (var dst nil)
    (loop [oitem :iterate (do (set p (p+ p dir)) (get-in grid (reverse p)))]
      (if (= :. oitem) (set dst p) (break)))
    (if dst
      (do
        (put-in grid (reverse dst) :O)
        (put-in grid [y x] :.))))
  grid)

(judge/test (tilt (load test-input) E)
  @[@[:. :. :. :. :O :$ :. :. :. :.]
    @[:. :O :O :O :$ :. :. :. :. :$]
    @[:. :. :. :. :. :$ :$ :. :. :.]
    @[:. :O :O :$ :. :. :. :. :O :O]
    @[:. :. :. :. :. :. :O :O :$ :.]
    @[:. :O :$ :. :. :. :O :$ :. :$]
    @[:. :. :. :. :O :$ :. :. :O :O]
    @[:. :. :. :. :. :. :. :. :. :O]
    @[:$ :. :. :. :. :$ :$ :$ :. :.]
    @[:$ :. :. :O :O :$ :. :. :. :.]])

(defn calc-load [key len]
  (var result 0)
  (loop [[_ y] :in key
         :let [load (- len y)]]
    (+= result load))
  result)

(defn key [grid]
  (var result @[])
  (loop [[y line] :pairs grid
         [x char] :pairs line :when (= :O char)]
    (array/push result [x y]))
  (tuple ;result))

(defn part1 [str]
  (def grid (-> str (load) (tilt N)))
  (calc-load (key grid) (length grid)))

(judge/test (part1 test-input) 136)
(judge/test (part1 real-input) 108889)

(defn spin [grid]
  (each dir dirs (tilt grid dir))
  grid)

(defn grid/print [grid]
  (each line grid (print (string ;line))))

(judge/test-stdout (grid/print (load test-input)) `
  O....$....
  O.OO$....$
  .....$$...
  OO.$O....O
  .O.....O$.
  O.$..O.$.$
  ..O..$O..O
  .......O..
  $....$$$..
  $OO..$....
`)

(defn part2 [str]
  (def grid (load str))
  (def seen @{})

  (var cnt 0)
  (var loop-start 0)

  (loop [k :iterate (key (spin grid))]
    (when-let [start (seen k)]
      (set loop-start start)
      (break))
    (put seen k cnt)
    (++ cnt))

  (def loop-len (- cnt loop-start))
  (def target-cnt (-> 1000000000 (- 1) (- loop-start) (% loop-len) (+ loop-start)))
  (calc-load
    (find-index (partial = target-cnt) seen)
    (length grid)))

(judge/test (part2 test-input) 64)

# 13s 
# (judge/test (part2 real-input) 104671)
