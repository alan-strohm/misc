(import judge)
(use ../util)

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

(def [rock space] ["O" "."])

(judge/test-stdout (-> test-input grid/parse grid/format prin) `
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

(def dir-order [dir-N dir-W dir-S dir-E])

(defn tilt [grid dir]
  (def yt (if (= dir dir-S) reverse identity))
  (def xt (if (= dir dir-E) reverse identity))
  (loop [[y line] :in (yt (pairs (grid :content)))
         [x item] :in (xt (pairs line))
         :when (= rock item)]
    (var p [x y])
    (loop [next-p :iterate (p2+ p dir)
           :while (= space (grid/get grid next-p))]
      (set p next-p))
    (grid/set grid [x y] space)
    (grid/set grid p rock))
  grid)

(judge/test-stdout (-> (tilt (grid/parse test-input) dir-E) grid/format prin) `
  ....O#....
  .OOO#....#
  .....##...
  .OO#....OO
  ......OO#.
  .O#...O#.#
  ....O#..OO
  .........O
  #....###..
  #..OO#....
`)

(defn calc-load [grid]
  (def [_ height] (grid :dims))
  (sum-loop [[[_ y] v] :in (grid/pairs grid)
             :when (= rock v)]
            (- height y)))

(defn part1 [str]
  (def grid (-> str grid/parse (tilt dir-N)))
  (calc-load grid))

(judge/test (part1 test-input) 136)
(judge/test (part1 real-input) 108889)

(defn spin [grid]
  (each dir dir-order (tilt grid dir))
  grid)

(defn part2 [str]
  (def grid (grid/parse str))
  (def seen @{})

  (var cnt 0)
  (var loop-start 0)

  (loop [k :iterate (grid/format (spin grid))]
    (when-let [start (seen k)]
      (set loop-start start)
      (break))
    (put seen k cnt)
    (++ cnt))

  (def loop-len (- cnt loop-start))
  (def target-cnt (-> 1000000000 dec (- loop-start) (% loop-len) (+ loop-start)))
  (def target-grid (grid/parse (find-index |(= $ target-cnt) seen)))
  (calc-load target-grid))

(judge/test (part2 test-input) 64)

# 5s
# (judge/test (part2 real-input) 104671)
