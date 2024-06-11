(import judge)
(use ./util)

(def test-input `
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
`)

(def real-input (slurp "./input/16.txt"))

(defn traverse [pos tile in-dir]
  (defn out [dir] [(p2+ pos dir) dir])

  (match [tile in-dir]
    ["." _] [(out in-dir)]
    ["-" [_ 0]] [(out in-dir)]
    ["-" [0 _]] [(out [1 0]) (out [-1 0])]
    ["|" [0 _]] [(out in-dir)]
    ["|" [_ 0]] [(out [0 1]) (out [0 -1])]
    ["/" [x y]] [(out [(- y) (- x)])]
    [`\` [x y]] [(out [y x])]
    (errorf "unknown tile %n and direction %n" tile in-dir)))

(defn next-beams [grid [pos in-dir]]
  (def tile (grid/get grid pos))
  (traverse pos tile in-dir))

(judge/test (next-beams (grid/parse test-input) [[0 1] [1 0]]) [[[0 2] [0 1]] [[0 0] [0 -1]]])

(defn num-energized [grid start-pos start-dir]
  (def q @[[start-pos start-dir]])

  (def beams @{})
  (loop [in-beam :iterate (array/pop q)
         :let [[pos _] in-beam
               k in-beam]
         :unless (or
                   (not (grid/contains grid pos))
                   (get-in beams k))
         :before (put-in beams k true)]
    (array/push q ;(next-beams grid in-beam)))
  (length beams))

(defn part1 [str]
  (num-energized (grid/parse str) [0 0] [1 0]))

(judge/test (part1 test-input) 46)
(judge/test (part1 real-input) 7236)

(defn part2 [str]
  (def grid (grid/parse str))
  (def [h w] (grid :dims))
  (def inits (array/concat
               (seq [y :range [0 h]] [[0 y] [1 0]])
               (seq [y :range [0 h]] [[(dec w) y] [-1 0]])
               (seq [x :range [0 w]] [[x 0] [0 1]])
               (seq [x :range [0 w]] [[x (dec h)] [0 -1]])))

  (max-of (map |(num-energized grid ;$) inits)))

(judge/test (part2 test-input) 51)

# 4s on macbook
# (judge/test (part2 real-input) 7521)
