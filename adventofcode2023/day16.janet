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

(defn traverse [tile in-dir]
  (match [tile in-dir]
    ["." _] [in-dir]
    ["-" [_ 0]] [in-dir]
    ["-" [0 _]] [[1 0] [-1 0]]
    ["|" [0 _]] [in-dir]
    ["|" [_ 0]] [[0 1] [0 -1]]
    ["/" [x y]] [[(- y) (- x)]]
    [`\` [x y]] [[y x]]
    (errorf "unknown tile %n and direction %n" tile in-dir)))

(defn next-beams [grid [pos in-dir]]
  (def tile (grid/get grid pos))
  (def out-dirs (traverse tile in-dir))
  (map tuple (map |(vec+ pos $) out-dirs) out-dirs))

(judge/test (next-beams (grid/parse test-input) [[0 1] [1 0]]) @[[[0 2] [0 1]] [[0 0] [0 -1]]])

(defn num-energized [grid start-pos start-dir]
  (def q @[[start-pos start-dir]])

  (def beams @{})
  (loop [in-beam :iterate (array/pop q)
         :let [[pos _] in-beam]
         :unless (or
                   (not (grid/contains grid pos))
                   (get-in beams in-beam))
         :before (put-in beams in-beam true)
         next-beam :in (next-beams grid in-beam)]
    (array/push q next-beam))
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

# 10s
#(judge/test (part2 real-input) 7521)
