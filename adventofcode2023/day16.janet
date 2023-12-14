(import judge)

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
    ([c [_ _]] (= c (chr "."))) [in-dir]
    ([c [_ 0]] (= c (chr "-"))) [in-dir]
    ([c [0 _]] (= c (chr "-"))) [[1 0] [-1 0]]
    ([c [0 _]] (= c (chr "|"))) [in-dir]
    ([c [_ 0]] (= c (chr "|"))) [[0 1] [0 -1]]
    ([c [x y]] (= c (chr "/"))) [[(- y) (- x)]]
    ([c [x y]] (= c (chr `\`))) [[y x]]
    (errorf "unknown tile %c and direction %n" tile in-dir)))

(defn vec+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn board/parse [str]
  (->> str (string/trim) (string/split "\n")))

(defn board/get [board [c l]]
  (get-in board [l c]))

(defn board/oob? [board [c l]]
  (def [h w] [(length board) (length (first board))])
  (or
    (>= c w)
    (>= l h)
    (< l 0)
    (< c 0)))

(judge/test (board/oob? (board/parse test-input) [0 0]) false)

(defn next-beams [board pos in-dir]
  (def tile (board/get board pos))
  (def out-dirs (traverse tile in-dir))
  (map tuple (map (partial vec+ pos) out-dirs) out-dirs))

(judge/test (next-beams (board/parse test-input) [0 1] [1 0]) @[[[0 2] [0 1]] [[0 0] [0 -1]]])

(defn num-energized [board start-pos start-dir]
  (def q @[[start-pos start-dir]])

  (def beams @{})
  (while (not (empty? q))
    (def [pos in-dir] (array/pop q))
    #(printf "visiting %n going in direction %n" pos in-dir)
    (unless (or
              (board/oob? board pos)
              (get-in beams [pos in-dir]))
      (put-in beams [pos in-dir] true)
      (each beam (next-beams board pos in-dir)
        (array/push q beam))))
  (length beams))

(defn part1 [str]
  (num-energized (board/parse str) [0 0] [1 0]))

(judge/test (part1 test-input) 46)
(judge/test (part1 real-input) 7236)

(defn part2 [str]
  (def board (board/parse str))
  (def [h w] [(length board) (length (first board))])
  (def inits (array
    ;(map |(tuple [0 $] [1 0]) (range h))
    ;(map |(tuple [(dec w) $] [-1 0]) (range h))
    ;(map |(tuple [$ 0] [0 1]) (range w))
    ;(map |(tuple [$ (dec h)] [0 -1]) (range w))))

  (max-of (map |(num-energized board ;$) inits)))

(judge/test (part2 test-input) 51)

# 10s
# (judge/test (part2 real-input) 7521)
