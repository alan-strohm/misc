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

(def dir-order [dir-N dir-W dir-S dir-E])

(defn advance-rock
  "Advance a rock as far as it can go in dir from start in grid"
  [grid start dir]
  (var end start)
  (loop [next-p :iterate (p2+ end dir)
         :while (= space (grid/get grid next-p))]
    (set end next-p))
  (when (not= end start)
    (grid/set grid start space)
    (grid/set grid end rock))
  end)

(defn rot90ccw [[x y]] [y (* -1 x)])
(defn dot [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))
(defn udot [a b] (math/abs (dot a b)))

(defn make-tilter [grid]
  (var dir dir-N)
  (fn []
    (def yt (if (= dir dir-S) reverse identity))
    (def xt (if (= dir dir-E) reverse identity))
    (loop [[y line] :in (yt (pairs (grid :content)))
           [x item] :in (xt (pairs line))
           :when (= rock item)]
      (advance-rock grid [x y] dir))
    (set dir (rot90ccw dir))
    grid))

# This is much more complicated and only slightly faster.
(defn make-tilter [grid]
  (var [rows dir] [(array/new-filled (grid/height grid)) dir-N])

  (defn append-rock [rocks i p]
    (def arr (or (in rocks i)
                 (set (rocks i) @[])))
    (array/push arr p))

  (loop [[p v] :in (grid/pairs grid)
         :when (= v rock)]
    (append-rock rows (in p 0) p))

  (defn next-idx [a dir len]
    (let [signed (dot a dir)]
      (cond
       (zero? signed) (dec len)
       (neg? signed) (+ signed (dec len))
       signed)))

  (fn []
    (def len (udot (grid :dims) dir))
    (def next-rows (array/new-filled len))
    (loop [row :in rows
           :when row
           start :in row]
      (def end (advance-rock grid start dir))
      (append-rock next-rows (next-idx end dir len) end))
    (set dir (rot90ccw dir))
    (set rows next-rows)
    grid))

(judge/test-stdout (-> ((make-tilter (grid/parse test-input))) grid/format prin) `
  OOOO.#.O..
  OO..#....#
  OO..O##..O
  O..#.OO...
  ........#.
  ..#....#.#
  ..O..#.O.O
  ..O.......
  #....###..
  #....#....
`)

(defn calc-load [grid]
  (sum-loop [[[_ y] v] :in (grid/pairs grid)
             :when (= rock v)]
            (- (grid/height grid) y)))

(defn part1 [str] (calc-load ((-> str grid/parse make-tilter))))

(judge/test (part1 test-input) 136)
(judge/test (part1 real-input) 108889)

(defn spin-once [grid]
  (def tilt (make-tilter grid))
  (tilt) (tilt) (tilt) (tilt))

(judge/test-stdout (-> (spin-once (grid/parse test-input)) grid/format prin) `
  .....#....
  ....#...O#
  ...OO##...
  .OO#......
  .....OOO#.
  .O#...O#.#
  ....O#....
  ......OOOO
  #...O###..
  #..OO#....
`)

(defn part2 [str]
  (def grid (grid/parse str))
  (def seen @{})

  (var cnt 0)
  (var loop-start 0)

  (def tilt (make-tilter grid))
  (defn spin []
    (loop [:repeat 4] (tilt))
    (grid/format grid))

  (loop [k :iterate (spin)]
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
#(judge/test (part2 real-input) 104671)
