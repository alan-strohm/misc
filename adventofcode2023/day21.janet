(import judge)

(use ./util)

(def test-input `
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........`)

(def real-input (slurp "./input/21.txt"))

(defn pos [] ~(/ (group (* (column) (line))) ,|(tuple/slice (map dec $))))

(def start-peg
  (peg/compile ~(any (+ (* ,(pos) "S") 1))))

(def grid-peg
  (peg/compile ~{:line (some (+ '(set ".#") (/ "S" ".")))
                 :main ,(sep ~(/ (group :line) ,string/join) "\n")}))

(defn load [str]
  (def start (in (peg/match start-peg str) 0))
  (def grid (peg/match grid-peg str))
  (def rows (length grid))
  (def cols (length (first grid)))
  [start {:contents grid :size [rows cols]}])

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn wrap [limit coord]
  (let [rem (% coord limit)]
    (if (neg? rem) (+ rem limit) rem)))

(judge/test (wrap 3 -1) 2)

(defn grid/get [{:contents contents :size [h w]} [c l]]
  (get-in contents [(wrap h l) (wrap c w)]))

(defn print-options [grid options]
  (var count 0)
  (eachp [y line] (grid :contents)
    (eachp [x char] line
      (if (options [x y])
        (do (prin "O") (++ count))
        (prin (string/from-bytes char))))
    (print))
  (printf "count: %d\n" count))

(defn expand [grid seeds]
  (as-> (keys seeds) _
        (seq [seed :in _
              dir :in dirs
              :let [neighbor (vec+ seed dir)]
              :when (= (grid/get grid neighbor) (chr "."))]
          neighbor)
        (interleave _ (seq [x :in _] true))
        (struct ;_)))

(defn find-loop [start grid]
  (def seen @{})
  (var seeds {start true})
  (var steps 0)
  (forever
    (put seen seeds steps)
    (set seeds (expand grid seeds))
    (++ steps)
    (when (seen seeds)
      (break)))
  [(seen seeds) steps])

#(judge/test (find-loop ;(load test-input)) [13 15])
# Takes about 5s
#(judge/test (find-loop ;(load real-input)) [129 131])

(defn num-grids [[x y] grid max-steps]
  (def sw (+ x max-steps (- max-steps x)))
  (def sh (+ y max-steps (- max-steps y)))
  (def [gsh gsw] [(length grid) (length (first grid))])
  [(/ sw gsw) (/ sh gsh)]
  )

#(judge/test (num-grids ;(load real-input) 26501365) [404601 404601])

(defn run [str start steps]
  (def [_ grid] (load str))
  (var options @{start true})
  (for i 0 steps
    (def seeds options)
    (set options @{})
    (each seed (keys seeds)
      (each dir dirs
        (def neighbor (vec+ seed dir))
        (if (= (grid/get grid neighbor) (chr "."))
          (put options neighbor true)))))
  (print-options grid options)
)

(defn part1 [str steps]
  (def [start grid] (load str))
  (var seeds {start true})
  (for i 0 steps
    (set seeds (expand grid seeds)))
  (length seeds)
  )

#(judge/test (part1 test-input 6) 16)
#(judge/test (part1 real-input 64) 3841)

#(judge/test (run test-input [5 5] 13) nil)

