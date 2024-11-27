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

(defn grid/find [grid f]
  (first (find (fn [[_ v]] (f v)) (grid/pairs grid))))

(defn wrap [limit coord]
  (let [rem (% coord limit)]
    (if (neg? rem) (+ rem limit) rem)))

(judge/test (wrap 3 0) 0)
(judge/test (wrap 3 -1) 2)

(defn wrap-get [grid [x y]]
  (def wrapped-x (wrap (grid/width grid) x))
  (def wrapped-y (wrap (grid/height grid) y))
  (grid/get grid [wrapped-x wrapped-y]))

(defn load [str]
  (def grid (grid/parse str))
  (def start (grid/find grid |(= $ "S")))
  [start (grid/map grid |(if (= $ "S") "." $))])

(defn advance [grid options]
  (tabseq [option :in (keys options)
           dir :in dirs4
           :let [neighbor (p2+ option dir)]
           :when (= (wrap-get grid neighbor) ".")]
    neighbor true))

(judge/test (advance (grid/parse test-input) {[5 5] true}) @{[4 5] true [5 4] true})

# Part 2 solution from
# https://work.njae.me.uk/2023/12/29/advent-of-code-2023-day-21/
# and https://github.com/ianthehenry/aoc-2023/blob/master/21.janet
(defn print-num-options-per-sector [options [cols rows]]
  (loop [R :range-to [-4 4] :after (print)
         C :range-to [-4 4]]
    (prinf "%6d"
           (sum-loop [r :range [0 rows]
                      c :range [0 cols]
                      :let [cell [(+ c (* C cols)) (+ r (* R rows))]]
                      :when (options cell)]
                     1))))

(defn find-options [[start grid] steps]
  (var options {start true})
  (for i 0 steps
    (set options (advance grid options)))
  options)

(defn part1 [str steps]
  (length (find-options (load str) steps)))

(judge/test (part1 test-input 6) 16)
(judge/test (part1 real-input 64) 3841)

(defn part2 [str steps]
  (def [start grid] (load str))
  (def options (find-options [start grid] steps))
  (print-num-options-per-sector options (grid :dims)))

(judge/test-stdout (part2 test-input 33) `
       0     0     0     0     0     0     0     0     0
       0     0     0     0     2     0     0     0     0
       0     0     0    18    34    24     0     0     0
       0     0    18    39    42    39    24     0     0
       0     2    32    42    39    42    31     1     0
       0     0    21    39    42    37    12     0     0
       0     0     0    21    30    12     0     0     0
       0     0     0     0     1     0     0     0     0
       0     0     0     0     0     0     0     0     0
`)

# This takes 90s to run.
#(judge/test-stdout (solve real-input (+ 66 (* 2 131) -1)) `
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0   982  5839   984     0     0     0
#       0     0   982  6796  7806  6787   984     0     0
#       0     0  5849  7806  7744  7806  5842     0     0
#       0     0  1000  6797  7806  6799   999     0     0
#       0     0     0  1000  5852   999     0     0     0
#       0     0     0     0     0     0     0     0     0
#       0     0     0     0     0     0     0     0     0
#`
#  97459)

(def num-sectors 202300)
(def part2
  (+
   (* num-sectors (+ 982 984 999 1000))
   (* (dec num-sectors) (+ 6796 6787 6799 6797))
   (+ 5849 5839 5842 5852)
   (* 7806 (* num-sectors num-sectors))
   (* 7744 (* (dec num-sectors) (dec num-sectors)))))

(judge/test part2 636391426712747)
