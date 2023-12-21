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
  [start grid])

(def dirs [[0 -1] [1 0] [0 1] [-1 0]])

(defn grid/get [grid [c l]] (get-in grid [l c]))

(defn part1 [str steps]
  (def [start grid] (load str))
  (var options @{start true})
  (for i 0 steps 
    (def seeds options)
    (set options @{})
    (each seed (keys seeds)
      (each dir dirs
        (def neighbor (vec+ seed dir))
        (if (= (grid/get grid neighbor) (chr "."))
          (put options neighbor true)))))

  (length options)
  )

(judge/test (part1 test-input 6) 16)
(judge/test (part1 real-input 64) 3841)
