(import judge)
(use ../util)

(def test-input `
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
`)

(def real-input (slurp "./day10/input.txt"))

(def [N S E W] [[0 -1] [0 1] [1 0] [-1 0]])
(def dirs [N S E W])

(def pipes
  "Map pipe types to a map of input direction to output direction."
  {"|" {N N S S} "-" {E E W W}
   "L" {S E W N} "J" {S W E N}
   "7" {N W E S} "F" {N E W S}
   "." {} "S" {}})

(def test-grid (grid/parse test-input))

(defn find-start
  "Find the start keyword in the result of make-grid."
  [grid]
  (some (fn [[p type]] (if (= "S" type) p))
        (grid/pairs grid)))

(judge/test (find-start test-grid) [1 1])

(defn p+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn move
  "Move in direction dir from point p. Result is the new point and new direction."
  [grid p dir]
  (if-let [p2 (p+ p dir)
           type (grid/get grid p2)
           new-dir ((pipes type) dir)]
    [p2 new-dir]))

(judge/test (move test-grid [1 1] [0 1]) [[1 2] [0 1]])
(judge/test (move test-grid [1 2] [0 1]) [[1 3] [1 0]])

(defn openings
  "Find all the openings around point p."
  [grid p] (filter |(move grid p $) dirs))

(judge/test (openings test-grid (find-start test-grid)) @[[0 1] [1 0]])

(defn map-pipe [grid s]
  # Detect which type of pipe is at position s
  (def [d1 d2] (openings grid s))
  (def type (find-index |(and (index-of d1 $) (index-of d2 $)) pipes))
  (grid/set grid s type)

  (def visited @{})
  (var state [s d1])
  (forever
    (def [p dir] state)
    (when (visited p) (break))
    (put visited p true)
    (set state (move grid ;state)))
  visited)

(defn part1 [str]
  (let [grid (grid/parse str)
        start (find-start grid)
        pipe-len (length (map-pipe grid start))]
    (/ pipe-len 2)))

(judge/test (part1 test-input) 4)
(judge/test (part1 real-input) 6867)

(defn pipe-to-str [input]
  (def grid (grid/parse input))
  (def in-pipe? (map-pipe grid (find-start grid)))

  (loop [[p _] :in (grid/pairs grid)
         :unless (in-pipe? p)]
    (grid/set grid p "."))
  (grid/format grid))

(judge/test (pipe-to-str test-input) ".....\n.F-7.\n.|.|.\n.L-J.\n.....")

(def inside-peg
  "A PEG to count how many \".\" characters are inside the loop."
  (peg/compile
    ~{:main (/ (group (some :line)) ,sum)
      :line (* (some (* (drop :space) (? :inside))) (+ "\n" -1))
      :space (any (+ (/ '(some ".") ,length) :edge))
      # Edges are pieces of pipe that don't change whether we're inside or outside the loop.
      :edge (+ (* "F" (any "-") "7") (* "L" (any "-") "J"))
      :inside (* :cross :space :cross)
      # Crosses are pieces of pipe that toggle whether we are inside or outside the loop.
      :cross (+ "|" (* "F" (any "-") "J") (* "L" (any "-") "7"))
      }))

(judge/test (peg/match inside-peg "...") @[0])
(judge/test (peg/match inside-peg ".F-7.") @[0])
(judge/test (peg/match inside-peg ".|.|.") @[1])
(judge/test (peg/match inside-peg ".|..|.") @[2])
(judge/test (peg/match inside-peg ".L-J.") @[0])
(judge/test (peg/match inside-peg ".|-|.") nil)
(judge/test (peg/match inside-peg ".J.") nil)
(judge/test (peg/match inside-peg ".||||.") @[0])
(judge/test (peg/match inside-peg ".|||.") nil)
(judge/test (peg/match inside-peg ".|F7.|.") @[1])
(judge/test (peg/match inside-peg ".|FJ.") @[0])
(judge/test (peg/match inside-peg ".|.FJ.") @[1])
(judge/test (peg/match inside-peg ".|.L-7.") @[1])
(judge/test (peg/match inside-peg ".FJ.L-7.") @[1])
(judge/test (peg/match inside-peg ".|.||.|.") @[2])

(defn part2 [input]
  (->> input (pipe-to-str) (peg/match inside-peg) (first)))

(judge/test (part2 test-input) 1)
(judge/test (part2 real-input) 595)
