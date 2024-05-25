(import judge)

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
  {(keyword "|") {N N S S} :- {E E W W}
   :L {S E W N} :J {S W E N}
   :7 {N W E S} :F {N E W S}
   :. {} :S {}})

(defn make-grid
  "Convert an input string into a 2D array of pipe types"
  [input]
  (defn make-entry [byte] (keyword (string/from-bytes byte)))
  (->> (string/trim input)
       (string/split "\n")
       (map |(map make-entry $))))

(def test-grid (make-grid test-input))

(defn get-type [grid [x y]] (get-in grid [y x]))
(defn set-type [grid [x y] t] (put-in grid [y x] t))

(defn find-start
  "Find the start keyword in the result of make-grid."
  [grid]
  (some (fn [[p type]] (if (= :S type) p))
        (generate [[y line] :pairs grid
                   [x type] :pairs line]
          [[x y] type])))

(judge/test (find-start test-grid) [1 1])

(defn p+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn move
  "Move in direction dir from point p. Result is the new point and new direction."
  [grid p dir]
  (if-let [p2 (p+ p dir)
           type (get-type grid p2)
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
  (set-type grid s type)

  (def visited @{})
  (var state [s d1])
  (forever
    (def [p dir] state)
    (when (visited p) (break))
    (put visited p true)
    (set state (move grid ;state)))
  visited)

(defn part1 [str]
  (let [grid (make-grid str)
        start (find-start grid)
        pipe-len (length (map-pipe grid start))]
    (/ pipe-len 2)))

(judge/test (part1 test-input) 4)
(judge/test (part1 real-input) 6867)

(defn pipe-to-str [input]
  "1. Find the start
   2. Set the start to correct pipe type.
   3. Map the pipe
   4. Clear all cells not on the pipe.
   5. Convert back to a string."
  (let [grid (make-grid input)
        start (find-start grid)]
    (def in-pipe? (map-pipe grid start))
    (var lines @[])
    (loop [[y grid-line] :pairs grid
           :before (var line (buffer/new (length grid-line)))
           :after (array/push lines line)
           [x type] :pairs grid-line
           :let [p [x y]]]
      (buffer/push line (if (in-pipe? p) type :.)))
    (string/join lines "\n")))

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
