(import judge)

(def example1 `
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
`)

(def N [0 -1])
(def S [0 1])
(def E [1 0])
(def W [-1 0])
(def dirs [N S E W])

# Map pipe types to a map of input direction to output direction.
(def pipes
  {(keyword "|") {N N S S} :- {E E W W}
   :L {S E W N} :J {S W E N}
   :7 {N W E S} :F {N E W S}
   :. {} :S {}})

(defn make-grid [input]
  "Convert an input string into an array of pipe types"
  (defn make-entry [byte]
    (->> byte (string/from-bytes) (keyword) (table :type)))
  (->> input
      (string/trim)
       (string/split "\n")
       (map |(map make-entry $))))

(def example1-grid (make-grid example1))

(defn get-type [grid [x y]] (get-in grid [y x :type]))
(defn set-type [grid [x y] t] (put-in grid [y x :type] t))
(defn get-pipe [grid p] (pipes (get-type grid p)))
(defn set-fill [grid [x y]] (put-in grid [y x :fill] true))
(defn get-fill [grid [x y]] (get-in grid [y x :fill]))

(defn finds [grid]
  "Find the start keyword in the result of make-grid."
  (label r (loop [y :range [0 (length grid)]
                  :let [line (get grid y)]
                  x :range [0 (length line)]
                  :let [p [x y]]]
             (if (= :S (get-type grid p))
               (return r p)))
         (return r nil)))

(judge/test (finds example1-grid) [1 1])

(defn p+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn move [grid p dir]
  "Move in direction dir from point p. Result is the new point and new direction."
  (let [p2 (p+ p dir)
        pipe (get-pipe grid p2)
        out (pipe dir)]
    (if (nil? out) nil [p2 out])))

(judge/test (move example1-grid [1 1] [0 1]) [[1 2] [0 1]])
(judge/test (move example1-grid [1 2] [0 1]) [[1 3] [1 0]])

(defn openings [grid p]
  "Find all the openings around point p."
  (filter |(move grid p $) dirs))

(judge/test (openings example1-grid (finds example1-grid)) @[[0 1] [1 0]])

(defn farthest [grid s]
  "Find the farthest you can get from s by following the pipes.
  Record which cells are part of the loop."
  (set-fill grid s)
  (def [d1 d2] (openings grid s))
  (var p1 [s d1])
  (var p2 [s d2])
  (var steps 0)
  (forever
    (++ steps)
    (set p1 (move grid ;p1))
    (set p2 (move grid ;p2))
    (let [p1 (get p1 0)
          p2 (get p2 0)]
      (set-fill grid p1)
      (set-fill grid p2)
      #(printf " moved to %n and %n\n" p1 p2)
      (if (= p1 p2) (break))))
  steps)

(farthest example1-grid (finds example1-grid))
(judge/test (farthest example1-grid (finds example1-grid)) 4)

(defn part1 [file]
  (let [grid (make-grid (slurp file))
        start (finds grid)]
    (farthest grid start)))

(judge/test (part1 "./day10/input.txt") 6867)

(defn detect-pipe-type [grid start]
  "Detect which type of pipe is at position start"
  (def [d1 d2] (openings grid start))
  (each [t pipe] (pairs pipes)
    (if (and (index-of d1 pipe) (index-of d2 pipe))
      (set-type grid start t))))

(defn pipe-to-str [input]
  "1. Find the start
   2. Set the start to correct pipe type.
   3. Map the pipe
   4. Clear all cells not on the pipe.
   5. Convert back to a string."
  (let [grid (make-grid input)
        start (finds grid)]
    (detect-pipe-type grid start)
    (farthest grid start)
    (var lines @[])
    (loop [y :range [0 (length grid)]
           :let [len (length (get grid y))]
           :before (var line (buffer/new len))
           :after (array/push lines line)
           x :range [0 len]
           :let [p [x y]]]
      (buffer/push line (if (get-fill grid p) (get-type grid p) :.)))
    (string/join lines "\n")))

(judge/test (pipe-to-str example1) ".....\n.F-7.\n.|.|.\n.L-J.\n.....")

# A PEG to count how many "." characters are inside the loop.
(def inside-peg
  (peg/compile
    ~{:edge (+ (* "F" (any "-") "7") (* "L" (any "-") "J"))
      :space (any (+ (/ '(some ".") ,length) :edge))
      :cross (+ "|" (* "F" (any "-") "J") (* "L" (any "-") "7"))
      :inside (* :cross :space :cross)
      :line (* (some (* (drop :space) (? :inside))) (+ "\n" -1))
      :main (/ (group (some :line)) ,sum)}))

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
  (as-> input _ (pipe-to-str _) (peg/match inside-peg _) (get _ 0)))

(judge/test (part2 example1) 1)
(judge/test (part2 (slurp "./day10/input.txt")) 595)
