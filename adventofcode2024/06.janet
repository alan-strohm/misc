(import judge)
(use ./util)

(def test-input `
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...`)

(def real-input (slurp "./input/06.txt"))

(defn read [str]
  (def grid (grid/parse str))
  (var pos nil)
  (loop [[p v] :in (grid/pairs grid)
         :when (= v "^")]
    (set pos p)
    (grid/set grid p "."))
  [grid pos dir-N])

(def next-dir {dir-N dir-E
               dir-E dir-S
               dir-S dir-W
               dir-W dir-N})

(def format-dir {dir-N "^" dir-E ">" dir-W "<" dir-S "v"})

# Move forward one step if possible.
# Turn if the next step would be a #.
# Return :exit if the next position is off the grid.
# Return :loop if the next position has already been visited.
# Otherwise return :cont.
(defn walker/advance [self]
  (def {:grid grid :pos pos :dir dir :visited visited} self)
  (put visited [pos dir] true)
  (def next-pos (p2+ pos dir))
  (def next-val (grid/get grid next-pos))
  (cond
    (nil? next-val)
    (put self :state :exit)
    (in visited [next-pos dir])
    (put self :state :loop)
    (= "#" next-val)
    (do
      (merge-into self {:dir (next-dir dir)
                        :state :cont})
      (grid/set (self :vis) pos (format-dir (self :dir))))
    (do
      (merge-into self {:pos next-pos
                        :state :cont})
      (grid/set (self :vis) next-pos (format-dir dir))))
  (in self :state))

(defn walker/fork [self]
  (def forked (table/clone self))
  (put forked :visited (table/clone (self :visited))))

(defn walker/num-visited [{:visited visited}]
  (->> visited keys (map first) distinct length))

(defn walker/walk [self]
  (while (= :cont (self :state))
    (:advance self))
  (self :state))

(defn walker [grid start-pos start-dir]
  (def vis (grid/map grid identity))
  (grid/set vis start-pos (format-dir start-dir))

  @{:grid grid
    :pos start-pos :dir start-dir :state :cont
    :visited @{} :vis vis
    :advance walker/advance :fork walker/fork :num-visited walker/num-visited
    :walk walker/walk})

(judge/test (do (def w (walker ;(read test-input)))
              (repeat 3 (:advance w))
              (:num-visited w))
  3)

(defn part1 [str]
  (def w (walker ;(read str)))
  (while (= :cont (:advance w)))
  (:num-visited w))

(judge/test (part1 test-input) 41)
(judge/test (part1 real-input) 5329)

(judge/test (part1 `
###
#.#
#^#`) 2)

(defn creates-loop? [grid pos walker]
  (when (not= "." (grid/get grid pos))
    (break false))
  (grid/set grid pos "#")
  (def result (:walk walker))
  (grid/set grid pos ".")
  (= :loop result))

# Follow the protocol but before each step attempt to place an obstacle in
# front of you and see if it results in a loop.
# This should work faster but it doesn't give the right result yet.  I think my
# fork implementation might be broken.
(defn part2 [str]
  (def [grid start-pos start-dir] (read str))
  (def w (walker grid start-pos start-dir))
  (var loops @{})
  (while (= :cont (w :state))
    (def {:pos pos :dir dir} w)
    (def next-pos (p2+ pos dir))
    (def next-value (grid/get grid next-pos))
    (when (and (not= next-pos start-pos)
               (creates-loop? grid next-pos (:fork w)))
      (put loops next-pos true))
    (:advance w))
  (length loops))

(judge/test (part2 test-input) 6)
# 9s
# (judge/test (part2 real-input) 2162)

# For each position in the grid, attempt to put an obstacle there and see if it
# causes a loop.
(defn part2 [str]
  (def [grid start-pos start-dir] (read str))

  (sum-loop [[p _] :in (grid/pairs grid)
             :let [w (walker grid start-pos start-dir)]
             :when (and (not= p start-pos)
                        (creates-loop? grid p w))]
            1))

(judge/test (part2 test-input) 6)
# Takes about 3 minutes
# (judge/test (part2 real-input) 2162)

