(import judge)

(use ./util)

(def test-input `
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#`)

(def real-input (slurp "./input/23.txt"))

(def test-grid (grid/parse test-input))

(def slope-to-dir {"^" dir-N ">" dir-E "v" dir-S "<" dir-W})

(def dir-to-slope (invert slope-to-dir))

(defn longest-path [start goal next-options]
  (var best 0)
  (def visited @{})

  (defn visit [pos steps]
    (when (= pos goal)
      (set best (max best steps))
      (break))

    (put visited pos true)
    (loop [[p d] :in (next-options pos)
           :unless (visited p)]
      (visit p (+ d steps)))
    (put visited pos nil))
  (visit start 0)
  best)

(varfn dir-options [value]
  (case value
    nil []
    "#" []
    "." dirs4
    "^" [dir-N]
    ">" [dir-E]
    "v" [dir-S]
    "<" [dir-W]
    (errorf "unknown value: %n" value)))

(defn open-options [grid p]
  (seq [dir :in (dir-options (grid/get grid p))
        :let [next-p (vec+ p dir)]
        :when (not (empty? (dir-options (grid/get grid next-p))))]
    next-p))

(defn load [str]
  (def grid (grid/parse str))
  [grid [1 0] [(- (grid/width grid) 2)
               (- (grid/height grid) 1)]])

(defn map-grid [grid start goal]
  (def result @{})
  (defn update-result [from to dist]
    (update-in result [from to] |(max (or $ 0) dist)))

  (defn map-segment [start cursor]
    #(printf "start: %n cursor: %n" start cursor)
    (def visited @{start true})
    (var cursor cursor)
    (var segment-length 0)

    (forever
      (++ segment-length)
      (def next-options (filter |(not (visited $))
                                (open-options grid cursor)))
      #(printf "cursor: %n next-options: %n" cursor next-options)
      (when (> (length next-options) 1)
        (update-result start cursor segment-length)
        (loop [option :in next-options]
          (map-segment cursor option))
        (break))

      (when (empty? next-options)
        (if (= cursor goal)
          (update-result start cursor segment-length))
        (break))

      (put visited cursor true)
      (set cursor (first next-options))
      (when (result cursor)
        (update-result start cursor (inc segment-length))
        (break))))
  (map-segment start start)
  result)

(judge/test (map-grid ;(load `
#.#
#.#`))
  @{[1 0] @{[1 1] 2}})

(judge/test (map-grid ;(load `
#.###
#...#
#.#.#
#...#
###.#`))
  @{[1 0] @{[1 1] 2}
    [1 1] @{[3 3] 4}
    [3 3] @{[1 1] 4 [3 4] 1}})

(judge/test (map-grid ;(load test-input))
  @{[1 0] @{[3 5] 16}
    [3 5] @{[5 13] 22 [11 3] 22}
    [5 13] @{[13 13] 12 [13 19] 38}
    [11 3] @{[13 13] 24 [21 11] 30}
    [13 13] @{[13 19] 10 [21 11] 18}
    [13 19] @{[19 19] 10}
    [19 19] @{[21 22] 5}
    [21 11] @{[19 19] 10}})

(defn solve [str]
  (def [grid start goal] (load str))
  (def options (map-grid grid start goal))

  (defn next-options [p] (pairs (options p)))

  (dec # Don't count the start tile.
    (longest-path start goal next-options)))

(judge/test (solve test-input) 94)
(judge/test (solve real-input) 2086)

(varfn dir-options [value]
  (case value
    nil []
    "#" []
    dirs4))

(judge/test (map-grid ;(load test-input))
  @{[1 0] @{[3 5] 16}
    [3 5] @{[5 13] 22 [11 3] 22}
    [5 13] @{[3 5] 22 [13 19] 38}
    [11 3] @{[13 13] 24 [21 11] 30}
    [13 13] @{[5 13] 12 [11 3] 24 [21 11] 18}
    [13 19] @{[5 13] 38 [13 13] 10}
    [19 19] @{[13 19] 10 [21 22] 5}
    [21 11] @{[13 13] 18 [19 19] 10}})

(judge/test (solve test-input) 154)
(judge/test (solve real-input) 5078)  # Still wrong!
