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

(defn is-open? [v dir]
  (def backwards (vec*n dir -1))
  (def open-slopes
    (->> dirs4 (filter |(not= backwards $)) (map dir-to-slope)))
  (def open-set (string ;open-slopes "."))
  (string/check-set open-set v))

(defn advance
  `Advance from start-pos in start-dir until as long as no choice must be made.
  Returns the new position, the set of choices and the distance traveled.`
  [grid start-pos start-dir]
  (var [pos dir dist] [start-pos start-dir 0])
  (var open-dirs nil)
  (forever
    (def backwards (vec*n dir -1))

    (vec+= pos dir)
    (++ dist)

    (set open-dirs
         (seq [dir :in dirs4
               :let [p (vec+ pos dir)
                     c (grid/get grid p)]
               :when (and
                       (not (nil? c))
                       (not= backwards dir)
                       (is-open? c dir))]
           dir))

    (if (= 1 (length open-dirs))
      (set dir (first open-dirs))
      (break)))

  [pos open-dirs dist])

(judge/test (advance test-grid [1 0] dir-S) [[3 5] @[[1 0] [0 1]] 15])
(judge/test (advance test-grid [19 19] dir-S) [[21 22] @[] 5])
# Part 2
# (judge/test (advance test-grid [11 3] dir-S) [[13 13] @[[1 0] [0 1] [-1 0]] 24])
(judge/test (advance test-grid [11 3] dir-S) [[13 13] @[[1 0] [0 1]] 24])

(defn longest-hike [grid start-pos start-dir dest]
  (def visited @{})
  (defn find-hikes-rec [pos dir]
    (put visited pos (length visited))
    (def [next-pos open-dirs dist] (advance grid pos dir))
    (def segment @{pos {:to next-pos :dist dist :dir dir}})
    (def result
      (cond
        (= next-pos dest) [segment]
        (in visited next-pos) nil
        (seq [next-dir :in open-dirs
              :let [hikes (find-hikes-rec next-pos next-dir)]
              :when (not (nil? hikes))
              hike :in hikes]
          (merge hike segment))))
    (put visited pos nil)
    result)

  (def hikes (find-hikes-rec start-pos start-dir))
  (defn hike/length [hike] (sum (map |($ :dist) hike)))
  (max-of (map hike/length hikes)))

(defn solve [str]
  (def grid (grid/parse str))
  (def [start-pos start-dir] [[1 0] dir-S])
  (def dest [(- (grid/width grid) 2)
             (- (grid/height grid) 1)])
  (longest-hike grid start-pos start-dir dest))

(defn part1 [str] (solve str))

# Part1
(judge/test (part1 test-input) 94)
# Part2

(defn clear-slopes [str]
  (first (peg/match ~(% (any (+ (/ (set "^>v<") ".") '1))) str)))

(judge/test (clear-slopes "ab<c") "ab.c")

(defn part2 [str] (-> str clear-slopes solve))

(judge/test (part2 test-input) 154)

# Takes too long...
# (judge/test (part2 real-input) 154)
