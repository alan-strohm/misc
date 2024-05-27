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

(def slope-to-dir {(chr "^") dir-N
                   (chr ">") dir-E
                   (chr "v") dir-S
                   (chr "<") dir-W})

(def dir-to-slope (invert slope-to-dir))

(defn advance [grid start-pos start-dir]
  (var [pos dir dist] [start-pos start-dir 0])
  (var open-dirs nil)
  (forever
    (def backwards (vec*n dir -1))

    (vec+= pos dir)
    (++ dist)

    (defn open? [v dir]
      (def backwards (vec*n dir -1))
      (def open-slopes
        (->> dirs4 (filter |(not= backwards $)) (map dir-to-slope)))
      (def open-set (string/from-bytes ;open-slopes (chr ".")))
      (string/check-set open-set (string/from-bytes v)))

    (defn open2? [v dir]
      (string/check-set "<>^v." (string/from-bytes v)))

    (set open-dirs
         (seq [dir :in dirs4
               :let [p (vec+ pos dir)
                     c (grid/get grid p)]
               :when (and
                       (not (nil? c))
                       (not= backwards dir)
                       (open2? c dir))]
           dir))

    (if (= 1 (length open-dirs))
      (set dir (first open-dirs))
      (break)))

  [pos open-dirs dist])

(judge/test (advance test-grid [1 0] dir-S) [[3 5] @[[1 0] [0 1]] 15])
(judge/test (advance test-grid [19 19] dir-S) [[21 22] @[] 5])
(judge/test (advance test-grid [11 3] dir-S) [[13 13] @[[1 0] [0 1] [-1 0]] 24])

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
  #(map |[$ (hike/length $)] hikes))
  (max-of (map hike/length hikes)))

(defn part1 [str]
  (def grid (grid/parse str))
  (def [start-pos start-dir] [[1 0] [0 1]])
  (def dest
    (let [[h w] (grid :size)]
      [(- w 2) (- h 1)]))
  (longest-hike grid start-pos start-dir dest))

# Part1
# (judge/test (part1 test-input) 94)
# Part2
(judge/test (part1 test-input) 154)

# Runs in 2.5s
# (judge/test (part1 real-input) 2086)
