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

    (set open-dirs
         (seq [dir :in dirs4
               :let [p (vec+ pos dir)
                     c (grid/get grid p)]
               :when (and
                       (not (nil? c))
                       (not= backwards dir)
                       (open? c dir))]
           dir))

    (if (= 1 (length open-dirs))
      (set dir (first open-dirs))
      (break)))

  [pos open-dirs dist])

(judge/test (advance test-grid [1 0] dir-S) [[3 5] @[[1 0] [0 1]] 15])
(judge/test (advance test-grid [19 19] dir-S) [[21 22] @[] 5])
(judge/test (advance test-grid [11 3] dir-S) [[13 13] @[[1 0] [0 1]] 24])

(defn map-grid [grid pos dir]
  (def graph @{})

  (defn map-grid-rec [pos dir]
    (printf "visiting position %n" pos)
    (def [next-pos open-dirs dist] (advance grid pos dir))
    (put-in graph [pos next-pos] dist)
    (unless (in graph next-pos)
      (each dir open-dirs
        (map-grid-rec next-pos dir))))

  (map-grid-rec pos dir)
  graph)

(judge/trust
  (map-grid test-grid [1 0] [0 1])
  @{[1 0] @{[3 5] 15}
    [3 5] @{[5 13] 22 [11 3] 22}
    [5 13] @{[11 3] 44 [13 19] 38}
    [11 3] @{[13 13] 24 [21 11] 30}
    [13 13] @{[5 13] 12 [21 11] 18}
    [13 19] @{[13 13] 10 [19 19] 10}
    [19 19] @{[13 13] 20 [21 22] 5}
    [21 11] @{[13 13] 18 [19 19] 10}})

(defn max-or-nil [arr]
  (if (empty? arr) nil (max ;arr)))

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

(judge/test (part1 test-input) 94)

# Runs in 2.5s
# (judge/test (part1 real-input) 2086)
