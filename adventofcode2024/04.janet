(import judge)

(use ./util)

(def real-input (slurp "./input/04.txt"))

(def test-input `
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX`)

(defn lookup-word [grid p dir]
  (var cursor p)
  (tuple/slice
    (seq [:repeat 3]
      (set cursor (p2+ cursor dir))
      (grid/get grid cursor))))

(judge/test (lookup-word (grid/parse test-input) [0 0] dir-SE) ["S" "X" "M"])
(judge/test (lookup-word (grid/parse test-input) [0 0] dir-W) [nil nil nil])

(defn num-found [grid p]
  (sum-loop [dir :in dirs8
             :when (= ["M" "A" "S"] (lookup-word grid p dir))]
            1))

(defn part1 [str]
  (def grid (grid/parse str))
  (sum-loop [[p v] :in (grid/pairs grid)
             :when (= v "X")]
            (num-found grid p)))

(judge/test (part1 test-input) 18)
(judge/test (part1 real-input) 2593)

(defn is-ms? [grid p dir1 dir2]
  (def [v1 v2] (map |(grid/get grid (p2+ p $)) [dir1 dir2]))
  (or
    (and (= v1 "M") (= v2 "S"))
    (and (= v1 "S") (= v2 "M"))))

(defn is-mas? [grid p]
  (and
    (is-ms? grid p dir-NE dir-SW)
    (is-ms? grid p dir-SE dir-NW)))

(judge/test (is-mas? (grid/parse test-input) [2 1]) true)

(defn part2 [str]
  (def grid (grid/parse str))
  (sum-loop [[p v] :in (grid/pairs grid)
             :when (and (= v "A")
                        (is-mas? grid p))]
            1))

(judge/test (part2 test-input) 9)
(judge/test (part2 real-input) 1950)
