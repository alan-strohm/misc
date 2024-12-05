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

(defn read-cells [grid p dir num]
  (tuple/slice
    (seq [:before (var cursor p) :repeat num :after (p2+= cursor dir)]
      (grid/get grid cursor))))

(judge/test (read-cells (grid/parse test-input) [0 0] dir-SE 3) ["M" "S" "X"])
(judge/test (read-cells (grid/parse test-input) [0 0] dir-W 3) ["M" nil nil])

(defn is-mas? [grid p dir] (= ["M" "A" "S"] (read-cells grid p dir 3)))

(defn part1 [str]
  (def grid (grid/parse str))
  (sum-loop [[p v] :in (grid/pairs grid)
             :when (= v "X")
             dir :in dirs8
             :when (is-mas? grid (p2+ p dir) dir)]
            1))

(judge/test (part1 test-input) 18)
(judge/test (part1 real-input) 2593)

(defn is-x-mas? [grid p]
  (and
    (= "A" (grid/get grid p))
    (or
      (is-mas? grid (p2+ p dir-NE) dir-SW)
      (is-mas? grid (p2+ p dir-SW) dir-NE))
    (or
      (is-mas? grid (p2+ p dir-NW) dir-SE)
      (is-mas? grid (p2+ p dir-SE) dir-NW))))

(judge/test (is-x-mas? (grid/parse test-input) [2 1]) true)

(defn part2 [str]
  (def grid (grid/parse str))
  (sum-loop [[p v] :in (grid/pairs grid)
             :when (is-x-mas? grid p)]
            1))

(judge/test (part2 test-input) 9)
(judge/test (part2 real-input) 1950)
