(import judge)

(def example `
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#`)

(defn mirror [len m x]
  (let [r (+ m (- m x 1))]
    (if (or (>= x m) (>= r len)) nil r)))

(judge/test (mirror 2 1 0) 1)
(judge/test (mirror 3 2 0) nil)
(judge/test (mirror 4 2 0) 3)

(defn rotate [lines]
  (var result @[])
  (loop [x :range [0 (length (get lines 0))]
         :before (var line @"")
         :after (array/push result (string line))
         y :range [0 (length lines)]
         :let [char (x (y lines))]]
    (buffer/push-byte line char))
  result)

(judge/test (rotate ["abc" "def"]) @["ad" "be" "cf"])
(judge/test (rotate ["ad" "be" "cf"]) @["abc" "def"])

(defn find-mirror-1d [lines]
  (def valid-mirrors (array/new-filled (length (0 lines)) true))
  (put valid-mirrors 0 false)
  (loop [line :in lines
         x :range [0 (length line)]
         :let [char (x line)]
         m :range [0 (length valid-mirrors)] :when (m valid-mirrors)
         :let [mirror-x (mirror (length line) m x)]]
    (if (and mirror-x (not= char (mirror-x line)))
      (put valid-mirrors m false)))
  (find-index identity valid-mirrors))

(defn deb [x] (pp x) x)

(defn load [str]
  (->> str (string/trim) (string/split "\n\n") (map |(string/split "\n" $))))

(defn part1 [str]
  (var total 0)
  (def inputs (load str))
  (loop [input :in inputs
         :let [vm (find-mirror-1d input)]]
    (if vm
      (+= total vm)
      (+= total (* 100 (find-mirror-1d (rotate input))))))
  total)

(judge/test (part1 example) 405)
(judge/test (part1 (slurp "./day13/input.txt")) 31265)

