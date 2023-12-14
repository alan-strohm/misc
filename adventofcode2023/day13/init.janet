(import judge)

(def example `
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

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

(defn mirror-errs-1d [lines]
  (def errs (array/new-filled (length (0 lines)) 0))
  (put errs 0 math/int-max)
  (loop [line :in lines
         x :range [0 (length line)]
         :let [char (x line)]
         m :range [0 (length errs)]
         :let [mirror-x (mirror (length line) m x)]]
    (if (and mirror-x (not= char (mirror-x line)))
      (update errs m inc)))
  errs)

(defn deb [x] (pp x) x)

(defn load [str]
  (->> str (string/trim) (string/split "\n\n") (map |(string/split "\n" $))))

(defn run [str num-errs]
  (var total 0)
  (def inputs (load str))
  (defn find-mirror-1d [input]
    (find-index |(= num-errs $) (mirror-errs-1d input)))
  (loop [input :in inputs
         :let [vm (find-mirror-1d input)]]
    (if vm
      (+= total vm)
      (+= total (* 100 (find-mirror-1d (rotate input))))))
  total)

(defn part1 [str] (run str 0))

(judge/test (part1 example) 405)
(judge/test (part1 (slurp "./day13/input.txt")) 31265)

(defn part2 [str] (run str 1))

(judge/test (part2 example) 400)
(judge/test (part2 (slurp "./day13/input.txt")) 39359)
