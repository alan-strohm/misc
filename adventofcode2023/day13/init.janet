(import judge)
(use ../util)

(def test-input `
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

(def real-input (slurp "./day13/input.txt"))

(defn mirror-idx
  "Find the mirror of x around a midpoint mid."
  [x mid len]
  (def result (+ mid (- mid x 1)))
  (if (and (< x mid) (< result len))
    result))

(judge/test (mirror-idx 0 1 2) 1)
(judge/test (mirror-idx 0 2 3) nil)
(judge/test (mirror-idx 0 2 4) 3)

(defn transpose [strings] (map string/from-bytes ;strings))

(judge/test (transpose ["abc" "def"]) @["ad" "be" "cf"])
(judge/test (transpose ["ad" "be" "cf"]) @["abc" "def"])

(defn load [str]
  (->> str string/trim (string/split "\n\n") (map |(string/split "\n" $))))

(defn mirror-errs-1d
  `Return an array where the value at index i is the number of errors it would take
  to make i a valid mirror point`
  [lines]
  (def errs (seq [:repeat (length (first lines))] 0))
  (put errs 0 math/int-max)
  (loop [line :in lines
         [x char] :pairs line
         m :range [1 (length errs)]
         :let [mirror-x (mirror-idx x m (length line))]]
    (if (and mirror-x (not= char (mirror-x line)))
      (update errs m inc)))
  errs)

(defn solve [str num-errs]
  (defn find-mirror-1d [input]
    (find-index |(= num-errs $) (mirror-errs-1d input)))

  (sum-loop [input :in (load str)
             :let [vertical-mirror (find-mirror-1d input)]]
    (or vertical-mirror
        (* 100 (find-mirror-1d (transpose input))))))

(defn part1 [str] (solve str 0))

(judge/test (part1 test-input) 405)
(judge/test (part1 real-input) 31265)

(defn part2 [str] (solve str 1))

(judge/test (part2 test-input) 400)
(judge/test (part2 real-input) 39359)
