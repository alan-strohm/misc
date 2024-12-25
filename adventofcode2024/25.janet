(use judge)
(use ./util)

(def real-input (slurp "./input/25.txt"))

(def test-input `
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
`)

(defn parse-entry [& lines]
  (var kind :lock)
  (var lines lines)
  (when (not= "#####" (first lines))
    (assert (= "#####" (last lines)) (printf "invalid: %n" lines))
    (set kind :key)
    (set lines (reverse lines)))
  (def result @[0 0 0 0 0])
  (loop [[r line] :pairs lines
         [c value] :pairs line
         :when (and (> r 0) (= value (chr "#")))]
    (update result c inc))
  [kind result])

(defn read [str]
  (peg/match ~{:main (split "\n\n" (+ -1 (/ :lines ,parse-entry)))
               :lines (split "\n" (% (any '1)))}
             (string/trim str)))

(test (read test-input)
  @[[:lock @[0 5 3 4 3]]
    [:lock @[1 2 0 5 3]]
    [:key @[5 0 2 1 3]]
    [:key @[4 3 4 0 2]]
    [:key @[3 0 2 0 1]]])

(defn fits [[a-kind a-heights] [b-kind b-heights]]
  (label result
     (when (or (= a-kind :key) (= b-kind :lock))
       (return result false))
     (loop [i :range [0 (length a-heights)]
            :let [total (+ (in a-heights i) (in b-heights i))]
            :when (> total 5)]
       (return result false))
     true))

(defn part1 [str]
  (def entries (read str))
  (sum-loop [a :in entries b :in entries :when (fits a b)] 1))

(test (part1 test-input) 3)
(test (part1 real-input) 3223)
