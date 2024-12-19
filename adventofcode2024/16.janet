(use judge)
(use ./util)
(import heap)

(def real-input (slurp "./input/16.txt"))

(def test-input `
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
`)

(defn read [str]
  (def grid (grid/parse str))
  (var [start end] [nil nil])
  (loop [[p v] :in (grid/pairs grid)]
    (case v
      "E" (set end p)
      "S" (set start p)))
  [grid start end])

(def rot90ccw {dir-N dir-E dir-E dir-S dir-S dir-W dir-W dir-N})
(def rot90cw {dir-N dir-W dir-W dir-S dir-S dir-E dir-E dir-N})

(defn neighbors [grid [cur-pos cur-dir]]
  (seq [next-dir :in [cur-dir (rot90cw cur-dir) (rot90ccw cur-dir)]
        :let [next-pos (p2+ cur-pos next-dir)
              value (grid/get grid next-pos)]
        :unless (= "#" value)]
    (if (= cur-dir next-dir)
      [[next-pos cur-dir] 1]
      [[cur-pos next-dir] 1000])))

(deftest neighbors
  (def [grid _ _] (read test-input))
  (test (neighbors grid [[1 13] dir-E])
    @[[[[2 13] [1 0]] 1]
      [[[1 13] [0 -1]] 1000]]))

(defn lowest-costs [grid start-pos end-pos]
  (def start [start-pos dir-E])
  (def in-shore @{start true})
  (def cost @{start 0})
  (def prevs @{})
  (def shore (heap/new |(cmp (cost $0) (cost $1))))
  (heap/push shore start)

  (var lowest-cost nil)

  (loop [cur :iterate (heap/pop-min shore nil)]
    (def cur-cost (cost cur))
    (when (= end-pos (first cur))
      (if (and lowest-cost (> cur-cost lowest-cost))
        (break)
        (set lowest-cost cur-cost)))
    (put in-shore cur nil)
    
    (loop [[neighbor neighbor-cost] :in (neighbors grid cur)
           :when (or (not (cost neighbor)) (in-shore neighbor))]
      (def cost-through-cur (+ cur-cost neighbor-cost))
      (def lowest-so-far (or (cost neighbor) math/int-max))
      (when (< cost-through-cur lowest-so-far)
        (put cost neighbor cost-through-cur)
        (put prevs neighbor @[cur]))
      (when (= cost-through-cur lowest-so-far)
        (update prevs neighbor |(array/push $ cur)))
      (when (not (in-shore neighbor))
        (put in-shore neighbor true)
        (heap/push shore neighbor))))
  [lowest-cost prevs])

(defn part1 [str]
  (first (lowest-costs ;(read str))))

(test (part1 test-input) 7036)
(test (part1 real-input) 130536)

(defn part2 [str]
  (def [grid start end] (read str))
  (def [_ prevs] (lowest-costs grid start end))
  (def in-lowest @{end true})
  (defn fill-in-lowest [node]
    (loop [[pos dir] :in (or (prevs node) @[])]
      (put in-lowest pos true)
      (fill-in-lowest [pos dir])))
  (loop [d :in dirs4] (fill-in-lowest [end d]))

  (length in-lowest))

(test (part2 test-input) 45)
(test (part2 real-input) 1024)
