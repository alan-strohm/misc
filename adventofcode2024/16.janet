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

(defn shortest-path [grid start end]
  (def start [start dir-E])
  (def in-shore @{start true})
  (def cost @{start 0})
  (def shore (heap/new |(cmp (cost $0) (cost $1))))
  (heap/push shore start)

  (var result nil)
  
  (loop [cur :iterate (heap/pop-min shore nil)]
    (def cur-cost (cost cur))
    (when (= end (first cur))
      (set result cur-cost)
      (break))
    (put in-shore cur nil)
    
    (loop [[neighbor neighbor-cost] :in (neighbors grid cur)
           :when (or (not (cost neighbor)) (in-shore neighbor))]
      (def cost-through-cur (+ cur-cost neighbor-cost))
      (update cost neighbor |(min (or $ math/int-max) cost-through-cur))
      (when (not (in-shore neighbor))
        (put in-shore neighbor true)
        (heap/push shore neighbor))))
  result)

(defn part1 [str]
  (shortest-path ;(read str)))

(test (part1 test-input) 7036)
(test (part1 real-input) 130536)
