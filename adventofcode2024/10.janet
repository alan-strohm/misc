(import judge)
(use ./util)

(def real-input (slurp "./input/10.txt"))

(def test-input `
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
`)

(defn read [str]
  (def grid (grid/map (grid/parse str) scan-number))
  (def trailheads (seq [[p v] :in (grid/pairs grid)
                        :when (= v 0)] p))
  [trailheads grid])

(defn goals [grid pos height]
  (when (= 9 height)
    (break {pos true}))
  (merge ;(seq [dir :in dirs4
                :let [next-pos (p2+ pos dir)
                      next-height (grid/get grid next-pos)]
                :when (= next-height (inc height))]
            (goals grid next-pos next-height))))

(judge/test (-> test-input read last (goals [2 0] 0) length) 5)

(defn part1 [str]
  (def [trailheads grid] (read str))
  (sum-loop [head :in trailheads]
            (length (goals grid head 0))))

(judge/test (part1 test-input) 36)
(judge/test (part1 real-input) 501)

(defn rating [grid pos height]
  (when (= 9 height)
    (break 1))
  (sum-loop [dir :in dirs4
             :let [next-pos (p2+ pos dir)
                   next-height (grid/get grid next-pos)]
             :when (= next-height (inc height))]
            (rating grid next-pos next-height)))

(judge/test (-> test-input read last (rating [2 0] 0)) 20)

(defn part2 [str]
  (def [trailheads grid] (read str))
  (sum-loop [head :in trailheads]
            (rating grid head 0)))

(judge/test (part2 test-input) 81)
(judge/test (part2 real-input) 1017)
