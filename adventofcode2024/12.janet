(import judge)
(use ./util)

(def real-input (slurp "./input/12.txt"))
(def test-input `
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
`)

(defn map-regions [grid]
  (def to-map (tabseq [[p v] :in (grid/pairs grid)] p v))
  (def by-point (grid/map grid (fn [&] nil)))
  (def perims (tabseq [[p _] :in (grid/pairs grid)] p 4))
  (def by-region @{})
  (var region-id 0)

  (defn map-cell [p id]
    (put-in by-region [id p] true)
    (grid/set by-point p id)
    (put to-map p nil))

  (while (not (empty? to-map))
    (def q @[(next to-map)])
    (loop [p :iterate (array/pop q)
           :before (map-cell p region-id)
           dir :in dirs4
           :let [v (grid/get grid p)
                 other-p (p2+ p dir)
                 other-v (grid/get grid other-p)]
           :when (= v other-v)]
      (when (to-map other-p)
        (map-cell other-p region-id)
        (array/push q other-p))
      (update perims p dec))
    (++ region-id))
  [by-point by-region perims])

(judge/test (first (map-regions (grid/parse test-input)))
  {:content @[@[8 8 8 8 7 7 0 0 1 1]
              @[8 8 8 8 7 7 0 0 0 1]
              @[6 6 8 8 8 0 0 1 1 1]
              @[6 6 8 0 0 0 2 1 1 1]
              @[6 6 6 6 0 2 2 10 1 4]
              @[6 6 3 6 0 0 2 2 4 4]
              @[6 6 3 3 3 0 2 2 4 4]
              @[9 3 3 3 3 3 2 2 4 4]
              @[9 3 3 3 5 3 2 4 4 4]
              @[9 9 9 3 5 5 2 4 4 4]]
   :dims [10 10]})

(defn part1 [str]
  (def grid (grid/parse str))
  (def [_ by-region perims] (map-regions grid))
  (sum-loop [[id ps] :pairs by-region
             :let [perim (sum-loop [p :keys ps] (perims p))]]
            (* perim (length ps))))

(judge/test (part1 test-input) 1930)
(judge/test (part1 real-input) 1361494)
