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
  (def perims (tabseq [[p _] :in (grid/pairs grid)]
                      p (tabseq [dir :in dirs4] dir true)))
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
      (put-in perims [p dir] nil))
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
             :let [perim (sum-loop [p :keys ps] (length (perims p)))]]
            (* perim (length ps))))

(judge/test (part1 test-input) 1930)
(judge/test (part1 real-input) 1361494)

(def corners {dir-W dir-N
              dir-N dir-E
              dir-E dir-S
              dir-S dir-W})

(defn num-corners [in-perim]
  (sum-loop [c :pairs corners
             :when (all in-perim c)]
            1))

(defn corner-reqs [in-perim]
  (seq [[from to] :pairs corners
        :when (and (in-perim from)
                   (not (in-perim to)))]
    [to from]))

(defn format-corner-reqs [in-perim]
  (map |(map format-dir $) (corner-reqs in-perim)))

(judge/test (num-corners (tabseq [d :in dirs4] d true)) 4)
(judge/test (corner-reqs (tabseq [d :in dirs4] d true)) @[])
(judge/test (num-corners        {dir-N true dir-S true}) 0)
(judge/test (format-corner-reqs {dir-N true dir-S true})
            @[@[">" "^"] @["<" "v"]])

(judge/test (num-corners        {dir-W true dir-N true dir-E true}) 2)
(judge/test (format-corner-reqs {dir-W true dir-N true dir-E true})
            @[@["v" ">"]])
(judge/test (num-corners        {dir-S true dir-W true dir-N true}) 2)
(judge/test (format-corner-reqs {dir-S true dir-W true dir-N true})
            @[@[">" "^"]])
(judge/test (num-corners        {dir-W true dir-E true}) 0)
(judge/test (format-corner-reqs {dir-W true dir-E true})
  @[@["^" "<"] @["v" ">"]])

(defn part2 [str]
  (def grid (grid/parse str))
  (def [_ by-region perims] (map-regions grid))
  (def plant-type
    (tabseq [[id ps] :pairs by-region] id (grid/get grid (next ps))))
  (def sides (tabseq [[p in-perim] :pairs perims] p (num-corners in-perim)))
  (loop [[p in-perim] :pairs perims
         [neighbor-dir req] :in (corner-reqs in-perim)
         :let [neighbor (p2+ p neighbor-dir)]
         :when (not (get-in perims [neighbor req]))]
    (update sides p inc))
  (sum-loop [[id ps] :pairs by-region
             :let [n-sides (sum-loop [p :keys ps] (sides p))]]
            (* n-sides (length ps))))

(judge/test (part2 test-input) 1206)
(judge/test (part2 real-input) 830516)
