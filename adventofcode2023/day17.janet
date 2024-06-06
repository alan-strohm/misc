(import judge)
(import heap)
(use ./util)

(def test-input `
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
`)

(def test-input2 `
111111111111
999999999991
999999999991
999999999991
999999999991
`)


(def real-input (slurp "./input/17.txt"))

(def dir/opp {dir-N dir-S dir-S dir-N
              dir-E dir-W dir-W dir-E})

(defn neighbors [grid [pos dir streak] min-streak max-streak]
  #(printf "\nneighbors of %q:" [pos dir streak])
  (seq [new-dir :in dirs4
        :when (not= new-dir (dir/opp dir))
        :when (or (zero? streak) (>= streak min-streak) (= new-dir dir))
        :let [new-streak (if (= new-dir dir) (inc streak) 1)]
        :when (<= new-streak max-streak)
        :let [new-pos (p2+ pos new-dir)]
        :when (grid/contains grid new-pos)]
    #(printf " %q" [new-pos new-dir new-streak])
    [new-pos new-dir new-streak]))

(judge/test (neighbors (grid/parse test-input) [[0 0] dir-E 1] 2 4) @[[[1 0] [1 0] 2]])
(judge/test (neighbors (grid/parse test-input) [[0 0] dir-E 1] 0 1) @[[[0 1] [0 1] 1]])
(judge/test (neighbors (grid/parse test-input) [[0 0] dir-E 0] 4 10) @[[[1 0] [1 0] 1] [[0 1] [0 1] 1]])
(judge/test (neighbors (grid/parse test-input2) [[0 4] dir-S 4] 4 10) @[[[1 4] [1 0] 1]])

(defn done? [[pos _ streak] end min-streak]
  (and (= pos end)
       (>= streak min-streak)))

(defn shortest-path [grid start end min-streak max-streak]
  (def start [start dir-E 0])

  (def in-shore @{start true})
  (def distance @{start 0})
  (def shore (heap/new |(cmp (distance $0) (distance $1))))
  (heap/push shore start)

  (var result nil)

  (loop [cur :iterate (heap/pop-min shore nil)]
    (def cur-dist (distance cur))
    (when (done? cur end min-streak)
      (set result cur-dist)
      (break))
    (put in-shore cur nil)

    (loop [neighbor :in (neighbors grid cur min-streak max-streak)
           :when (or (not (distance neighbor)) (in-shore neighbor))]
      (def dist-through-cur (+ cur-dist (grid/get grid (neighbor 0))))
      (update distance neighbor |(if $
                                   (min $ dist-through-cur)
                                   dist-through-cur))
      (when (not (in-shore neighbor))
        (put in-shore neighbor true)
        (heap/push shore neighbor))))
  result)

(defn solve [str min-streak max-streak]
  (def grid (grid/map (grid/parse str) scan-number))
  (def end (let [{:dims [w h]} grid] [(dec w) (dec h)]))
  (shortest-path grid [0 0] end min-streak max-streak))

(judge/test (solve test-input 0 3) 102)
# 6s
#(judge/test (solve real-input 0 3) 1008)

(judge/test (solve test-input 4 10) 94)
(judge/test (solve test-input2 4 10) 71)

# 25s
#(judge/test (solve real-input 4 10) 1210)
