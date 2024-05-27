(import judge)
(import heap)

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

(defn grid/parse [str]
  (->> str string/trim (string/split "\n")
       (map (partial map |(- $ (chr "0"))))))

(judge/test (grid/parse test-input)
  @[@[2 4 1 3 4 3 2 3 1 1 3 2 3]
    @[3 2 1 5 4 5 3 5 3 5 6 2 3]
    @[3 2 5 5 2 4 5 6 5 4 2 5 4]
    @[3 4 4 6 5 8 5 8 4 5 4 5 2]
    @[4 5 4 6 6 5 7 8 6 7 5 3 6]
    @[1 4 3 8 5 9 8 7 9 8 4 5 4]
    @[4 4 5 7 8 7 6 9 8 7 7 6 6]
    @[3 6 3 7 8 7 7 9 7 9 6 5 3]
    @[4 6 5 4 9 6 7 9 8 6 8 8 7]
    @[4 5 6 4 6 7 9 9 8 6 4 5 3]
    @[1 2 2 4 6 8 6 8 6 5 5 6 3]
    @[2 5 4 6 5 4 8 8 8 7 7 3 5]
    @[4 3 2 2 6 7 4 6 5 5 5 3 3]])

(def dirs [[0 1] [0 -1] [1 0] [-1 0]])
(defn vec+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defn vec- [[x1 y1] [x2 y2]] [(- x2 x1) (- y2 y1)])
(defn vec*n [[x y] s] [(* x s) (* y s)])

(defn back [[x y]] [(- x) (- y)])

(defn grid/oob? [grid [c l]]
  (def [h w] [(length grid) (length (first grid))])
  (or
    (< c 0)
    (< l 0)
    (>= l h)
    (>= c w)))

(defn grid/get [grid [c l]]
  (get-in grid [l c]))

(defn next-dirs [in-dir stretch]
  (def back-dir (if (nil? in-dir) nil (back in-dir)))
  (defn ok? [dir]
    (and
      (not= dir back-dir)
      (or
        (< stretch 3)
        (not= dir in-dir))))
  (filter ok? dirs))

(defn state/start [start]
  @{:pos start
    :dirs (array ;(next-dirs nil 0))
    :stretch [nil 0]})

(defn state/next [prev]
  (def dir (array/pop (prev :dirs)))
  (def {:pos from :stretch [prev-dir cnt]} prev)
  (def stretch (if (= dir prev-dir) (inc cnt) 0))

  (if dir
    @{:pos (vec+ from dir)
      :dirs (array ;(next-dirs dir stretch))
      :stretch [dir stretch]}
    nil))

(defn nodes/get [nodes pos]
  (if-let [node (nodes pos)]
    node
    (do
      (def node @{:pos pos :g-score math/int-max :f-score math/int-max})
      (put nodes pos node)
      node)))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (+ (math/abs (- x2 x1))
     (math/abs (- y2 y1))))

(defn first-n [t n]
  (tuple/slice t 0 (min n (length t))))

(judge/test (first-n [] 2) [])
(judge/test (first-n [1] 2) [1])
(judge/test (first-n [1 2 3] 2) [1 2])

(defn ok-dir [grid current dir]
  (def prev (current :came-from))
  (def backwards (and (not (nil? prev)) (= dir (vec- (current :pos) (prev :pos)))))
  (defn too-long [current dir n]
    (def prev (current :came-from))
    (cond
      (= n 0) true
      (nil? prev) false
      (= dir (vec- (prev :pos) (current :pos))) (too-long prev dir (dec n))
      #(printf " dir: %n other: %n" dir (vec- (current :pos) (prev :pos)))
      false
      ))
  (not
    (or
      (grid/oob? grid (vec+ (current :pos) dir))
      backwards
      (too-long current dir 4))))

(defn get-path [dest]
  (var path @[])
  (var cur dest)
  (while cur
    (array/insert path 0 (cur :pos))
    (set cur (cur :came-from)))
  path)

(defn shortest-path [grid start]
  (def dest [(dec (length (first grid))) (dec (length grid))])
  (var current @{:pos start :g-score 0 :f-score (manhattan-dist start dest)})
  (def nodes @{start current})
  (def open-set @{start current})

  (while (not (empty? open-set))
    (pp open-set)
    (set current
         (reduce2 |(if (< ($1 :f-score) ($0 :f-score)) $1 $0) open-set))
    (printf "pos: %n" (current :pos))

    (if (= (current :pos) dest)
      (break))

    (put open-set (current :pos) nil)
    (each dir (filter (partial ok-dir grid current) dirs)
      (def neighbor (nodes/get nodes (vec+ (current :pos) dir)))
      (printf "  dir: %n" dir)

      (def g-score (+ (current :g-score) (grid/get grid (neighbor :pos))))
      (when (< g-score (neighbor :g-score))
        (put neighbor :came-from current)
        (put neighbor :g-score g-score)
        (put neighbor :f-score (+ g-score (manhattan-dist (neighbor :pos) dest)))
        (put open-set (neighbor :pos) neighbor))))

  (loop [l :range [0 (length grid)]
         :after (print)
         c :range [0 (length (first grid))]]
    (prinf "%3d " (get-in nodes [[c l] :g-score] -1)))
  (pp (get-path current))
  (current :g-score))

(def test-input `
241343231
321545353
`)
#(judge/test (shortest-path (grid/parse test-input) [0 0]) 15)
