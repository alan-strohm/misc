(use judge)
(use ./util)
(import heap)

(def real-input (slurp "./input/18.txt"))
(def test-input `
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
`)

(defn read [str num]
  (def peg ~{:main (split "\n" (+ -1 (/ :line ,tuple)))
             :line (split "," (number :d+))})
  (def bytes (take num (peg/match peg str)))
  (tabseq [b :in bytes] b true))

(test (read test-input 2)
  @{[4 2] true [5 4] true})
(test ((read real-input 1024) [0 0]) nil)

(defn contains? [max-dim [x y]]
  (and
    (<= x max-dim)
    (>= x 0)
    (<= y max-dim)
    (>= y 0)))

(defn neighbors [blocked? dim p]
  (seq [d :in dirs4
        :let [n (p2+ p d)]
        :when (and
                (not (blocked? n))
                (contains? dim n))]
    n))

(test (neighbors @{} 2 [1 1]) @[[1 0] [2 1] [1 2] [0 1]])
(test (neighbors @{[1 0] true} 2 [1 1]) @[[2 1] [1 2] [0 1]])
(test (neighbors @{} 2 [0 0]) @[[1 0] [0 1]])

(defn min-steps [blocked? dim]
  (def [start end] [[0 0] [dim dim]])
  (def in-shore @{start true})
  (def num-steps @{start 0})
  (def prev @{})
  (def shore (heap/new |(cmp (num-steps $0) (num-steps $1))))
  (heap/push shore start)

  (var result nil)

  (loop [cur :iterate (heap/pop-min shore nil)]
    (def cur-steps (num-steps cur))
    (when (= end cur)
      (set result cur-steps)
      (break))
    (put in-shore cur nil)

    (loop [neighbor :in (neighbors blocked? dim cur)
           :when (or (not (num-steps neighbor)) (in-shore neighbor))]
      (def steps-through-cur (inc cur-steps))
      (def lowest-so-far (or (num-steps neighbor) math/int-max))
      (when (<= steps-through-cur lowest-so-far)
        (put num-steps neighbor steps-through-cur)
        (put prev neighbor cur))
      (when (not (in-shore neighbor))
        (put in-shore neighbor true)
        (heap/push shore neighbor))))
  [result prev])

(defn part1 [str num dim]
  (def blocked? (read str num))
  (first (min-steps blocked? dim)))

(test (part1 test-input 12 6) 22)
(test (part1 real-input 1024 70) 272)
(test (part1 real-input 3450 70) nil)

(defn is-blocked? [num] (nil? (part1 real-input num 70)))
(test (is-blocked? 1024) false)
(test (is-blocked? 2966) false)
(test (is-blocked? 2967) true)

(defn find-when-blocked [is-blocked? from to]
  (when (= from to) (break from))
  (when (= (inc from) to) (break from))
  (def mid (div (+ from to) 2))
  (if (is-blocked? mid)
    (find-when-blocked is-blocked? from mid)
    (find-when-blocked is-blocked? mid to)))

(test (find-when-blocked is-blocked? 1024 3450) 2966)

(defn part2 [str dim]
  (def lines (string/split "\n" str))
  (def is-blocked? |(nil? (part1 str $ dim)))
  (def idx (find-when-blocked is-blocked? 0 (length lines)))
  (in lines idx))

(test (part2 test-input 6) "6,1")
(test (part2 real-input 70) "16,44")

(defn get-visited [blocked? dim]
  (def [result prev] (min-steps blocked? dim))
  (tabseq [:before (var cur [dim dim])
           n :iterate (prev cur)
           :after (set cur n)] n true))

(test-stdout
  (do
    (def blocked? (read real-input 2966))
    (def visited? (get-visited blocked? 70))
    (loop [y :range-to [0 70]
           :after (print)
           x :range-to [0 70]]
      (cond
        (blocked? [x y]) (prin "#")
        (visited? [x y]) (prin "O")
        (prin "."))
      )) `
  O#OOOOO....#.#.###.....#...##..###.#...#.........######.....##.#....##.
  O#O###O###.#.###.#.#####.#######.#.#.#####.#####.#.#######.###.#####.#.
  OOO..#O#.#.#.##..#.#.....#...#..##.#.#..##..##...#.###...#.#..##.##.##.
  ######O#.#####.#####.#####.#######.###.#.#####.###.#.#.#.#.#####.######
  .###OOO#....#..#...#.#.#.#.#.####.....##.#.#.#.#.###...#.#..##.#.##.###
  ##.#O###.#########.###.#.###.###########.#.###.#.#######.###.#.#####.#.
  .#.#OOO#.#....##.##..#.#.#.####...#..#...#.#.#...#.##..#....##...#...#.
  .#.###O###########.#####.#.###.#####.#.###.#.#########.#######.#.#.###.
  ..#..#O#.#.....#.#.#...#.#........##.#.###.#...#........##..#..###....#
  .#####O###.###.#####################.###.#.###.#.#########.#.###.#####.
  #..#OOO#.#...#.#...#...#..##..##..#..#...#...#.#...#..#.##.#.###.#.#.##
  ####O#################.###.#####################.#.#.###.#.#.##########
  OOOOO#.#........##.#...#####..##.#...##....#..####.#########.#.##....#.
  O#.#######################.###.#.#########.#.#.#.#.#.#.#######.#.#####.
  O#.#.#.##..#.#.#.#.#.#..##...###.#.#.###.#.#.#.#.##.#.#....#.#...#...#.
  O###.#####.#.#######.#######.#.#########.#.###############.#.#####.#.#.
  O#..##.....#.###.#.#....##.#.#.#........##...#.#.......#####...#...#..#
  O#######.#.###.###########.###########.#####.#.#########.#######.#####.
  OOOOOOO######..#.#....#.###......#.#...##..###.#.#.##..####.#.####...#.
  ######O#.#######.#################.#.#.#.#.#.###.#.#.###.#####.###.###.
  ###.##O#...#OOOOO#...###.#.#..##.#.#.#.###...#######.###.#.####.##.#...
  .###.#O#####O###O###.#############.#####.#####.###.#.#.###########.####
  OOO#.#O#OOO#OOO#OOO#..##.#.###....##...#.#...#.#.###.##.##....#...##...
  O#O###O#O#O###O###O###.#.###########.#.#.#.###.#.#.#.#.#.#.#########.##
  O#O#OOO#O#OOOOO#.#OOO#.###...###.#.#.##..#.#..#.######.##..#..#..#.....
  O#O#O###O#######.###O#.#.#.#.###.#########.#################.#########.
  O#OOO#.#O##..#OOOOOOO#.#.#.#.###.#.#.#.#.#.#.......#####..#..#...#...##
  O#####.#O#####O#######.#######.#.#.###.#####.#####.#.#.#############.#.
  OOOOO#.#O#.###OOO#OOOOO#.#..####.....#.....#..##.#######...#...#.#...#.
  ####O###O#.#####O#O###O#.#.#####.#########.#.#.#.#.###.#####.#########.
  ...#OOO#O####.##OOO###OOO#.#...###.#..#..###.#.#.#.#.#.#.#...#........#
  .#.###O#O#####.#########O#####.#.#.#####.#.###.#.#.#######.###########.
  .##..#OOO#.#.#.###OOO#OOO#OOO##..#..#####..##..#.##..#..#..#.#.#.#.#.#.
  ####.#####.#.###.#O#O#O###O#O#######.#.#########.#####.#####.#########.
  ...##......#.#.#.#O#O#OOOOO#OOO#...#.###.##..#.#...#.#.##....##..#...#.
  ##############.###O#O#########O#.#.#.#.###.###.#####.#######.#.#.#####.
  .#..#....#.#.#####O#O..#OOOOOOO#.#####.#.#.###.#.#..####.#.#.#.#.#.#...
  ##########.#.#.#.#O#O###O#############.#####.#############.#.#.###.####
  .#.......#.###.#.#O#OOOOO###.#...#.###...#.#.....#.......#...##..#.#.#.
  .###.###.#.#.#.#.#O#######.#.#######.###.#.#.#####################.#.#.
  #..###.###...#.#.#O#.#...#.#....####.#.#.#.#.#.#.##..#.#.....#.#..##..#
  ########.#####.###O#####.#.#####.#########.#.###########.#######.######
  .#...#..###...##.#O#...#...#..##.#.#.#...#.#...#...##..#.#..##.#.......
  ##########.#######O#.#####.#########.#.###.###.#######.#.#.###.#######.
  #.##.#.#...#OOOOOOO#.#OOO###.#.#OOO#..###..#...#..#..###.#.#.#.#.#.#.#.
  .#.#####.###O#######.#O#O###.###O#O###.#.#############.#####.#.#.#####.
  .#.#.#.#.#.#O#OOOOO#.#O#O#.#.#.#O#OOO###.#.#..##...#.#.#.#.######..#.##
  .###.#######O#O###O###O#O#.###.#O###O#.###.#######.#####.#.#.##########
  ##.#.##.##.#O#OOO#O#OOO#O#.#.#.#OOO#O#..##.....#..####...#.#.#.##....#.
  .#####.#.#.#O###O#O#O###O#########O#O###########.#########.#.#.#.#####.
  ##.....#.#.#OOO#O#OOO#OOO#.#OOOOOOO#OOOOOOOOOOO#.#....#.##.#.#.#.#.#.##
  .#.#######.###O#O#####O###.#O#################O#####.###.#.#.#.#.#.#.#.
  .#..#..#...#.#O#O#...#O##..#OOOOOOOOOOO##..#OOO#.##..###...#.#.#.#.#...
  .#######.###.#O#O#.###O#.#############O#.###O#########.#####.#.###.####
  #..#...#.#...#OOO#OOOOO#.#.#.#.....#.#O#.###O#.....##.#....#...#.###..#
  ####.#.###########O#####.#####.#######O#.###O#.#############.#######.#.
  #....#.#OOOOOOOOOOO#..#..###..##.#.#.#O#...#O#.#..#.#..#...#.#.###...##
  .#.#####O###########################.#O#.###O#.###########.###.#######.
  .#.#OOOOO#.#.#..OOOOOOOOO#...#.....###O#.#.#O#...##.#.##.#.#..#..#OOOOO
  .#.#O#####.#####O#######O#########.###O###.#O#.#######.###.#####.#O###O
  .#.#O#.........#O##....#OOO#OOOOO###.#OOO#OOO#.#.#.....#...#.#.###OOO#O
  ##.#O###########O#.#######O#O###O#######O#O#############.###.###.###O#O
  .###OOOOO#.#OOO#O#OOOOO#OOO#OOO#OOOOOOOOO#OOOOOOOOO#OOO#.#...#...#OOO#O
  .#.#####O###O#O#O#O###O#O###.#O###################O#O#O###########O###O
  .#.....#OOOOO#OOO#OOO#OOO#...#O#....##..##...#...#OOO#O###.#...#OOO#.#O
  .###################O#####.###O#########.#.#.###.#####O#####.###O#####O
  .#...#OOO###OOOOOOO#OOO#...#OOO#.....#.....#...#.#.#.#O#.......#OOO#OOO
  ##.###O#O###O#####O###O#####O###.#############.#.###.#O###########O#O##
  ...#.#O#OOOOO#.#.#OOOOO#OOO#O#..#..#..#..#...#.#.#...#OOOOO###OOO#O#OOO
  .###.#O#########.#######O#O#O#######.###.#.#.#############O###O#O#O###O
  ...#.#OOOOOOOOOOOOOOOOOOO#OOO#...#.#.#####.#.##.....###.#.OOOOO#OOO#...
`)
  
