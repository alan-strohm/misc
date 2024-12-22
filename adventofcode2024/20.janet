(use judge)
(use ./util)

(def real-input (slurp "./input/20.txt"))

(def test-input `
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
`)

(defn read [str]
  (def grid (grid/parse str))
  (var [start end] [nil nil])
  (loop [[p v] :in (grid/pairs grid)]
    (cond
      (= "S" v) (set start p)
      (= "E" v) (set end p)))
  (map |(grid/set grid $ ".") [start end])

  (def dists @{start 0})

  (loop [:before (var cur start)
         _ :iterate (not= cur end)
         dir :in dirs4
         :let [n (p2+ cur dir)]
         :when (and (= "." (grid/get grid n))
                    (not (dists n)))]
    (put dists n (length dists))
    (set cur n)
    (break))
  [grid start end dists])

(deftest dists
  (def [grid _ end dists] (read test-input))
  (loop [[p d] :pairs dists] (grid/set grid p (string (mod d 10))))
  (test-stdout (prin (grid/format grid)) `
    ###############
    #234#012#67890#
    #1#5#9#3#5###1#
    #0#678#4#4#432#
    #######5#3#5###
    #######6#2#678#
    #######7#1###9#
    ###234#890#210#
    ###1#######3###
    #890###098#456#
    #7#####1#7###7#
    #6#098#2#6#098#
    #5#1#7#3#5#1###
    #432#654#432###
    ###############
  `)
  (test (dists end) 84))

(defn diamond [pos n]
  (def rot90cw {dir-N dir-E dir-E dir-S dir-S dir-W dir-W dir-N})
  (seq [d1 :in dirs4
        :before (var p1 (p2+ pos d1))
        i :range-to [1 n]
        :after (p2+= p1 d1)
        :before (var p2 p1)
        :let [d2 (rot90cw d1)]
        j :range-to [0 (- n i)]
        :after (p2+= p2 d2)]
    p2))

(deftest diamond
  (def grid (grid/parse test-input))
  (test (diamond [1 1] 1) @[[1 0] [2 1] [1 2] [0 1]])
  (loop [p :in (diamond [1 1] 1)]
    (grid/set grid p "d"))
  (loop [p :in (diamond [3 4] 3)]
    (grid/set grid p "*"))
  (test-stdout (prin (grid/format grid)) `
    #d#############
    d.d*#...#.....#
    #d***.#.#.###.#
    #*****#.#.#...#
    ***#***.#.#.###
    #*****#.#.#...#
    ##***##.#.###.#
    ###*.E#...#...#
    ###.#######.###
    #...###...#...#
    #.#####.#.###.#
    #.#...#.#.#...#
    #.#.#.#.#.#.###
    #...#...#...###
    ###############
  `))

(defn cheats [grid pos num]
  (seq [p :in (diamond pos num)
        :when (= "." (grid/get grid p))]
    p))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))))
(test (manhattan-dist [0 0] [1 1]) 2)

(defn run [num-cheats str savings-wanted]
  (def [grid start end dists] (read str))
  (sum-loop [[p before] :pairs dists
             cheat :in (cheats grid p num-cheats)
             :let [after (dists cheat)
                   saved (- after before (manhattan-dist p cheat))]
             :when (>= saved savings-wanted)]
            1))

(def part1 (partial run 2))

(test (part1 test-input 64) 1)
(test (part1 test-input 20) 5)
(test (part1 real-input 100) 1530)

(def part2 (partial run 20))

(test (part2 test-input 74) 7)
# 6s
(test (part2 real-input 100) 1033983)
