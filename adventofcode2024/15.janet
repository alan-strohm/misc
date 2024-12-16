(use judge)
(use ./util)

(def real-input (slurp "./input/15.txt"))

(def test-input `
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
`)

(def test-input2 `
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
`)

(defn find-space [grid p dir]
  (def next-p (p2+ p dir))
  (match (grid/get grid next-p)
    nil nil
    "#" nil
    "." next-p
    "O" (find-space grid next-p dir)
    v (errorf "unexpected value %n at pos %n" v next-p)))

(defn state/move [self]
  (def {:grid grid :pos pos} self)
  (def dir (parse-dir (array/pop (self :moves))))
  (def space (find-space grid pos dir))
  (when (not space) (break))

  (grid/set grid pos ".")
  (var p (set (self :pos) (p2+ pos dir)))
  (grid/set grid p "@")
  (while (not= p space)
    (grid/set grid (p2+= p dir) "O")))

(defn state/format [{:grid grid}] (grid/format grid))

(defn state/run [self]
  (while (not (empty? (self :moves)))
    (:mv self)))

(defn state/score [{:grid grid}]
  (sum-loop [[[x y] v] :in (grid/pairs grid)
             :when (= v "O")]
            (+ x (* 100 y))))

(defn read [str]
  (def [grid-str moves-str] (string/split "\n\n" str))
  (def grid (grid/parse grid-str))
  @{:grid grid
    :pos (grid/find grid |(= "@" $))
    :moves (->> moves-str (string/replace-all "\n" "") (map string/from-bytes) reverse)
    :mv state/move :fmt state/format :run state/run
    :score state/score})

(test-stdout (let [state (read test-input)] (:run state) (prin (:fmt state))) `
  ##########
  #.O.O.OOO#
  #........#
  #OO......#
  #OO@.....#
  #O#.....O#
  #O.....OO#
  #O.....OO#
  #OO....OO#
  ##########
`)

(defn part1 [str]
  (def state (read str))
  (:run state)
  (:score state))

(test (part1 test-input) 10092)
(test (part1 real-input) 1437174)

(defn read2 [str]
  (def expand-peg
    ~(% (any (+ (/ "#" "##")
                (/ "O" "[]")
                (/ "." "..")
                (/ "@" "@.")
                '1))))
  (def expanded (first (peg/match expand-peg str)))
  (read expanded))

(test-stdout (-> test-input read2 (in :grid) grid/format prin) `
  ####################
  ##....[]....[]..[]##
  ##............[]..##
  ##..[][]....[]..[]##
  ##....[]@.....[]..##
  ##[]##....[]......##
  ##[]....[]....[]..##
  ##..[][]..[]..[][]##
  ##........[]......##
  ####################
`)

(defn box/right [[x y]] [(inc x) y])
(defn box/from-left [[x y]] [(dec x) y])
(defn box/occupies [b] [b (box/right b)])

(defn box/neighbors [b dir]
  (when (= dir dir-W)
    (break [(p2+ b dir)]))
  (when (= dir dir-E)
    (break [(p2+ (box/right b) dir)]))
  (assert (or (= dir-N dir) (= dir-S dir)))
  (seq [p :in (box/occupies b)] (p2+ p dir)))

(defn box/can-move
  "Can b move in dir? Returns true, false or an array of boxes that would have to move first."
  [b grid dir]
  (def next-v
    (seq [p :in (box/neighbors b dir)]
      (match (grid/get grid p)
        nil false
        "#" false
        "." true
        "[" p 
        "]" (box/from-left p))))
  (cond
    (some false? next-v) false
    (all true? next-v) true
    (filter tuple? (distinct next-v))))

(deftest basic-can-move
  (def grid ((read2 test-input) :grid))
  (test (box/can-move [2 6] grid dir-W) false)
  (test (box/can-move [2 6] grid dir-E) true)
  (test (box/can-move [2 6] grid dir-S) true)
  (test (box/can-move [2 6] grid dir-N) @[[2 5]])
  (test (box/can-move [2 5] grid dir-S) @[[2 6]])
  (test (box/can-move [2 5] grid dir-E) false)
  (test (box/can-move [4 7] grid dir-E) @[[6 7]])
  (test (box/can-move [6 7] grid dir-W) @[[4 7]]))

(defn box/move [b grid dir]
  (assert (box/can-move b grid dir))
  (loop [p :in (box/occupies b)] (grid/set grid p "."))
  (def [l r] (map |(p2+ dir $) (box/occupies b)))
  (grid/set grid l "[")
  (grid/set grid r "]"))

(defn box/make-space [b grid dir]
  (def to-move @[[b]])
  (label result
    (forever
      (def next-boxes
        (catseq [b :in (array/peek to-move)
                 :let [can-move (box/can-move b grid dir)]]
                 (case can-move
                   false (return result false)
                   true @[]
                   can-move)))
      (if (empty? next-boxes)
        (break)
        (array/push to-move next-boxes)))
    (loop [bs :iterate (array/pop to-move)
           b :in bs]
      (box/move b grid dir))
    true))

(deftest test-everything
  (def grid ((read2 test-input) :grid))
  (box/move [2 6] grid dir-E)
  (box/move [6 4] grid dir-W)
  (box/move [12 1] grid dir-S)
  (box/move [14 2] grid dir-N)
  (box/move [14 6] grid dir-E)
  (test-stdout (prin (grid/format grid)) `
    ####################
    ##....[]......[][]##
    ##..........[]....##
    ##..[][]....[]..[]##
    ##...[].@.....[]..##
    ##[]##....[]......##
    ##.[]...[].....[].##
    ##..[][]..[]..[][]##
    ##........[]......##
    ####################
  `)
  (test (box/can-move [3 6] grid dir-S) @[[4 7]])
  (test (box/can-move [3 6] grid dir-N) false)
  (test (box/can-move [5 4] grid dir-N) @[[4 3] [6 3]])
  (test (box/can-move [5 4] grid dir-S) false)
  (test (box/can-move [15 6] grid dir-S) @[[14 7] [16 7]])

  (test (box/make-space [5 4] grid dir-N) true)
  (test (box/make-space [5 3] grid dir-N) false)
  (test (box/make-space [4 7] grid dir-E) true)
  (test (box/make-space [2 5] grid dir-S) true)
  (test-stdout (prin (grid/format grid)) `
    ####################
    ##....[]......[][]##
    ##..[][]....[]....##
    ##...[].....[]..[]##
    ##......@.....[]..##
    ##..##....[]......##
    ##[]....[].....[].##
    ##.[][][].[]..[][]##
    ##........[]......##
    ####################
  `))

(defn state/move [self]
  (def {:grid grid :pos pos} self)
  (def dir (parse-dir (array/pop (self :moves))))
  (def neighbor-p (p2+ pos dir))
  (def neighbor-v (grid/get grid neighbor-p))
  (when (= "#" neighbor-v)
    (break))
  (def box (cond
             (= neighbor-v "[") neighbor-p
             (= neighbor-v "]") (box/from-left neighbor-p)
             (assert (= neighbor-v ".") (errorf "unexpected neighbor %n at %n" neighbor-v neighbor-p))))

  (when (or (= neighbor-v ".")
            (box/make-space box grid dir))
    (grid/set grid pos ".")
    (grid/set grid neighbor-p "@")
    (set (self :pos) neighbor-p)))

(deftest test-input2
  (def state (read2 test-input2))
  (put state :mv state/move)
  (test-stdout (prin (:fmt state)) `
    ##############
    ##......##..##
    ##..........##
    ##....[][]@.##
    ##....[]....##
    ##..........##
    ##############
  `)
  (:mv state)
  (test-stdout (prin (:fmt state)) `
    ##############
    ##......##..##
    ##..........##
    ##...[][]@..##
    ##....[]....##
    ##..........##
    ##############
  `))

(defn state/score [{:grid grid}]
  (sum-loop [[[x y] v] :in (grid/pairs grid)
             :when (= v "[")]
            (+ x (* 100 y))))

(defn part2 [str]
  (def state (read2 str))
  (put state :mv state/move)
  (put state :score state/score)
  (:run state)
  (:score state))

(test (part2 test-input) 9021)
(test (part2 real-input) 1437468)
