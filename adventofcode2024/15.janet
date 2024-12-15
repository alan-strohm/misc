(import judge)
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

(judge/test-stdout (let [state (read test-input)] (:run state) (prin (:fmt state))) `
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

(judge/test (part1 test-input) 10092)
(judge/test (part1 real-input) 1437174)
