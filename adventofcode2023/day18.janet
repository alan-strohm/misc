(import judge)
(use ./util)

(def test-input `
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)

`)

(def real-input (slurp "./input/18.txt"))

(def peg ~{:main (some (group :line))
           :line (* :dir " " (number :d+) " " (thru (+ "\n" -1)))
           :dir (/ '1 ,{"R" dir-E "L" dir-W "U" dir-N "D" dir-S})
           })

(judge/test (peg/match peg test-input)
  @[@[[1 0] 6]
    @[[0 1] 5]
    @[[-1 0] 2]
    @[[0 1] 2]
    @[[1 0] 2]
    @[[0 1] 2]
    @[[-1 0] 5]
    @[[0 -1] 2]
    @[[-1 0] 1]
    @[[0 -1] 2]
    @[[1 0] 2]
    @[[0 -1] 3]
    @[[-1 0] 2]
    @[[0 -1] 2]])

(defn solve [steps]
  (def area (sum-loop
              [:before (var cur [0 0])
               [dir cnt] :in steps
               :let [[x1 y1] cur
                     [x2 y2] (set cur (vec+ cur (vec*n dir cnt)))]]
              (* 0.5 (+ x1 x2) (- y1 y2))))
  (def perimeter (sum-loop [[_ cnt] :in steps] cnt))
  (def interior (+ (math/abs area) (* -0.5 perimeter) 1))
  (+ perimeter interior))

(defn part1 [input] (solve (peg/match peg input)))

(judge/test (part1 test-input) 62)
(judge/test (part1 real-input) 46334)

(def peg ~{:main (some (/ (group :line) ,reverse))
           :line (* 1 " " :d+ " (#" (number 5 16) :dir ")" (+ "\n" -1))
           :dir (/ '1 ,{"0" dir-E "1" dir-S "2" dir-W "3" dir-N})
           })

(judge/test (peg/match peg test-input)
  @[@[[1 0] 461937]
    @[[0 1] 56407]
    @[[1 0] 356671]
    @[[0 1] 863240]
    @[[1 0] 367720]
    @[[0 1] 266681]
    @[[-1 0] 577262]
    @[[0 -1] 829975]
    @[[-1 0] 112010]
    @[[0 1] 829975]
    @[[-1 0] 491645]
    @[[0 -1] 686074]
    @[[-1 0] 5411]
    @[[0 -1] 500254]])

(defn part2 [input] (solve (peg/match peg input)))

(judge/test (part2 test-input) 952408144115)
(judge/test (part2 real-input) 102000662718092)
