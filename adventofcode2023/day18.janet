(import judge)

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

(def char-to-dir {"R" [1 0] "L" [-1 0] "U" [0 -1] "D" [0 1]})

(def peg
  (peg/compile
    ~{:rgb (* "(#" (6 :h) ")")
      :line (* (/ ':a ,char-to-dir) " " (number :d+) " " :rgb (+ "\n" -1))
      :main (some (group :line))}))

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

(defn vec*n [[x y] s] [(* s x) (* s y)])
(defn vec+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(defmacro vec+= [v1 v2]
  ~(set ,v1 (,vec+ ,v1 ,v2)))

(def test-input3 `
R 2 (#70c710)
D 2 (#70c710)
L 2 (#70c710)
U 2 (#70c710)
`)

(defn get-perimeter [plan]
  (var pos [0 0])
  (seq [[dir steps] :in plan]
    (vec+= pos (vec*n dir steps))))

(defn area-inside [vertices]
  (/ (sum
       (seq [i :range [0 (dec (length vertices))]]
         (def [x1 y1] (vertices i))
         (def [x2 y2] (vertices (inc i)))
         (* (- x1 x2) (+ y1 y2))))
     2))

(judge/test (area-inside @[[2 0] [2 1] [0 1] [0 0]]) 2)

(defn part1 [str]
  (def plan (peg/match peg str))
  (+
   (math/abs (area-inside (get-perimeter plan)))
   (sum (map last plan))))

(judge/test (area-inside (get-perimeter (peg/match peg test-input))) 42)
(judge/test (sum (map last (peg/match peg test-input))) 38)
(judge/test (part1 test-input) 80)

(defn dig-outline [plan]
  (var pos [0 0])
  (def dug @{pos true})
  (loop [[dir steps] :in plan
         step :range [0 steps]]
    (vec+= pos dir)
    (put dug pos true))
  dug)

(defn dig/print [dug]
  (let [max-x (max-of (map first (keys dug)))
        min-x (min-of (map first (keys dug)))
        max-y (max-of (map last (keys dug)))
        min-y (min-of (map last (keys dug)))]
    (loop [y :range [min-y (inc max-y)]
           :after (print)
           x :range [min-x (inc max-x)]]
      (if (dug [x y]) (prin "#") (prin ".")))))

#(dig/print (dig-outline (peg/match peg real-input)))

(defn part1 [str]
  (dig/print (dig-outline (peg/match peg str))))

(defn rot-90-ccw [[x y]] [y x])


(def test-input2 `
U 1 (#70c710)
R 1 (#70c710)
D 1 (#70c710)
R 1 (#70c710)
D 1 (#70c710)
L 2 (#70c710)
U 1 (#70c710)
`)

(judge/test-stdout (part1 test-input3) `
  ###
  #.#
  ###
`)

