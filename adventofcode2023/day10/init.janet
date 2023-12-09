(import judge)

(def example1 `
-L|F7
7S-7|
L|7||
-L-J|
L|-JF
`)

(def N [0 -1])
(def S [0 1])
(def E [1 0])
(def W [-1 0])
(def dirs [N S E W])

(def pipes
  {(keyword "|") {N N S S} :- {E E W W}
   :L {S E W N} :J {S W E N}
   :7 {N W E S} :F {N E W S}
   :. {} :S {}})

(defn make-grid [input]
  (map |(map |(keyword (string/from-bytes $)) $) (string/split "\n" input)))

(def example1-grid (make-grid example1))

(defn getp [grid [x y]] (get-in grid [y x]))

(defn finds [grid]
  (label r (loop [y :range [0 (length grid)]
                  x :range [0 (length (get grid y))]]
             (if (= :S (get-in grid [y x]))
               (return r [x y])))
         (return r nil)))

(judge/test (finds example1-grid) [1 1])

(defn p+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn move [grid p dir]
  (let [p2 (p+ p dir)
        pipe-type (getp grid p2)
        pipe (pipes pipe-type)
        out (pipe dir)]
    (if (nil? out) nil [p2 out])))

(judge/test (move example1-grid [1 1] [0 1]) [[1 2] [0 1]])
(judge/test (move example1-grid [1 2] [0 1]) [[1 3] [1 0]])

(defn openings [grid p]
  (filter |(move grid p $) dirs))

(judge/test (openings example1-grid (finds example1-grid)) @[[0 1] [1 0]])

(defn farthest [grid s]
  (def [d1 d2] (openings grid s))
  (var p1 [s d1])
  (var p2 [s d2])
  (var steps 0)
  (forever
    (++ steps)
    (set p1 (move grid ;p1))
    (set p2 (move grid ;p2))
    #(printf " moved to %n and %n\n" p1 p2)
    (if (= (get p1 0) (get p2 0)) (break)))
  steps)

(judge/test (farthest example1-grid (finds example1-grid)) 4)

(defn part1 [file]
  (let [grid (make-grid (slurp file))
        start (finds grid)]
    (farthest grid start)))

(judge/test (part1 "./day10/input.txt") 6867)
