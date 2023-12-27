(import judge)

(use ./util)

(def test-input `
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3`)

(def real-input (slurp "./input/24.txt"))

(defn read [str]
  (peg/match ~{:number (number (* (? "-") :d+))
               :numbers (group ,(sep ':number '(* "," :s+)))
               :line (* :numbers " @ " :numbers)
               :main ,(sep '(group :line) "\n")}
             str))

(judge/test (read test-input)
  @[@[@[19 13 30] @[-2 1 -2]]
    @[@[18 19 22] @[-1 -1 -2]]
    @[@[20 25 34] @[-2 -2 -4]]
    @[@[12 31 28] @[-1 -2 -1]]])

(defn det2d [[[a c] [b d]]] (- (* a d) (* b c)))
(judge/test (det2d [[3 1] [7 -4]]) -19)

(defn solve2d [A y]
  (let [detA (det2d A)
        x0 (/ (det2d [y (in A 1)]) detA)
        x1 (/ (det2d [(in A 0) y]) detA)]
    (if (= 0 detA)
      nil
      [x0 x1])))
(judge/trust (solve2d [[-2 1] [1 1]] [-1 6]) [2.33333 3.66667])

(defn intersection [[m0 b0] [m1 b1]]
  (def none [nil nil])
  (match (solve2d [m0 (vec*n m1 -1)] (vec- b1 b0))
    nil none
    ([x0 _] (< x0 0)) none
    ([_ x1] (< x1 0)) none
    [x0 _] (vec+ (vec*n m0 x0) b0)))

(judge/trust
  (intersection [[-2 1] [19 13]] [[-1 -1] [18 19]])
  [14.3333 15.3333])
(judge/test
  (intersection [[-2 1] [19 13]] [[1 -5] [20 19]])
  [nil nil])
(judge/test
  (intersection [[-2 -2] [20 25]] [[-1 -1] [18 19]])
  [nil nil])

(defn part1 [str min max]
  (def entries 
    (map
      |(reverse (map |(array/slice $ 0 2) $))
      (read str)))

  (defn ok? [v]
    (not (or
           (nil? v)
           (< v min)
           (> v max))))

  (length
    (seq [[i e0] :pairs entries
          e1 :in (array/slice entries (inc i))
          :let [p (intersection e0 e1)]
          :when (all ok? p)])))

(judge/test (part1 test-input 7 27) 2)
(judge/test (part1 real-input 200000000000000 400000000000000) 12015)

