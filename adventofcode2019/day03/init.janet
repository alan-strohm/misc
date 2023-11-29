(import judge)

(defn scan-op [op]
  (def [dir-byte & step-bytes] (string/bytes op))
  (def dir
    (case (string/from-bytes dir-byte)
      "R" [1 0]
      "L" [-1 0]
      "U" [0 1]
      "D" [0 -1]
      (errorf "invalid direction in op: %s" op)))
  (def steps (scan-number (string/from-bytes ;step-bytes)))
  (assert (not (nil? steps)) (errorf "invalid step in op: %s" op))
  [dir steps])

(judge/test (scan-op "R994") [[1 0] 994])
(judge/test (scan-op "D213") [[0 -1] 213])
(judge/test (scan-op "L483") [[-1 0] 483])
(judge/test (scan-op "U102") [[0 1] 102])

(defn manhattan-dist [[up down]]
  (+ (math/abs up) (math/abs down)))

(judge/test (manhattan-dist [-1 -1]) 2)
(judge/test (manhattan-dist [3 3]) 6)

(defn point+ [[p1x p1y] [p2x p2y]]
  [(+ p1x p2x) (+ p1y p2y)])

(defn closest-intersection [wires p1]
  (var grid @{})
  (var min-dist math/int-max)
  (for wire-i 0 (length wires)
    (var p [0 0])
    (var wire-dist 0)
    (each [dir steps] (get wires wire-i)
      (for step-i 0 steps
        (+= wire-dist 1)
        (set p (point+ p dir))
        (def prev (get grid p))
        (def [prev-i prev-dist]
          (match prev
            [_ _] prev
            nil [nil wire-dist]))
        (def intersection? (and (not (nil? prev-i)) (not= prev-i wire-i)))
        (def sum-dist (if p1 (manhattan-dist p) (+ wire-dist prev-dist)))
        (if intersection? (set min-dist (min-of [min-dist sum-dist])))
        (put grid p [wire-i prev-dist])
        )))
  min-dist)

(defn lines-to-ops [lines]
  (map (fn [line] (map scan-op (string/split "," line))) lines))

(judge/test (closest-intersection [[] []] true) 9007199254740992)
(judge/test (closest-intersection (lines-to-ops ["U5,R5" "R5,U5"]) true) 10)
(judge/test (closest-intersection (lines-to-ops ["U1" "U5"]) true) 1)
(judge/test (closest-intersection
              (lines-to-ops ["R8,U5,L5,D3" "U7,R6,D4,L4"]) true) 6)
(judge/test (closest-intersection
              (lines-to-ops ["R8,U5,L5,D3" "U7,R6,D4,L4"]) false) 30)
(judge/test (closest-intersection
              (lines-to-ops
                ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                 "U62,R66,U55,R34,D71,R55,D58,R83"]) true) 159)
(judge/test (closest-intersection
              (lines-to-ops
                ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                 "U62,R66,U55,R34,D71,R55,D58,R83"]) false) 610)
(judge/test (closest-intersection
              (lines-to-ops
                ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]) true) 135)
(judge/test (closest-intersection
              (lines-to-ops
                ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]) false) 410)


(defn ops [file]
  (as-> (file/open file) $
        (file/read $ :all)
        (string/trim $)
        (string/split "\n" $)
        (lines-to-ops $)))

(judge/test (length (ops "./day03/input.txt")) 2)
(judge/test (get (get (ops "./day03/input.txt") 0) 0) [[1 0] 994])

(defn part1 [file] (closest-intersection (ops file) true))

(judge/test (part1 "./day03/input.txt") 293)

(defn part2 [file] (closest-intersection (ops file) false))

(judge/test (part2 "./day03/input.txt") 27306)
