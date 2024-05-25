(import judge)

(def test-input1 `
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ
`)

(def test-input2 `
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
`)

(def real-input (slurp "./day08/input.txt"))

(def peg ~{:main (* :instructions :s+ (/ (some :map-entry) ,table))
           :instructions (group (some (+ (/ "L" 0) (/ "R" 1))))
           :map-entry (* ':w+ " = (" (group (* ':w+ ", " ':w+)) ")\n")})

(judge/test (peg/match peg test-input1)
  @[@[0 0 1]
    @{"AAA" @["BBB" "BBB"]
      "BBB" @["AAA" "ZZZ"]}])

(defn count-steps [start done? instructions network]
  (var steps 0)
  (var node start)
  (while (not (done? node))
    (def next-dir (in instructions (% steps (length instructions))))
    (set node (get-in network [node next-dir]))
    (++ steps))
  steps)

(defn part1 [str] (count-steps "AAA" |(= "ZZZ" $) ;(peg/match peg str)))

(judge/test (part1 test-input1) 6)
(judge/test (part1 real-input) 18727)

(defn find-starts [network]
  (filter |(string/has-suffix? "A" $) (keys network)))

(judge/test (find-starts (last (peg/match peg test-input2))) @["11A" "22A"])

(defn part2 [str]
  (def [instructions network] (peg/match peg str))
  (def done? |(string/has-suffix? "Z" $))
  (->> (find-starts network)
       (map |(count-steps $ done? instructions network))
       (reduce2 |(math/lcm $0 $1))))

(judge/test (part2 test-input2) 6)
(judge/test (part2 real-input) 18024643846273)
