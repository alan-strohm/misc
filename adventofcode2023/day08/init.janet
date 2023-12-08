(import judge)

(defn make-map [entries]
  (reduce |(put $0 (get $1 0) (array/slice $1 1)) @{} entries))

(def peg
  (peg/compile
    ~{:instructions (group (some (/ '(+ "L" "R") {"L" 0 "R" 1})))
      :map-entry (group (* ':w+ " = (" ':w+ ", " ':w+ ")\n"))
      :main (* :instructions :s+ (/ (group (some :map-entry )) ,make-map))}
      ))

(def example1 `
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ
`)

(judge/test (peg/match peg example1)
  @[@[0 0 1]
    @{"AAA" @["BBB" "BBB"]
      "BBB" @["AAA" "ZZZ"]}])

(defn run [[insts network]]
  (var steps 0)
  (var node "AAA")
  (var idx 0)
  (while (not= node "ZZZ")
    (set node (get-in network [node (get insts idx)]))
    (set idx (% (+ 1 idx) (length insts)))
    (++ steps))
  steps)

(judge/test (run (peg/match peg example1)) 6)

(defn part1 [file]
  (run (peg/match peg (slurp file))))

(judge/test (part1 "./day08/input.txt") 18727)

(def example2 `
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

(defn find-starts [network]
  (filter |(string/has-suffix? "A" $) (keys network)))

(judge/test (find-starts (get (peg/match peg example2) 1)) @["11A" "22A"])

(defn run2 [[insts network]]
  (var steps 0)
  (var nodes (find-starts network))
  (var inst-i 0)
  (defn next-step []
    (if (= (% steps 1000000) 0) (print (string/format "on step %d" steps)))
    (set inst-i (% (+ 1 inst-i) (length insts)))
    (++ steps))
  (loop [done :iterate (not (all |(string/has-suffix? "Z" $) nodes))
         :after (next-step)
         node-i :range [0 (length nodes)]]
    (put nodes node-i (get-in network [(get nodes node-i) (get insts inst-i)]))
    )
  steps)

(defn run2 [[insts network]]
  (var nodes (find-starts network))
  (var steps-per-node @[])
  (var inst-i 0)
  (var steps 0)
  (defn next-node [node-i]
    (put steps-per-node node-i steps)
    (set inst-i 0)
    (set steps 0))
  (loop [node-i :range [0 (length nodes)]
         :after (next-node node-i)
         done :iterate (not (string/has-suffix? "Z" (get nodes node-i)))]
    (put nodes node-i (get-in network [(get nodes node-i) (get insts inst-i)]))
    (set inst-i (% (+ 1 inst-i) (length insts)))
    (++ steps))
  (reduce2 |(math/lcm $0 $1) steps-per-node))


(judge/test (run2 (peg/match peg example2)) 6)

(defn part2 [file]
  (run2 (peg/match peg (slurp file))))

(judge/test (part2 "./day08/input.txt") 18024643846273)
