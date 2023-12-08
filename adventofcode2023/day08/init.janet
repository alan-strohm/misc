(import judge)

(defn make-map [entries]
  (reduce |(put $0 (get $1 0) (array/slice $1 1)) @{} entries))

(def peg
  (peg/compile
    ~{:instructions (group (some (/ '(+ "L" "R") {"L" 0 "R" 1})))
      :map-entry (group (* ':a+ " = (" ':a+ ", " ':a+ ")\n"))
      :main (* :instructions :s+ (/ (group (some :map-entry )) ,make-map))}
      ))

(def example `
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ
`)

(judge/test (peg/match peg example)
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

(judge/test (run (peg/match peg example)) 6)

(defn day1 [file]
  (run (peg/match peg (slurp file))))

(judge/test (day1 "./day08/input.txt") 18727)


