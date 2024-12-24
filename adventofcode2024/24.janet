(use judge)
(use ./util)

(def real-input (slurp "./input/24.txt"))

(def test-input1 `
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
`)

(defn read [str]
  (def peg ~{:main (* (/ (group :inits) ,from-pairs) (group :gates))
             :inits (sub (to "\n\n") (split "\n" (group :init)))
             :init (* ':w+ ": " ':d)
             :gates (split "\n" (+ -1 (group :gate)))
             :gate (* ':w+ " " ':w+ " " ':w+ " -> " ':w+) })
  (peg/match peg str))

(test (read test-input1)
  @[@{"x00" "1"
      "x01" "1"
      "x02" "1"
      "y00" "0"
      "y01" "1"
      "y02" "0"}
    @[@["x00" "AND" "y00" "z00"]
      @["x01" "XOR" "y01" "z01"]
      @["x02" "OR" "y02" "z02"]]])

(def test-input2 `
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
`)

(defn part1 [str]
  (def [states gates] (read str))
  (def by-input @{})
  (loop [g :in gates
         :let [[in1 _ in2] g]
         input :in [in1 in2]]
    (update by-input input |(array/push (or $ @[]) g)))

  (var result 0)
  (defn set-digit [wire]
    (when (= "1" (in states wire))
      (def place (-> wire (string/triml "z") scan-number))
      (+= result (math/exp2 place))))

  (def to-process (tabseq [gate :in gates] gate true))
  (loop [_ :iterate (not (empty? to-process))
         gate :keys to-process
         :let [[in1-id gate-type in2-id out-id] gate
               [in1 in2] [(in states in1-id) (in states in2-id)]]
         :when (and in1 in2)]
    (put to-process gate nil)
    (put states out-id
         (case gate-type
           "AND" (if (and (= "1" in1) (= "1" in2)) "1" "0")
           "XOR" (if (not= in1 in2) "1" "0")
           "OR" (if (or (= "1" in1) (= "1" in2)) "1" "0")
           (errorf "unexpected gate type: %n" gate-type)))
    (when (string/has-prefix? "z" out-id)
      (set-digit out-id)))
  result)

(test (part1 test-input1) 4)
(test (part1 test-input2) 2024)
(test (part1 real-input) 49430469426918)
