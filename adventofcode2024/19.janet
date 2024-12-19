(use judge)
(use ./util)

(def real-input (slurp "./input/19.txt"))

(def test-input `
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
`)

(defn read [str]
  (def peg '{:main (* (group :towels) (group :designs))
             :towels (sub (thru "\n\n") (split ", " ':a+))
             :designs (split "\n" (+ -1 ':a+))})
  (peg/match peg str))

(test (read test-input)
  @[@["r" "wr" "b" "g" "bwu" "rb" "gb" "br"]
    @["brwrr"
      "bggr"
      "gbbr"
      "rrbgbr"
      "ubwu"
      "bwurrg"
      "brgr"
      "bbrgwb"]])

(defn remove-prefix [pfx str]
  (if (string/has-prefix? pfx str)
    (string/slice str (length pfx))
    nil))

(defn possible? [towels design]
  (label result
    (when (empty? design) (return result true))
    (loop [towel :in towels :let [trimmed (remove-prefix towel design)]
           :when (and trimmed (possible? towels trimmed))]
      (return result true))
    false))

(deftest possible?
  (def [towels _] (read test-input))
  (test (possible? towels "brwrr") true)
  (test (possible? towels "bbrgwb") false))

(defn part1 [str]
  (def [towels designs] (read str))
  (sum-loop [design :in designs :when (possible? towels design)]
            1))

(test (part1 test-input) 6)
(test (part1 real-input) 247)

(defmemo num-ways [towels design]
  (label result
    (when (empty? design) (return result 1))
    (sum-loop [towel :in towels :let [trimmed (remove-prefix towel design)]
               :when trimmed]
              (num-ways towels trimmed))))

(deftest num-ways
  (def [towels _] (read test-input))
  (test (num-ways towels "brwrr") 2)
  (test (num-ways towels "rrbgbr") 6)
  (test (num-ways towels "bbrgwb") 0))

(defn part2 [str]
  (def [towels designs] (read str))
  (sum-loop [[i design] :pairs designs]
            (num-ways towels design)))

(test (part2 test-input) 16)
(test (part2 real-input) 692596560138745)
