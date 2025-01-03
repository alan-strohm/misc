(use judge)
(use ./util)

(defn get-combo [regs n]
  (cond
    (and (>= n 0) (<= n 3)) n
    (and (>= n 4) (<= n 6)) (-> n (- 4) (["A" "B" "C"]) regs)
    (errorf "invalid operand: %n" n)))

(test (get-combo @{} 0) 0)
(test (get-combo @{} 3) 3)
(test (get-combo @{"A" :a} 4) :a)
(test (get-combo @{"C" :c} 6) :c)
(test-error (get-combo @{} :a) "invalid operand: :a")

(defn x-dv [output-reg regs operand]
  (def numerator (in regs "A"))
  (def denominator (math/exp2 (get-combo regs operand)))
  (put regs output-reg (div numerator denominator)))
(def adv (partial x-dv "A"))
(def bdv (partial x-dv "B"))
(def cdv (partial x-dv "C"))

(defn bst [regs operand] (put regs "B" (mod (get-combo regs operand) 8)))
(defn bxl [regs operand] (update regs "B" |(xor $ operand)))
(defn bxc [regs operand] (update regs "B" |(xor $ (in regs "C"))))

(defn run [regs opcodes]
  (var pc 0)
  (def output @[])

  (defn jnz [regs operand]
    (if (zero? (in regs "A"))
      (+= pc 2)
      (set pc operand)))

  (defn out [regs operand]
    (array/push output (mod (get-combo regs operand) 8)))

  (def instruction [adv bxl bst nil bxc out bdv cdv])

  (while (< pc (length opcodes))
    (def [opcode operand] [(in opcodes pc) (in opcodes (inc pc))])
    (case opcode
      3 (jnz regs operand)
      (do
        ((instruction opcode) regs operand)
        (+= pc 2))))
  [output regs])

(test (run @{"C" 9} [2 6]) [@[] @{"B" 1 "C" 9}])
(test (run @{"A" 10} [5 0 5 1 5 4]) [@[0 1 2] @{"A" 10}])
(test (run @{"A" 2024} [0 1 5 4 3 0]) [@[4 2 5 6 7 7 7 7 3 1 0] @{"A" 0}])
(test (run @{"B" 29} [1 7]) [@[] @{"B" 26}])
(test (run @{"B" 2024 "C" 43690} [4 0]) [@[] @{"B" 44354 "C" 43690}])

(test (run @{"A" 9 "B" 2} [7 5]) [@[] @{"A" 9 "B" 2 "C" 2}])

(defn read [str] (peg/match '(split "," (number :d)) str))
(defn part1 [regs ops]
  (def opcodes (read ops))
  (-> (run regs opcodes) first (|(map string $)) (string/join ",")))

(test (part1 @{"A" 729 "B" 0 "C" 0} "0,1,5,4,3,0")
  "4,6,3,5,6,3,5,2,1,0")
(test (part1 @{"A" 22571680 "B" 0 "C" 0} "2,4,1,3,7,5,0,3,4,3,1,5,5,5,3,0")
  "2,0,1,3,4,0,2,1,7")

(defn run-a [a prog]
  (first (run @{"A" a "B" 0 "C" 0} prog)))

(defn build-a [digits] (reduce |(-> $0 (* 8) (+ $1)) 0 digits))

(defn find-quine [prog digits i]
  (label result
    (when (< i 0)
      (return result (build-a digits)))
    (loop [digit :range-to [0 7]
           :before (array/push digits digit)
           :after (array/pop digits)
           :let (out (run-a (build-a digits) prog))
           :when (= (first out) (prog i))]
      (if-let [r (find-quine prog digits (dec i))]
        (return result r)))))

(defn part2 []
  (def prog (read "2,4,1,3,7,5,0,3,4,3,1,5,5,5,3,0"))
  (find-quine prog @[] (dec (length prog))))

(test (part2) 236580836040301)
