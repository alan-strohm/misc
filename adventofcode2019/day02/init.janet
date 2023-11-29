#!/usr/bin/env janet

(import judge)

(defn nums [file]
  (as-> (file/open file) $
        (file/read $ :line)
        (string/trim $)
        (string/split "," $)
        (map scan-number $)))


(judge/test (length (nums "./day02/input.txt")) 153)
(judge/test (get (nums "./day02/input.txt") 152) 0)
(judge/test (get (nums "./day02/input.txt") 0) 1)

(defn doOp [op nums i]
  (def left (get nums (get nums (+ i 1))))
  (def right (get nums (get nums (+ i 2))))
  (def out (get nums (+ i 3)))
  (def result (op left right))
  (put nums out result)
  nums)

(judge/test (doOp + @[0 1 2 0] 0) @[3 1 2 0])

(defn run [nums]
  (var i 0)
  (while (not= 99 (get nums i))
    (def opcode (get nums i))
    (def op (case opcode
              1 +
              2 *
              (assert false (string "invalid opcode: " i))))
    (doOp op nums i)
    (+= i 4))
  nums)

(judge/test (run @[1 1 2 0 99]) @[3 1 2 0 99])
(judge/test (run @[2 1 2 0 99]) @[2 1 2 0 99])
(judge/test (run @[99 1]) @[99 1])
(judge/test (run @[2 4 4 5 99 0]) @[2 4 4 5 99 9801])

(defn make-mem [nums noun verb]
  (as-> (array/new (length nums)) $
        (array/insert $ 0 ;nums)
        (put $ 1 noun)
        (put $ 2 verb)))

(defn part1 [file]
  (as-> (nums file) $
        (make-mem $ 12 2)
        (run $)
        (get $ 0)))

(judge/test (part1 "./day02/input.txt") 3562672)

(defn part2 [file]
  (def input (nums file))
  (var res -1)
  (each noun (range 0 99)
    (each verb (range 0 99)
      (as-> (make-mem input noun verb) $
            (run $)
            (get $ 0)
            (if (= $ 19690720)
              (do (set res (+ (* 100 noun) verb)) (break)))))
    (if (not= res -1) (break)))
  res)

(judge/test (part2 "./day02/input.txt") 8250)
