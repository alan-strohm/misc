(import judge)

(def numbers ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def numbers-reverse (map string/reverse numbers))

(defn load [file]
  (as-> (file/open file) _
        (file/read _ :all)
        (string/trim _)
        (string/split "\n" _)))

(defn make-num-peg [nums]
  (def num-pats (map (fn [num str] ~(/ ,str ,num)) (range 1 (+ (length nums) 1)) nums))
  (peg/compile ~(any (+ (number :d) ,;num-pats :a))))

(defn sum-digits [strs peg place]
  (defn digit [str]
    (as-> (peg/match peg str) _
          (get _ 0)
          (* _ place)))
  (+ ;(map digit strs)))

(defn run [file forward-peg reverse-peg]
  (def nums (load file))
  (def tens (sum-digits nums forward-peg 10))
  (def nums-reversed (map string/reverse nums))
  (def ones (sum-digits nums-reversed reverse-peg 1))
  (+ tens ones))

(defn part1 [file]
  (def num-peg (peg/compile ~(any (+ (number :d) :a))))
  (run file num-peg num-peg))

(judge/test (part1 "./day01/input.txt") 54304)

(defn part2 [file]
  (run file (make-num-peg numbers) (make-num-peg numbers-reverse)))

(judge/test (part2 "./day01/input.txt") 54418)
(judge/test (part2 "./day01/example.txt") 281)
