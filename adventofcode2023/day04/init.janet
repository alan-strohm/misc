(import judge)

(def peg
  (peg/compile
    '{:card (* "Card" :s+ (constant :card-num) (number :d+) ":")
      :number-list (some (+ :s+ (number :d+)))
      :winning-numbers (* (constant :winning) :number-list)
      :have-numbers (* (constant :have) :number-list)
      :line (* (group :card) (group :winning-numbers) "|" (group :have-numbers))
      :main (some (group :line))}))

(defn load [file]
  (peg/match peg (slurp file)))

(judge/test (load "./day04/example.txt")
  @[@[@[:card-num 1]
      @[:winning 41 48 83 86 17]
      @[:have 83 86 6 31 17 9 48 53]]
    @[@[:card-num 2]
      @[:winning 13 32 20 16 61]
      @[:have 61 30 68 82 17 32 24 19]]
    @[@[:card-num 3]
      @[:winning 1 21 53 59 44]
      @[:have 69 82 63 72 16 21 14 1]]
    @[@[:card-num 4]
      @[:winning 41 92 73 84 69]
      @[:have 59 84 76 51 58 5 54 83]]
    @[@[:card-num 5]
      @[:winning 87 83 26 28 32]
      @[:have 88 30 70 12 93 22 82 36]]
    @[@[:card-num 6]
      @[:winning 31 18 13 56 72]
      @[:have 74 77 10 23 35 67 36 11]]])

(defn make-set [a]
  (reduce |(put $0 $1 true) @{} a))

(judge/test (make-set [1 2 3 4]) @{1 true 2 true 3 true 4 true})

(defn num-winning [card]
  (def winning (make-set (get card 1)))
  (def have (get card 2))
  (reduce |(+ $0 (if (get winning $1) 1 0)) 0 have))

(judge/test (num-winning @[@[:card-num 1]
                           @[:winning 41 48 83 86 17]
                           @[:have 83 86 6 31 17 9 48 53]]) 4)

(defn points [num-winning]
  (if (> num-winning 0)
    (math/exp2 (- num-winning 1))
    0))

(judge/test (points 0) 0)
(judge/test (points 1) 1)
(judge/test (points 2) 2)
(judge/test (points 3) 4)

(defn day1 [file]
  (sum (map |(points (num-winning $)) (load file))))

(judge/test (day1 "./day04/example.txt") 13)
(judge/test (day1 "./day04/input.txt") 28538)
