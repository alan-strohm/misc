(import judge)

(use ../util)

(def test-input `
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
`)

(def real-input (slurp "./day03/input.txt"))

(def peg
  (peg/compile
    ~{:main (some (+ :nothing (group :number) (group :symbol)))
      :nothing (+ "." "\n")
      :number (* (constant :number) (column) (line) (number :d+) (/ (column) ,dec))
      :symbol (* (constant :symbol) (column) (line) '1)
      }))

(judge/test (peg/match peg test-input)
  @[@[:number 1 1 467 3]
    @[:number 6 1 114 8]
    @[:symbol 4 2 "*"]
    @[:number 3 3 35 4]
    @[:number 7 3 633 9]
    @[:symbol 7 4 "#"]
    @[:number 1 5 617 3]
    @[:symbol 4 5 "*"]
    @[:symbol 6 6 "+"]
    @[:number 8 6 58 9]
    @[:number 3 7 592 5]
    @[:number 7 8 755 9]
    @[:symbol 4 9 "$"]
    @[:symbol 6 9 "*"]
    @[:number 2 10 664 4]
    @[:number 6 10 598 8]])

(defn neighbors [[lx ly] [hx hy]]
  (seq [x :range-to [(dec lx) (inc hx)]
        y :range-to [(dec ly) (inc hy)]
        :when (or (< x lx) (> x hx) (< y ly) (> y hy))]
    [x y]))

(judge/test (neighbors [6 1] [8 1])
  @[[5 0]
    [5 1]
    [5 2]
    [6 0]
    [6 2]
    [7 0]
    [7 2]
    [8 0]
    [8 2]
    [9 0]
    [9 1]
    [9 2]])

(defn part1 [input]
  (var symbols @{})
  (var parts @[])
  (loop [item :in (peg/match peg input)]
    (match item
      [:symbol col line sym] (put symbols [col line] sym)
      [:number first-col line num last-col] (array/push parts {:number num :lo [first-col line] :hi [last-col line]})
      ))
  (sum-loop [{:number num :lo lo :hi hi} :in parts
             :when (some |(in symbols $) (neighbors lo hi))]
            num))

(judge/test (part1 test-input) 4361)
(judge/test (part1 real-input) 535078)

(defn part2 [input]
  (var gears @[])
  (var parts @{})
  (loop [item :in (peg/match peg input)]
    (match item
      [:symbol col line "*"] (array/push gears [col line])
      [:number first-col line num last-col] (loop [col :range-to [first-col last-col]]
                                              (put parts [col line] num))
      ))

  (defn neighbor-parts [p]
    (distinct (seq [n :in (neighbors p p)
                    :let [num (in parts n)]
                    :when num]
                num)))

  (sum-loop [p :in gears
             :let [parts (neighbor-parts p)]
             :when (= 2 (length parts))]
            (product parts)))

(judge/test (part2 test-input) 467835)
(judge/test (part2 real-input) 75312571)
