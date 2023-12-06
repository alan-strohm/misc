(import judge)

(defn make-part [id-str]
  @{:id (scan-number id-str) :length (length id-str)})

(def peg
  (peg/compile
    '{:nothing (+ "." "\n")
      :number (group (* (constant :number) (column) (line) (number :d+) (column)))
      :symbol (group (* (constant :symbol) (column) (line) '1))
      :main (some (+ :nothing :number :symbol))}))

(defn load [file] (peg/match peg (slurp file)))

(defn map-symbols [parsed]
  (defn acc [map_ row]
    (match row [:symbol x y c] (put map_ [x y] @{:char c :parts @{}}) map_))
  (reduce acc @{} parsed))

(judge/test (load "./day03/example.txt")
            @[@[:number 1 1 467 4]
              @[:number 6 1 114 9]
              @[:symbol 4 2 "*"]
              @[:number 3 3 35 5]
              @[:number 7 3 633 10]
              @[:symbol 7 4 "#"]
              @[:number 1 5 617 4]
              @[:symbol 4 5 "*"]
              @[:symbol 6 6 "+"]
              @[:number 8 6 58 10]
              @[:number 3 7 592 6]
              @[:number 7 9 755 10]
              @[:symbol 4 10 "$"]
              @[:symbol 6 10 "*"]
              @[:number 2 11 664 5]
              @[:number 6 11 598 9]])

(defn point+ [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
(def dirs [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn map-parts [parsed symbol-map]
  (defn acc [total row]
    (match row [:number x1 y id x2]
      (do
        (var valid? false)
        (for x x1 x2
          (each dir dirs
            (match (get symbol-map (point+ [x y] dir))
              nil nil
              result (do
                       (set valid? true)
                       (put (result :parts) id true)))))
        (if valid? (+ total id) total))
      total))
  (reduce acc 0 parsed))

(defn day1 [file]
  (def parsed (load file))
  (def symbol-map (map-symbols parsed))
  (map-parts parsed symbol-map))

(judge/test (day1 "./day03/example.txt") 4361)
(judge/test (day1 "./day03/input.txt") 535078)

(defn day2 [file]
  (def parsed (load file))
  (def symbol-map (map-symbols parsed))
  (map-parts parsed symbol-map)
  (reduce (fn [total {:parts parts}]
            (if (= 2 (length parts))
              (+ total (* ;(keys parts)))
              total))
          0 symbol-map))

(judge/test (day2 "./day03/example.txt") 467835)
(judge/test (day2 "./day03/input.txt") 75312571)
