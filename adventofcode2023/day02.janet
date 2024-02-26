(import judge)

(use ./util)

(def test-input `
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`)

(def real-input (slurp "./input/02.txt"))

(def colors [:blue :red :green])

(def peg
  (peg/compile
    ~{:main ,(sep :line "\n")
      :line (/ (* :id ,(sep :round "; ")) ,|{:id $0 :rounds $&})
      :id (* "Game " (number :d+) ": ")
      :round (/ ,(sep :reveal ", ") ,|(table ;(reverse $&)))
      :reveal (* (number :d+) " " :color)
      :color (/ '(+ ,;(map string colors)) ,keyword)
      }))

(defn load [str]
  (peg/match peg str))

(judge/test
  (load test-input)
  @[{:id 1
     :rounds [@{:blue 3 :red 4}
              @{:blue 6 :green 2 :red 1}
              @{:green 2}]}
    {:id 2
     :rounds [@{:blue 1 :green 2}
              @{:blue 4 :green 3 :red 1}
              @{:blue 1 :green 1}]}
    {:id 3
     :rounds [@{:blue 6 :green 8 :red 20}
              @{:blue 5 :green 13 :red 4}
              @{:green 5 :red 1}]}
    {:id 4
     :rounds [@{:blue 6 :green 1 :red 3}
              @{:green 3 :red 6}
              @{:blue 15 :green 3 :red 14}]}
    {:id 5
     :rounds [@{:blue 1 :green 3 :red 6}
              @{:blue 2 :green 2 :red 1}]}])

(defn possible? [sets maxes]
  (var result true)
  (loop [s :in sets :while result
         c :in colors :while result
         :let [cnt (in s c 0)
               max (in maxes c)]]
    (if (> cnt max) (set result false)))
  result)
      
(judge/test (possible? [{:blue 2}] {:blue 1}) false)
(judge/test (possible? [{:red 2}] {:red 3}) true)
(judge/test (possible? [{:red 2}] {:red 1 :blue 2}) false)
(judge/test (possible? [{:red 2}] {:red 2 :blue 1}) true)

(defn part1 [str]
  (def maxes {:red 12 :green 13 :blue 14})
  (sum (seq [{:id id :rounds rounds} :in (load str)
             :when (possible? rounds maxes)]
         id)))

(judge/test (part1 real-input) 2149)

(defn merge-with [f x & others]
  (def result (from-pairs (pairs x)))
  (each t others
    (eachp [k v1] t
      (if-let [v2 (in result k)]
        (put result k (f v1 v2))
        (put result k v1))))
  result)

(judge/test (merge-with max {:a 1} {:b 2}) @{:a 1 :b 2})


(defn power [sets]
  (def maxes @{:red 0 :green 0 :blue 0})
  (var total 1)
  (each color colors
    (each set_ sets
      (def count_ (in set_ color 0))
      (if (> count_ (get maxes color)) (put maxes color count_)))
    (*= total (get maxes color)))
  total)

(judge/test (power (get-in (load test-input) [0 :rounds])) 48)

(defn part2 [str]
  (def games (load str))
  (var total 0)
  (each game games
    (+= total (power (get game :rounds))))
  total)

(judge/test (part2 real-input) 71274)
