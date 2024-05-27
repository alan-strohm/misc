(import judge)

(use ../util)

(def test-input `
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green`)

(def real-input (string/trim (slurp "./day02/input.txt")))

(def colors [:blue :red :green])

(defn rgb [reveal]
  [(in reveal :red 0)
   (in reveal :green 0)
   (in reveal :blue 0)])

(def peg
  (peg/compile
    ~{:main (split "\n" :line)
      :line (/ (* :id (split "; " :round)) ,|{:id $0 :rounds $&})
      :id (* "Game " (number :d+) ": ")
      :round (/ (split ", " :reveal) ,|(rgb (table ;(reverse $&))))
      :reveal (* (number :d+) " " (/ ':w+ ,keyword))
      }))

(judge/test
  (peg/match peg test-input)
  @[{:id 1
     :rounds [[4 0 3] [1 2 6] [0 2 0]]}
    {:id 2
     :rounds [[0 2 1] [1 3 4] [0 1 1]]}
    {:id 3
     :rounds [[20 8 6] [4 13 5] [1 5 0]]}
    {:id 4
     :rounds [[3 1 6] [6 3 0] [14 3 15]]}
    {:id 5 :rounds [[6 3 1] [1 2 2]]}])

(defn part1 [str]
  (sum-loop [{:id id :rounds rounds} :in (peg/match peg str)
             :let [maxes (map max ;rounds)]
             :when (all <= maxes [12 13 14])]
            id))

(judge/test (part1 test-input) 8)
(judge/test (part1 real-input) 2149)


(defn part2 [str]
  (sum-loop [{:id id :rounds rounds} :in (peg/match peg str)
             :let [maxes (map max ;rounds)]]
            (product maxes)))

(judge/test (part2 test-input) 2286)
(judge/test (part2 real-input) 71274)
