(import judge)

(def colors ["blue" "red" "green"])

(def game-peg
  (peg/compile
    ~{:main (* :game (group (* :set (any (* "; " :set)))) -1)
      :one-color-reversed (+ ,;(map (fn [color] ~(* (number (some :d)) " " (<- ,color))) colors))
      :one-color (/ (group :one-color-reversed) ,reverse)
      :set-array (* :one-color (between 0 2 (* ", " :one-color)))
      :set (/ (group :set-array) ,from-pairs)
      :game (* "Game " (number (some :d)) ": ")
      }))

(defn parse-game [str]
  (peg/match game-peg str))

(judge/test (parse-game "Game 100: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  @[100
    @[@{"blue" 3 "red" 4}
      @{"blue" 6 "green" 2 "red" 1}
      @{"green" 2}]])

(defn load [file]
  (as-> (file/open file) _
        (file/read _ :all)
        (string/trim _)
        (string/split "\n" _)
        (map parse-game _)))

(defn possible? [sets maxes]
  (label result
         (each set_ sets
           (each color colors
             (def count_ (|(if (nil? $) 0 $) (get set_ color)))
             (if (> count_ (get maxes color)) (return result false))
             ))
         (return result true)))

(judge/test (possible? [{"blue" 2}] {"blue" 1}) false)
(judge/test (possible? [{"red" 2}] {"red" 3}) true)
(judge/test (possible? [{"red" 2}] {"red" 1 "blue" 2}) false)
(judge/test (possible? [{"red" 2}] {"red" 2 "blue" 1}) true)

(defn part1 [file]
  (def games (load file))
  (def maxes {"red" 12 "green" 13 "blue" 14})
  (var total 0)
  (each [id sets] games
    (if (possible? sets maxes) (+= total id)))
  total)

(judge/test (part1 "./day02/input.txt") 2149)

(defn power [sets]
  (def maxes (from-pairs (map (fn [$] [$ 0]) colors)))
  (var total 1)
  (each color colors
    (each set_ sets
      (def count_ (|(if (nil? $) 0 $) (get set_ color)))
      (if (> count_ (get maxes color)) (put maxes color count_)))
    (*= total (get maxes color)))
  total)

(judge/test (power (get (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") 1)) 48)

(defn part2 [file]
  (def games (load file))
  (var total 0)
  (each game games
    (+= total (power (get game 1))))
  total)

(judge/test (part2 "./day02/input.txt") 71274)
