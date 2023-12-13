(import judge)

(def example `
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
`)

(def input-peg
  (peg/compile
    '{:record '(some (set ".?#"))
      :check (group (some (+ (number :d+) ",")))
      :line (* :record " "  :check (+ "\n" -1))
      :main (some (group :line))}))

(defn load [str] (peg/match input-peg str))

(judge/test (load "???.### 1,1,3") @[@["???.###" @[1 1 3]]])
(judge/test (load example)
  @[@["???.###" @[1 1 3]]
    @[".??..??...?##." @[1 1 3]]
    @["?#?#?#?#?#?#?#?" @[1 3 1 6]]
    @["????.#...#..." @[4 1 1]]
    @["????.######..#####." @[1 6 5]]
    @["?###????????" @[3 2 1]]])

(defn consume-group [str len] 
  (match (peg/match ~(* (,len (set "#?")) (+ (set "?.") -1) '(any 1)) str)
    [result] result
    [] ""
    nil))

(judge/test (consume-group "##" 3) nil)
(judge/test (consume-group "###?" 3) "")

(defn num-arrangements [str lens]
  (var q @[])
  (var result 0)
  (array/push q [str lens])
  (loop [[str lens] :in q
         :let [str (string/trim str ".")]]
    (if (= 0 (length lens))
      (if (string/find "#" str) nil (++ result))
      (case (get str 0)
        nil nil
        (chr "#") (match (consume-group str (get lens 0))
                    nil nil
                    new (array/push q [new (array/slice lens 1)]))
        (chr ".") (array/push q [(string/slice str 1) lens])
        (chr "?") (do
                    (array/push q [(string/replace "?" "." str) lens])
                    (array/push q [(string/replace "?" "#" str) lens])))
      ))
  result)


(judge/test (num-arrangements "???.###" [1 1 3]) 1)
(judge/test (num-arrangements ".??..??...?##." [1 1 3]) 4)
(judge/test (num-arrangements "?###????????" [3 2 1]) 10)
(judge/test (num-arrangements "?#?#?#?#?#?#?#?" [1 3 1 6]) 1)
(judge/test (num-arrangements "????.######..#####." [1 6 5]) 4)

(defn part1 [str]
  (sum (map |(num-arrangements ;$) (load str))))

(judge/test (part1 example) 21)
(judge/test (part1 (slurp "./day12/input.txt")) 7792)

(defn multiply [str lens]
  (print str)
  [(string/join (map (fn [_] str) (range 5)) "?")
   (flatten (array/concat (map (fn [_] lens) (range 5))))])
(judge/test (multiply ".#" [1]) [".#?.#?.#?.#?.#" @[1 1 1 1 1]])

#(judge/test (num-arrangements ;(multiply "????.######..#####." [1 6 5])) 2500)
#(judge/test (num-arrangements ;(multiply "?###????????" [3 2 1])) 506250)

(defn part2 [str]
  (->> str (load) (map |(num-arrangements ;(multiply ;$))) (sum)))
#(judge/test (part2 (slurp "./day12/input.txt")) 7792)

# TODO, maybe:
# 1) A function to find the most conservative solution (the one closest to the start)
# 2) Run the function, reverse the lists and run it again to find the two
# extremes and how they map to regions of ?
# 3) Within the regions of ?, compute the options between the two extremes.
