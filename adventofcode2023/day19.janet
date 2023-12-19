(import judge)

(use ./util)

(def test-input `
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}

`)
(def real-input (slurp "./input/19.txt"))

(def peg
  (peg/compile
    ~{:cat (/ '(set "xmas") ,keyword)
      :cond (* :cat (/ '1 ,symbol) (number :d+) ":")
      :rule (* (? (group :cond)) ':a+)
      :workflow (* ':a+ "{" (group ,(sep ':rule ",")) "}" "\n")
      :workflows (/ (group (some (group :workflow))) ,from-pairs)
      :rating (group (* :cat "=" (number :d+)))
      :ratings (* "{" ,(sep :rating ",") "}" "\n")
      :part (/ (group :ratings) ,from-pairs)
      :main (* :workflows "\n" (group (some :part)) -1)}
    ))

(defn terminal? [state]
  (or
    (= state "R")
    (= state "A")))

(defn next-state [rules part]
  (defn make-case [rule]
    (match rule
      [cat op arg] ~(,op (,part ,cat) ,arg)
      state state))
  ((compile ~(cond ,;(map make-case rules)))))

(defn accepted? [workflows part]
  (var state "in")
  (while (not (terminal? state))
    (set state (next-state (workflows state) part)))
  (= state "A"))

(defn part1 [str]
  (def [workflows parts] (peg/match peg str))
  (->> parts
       (filter |(accepted? workflows $))
       (map sum)
       (sum))
  )

(judge/test (part1 test-input) 19114)
(judge/test (part1 real-input) 342650)

(defn split [[l h] [_ op arg]]
  (case op
    '< (if (< arg h)
         (if (< arg l)
           [[0 0] [l h]]
           [[l (dec arg)] [arg h]])
         [[l h] [0 0]])
    '> (if (> arg l)
         (if (> arg h)
           [[0 0] [l h]]
           [[(inc arg) h] [l arg]])
         [[l h] [0 0]])))

(judge/test (split [0 4000] [:a '< 2006]) [[0 2005] [2006 4000]])
(judge/test (split [3000 4000] [:a '< 2006]) [[0 0] [3000 4000]])
(judge/test (split [1000 2000] [:a '< 2006]) [[1000 2000] [0 0]])
(judge/test (split [0 4000] [:a '> 1716]) [[1717 4000] [0 1716]])
(judge/test (split [0 1000] [:a '> 1716]) [[0 0] [0 1000]])
(judge/test (split [2000 3000] [:a '> 1716]) [[2000 3000] [0 0]])

(defn intersect [[l1 h1] [l2 h2]]
  (if (and
        (>= h1 l2)
        (<= l1 h2))
    [(max l1 l2) (min h1 h2)]
    [0 0]))

(judge/test (intersect [1 2] [0 1]) [1 1])
(judge/test (intersect [0 1] [1 2]) [1 1])
(judge/test (intersect [1 1] [0 3]) [1 1])
(judge/test (intersect [1 1] [2 2]) [0 0])
(judge/test (intersect [2 2] [1 1]) [0 0])
(judge/test (intersect [0 3] [1 1]) [1 1])

(defn range/intersect [r1 r2]
  (def r3 (table/clone r1))
  (eachp [k v2] r2
    (put r3 k
         (if-let [v1 (r3 k)]
           (intersect v1 v2)
           v2)))
  r3)

(judge/test (range/intersect @{:a [1 2]} @{:a [0 1]}) @{:a [1 1]})
(judge/test (range/intersect @{:a [1 2]} @{:b [0 1]}) @{:a [1 2] :b [0 1]})

(defn num-combos [ranges]
  (defn sum-combos [ranges]
    (->> ranges
         (map |(map (fn [[l h]] (inc (- h l))) $))
         (map product)
         (sum)))
  (def overlaps
    (seq [[i r1] :pairs ranges
          r2 :in (array/slice ranges (inc i))]
      (range/intersect r1 r2)))
  (- (sum-combos ranges) (sum-combos overlaps)))

(judge/test (num-combos [@{:a [1 10] :b [1 10]}]) 100)
(judge/test (num-combos [@{:a [1 10]} @{:a [1 10]}]) 10)
(judge/test (num-combos [@{:a [1 10]} @{:a [5 15]}]) 15)
(judge/test (num-combos [@{:a [1 1] :b [1 1]} @{:a [1 1]}]) 1)
(judge/test (num-combos [@{:a [1 2] :b [1 2]}]) 4)

(defn part2 [str]
  (def [workflows _] (peg/match peg str))
  (def accept-ranges @[])
  (def q @[["in" @{:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}]])

  (defn visit-workflow [workflow ranges]
    (def false-ranges (table/clone ranges))
    (var true-ranges nil) 
    (each rule workflow
      (match rule
        [cat _ _] (do
                    (def [new-true new-false] (split (in false-ranges cat) rule))
                    (put false-ranges cat new-false)
                    (set true-ranges (table/clone ranges))
                    (put true-ranges cat new-true))
        state (if true-ranges
                (do
                  (array/push q [state true-ranges])
                  (set true-ranges nil))
                (array/push q [state false-ranges])))))

  (while (not (empty? q))
    (def [state ranges] (array/pop q))
    (case state
      "R" nil
      "A" (array/push accept-ranges ranges)
      (do
        (visit-workflow (workflows state) ranges))
      )
    )
  (num-combos accept-ranges))

#(judge/test (part2 test-input) 167409079868000)
(judge/test (part2 test-input) 173601427943866)
