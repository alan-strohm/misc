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

(def peg ~{:main (* (/ :workflows ,table) "\n" (group :parts))
           :workflows (some (* ':a+ "{" (group :rules) "}\n"))
           :rules (* (any (* :rule ':a+ ",")) ':a+)
           :rule (group (* :category '1 (number :d+) ":"))
           :category (/ '(set "xmas") ,keyword)
           :parts (some (* "{" (/ :part ,table) "}\n"))
           :part (4 (* (? ",") :category "=" (number :d+)))
           })

(judge/test (peg/match peg test-input)
  @[@{"crn" @[@[:x ">" 2662] "A" "R"]
      "gd" @[@[:a ">" 3333] "R" "R"]
      "hdj" @[@[:m ">" 838] "A" "pv"]
      "in" @[@[:s "<" 1351] "px" "qqz"]
      "lnx" @[@[:m ">" 1548] "A" "A"]
      "pv" @[@[:a ">" 1716] "R" "A"]
      "px" @[@[:a "<" 2006]
             "qkq"
             @[:m ">" 2090]
             "A"
             "rfg"]
      "qkq" @[@[:x "<" 1416] "A" "crn"]
      "qqz" @[@[:s ">" 2770]
              "qs"
              @[:m "<" 1801]
              "hdj"
              "R"]
      "qs" @[@[:s ">" 3448] "A" "lnx"]
      "rfg" @[@[:s "<" 537]
              "gd"
              @[:x ">" 2440]
              "R"
              "A"]}
    @[@{:a 1222 :m 2655 :s 2876 :x 787}
      @{:a 2067 :m 44 :s 496 :x 1679}
      @{:a 79 :m 264 :s 2244 :x 2036}
      @{:a 466 :m 1339 :s 291 :x 2461}
      @{:a 2188 :m 1623 :s 1013 :x 2127}]])

(defn make-accept-fn [workflows]
  (def funcs @{})
  (defn accept? [name part] ((funcs name) part))

  (loop [[k vs] :pairs workflows]
    (def fun-ast (with-syms [$part]
      ~(fn [,$part]
         (cond ,;(seq [item :in vs]
                   (match item
                     [cat op arg] ~(,(symbol op) (,in ,$part ,cat) ,arg)
                     "A" true
                     "R" false
                     state ~(,accept? ,state ,$part)))))))
    (def compiled (compile fun-ast))
    (assert (function? compiled) (compiled :error))
    (put funcs k (compiled)))
  (funcs "in"))

(judge/test-error (make-accept-fn {"in" [[:x "foobar" 1000] "A" "R"]}) "unknown symbol foobar")

(defn accepted? [str part]
  (def [workflows _] (peg/match peg str))
  ((make-accept-fn workflows) part))

(judge/test (accepted? test-input {:a 1222 :m 2655 :s 2876 :x 787}) true)
(judge/test (accepted? real-input {:a 69 :m 785 :s 923 :x 549}) false)

(defn part1 [str]
  (def [workflows parts] (peg/match peg str))
  (->> parts
       (filter (make-accept-fn workflows))
       (map sum)
       sum))

(judge/test (part1 test-input) 19114)
(judge/test (part1 real-input) 342650)

(defn split [[l h] op arg]
  (case op
    "<" (if (<= h arg)
         [[l h] [0 0]]
         (if (< l arg)
           [[l arg] [arg h]]
           [[0 0] [l h]]))
    ">" (if (> l arg)
         [[l h] [0 0]]
         (if (> h arg)
           [[(inc arg) h] [l (inc arg)]]
           [[0 0] [l h]]))))

(judge/test (split [0 4000] "<" 2006) [[0 2006] [2006 4000]])
(judge/test (split [2006 4000] "<" 2006) [[0 0] [2006 4000]])
(judge/test (split [1000 2006] "<" 2006) [[1000 2006] [0 0]])
(judge/test (split [0 4000] ">" 1716) [[1717 4000] [0 1717]])
(judge/test (split [0 1716] ">" 1716) [[0 0] [0 1716]])
(judge/test (split [1717 3000] ">" 1716) [[1717 3000] [0 0]])

(defn apply-op [[cat op arg] ranges]
  (def [true-range false-range] (split (in ranges cat) op arg))
  (def result (table/clone ranges))
  (put ranges cat false-range)
  (put result cat true-range))

(defn range/len [[start end]] (- end start))

(defn num-combos [ranges]
  (->> ranges
       (map |(map range/len $))
       (map product)
       sum))

(defn accepted-ranges [workflow ranges state]
  (case state
    "A" (break [ranges])
    "R" (break []))

  (def false-ranges (table/clone ranges))
  (var true-ranges nil)
  (catseq [rule :in (partition 2 (in workflow state))]
    (match rule
      [condition state] (let [true-ranges (apply-op condition false-ranges)]
                          (accepted-ranges workflow true-ranges state))
      [default-state] (accepted-ranges workflow false-ranges default-state))))

(defn part2 [str]
  (def [workflows _] (peg/match peg str))
  (def ranges (accepted-ranges workflows @{:x [1 4001] :m [1 4001] :a [1 4001] :s [1 4001]} "in"))
  (num-combos ranges))

(judge/test (part2 test-input) 167409079868000)
(judge/test (part2 real-input) 130303473508222)
