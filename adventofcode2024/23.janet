(use judge)
(use ./util)

(def real-input (slurp "./input/23.txt"))

(def test-input `
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
`)

(defn read [str]
  (def conns (peg/match '(split "\n" (+ -1 (group (* ':a+ "-" ':a+)))) str))
  (def result @{})
  (loop [[a b] :in conns
         conn :in [[a b] [b a]]]
    (put-in result conn true))
  result)

(defn part1 [str]
  (def connects (read str))
  (def cliques @{})
  (loop [[from tos] :pairs connects
         :when (string/has-prefix? "t" from)
         to1 :keys tos
         to2 :in (filter |(get-in connects [to1 $]) (keys tos))]
    (put cliques (tuple/slice (sort @[from to1 to2])) true))
  (length cliques))

(test (part1 test-input) 7)
(test (part1 real-input) 1330)

(defn key [arr] (tuple/slice (sort arr)))

(defmemo max-clique [connects already maybe]
  (when (empty? maybe) (break already))

  (var result [])
  (loop [m :in maybe]
    (def new-maybe (filter |(get-in connects [m $]) maybe))
    (def new-already (array ;already m))
    (def max-sub (max-clique connects (key new-already) (key new-maybe)))
    (when (> (length max-sub) (length result))
      (set result max-sub)))
  result)

(defn part2 [str]
  (def connects (read str))
  (def max-clique (max-clique connects @[] (keys connects)))
  (string/join max-clique ","))

(test (part2 test-input) "co,de,ka,ta")
# 25s
#(test (part2 real-input) "hl,io,ku,pk,ps,qq,sh,tx,ty,wq,xi,xj,yp")
