(import judge)

(use ./util)

(def test-input `
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9`)

(def real-input (slurp "./input/22.txt"))

(def peg
  (peg/compile
    ~{:point (group ,(sep '(number :d+) ","))
      :line (/ ,(sep ':point "~") ,|{:lo $0 :hi $1})
      :main ,(sep ':line "\n")}))

(judge/test (peg/match peg test-input)
  @[{:hi @[1 2 1] :lo @[1 0 1]}
    {:hi @[2 0 2] :lo @[0 0 2]}
    {:hi @[2 2 3] :lo @[0 2 3]}
    {:hi @[0 2 4] :lo @[0 0 4]}
    {:hi @[2 2 5] :lo @[2 0 5]}
    {:hi @[2 1 6] :lo @[0 1 6]}
    {:hi @[1 1 9] :lo @[1 1 8]}])

(defn vec- [a b] (map - a b))
(judge/test (vec- [2 2] [1 1]) @[1 1])
(judge/test (vec- [3 2 1] [1 2 3]) @[2 0 -2])

(defn d2 [dn] (array/slice dn 0 2))
(judge/test (d2 [1 0 1]) @[1 0])

(defn overlaps? [dim {:lo l1 :hi h1} {:lo l2 :hi h2}]
  (all
    identity
    (seq [i :range [0 dim]]
      (and
        (>= (h1 i) (l2 i))
        (>= (h2 i) (l1 i))))))

(defn is-1d? [{:lo l :hi h}]
  (= 1 (count |(not= $ 0) (vec- h l))))

(judge/test (is-1d? {:lo [1 0 1] :hi [1 2 1]}) true)
(judge/test (is-1d? {:lo [1 0 1] :hi [1 2 2]}) false)

(judge/test (all is-1d? (peg/match peg test-input)) true)
(judge/test (all is-1d? (peg/match peg real-input)) false)

(defn make-index [f arr]
  (def ks (map f arr))
  (defn make-set [arr]
    (tabseq [item :in arr] item true))
  (tabseq [[k v] :pairs (group-by |(in ks $) (keys arr))]
          k (make-set v)))

(defn invert [ds]
  (def ret @{})
  (loop [[k vs] :pairs ds
         v :in vs]
    (if (nil? (in ret v))
      (put ret v @[]))
    (array/push (in ret v) k))
  ret)

(judge/test (invert {:a [:b :c]}) @{:b @[:a] :c @[:a]})

(defn load [str]
  (def by-id (peg/match peg str))
  (def by-hi-z (make-index |(get-in $ [:hi 2]) by-id))
  (def rests-on @{})

  (defn lower [id]
    (def hi-z (get-in by-id [id :hi 2]))
    (update-in by-id [id :lo 2] dec)
    (update-in by-id [id :hi 2] dec)
    (put-in by-hi-z [hi-z id] nil)
    (put-in by-hi-z [(dec hi-z) id] true))

  (defn find-supports [brick]
    (def lo-z (get-in brick [:lo 2]))
    (def supports
      (->> (in by-hi-z (dec lo-z) {})
           (keys)
           (map |[$ (in by-id $)])
           (filter |(overlaps? 2 brick ($ 1)))
           (map first)
           ))
    (cond
      (= 1 lo-z) [:ground]
      (empty? supports) nil
      supports))

  (eachp [id _] by-id
    (forever
      (def supports (find-supports (in by-id id)))
      (put rests-on id supports)
      (if supports (break) (lower id))))

  (def necessary @{})
  (each v rests-on
    (if (= 1 (length v))
      (put necessary (in v 0) true)))

  (length
    (filter (comp not necessary) (keys by-id))))

(judge/test (load real-input) 448)
