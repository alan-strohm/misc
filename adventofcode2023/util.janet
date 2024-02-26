(import judge)

(defn sep [pat delim]
  ~(* ,pat (any (* ,delim ,pat)) (any ,delim)))

(judge/test (peg/match (sep '':a+ ",") "a,b,c") @["a" "b" "c"])
(judge/test (peg/match (sep '':a+ ",") "a") @["a"])
(judge/test (peg/match ~{:main ,(sep '':a+ ",")} "a") @["a"])

(defn vec+ [a & rest] (tuple/slice (map + a ;rest)))
(judge/test (vec+ [1 2] [3 4]) [4 6])
(defmacro vec+= [v1 v2] ~(set ,v1 (,vec+ ,v1 ,v2)))

(defn vec- [a & rest] (tuple/slice (map - a ;rest)))

(defn vec*n [a n] (tuple/slice (map |(* n $) a)))
(judge/test (vec*n [1 2] -1) [-1 -2])

(defn grid/parse [str]
  (def contents (->> str (string/trim) (string/split "\n")))
  {:contents contents
   :size [(length contents) (length (first contents))]})

(defn grid/get [grid [c l]]
  (get-in grid [:contents l c]))

(def dirs4 [[0 -1] [1 0] [0 1] [-1 0]])
(def [dir-N dir-E dir-S dir-W] dirs4)
