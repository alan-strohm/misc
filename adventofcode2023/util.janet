(import judge)

(defn sep [pat delim]
  ~(* ,pat (any (* ,delim ,pat)) (any ,delim)))

(judge/test (peg/match (sep '':a+ ",") "a,b,c") @["a" "b" "c"])
(judge/test (peg/match (sep '':a+ ",") "a") @["a"])
(judge/test (peg/match ~{:main ,(sep '':a+ ",")} "a") @["a"])

