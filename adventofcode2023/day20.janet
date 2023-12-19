(import judge)

(use ./util)

(def test-input1 `
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a`)

(def peg
  (peg/compile
    ~{:broadcaster (* "broadcaster" (constant nil) (constant :bcast))
      :typed (* (/ '(set "%&") ,keyword) (/ ':a+ ,keyword))
      :module (+ :broadcaster :typed)
      :outs (group ,(sep ~(/ ':a+ ,keyword) ", "))
      :line (/ (* :module " -> " :outs) ,(fn [t n os] [n @{:type t :outs os}]))
      :lines (/ (group ,(sep ':line "\n")) ,from-pairs)
      :main (* :lines (any "\n") -1)}))

(judge/test (peg/match peg test-input1)
  @[@{:a @{:outs @[:b] :type :%}
      :b @{:outs @[:c] :type :%}
      :bcast @{:outs @[:a :b :c]}
      :c @{:outs @[:inv] :type :%}
      :inv @{:outs @[:a] :type :&}}])
