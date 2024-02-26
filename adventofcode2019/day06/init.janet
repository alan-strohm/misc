(import judge)

(defn load [file]
  (as-> (file/open file) _
        (file/read _ :all)
        (string/trim _)
        (string/split "\n" _)
        (map |(string/split ")" $) _)
        (from-pairs _)
        ))

(load "./day06/input.txt")

