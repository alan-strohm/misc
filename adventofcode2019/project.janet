(declare-project
  :name "adventofcode2019"
  :description "Solutions to Advent of Code 2019"
  :dependencies [{:url "https://github.com/ianthehenry/judge.git"
                  :tag "v2.7.0"}
                 {:url "https://github.com/andrewchambers/jfmt.git"}])

(declare-source
  :source "./day01/day01.janet")
