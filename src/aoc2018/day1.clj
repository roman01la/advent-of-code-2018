(ns aoc2018.day1)

(def input
  (->> (slurp "resources/aoc2018/day1.txt")
       clojure.string/split-lines
       (map read-string)))

(defn part-1 []
  (reduce + 0 input))

(defn part-2 []
  (->> (cycle input)
       (reductions + 0)
       (reduce #(if (%1 %2) (reduced %2) (conj %1 %2)) #{})))
