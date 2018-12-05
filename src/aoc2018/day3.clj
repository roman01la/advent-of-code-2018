(ns aoc2018.day3)

(defn read-id [s]
  (->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)
       rest
       (map read-string)
       (zipmap [:id :x :y :w :h])))

(def input
  (->> (slurp "resources/aoc2018/day3.txt")
       clojure.string/split-lines
       (map read-id)))

(defn rect->area [{:keys [x y w h]}]
  (for [x (range x (+ x w))
        y (range y (+ y h))]
    [x y]))

(defn add-rect [canvas rect]
  (->> (rect->area rect)
       (reduce
         (fn [canvas point]
           (update canvas point conj (:id rect)))
         canvas)))

(defn overlapping? [ids]
  (> (count ids) 1))

(defn overlapping-areas [input]
  (->> (reduce add-rect {} input)
       vals
       (filter overlapping?)))

(defn part-1 []
  (count (overlapping-areas input)))

(defn part-2 []
  (let [overlapping-ids (reduce into #{} (overlapping-areas input))
        all-ids (map :id input)]
    (first (remove overlapping-ids all-ids))))
