(ns aoc2018.day2)

(def input
  (->> (slurp "resources/aoc2018/day2.txt")
       clojure.string/split-lines))


(defn part-1 []
  (->> input
       (map frequencies)
       (map vals)
       (map set)
       (mapcat identity)
       frequencies
       ((juxt #(% 2) #(% 3)))
       (apply *)))


;; part 2
(defn- next-row
  [previous current other-seq]
  (reduce
    (fn [row [diagonal above other]]
      (let [update-val (if (= other current)
                         diagonal
                         (inc (min diagonal above (peek row))))]
        (conj row update-val)))
    [(inc (first previous))]
    (map vector previous (next previous) other-seq)))

(defn distance
  "Compute the levenshtein distance between two [sequences]."
  [sequence1 sequence2]
  (cond
    (and (empty? sequence1) (empty? sequence2)) 0
    (empty? sequence1) (count sequence2)
    (empty? sequence2) (count sequence1)
    :else (peek
            (reduce (fn [previous current] (next-row previous current sequence2))
                    (map #(identity %2) (cons nil sequence2) (range))
                    sequence1))))

(defn part-2 []
  (->> input
       (map (fn [l] (->> input
                         (map #(vector (distance l %) l %))
                         (reduce #(when (= 1 (first %2)) (reduced %2)) nil))))
       (reduce #(when %2 (reduced %2)) nil)
       (drop 1)
       (apply map #(when (= %1 %2) %1))
       (filter identity)
       (apply str)))
