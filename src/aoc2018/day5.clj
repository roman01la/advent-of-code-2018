(ns aoc2018.day5)

(def input
  (-> (slurp "resources/aoc2018/day5.txt")
      seq
      butlast))

(def char-seq "abcdefghijklmnopqrstuvwxyz")

(def unit-types
  (map #(set [% (first (seq (clojure.string/upper-case %)))]) char-seq))

(defn react? [c1 c2]
  (= 32 (Math/abs (- (int c1) (int c2)))))

(defn reduce-polymer [input]
  (loop [[c1 c2 & cs] input
         ret []]
    (cond
      (nil? c1) ret
      (nil? c2) (conj ret c1)

      (and (react? c1 c2) (seq ret))
      (recur (cons (peek ret) cs) (pop ret))

      (react? c1 c2)
      (recur cs ret)

      :else (recur (cons c2 cs) (conj ret c1)))))

(defn part-1 []
  (reduce-polymer input))

(defn part-2 []
  (let [polymers (map #(remove % input) unit-types)]
    (->> (map reduce-polymer polymers)
         (map count)
         (reduce min))))
