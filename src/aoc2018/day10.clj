(ns aoc2018.day10)

(defn read-point [s]
  (->> (re-find #"position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)\s*>" s)
       rest
       (map #(Integer/parseInt %))))

(def input
  (->> (slurp "resources/aoc2018/day10.txt")
       clojure.string/split-lines
       (map read-point)))

(defn move-point [[px py vx vy]]
  [(+ px vx) (+ py vy) vx vy])

(defn move-points [points]
  (map move-point points))

(defn max-width [points]
  (let [sorted (sort-by first points)
        min-x (ffirst sorted)
        max-x (first (last sorted))]
    (- max-x min-x)))

(defn plot-sky [points]
  (let [[w] (last (sort-by first points))
        [_ h] (last (sort-by second points))
        points (set (map (fn [[x y]] [x y]) points))
        points (for [y (range h)
                     x (range w)]
                 (if (points [x y])
                   "*"
                   "."))]
    (->> (partition w points)
         (map #(apply str %))
         (clojure.string/join "\n"))))

(defn points-states [points]
  (->> (iterate move-points points)
       (reductions #(vector (max-width %2) %2) nil)))

(defn part-1 []
  (->> (points-states input)
       (reduce (fn [p1 p2]
                 (cond
                   (nil? p1) p2
                   (> (first p2) (first p1)) (reduced (last p1))
                   :else p2)))
       plot-sky))

(defn part-2 []
  (->> (points-states input)
       (reduce (fn [[p1 n] p2]
                 (cond
                   (nil? p1) [p2 n]
                   (> (first p2) (first p1)) (reduced n)
                   :else [p2 (inc n)]))
               [nil 0])))
