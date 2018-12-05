(ns aoc2018.day4)

(defn parse-int [^String s]
  (Integer/parseInt s))

(defn read-id [m]
  (if-let [s (re-find #"#(\d+)" (:entry m))]
    (->> (second s) parse-int (assoc m :id))
    m))

(defn read-numerics [[y m d hh mm & entry]]
  (-> (map parse-int [y m d hh mm])
      (concat entry)))

(defn entry->id [{:keys [entry] :as m}]
  (let [id (cond
             (= entry "falls asleep") :asleep
             (= entry "wakes up") :awake
             (contains? m :id) :start)]
    (assoc m :entry id)))

(defn format-entry [entry]
  (->> entry
       read-numerics
       (zipmap [:y :m :d :hh :mm :entry])
       read-id
       entry->id))

(defn read-entry [s]
  (rest (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)" s)))

(defn sort-by-date [entries]
  (sort-by (fn [[y m d hh mm]]
             (-> (java.text.SimpleDateFormat. "yyyyMMddhhmm")
                 (.parse (str y m d hh mm))))
           entries))

(def input
  (->> (slurp "resources/aoc2018/day4.txt")
       clojure.string/split-lines
       (map read-entry)
       sort-by-date
       (map format-entry)))




(defn identify-entries [entries]
  (->> entries
       (reduce #(if (contains? %2 :id)
                  (-> %1
                      (assoc :id (:id %2))
                      (update :entries conj %2))
                  (->> (assoc %2 :id (:id %1))
                       (update %1 :entries conj)))
               {:id      nil
                :entries []})
       :entries))

(defn time-asleep [[id entries]]
  (let [time (->> entries
                  (filter #(not= :start (:entry %)))
                  (partition 2)
                  (reduce
                    (fn [ret [a b]]
                      (+ ret (- (:mm b) (:mm a))))
                    0))]
    [id time]))

(defn most-minutes-asleep [entries]
  (->> entries
       (filter #(not= :start (:entry %)))
       (partition 2)
       (mapcat (fn [[a b]]
                 (range (:mm a) (:mm b))))
       frequencies))

(defn most-minute-asleep [entries]
  (->> (most-minutes-asleep entries)
       (sort-by second)
       last
       first))

(defn most-asleep-guard-id [guards]
  (->> guards
       (map time-asleep)
       (sort-by second)
       last
       first))

(defn part-1 []
  (let [guards (->> (identify-entries input)
                    (group-by :id))
        id (most-asleep-guard-id guards)
        minute-asleep (most-minute-asleep (get guards id))]
    (* id minute-asleep)))

(defn part-2 []
  (let [guards (->> (identify-entries input)
                    (group-by :id))
        entries (for [[id entries] guards
                      [minute freq] (most-minutes-asleep entries)]
                  [minute freq id])
        [minute _ id] (->> entries
                           (group-by first)
                           (reduce (fn [ret [_ entries]]
                                     (conj ret (->> entries (sort-by second) last)))
                                   [])
                           (sort-by second)
                           last)]
    (* id minute)))
