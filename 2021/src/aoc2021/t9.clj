(ns aoc2021.t9)

(def example "resources/t9-example.txt")
(def path "resources/t9.txt")
(def input example)
#_(def input path)

(defn neighbours [idx cols rows]
  (let [up (when (>= idx cols) (- idx cols))
        down (when (> (* cols rows) (+ idx cols) ) (+ idx cols))
        left (when (not= 0 (mod idx cols)) (dec idx))
        right (when (not= (dec cols) (mod idx cols)) (inc idx))]
    (filter some? [up down left right])))

(comment
  (neighbours 0 4 3)
  (neighbours 1 4 3)
  (neighbours 2 4 3)
  (neighbours 3 4 3)
  (neighbours 5 4 3)
  (neighbours 8 4 3)
  (neighbours 10 4 3)
  (neighbours 11 4 3))

(defn lowest? [cols rows v idx]
  (let [neighs (neighbours idx cols rows)
        t (nth v idx)
        other-min (apply min (map #(nth v %) neighs))]
    (> other-min t)))

(defn p1 []
  (let [ins (clojure.string/split-lines (slurp input))
        columns (first (map count ins))
        rows (count ins)
        intvals (mapv #(Character/getNumericValue %) (mapcat identity ins))
        low-indices (filter #(lowest? columns rows intvals %) (range 0 (* columns rows)))
        result (apply + (map #(inc (nth intvals %)) low-indices))
        ]
    (println "result:" result)))

(defn iter-neighbours-until-9
  "Start from the idx and then search on each direction recursively until 9 stops the fun. Returns all seen indices including start"
  [cols rows v idx]
  (loop [is [idx]
         seen #{}]
    (if (empty? is)
      seen
      (let [i (first is)
            norig (neighbours i cols rows)
            nonines (filter #(not= 9 (nth v %)) norig)
            neighs (filter (complement seen) nonines)
            new-is (concat (rest is) neighs)
            new-seen (conj seen i)]
        (recur new-is new-seen)))))

(defn p2 []
  (let [ins (clojure.string/split-lines (slurp input))
        _ (def ins (clojure.string/split-lines (slurp input)))
        columns (first (map count ins))
        _ (def columns (first (map count ins)))
        rows (count ins)
        _ (def         rows (count ins))
        intvals (mapv #(Character/getNumericValue %) (mapcat identity ins))
        _ (def         intvals (mapv #(Character/getNumericValue %) (mapcat identity ins)))
        low-indices (filter #(lowest? columns rows intvals %) (range 0 (* columns rows)))
        _ (def low-indices (filter #(lowest? columns rows intvals %) (range 0 (* columns rows))))
        basins (map #(iter-neighbours-until-9 columns rows intvals %) low-indices)
        _ (def basins (map #(iter-neighbours-until-9 columns rows intvals %) low-indices))
        largest3 (take 3 (reverse (sort (map count basins))))
        result (apply * largest3)
        ]
    (println "result:" result)))
