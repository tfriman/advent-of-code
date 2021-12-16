(ns aoc2021.day11)

(def example "resources/day11-example.txt")
(def short-example "resources/day11-short-example.txt")
(def path "resources/day11.txt")
#_(def input short-example)
(def input example)
(def input path)

(defn neighbours [cols rows idx]
  (let [n (when (>= idx cols) (- idx cols))
        s (when (> (* cols rows) (+ idx cols) ) (+ idx cols))
        w (when (not= 0 (mod idx cols)) (dec idx))
        e (when (not= (dec cols) (mod idx cols)) (inc idx))
        se (if (and s e) (inc s))
        sw (if (and s w) (dec s))
        ne (if (and n e) (inc n))
        nw (if (and n w) (dec n))]
    (filter some? [n s w e se sw ne nw])))

(defn- update-matrix
  "Updates given seq's indises by calling f their earlier value."
  [s idxs f]
  (reduce (fn [a i] (update a i f)) (vec s) (vec idxs)))

(defn- fix-over-nines [sin]
  (let [idxmap (map-indexed (fn [idx itm] [idx itm]) sin)
        filtered9ormore (map first (filter #(< 9 (second %)) idxmap))]
    (update-matrix sin filtered9ormore (fn [x] 0))))

(defn round
  "Return how many flashed on a given round and the new matrix
  {:flashed x :new m}"
  [cols rows sq]
  (let [nf (partial neighbours cols rows)
        in (map inc sq)]
    (loop [sin in
           ninesseen #{}]
      (let [idxmap (map-indexed (fn [idx itm] [idx itm]) sin)
            filtered9 (filter #(and
                                (nil? (ninesseen (first %)))
                                (< 9 (second %)))
                              idxmap)
            nineidx (ffirst filtered9)]
        (if (nil? nineidx)
          {:flashed (count ninesseen)
           :new (fix-over-nines sin)}
          (let [nei (nf nineidx)
                new-sin (update-matrix sin nei inc)]
            (recur new-sin (conj ninesseen nineidx))))))))

(defn do-until [origin rounds cols rows]
  (loop [i 0
         in origin
         flashcount 0]
    (if (= i rounds)
      (do
        #_(println "last:" (partition cols in))
        {:flashes flashcount
         :res in})
      (let [{:keys [flashed new]} (round cols rows in)]
        (recur (inc i) new (+ flashcount flashed))))))

(defn do-until-p2 [origin cols rows]
  (loop [i 0
         in origin
         flashcount 0]
    (if (= flashcount (* cols rows))
      i
      (let [{:keys [flashed new]} (round cols rows in)]
        (recur (inc i) new flashed)))))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        columns (first (map count raw))
        rows (count raw)
        intvals (mapv #(Character/getNumericValue %) (mapcat identity raw))
        fx (round columns rows intvals)
        result (do-until intvals 100 columns rows)
        result-p2 (do-until-p2 intvals columns rows)
        ]
    (println "result p1: " (:flashes result))
    (println "result p2: " result-p2)))
