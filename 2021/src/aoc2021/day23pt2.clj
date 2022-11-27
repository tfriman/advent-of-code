(ns aoc2021.day23pt2
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def amphicost {\A 1 \B 10 \C 100 \D 1000})

(def amphi-up-homes {\A 4
                     \B 5
                     \C 6
                     \D 7})

(def amphi-homes
  "Map containing tuples of coords, first up."
  {\A [4 0 -4 -8]
   \B [5 1 -3 -7]
   \C [6 2 -2 -6]
   \D [7 3 -1 -5]})

(def goal-orig {-8 \A
                -4 \A
                0  \A
                4  \A
                -7 \B
                -3 \B
                1  \B
                5  \B
                -6 \C
                -2 \C
                2  \C
                6  \C
                -5 \D
                -1 \D
                3  \D
                7  \D})

(def goal goal-orig)
(def example-pt2 "resources/day23-example-pt2-44169.txt")
(def path-pt2 "resources/day23-pt2.txt")
(def input example-pt2)
(def input path-pt2)

(defn- positions
  "Parse #F#O# -> F: 0 O:1"
  [row offset]
  (->> (clojure.string/replace row #"#| " "")
       (map-indexed (fn [idx itm] [(+ idx offset) itm]))))

(defn- ->positions
  "Parses start positions to map. Could be done better."
  [rows]
  (let [l4 (first (drop 2 rows))
        l3 (first (drop 3 rows))
        l2 (first (drop 4 rows))
        l1 (first (drop 5 rows))
        p1 (positions l1 -8)
        p2 (positions l2 -4)
        p3 (positions l3 0)
        p4 (positions l4 4)]
    (merge (into {} p1) (into {} p2) (into {} p3) (into {} p4))))

(def graph
  "Contains tuples containing index and distance to each reachable position.
   Always home -> hallway and hallway -> home, no other movements are possible.
   Each entry has :left and :right trees which are always in 'natural' order ie counted from the centre.
  "
  {-8 {:left  (reverse [[8 6] [9 5]])
       :right [[10 5] [11 7] [12 9] [13 11] [14 12]]}
   -7 {:left  (reverse [[8 8] [9 7] [10 5]])
       :right [[11 5] [12 7] [13 9] [14 10]]}
   -6 {:left  (reverse [[8 10] [9 9] [10 7] [11 5]])
       :right [[12 5] [13 7] [14 8]]}
   -5 {:left  (reverse [[8 12] [9 11] [10 9] [11 7] [12 5]])
       :right [[13 5] [14 6]]}
   -4 {:left  (reverse [[8 5] [9 4]])
       :right [[10 4] [11 6] [12 8] [13 10] [14 11]]}
   -3 {:left  (reverse [[8 7] [9 6] [10 4]])
       :right [[11 4] [12 6] [13 8] [14 9]]}
   -2 {:left  (reverse [[8 9] [9 8] [10 6] [11 4]])
       :right [[12 4] [13 6] [14 7]]}
   -1 {:left  (reverse [[8 11] [9 10] [10 8] [11 6] [12 4]])
       :right [[13 4] [14 5]]}
   0 {:left  (reverse [[8 4] [9 3]])
      :right [[10 3] [11 5] [12 7] [13 9] [14 10]]}
   1 {:left  (reverse [[8 6] [9 5] [10 3]])
      :right [[11 3] [12 5] [13 7] [14 8]]}
   2 {:left  (reverse [[8 8] [9 7] [10 5] [11 3]])
      :right [[12 3] [13 5] [14 6]]}
   3 {:left  (reverse [[8 10] [9 9] [10 7] [11 5] [12 3]])
      :right [[13 3] [14 4]]}
   4  {:left  (reverse [[8 3] [9 2]])
       :right [[10 2] [11 4] [12 6] [13 8] [14 9]]}
   5  {:left  (reverse [[8 5] [9 4] [10 2]])
       :right [[11 2] [12 4] [13 6] [14 7]]}
   6  {:left  (reverse [[8 7] [9 6] [10 4] [11 2]])
       :right [[12 2] [13 4] [14 5]]}
   7  {:left  (reverse [[8 9] [9 8] [10 6] [11 4] [12 2]])
       :right [[13 2] [14 3]]}
   })

(comment
  ;; sanity check for graph
  (defn- check-distancesums
    [elem]
    (->> elem
         vals
         (apply concat)
         (reduce (fn [a [i d]] (+ a d)) 0)))
  (map (fn [r] (map #(check-distancesums (get graph %)) r))
       (map (fn [x] (range (* 4 x) (* 4 (inc x)))) (range -2 2))))

(defn- hallways
  "Return set of reserved hallway indices"
  [current] (set (keys (select-keys current (range 8 15)))))

(defn- select-home
  "Return either up level index or bottom level index or nil"
  [current upidx]
  (if (get current upidx)
    upidx
    (if (get current (- upidx 4))
      (- upidx 4)
      (if (get current (- upidx 8))
        (- upidx 8)
        (when (get current (- upidx 12))
          (- upidx 12))))))

(defn- find-hallway
  "Return those [idx dist] tuples until blocked.
   Used to search available from home"
  [hallwaytuples reservedset]
  (take-while (fn [[idx _]] (not (reservedset idx))) hallwaytuples))

(defn- home-moves
  "Generate all the new states from home or nil if none found.
   Return tuple [newstate move-cost]"
  [current idx]
  (when-let [hix (select-home current idx)]
    (let [without  (dissoc current hix)
          amphi    (get current hix)
          moves    (get graph hix)
          blocked  (hallways current)
          left     (find-hallway (:left moves) blocked)
          right    (find-hallway (:right moves) blocked)
          allcoord (concat left right)]
      (map (fn [[iidx dist]]
             [(assoc without iidx amphi) (* (get amphicost amphi) dist)])
           allcoord))))

(defn- possible-moves-from-home
  "Get possible coords to move from home"
  [current]
  (->> (range 4 8)
       (map (partial select-home current))
       (remove nil?)))

(defn- possible-hallway-move
  "Check possible hallway move. Return tuple [new state, cost-to-home] or nil
   Move is possible when home is empty or occupied by a fellow amphi and there is a clear path to home."
  [current idx]
  (let [amphi           (get current idx)
        [l4 l0 l-4 l-8] (get amphi-homes amphi)
        l4-amphi        (get current l4)
        l0-amphi        (get current l0)
        l-4-amphi       (get current l-4)
        l-8-amphi       (get current l-8)
        target-home     (if (nil? l-8-amphi)
                          l-8
                          (if (and (= l-8-amphi amphi) (nil? l-4-amphi) (nil? l0-amphi) (nil? l4-amphi)) l-4
                              (if (and (= l-4-amphi amphi) (= l-8-amphi amphi) (nil? l0-amphi) (nil? l4-amphi)) l0
                                  (when (and (= l0-amphi amphi) (= l-8-amphi amphi) (= l-4-amphi amphi) (nil? l4-amphi)) l4))))]
    (when target-home
      (let [blocked  (hallways (dissoc current idx))
            moves    (get graph target-home)
            left     (find-hallway (:left moves) blocked)
            right    (find-hallway (:right moves) blocked)
            allcoord (concat left right)
            mov      (first (filter (fn [[hallwayidx _]] (= hallwayidx idx)) allcoord))]
        (when mov
          (let [[_ dist] mov]
            [(-> (dissoc current idx) (assoc target-home amphi)) (* (get amphicost amphi) dist)]))))))

(defn- new-paths
  "Get all positions reachable from current position. [new-pos cost-to-get-there]"
  [current]
  (let [home-moves-candidates (possible-moves-from-home current)
        hmoves                (partial home-moves current)
        home-paths            (remove nil? (apply concat (map hmoves home-moves-candidates)))
        hallway-indices       (hallways current)
        hallwaymoves          (partial possible-hallway-move current)
        hallway-paths         (remove nil? (map hallwaymoves hallway-indices))]
    (concat home-paths hallway-paths)))

(defn- solve
  [start-pos]
  (loop [open        (priority-map start-pos 0)
         cost-so-far {start-pos 0}
         idx         0]
    (when (= 0 (mod idx 10000))
      (println "round " idx "open" (count open) (count cost-so-far)))
    (if (empty? open)
      (do
        (println "failure, should not end up here ever. " idx)
        (def xxx-cost cost-so-far)

        nil)
      (let [current (first (peek open))]
        (if (= current goal)
          (-> open peek second)
          (let [newpaths (new-paths current)
                _        (def xxx-current current)
                _        (def xxx-newpaths newpaths)
                {:keys [acc-open acc-cost]}
                (reduce (fn [{:keys [acc-open acc-cost]} [newpos cost]]
                          (let [new-cost     (+ (acc-cost current) cost)
                                oldcost      (or (acc-cost newpos) Integer/MAX_VALUE)
                                acc-open-new (if (> oldcost new-cost)
                                               (assoc acc-open newpos new-cost)
                                               acc-open)
                                acc-cost-new (if (> oldcost new-cost)
                                               (assoc acc-cost newpos new-cost)
                                               acc-cost)
                                ]
                            {:acc-open acc-open-new
                             :acc-cost acc-cost-new}))
                        {:acc-open (pop open) :acc-cost cost-so-far}
                        newpaths)]
            (recur acc-open acc-cost (inc idx))))))))
(comment
  (def inputs (clojure.string/split-lines (slurp example-pt2)))
  )

(defn p2 []
  (let [inputs (clojure.string/split-lines (slurp input))
        start-pos (->positions inputs)]
    (solve start-pos)))
