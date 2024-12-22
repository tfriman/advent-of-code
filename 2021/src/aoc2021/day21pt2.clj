(ns aoc2021.day21pt2)

;; https://adventofcode.com/2021/day/21
;; Example
;; Player 1 starting position: 4
;; Player 2 starting position: 8
;; Player 1 wins in 444356092776315 universes, while player 2 merely wins in 341960390180808 universes.
;; Result: max meaning 444356092776315
;; Actual starting positions
;; Player 1 starting position: 10
;; Player 2 starting position: 2

(defn- next-pos
  "Loop positions based on dice value."
  [startpos dicevalue]
  (let [x (mod (+ startpos dicevalue) 10)]
    (if (= 0 x) 10 x)))

(def dirac-distribution (->>
                          (for [x (range 1 4) y (range 1 4) z (range 1 4)] [x y z])
                          (map #(apply + %))
                          frequencies))

(defn get-next
  "Calculate next based on situation
  {[points1 points2 next pos1 pos2] c}"
  [points1 points2 next-player pos1 pos2]
  (if (= :p1 next-player)
    (into {} (for [[a b] dirac-distribution] [[(+ (next-pos pos1 a) points1) points2 :p2 (next-pos pos1 a) pos2] b]))
    (into {} (for [[a b] dirac-distribution] [[points1 (+ (next-pos pos2 a) points2) :p1 pos1 (next-pos pos2 a)] b]))))

(defn update-state
  "m is existing state, k key and c count added"
  [m k c]
  (update-in m [k] (fnil +' 0) c))

(defn find-lowest-point
  "Finds lowest point given it has both under 21, returns nil if none found"
  [m]
  (->> (keys m)
       (remove (fn [[p1 p2 _ _ _]] (> (max p1 p2) 20)))
       (sort-by (fn [[p1 p2 _ _ _]] (min p1 p2)))
       first))

(comment
  (get-next 0 0 :p1 10 2)

  (get-next 4 0 :p2 4 2)
  (-> {}
      (update-state [0 21 :p2 3 2] 3)
      (update-state [3 0 :p2 4 2] 6)
      (update-state [3 122 :p2 4 2] 6)
      find-lowest-point
      )
  )

(defn- calc-next
  "Calc next state, remove key found"
  [sm [points1 points2 next-player pos1 pos2 :as next]]
  (let [orig-count (get sm next)
        newmap (get-next points1 points2 next-player pos1 pos2)]
    (reduce (fn [acc [k v]] (update-state acc k (*' orig-count v))) (dissoc sm next) newmap)))

(comment
  (calc-next {[3 0 :p2 4 2] 3} [3 0 :p2 4 2])
  (-> (calc-next {[3 0 :p2 4 2] 3} [3 0 :p2 4 2])
      find-lowest-point)
  )

(defn doit [p1 p2]
  (loop [seed (get-next 0 0 :p1 p1 p2)
         nxt (find-lowest-point seed)
         x 0]
    (if (= 1 (mod x 1000))
      (println x " " nxt " and keys " (count (keys seed))))
    (if-not nxt
      (do
        (println "x" x)
        seed)
      (let [newseed (calc-next seed nxt)]
        (recur newseed (find-lowest-point newseed) (inc x))))
    ))

(defn p2 []
  (let [results (doit 10 2)
        counts [(apply +
                       (map second (:p1 (group-by (fn [[[_ _ p _ _] _]] p) results)))) ;; 36086577212020
                (apply +
                       (map second (:p2 (group-by (fn [[[_ _ p _ _] _]] p) results)))) ;; 49982165861983
                ]
        ]
    (println "Result:" (apply max counts))
    )
  )

(time (p2))
;; Chance for optimization
;; (* 2 10 10 18 18) 64800 different ones.
;; 21 - 9 = 12, so starting from that we update winners.      1
;; 13 means 1 + 3 winners                                     4
;; 14 means 1 + 3 + 6 winners                                 10
;; 15 means 1 + 3 + 6 + 7 winners                             17
;; 16 means 1 + 3 + 6 + 7 + 6 winners                         23
;; 17 means 1 + 3 + 6 + 7 + 6 + 3 winners                     26
;; 18 means 1 + 3 + 6 + 7 + 6 + 3 + 1 winners                 2
