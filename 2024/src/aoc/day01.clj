(ns aoc.day01)

(def input "resources/day01.txt")

(def example1 (clojure.string/split-lines "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"))

(comment
  (first example1)
  (re-find #"(\d+)(?:\s+)(\d+)" (first example1))
  (update-in {:a [1]} [:a]  conj 2)

  (def xm (->> example1
               (map (fn [l] (re-find #"(\d+)(?:\s+)(\d+)" l)))
               (reduce (fn [acc [_ a b]] (-> acc
                                             (update-in [:a] conj (Integer/parseInt a))
                                             (update-in [:b] conj (Integer/parseInt b)))) {:a [] :b []})))
  (->> (map (fn [a b] (Math/abs (- a b))) (sort (:a xm)) (sort (:b xm)))
      (apply +))
  (get (frequencies (:b xm)) 99 0)
  )

(defn- part1 [lines]
  (let [xm (->> lines
                (map (fn [l] (re-find #"(\d+)(?:\s+)(\d+)" l)))
                (reduce (fn [acc [_ a b]] (-> acc
                                              (update-in [:a] conj (Integer/parseInt a))
                                              (update-in [:b] conj (Integer/parseInt b)))) {:a [] :b []}))]
    (->> (map (fn [a b] (Math/abs (- a b))) (sort (:a xm)) (sort (:b xm)))
         (apply +)))
  )

(defn- part2 [lines]
  (let [xm (->> lines
                (map (fn [l] (re-find #"(\d+)(?:\s+)(\d+)" l)))
                (reduce (fn [acc [_ a b]] (-> acc
                                              (update-in [:a] conj (Integer/parseInt a))
                                              (update-in [:b] conj (Integer/parseInt b)))) {:a [] :b []}))
        f (frequencies (:b xm))
        x (:a xm)]
    (->> (map (fn [l] (* l (get f l 0))) x)
         (apply +))
    )
  )


(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))
        all (clojure.string/join "" lines)]
    (part1 lines)))

(defn p2 []
  (let [lines (clojure.string/split-lines (slurp input))
        ]
    (part2 lines)))