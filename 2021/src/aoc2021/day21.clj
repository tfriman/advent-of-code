(ns aoc2021.day21)

;; https://adventofcode.com/2021/day/21
;; example
;; Player 1 starting position: 4
;; Player 2 starting position: 8
;; result: round 993, losing player has 745 points, result (* 993 745)
;; actual
;; Player 1 starting position: 10
;; Player 2 starting position: 2

(comment
  (assert (= 2 (next-pos 7 5)))
  (assert (= 2 (next-pos 7 15)))
  (assert (= 10 (next-pos 7 3)))
  (assert (= 1 (next-pos 7 24)))
  (def roundcount  (+ 444356092776315 341960390180808))
  (def rx (- roundcount 27))
  (apply + (map #(java.lang.Math/pow 3 %) (range 1 26)))
  (frequencies (map #(apply + %) (for [x (range 1 4) y (range 1 4) z (range 1 4)] [x y z])))
  {3 1, 4 3, 5 6, 6 7, 7 6, 8 3, 9 1}
  )

(defn- next-pos [startpos diceval]
  (let [x (mod (+ startpos diceval) 10)]
    (if (= 0 x) 10 x)))

(def dirac-dist (frequencies (map #(apply + %) (for [x (range 1 4) y (range 1 4) z (range 1 4)] [x y z]))))

;; miten erotetaan ne jotka ovat häviäviä? onko väliä, aina tulee se uusi?

(def basemap (into {} (map (fn [x] [x 0]) (range 1 30))))

(defn- gen-next
  "Calc new distribution map based on the pos and accumulated count using dirac-dist map"
  [pos acc]
  (into {} (map (fn [[k v]] [(+ pos (next-pos pos k)) (* v acc)]) dirac-dist)))

(defn- split-results [m]
  {:next (select-keys m (range 21))
   :wincount (apply + (vals (select-keys m (range 21 30))))})

;; is this the format
{16 {:positions [[3 245] [4 1255]]}}

(defn- loop-map
  "Loop all the keys and create new distribution about those."
  [initm]
  (reduce (partial merge-with +) (map (fn mfx [[k v]] (gen-next k v)) initm)))

(defn- do-dirac-dicing [p]
  (loop [result 0
         m (gen-next p 1)
         i 0]
    (println "dicing round" i)
    (if (empty? m)
      result
      (let [lm (loop-map m)
            {:keys [next wincount]} (split-results lm)]
        (recur (+ result wincount) next (inc i))))))

(defn- dirac-round [p1 p2]
  (let [dice (cycle (range 1 101))
        ]
    (loop [p1t 0
           p2t 0
           p1 p1
           p2 p2
           dice dice
           round 0]
      (if (or (<= 1000 p1t) (<= 1000 p2t))
        (* round (min p1t p2t))
        (let [c1 (apply + (take 3 dice))
              c2 (apply + (take 3 (drop 3 dice)))
              p1n (next-pos p1 c1)
              p1tn (+ p1t p1n)
              p2n (if (<= 1000 p1tn) p2 (next-pos p2 c2))
              p2tn (if (<= 1000 p1tn) p2t (+ p2t p2n))
              dicen (drop 6 dice)
              roundn (+ round (if (<= 1000 p1tn) 3 6))]
          (recur p1tn p2tn p1n p2n dicen roundn))))))

(defn p1 []
  (doto (dirac-round 10 2) (#(println "p1:" %))))
