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

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(defn- next-pos
  "Loop positions based on dice value."
  [startpos dicevalue]
  (let [x (mod (+ startpos dicevalue) 10)]
    (if (= 0 x) 10 x)))

(def dirac-dist
  (frequencies (map #(apply + %) (for [x (range 1 4) y (range 1 4) z (range 1 4)] [x y z])))
  #_(->> (frequencies (map #(apply + %) (for [x (range 1 4) y (range 1 4) z (range 1 4)] [x y z])))
      (map (fn [[k v]] [k (* 3 v)]))
      (into {}))
  )

;; miten erotetaan ne jotka ovat häviäviä? onko väliä, aina tulee se uusi? todo

(def basemap (into {} (map (fn [x] [x 0]) (range 1 30))))

(defn- gen-next
  "Calc new distribution map based on the pos and accumulated count using dirac-dist map.
  Returns map {4 16 5 22} where key/value pairs contain position and count."
  [pos acc]
  (into {} (map (fn [[k v]] (let [posnew (next-pos pos k)] [posnew (* v acc)])) dirac-dist)))

(defn- split-results [m]
  {:next (select-keys m (range 21))
   :wincount (apply + (flatten (map vals (vals (select-keys m (range 21 30))))))})

(defn- loop-map
  "Loop all the keys and create new distribution about those.
  {16 {1 13
       2 142
       3 234}}"
  [initm]
  (reduce (partial deep-merge-with +) (map (fn mfx [[points pospairs]]
                                             (println "p" points " pp " pospairs)
                                             (let [nexts (reduce (partial merge-with +) (map (fn [[pos cnt]] (gen-next pos cnt)) pospairs))
                                                   _ (def nexts nexts)
                                                   resu (into {} (map (fn [[k v]] [(+ k points) {k v}]) nexts))]
                                               (def resu resu)
                                               (println "resu" resu)

                                               resu))
                                           initm)))

(defn- do-dirac-dicing [p]
  (loop [result 0
         m {0 {p 1}}
         i 0]
    (println "dicing round" i m)
    (if (= i 30)
      m
      (if (empty? m)
        result
        (let [lm (loop-map m)
              _ (def lm lm)
              {:keys [next wincount]} (split-results lm)]
          (recur (+ result wincount) next (inc i)))))))

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
