(ns aoc.day08
  (:require [clojure.math :as math])
  )

(def input-example "resources/day08-example.txt")
(def input-real "resources/day08-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(defn- ->long [s]
  {:pre [(clojure.test/is (not (nil? s)))]}
  (if (char? s)
    (Long/parseLong (str s))
    (if (string? s)
      (Long/parseLong s)
      (if (int? s)
        s
        (throw (RuntimeException. (str "Nopes!" s " type was:" (type s))))))))

(defn- parse-coords
  "Parse x,y,z to map"
  [r]
  (let [[_ xs ys zs] (re-find #"(\d+),(\d+),(\d+)" r)]
    {
     :x (->long xs)
     :y (->long ys)
     :z (->long zs)
     }
    ))

(comment
  (assert (= {:x 11, :y 23, :z 938} (parse-coords "11,23,938")))
  )

(defn- calc-distance
  "Calculates euclidean distance"
  [{x1 :x y1 :y z1 :z} {x2 :x y2 :y z2 :z}]
  (let [xx (math/pow (- x1 x2) 2)
        yy (math/pow (- y1 y2) 2)
        zz (math/pow (- z1 z2) 2)
        ]
    (+ xx yy zz)
    ))

(comment
  (do
    (assert (= 25.0 (calc-distance (parse-coords "0,0,0") (parse-coords  "3,4,0"))))
    (assert (= 25.0 (calc-distance (parse-coords  "3,4,0") (parse-coords "0,0,0"))))
    (assert (= 25.0 (calc-distance (parse-coords  "3,0,0") (parse-coords "0,4,0"))))
    ))

(defn- coord-compare [{x1 :x y1 :y z1 :z} {x2 :x y2 :y z2 :z}]
  (let [cx (compare x1 x2)
        cy (compare y1 y2)
        cz (compare z1 z2)
        ]
    (if (zero? cx)
      (if (zero? cy)
        cz
        cy
        )
      cx
      )
    )
  )

(defn- find-closest-to
  "Finds closest pair"
  [pairs-seen reference coords-in]
  (loop [coords coords-in
         dist java.lang.Double/MAX_VALUE
         closest nil]
    (if (empty? coords)
      {:dist dist
       :closest closest
       :new-pair (sort coord-compare [closest reference])
       }
      (let [to-check (first coords)
            val (vec (sort coord-compare [to-check reference]))
            seen? (pairs-seen val)]
        (let [dnew (if seen? dist (calc-distance reference to-check))
              dist-new (if seen? dist (if (> dist dnew) dnew dist))
              closest-new (if seen? closest (if (> dist dnew) to-check closest))]
          (recur (rest coords) dist-new closest-new))))))

(defn- brute-all
  [reference coords-in]
  (-> (map (fn [c] [(calc-distance reference c) (sort coord-compare [c reference])]) coords-in)
      sort
      ))

(defn- test-sort-pairs [ps]
  (map #(sort coord-compare %) (partition 2 ps))
  )

(defn- brute-all-combs
  [coords-in]
  (loop [reference (first coords-in)
         coords (rest coords-in)
         results []
         ]
    (if (empty? coords)
      (->> results sort (map second) (map vec))
      (let [res (brute-all reference coords)]
        (recur (first coords) (rest coords) (concat results res))
        ))))

(defn- fix-map-both-exist
  "M contains keys for both c1 and c2. If both point to the same circuit already, noop.
  If both point to separate ones, find c2 circuit and replace all of those with c1 circuit value.
  "
  [m c1 c2]
  (let [f1 (get m c1)
        f2 (get m c2)]
    (if (= f1 f2)
      m ;; noop
      (let [keys-needing-fixing (->> m
                                (group-by val)
                                (#(get % f2))
                                (map key)
                                )]
        (reduce (fn [acc k] (assoc acc k f1)) m keys-needing-fixing)))))


(defn- calc-circuits
  "Pairs contain sorted [coord1 coord2] pairs. Find those circuits out of that."
  [pairs]
  ;; create set of sets and check if one or both are in the sets.
  ;; if one set contains already both, do nothing
  ;; if one set contains one of the pairs, add another to that set too and remove old one
  ;; if one set contains c1 and another c2, then you combine those sets and remove both from the sets
  (loop [[c1 c2] (first pairs)
         r (rest pairs)
         coll {}
         ]
    (if (nil? c1)
      coll
      (let [f1 (get coll c1)
            f2 (get coll c2)
            new-map (if (and (nil? f1) (nil? f2)) ;; new circuit
                      (-> coll
                          (assoc c1 [c1 c2])
                          (assoc c2 [c1 c2])
                          )
                      (if (nil? f1)
                        (assoc coll c1 f2) ;; f2 found, update map to have that for f1 as well
                        (if (nil? f2)
                          (assoc coll c2 f1) ;; f1 found, update map to have that for f1 as well
                          ;; both exist. Update those with f2 val to be f1.
                          (fix-map-both-exist coll c1 c2)
                          )
                        )
                      )
            ]
        (recur (first r) (rest r) new-map)
        )
      )
    )
  )

(comment
  (def input (->> input-example get-input (map parse-coords)))
  (brute-all (first input) (rest input))
  (def res (brute-all-combs input))

  (do
    (test-sort-pairs [
                      {:x 162, :y 817, :z 812} {:x 425, :y 690, :z 689}
                      {:x 162, :y 817, :z 812} {:x 431, :y 825, :z 988}
                      {:x 805, :y 96, :z 715} {:x 906, :y 360, :z 560}
                      ])

    (assert (= {:x 431, :y 825, :z 988} (:closest (find-closest-to
                                                   #{
                                                     (test-sort-pairs [
                                                                       {:x 162, :y 817, :z 812} {:x 425, :y 690, :z 689}
                                                                       {:x 162, :y 817, :z 812} {:x 431, :y 825, :z 988}
                                                                       {:x 906, :y 360, :z 560} {:x 805, :y 96, :z 715}
                                                                       ])
                                                     } (first input) (rest input)))))
    )
  (do
    (assert (= {:x 425, :y 690, :z 689} (:closest (find-closest-to #{} (first input) (rest input)))))
    (assert (= {:x 431, :y 825, :z 988} (:closest (find-closest-to #{(sort coord-compare [(first input) {:x 425,:y 690, :z 689}])} (first input) (rest input)))))

    (assert (= {:x 431, :y 825, :z 988} (:closest (find-closest-to #{(sort coord-compare [(first input) {:x 425,:y 690, :z 689}]) (sort coord-compare [{:x 906, :y 360, :z 560} {:x 805, :y 96, :z 715}])} (first input) (rest input))))))

  )

(defn p1
  [i c]
  (let [input (->> i get-input (map parse-coords))
        res (brute-all-combs input)
        _ (def res res)
        f (frequencies (vals (calc-circuits (take c res))))
        _ (def f f)
        ]
    (->> f
         vals
         sort
         reverse
         (take 3)
         (apply *)
         )))

(p1 input-example 10)
;; (time (p1 input-real 1000)) ;; takes about 33 seconds
