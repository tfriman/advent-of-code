(ns aoc2021.day19
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as op]))

(def example "resources/day19-example.txt")
(def example-orientations "resources/day19-diff-orientations.txt")
(def path "resources/day19.txt")
(def input example)
#_(def input path)

(defn- directions
  "Finds if up dir is the same or not"
  [[a1 a2 a3] [b1 b2 b3]]
  (letfn [ (dir [a b] (if (= 0 (+ a b)) -1 1))]
    [(dir a1 b1) (dir a2 b2) (dir a3 b3)]))

(defn- sum-vec [[a1 a2 a3] [b1 b2 b3]]
  [(+ a1 b1) (+ a2 b2) (+ a3 b3)])

(defn- subs-vec [[a1 a2 a3] [b1 b2 b3]]
  [(- a1 b1) (- a2 b2) (- a3 b3)])

(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))

(defn- vec-diff
  "Diff too vectors as an absolute values"
  [[a1 a2 a3] [b1 b2 b3]]
  (let [x      (Math/abs (- a1 b1))
        y      (Math/abs (- a2 b2))
        z      (Math/abs (- a3 b3))]
    [x y z]))

(defn- shuffle-vec [[a b c] v]
  [(nth v a) (nth v b) (nth v c)])

(defn- rel-distances
  "Does too many things..."
  [diff-fn filter-fn transform-fn s]
  (loop [f    (first s)
         r    (rest s)
         vecs []]
    (if (empty? r)
      vecs
      (let [newvecs (reduce (fn [acc x]
                              (let [diff (diff-fn x f)]
                                (if (filter-fn diff)
                                  (cons (transform-fn x f diff) acc)
                                  acc))
                              ) vecs r)]
        (recur (first r) (rest r) newvecs)))))

(defn- ->vec
  "'727,592,562' string to [int int int]"
  [s]
  (->> (re-matches #"(.*),(.*),(.*)" s)
       rest
       (mapv #(Integer/parseInt %))))

(defn- ->readings
  "Parses stuff"
  [s]
  (loop [re (take-while #(not= "" %) s)
         r (drop (count re) s)
         result []]
    (if (empty? re)
      (reverse (mapv #(map ->vec %) result))
      (let [readings (rest re)
            newresult (cons readings result)
            newre (take-while #(not= "" %) (rest r))
            newrest (drop (count newre) (rest r))]
        (recur newre newrest newresult)))))

(defn- find-common
  "Finds the common element. Expects to find key for 2"
  [found-coords]
  (let [ftn (mapcat identity found-coords)
        fs (frequencies ftn)
        _ (def fs fs)
        result (get (clojure.set/map-invert fs) 2)]
    (assert (not (nil? result)) (str "find-common assert failed:" found-coords))
    result))

(defn- find-points
  "Find two readings where diff is given, return dir-vectors from common to given as keys, also return common"
  [readings diffs]
  (let [rds (rel-distances vec-diff #((into #{} diffs) %) (fn [x y _] [x y]) readings)
        common (find-common rds)
        tm (apply merge
                  (map (fn [[a b]]
                         (let [sv (subs-vec common (if (= common a) b a))]
                           {(matrix/emap #(Math/abs %) sv) sv}
                           )) rds))]
    (merge tm {:common common})))

(defn- rebase-point
  "Map points to new base."
  [relv directionv p]
  (sum-vec relv (op/* p directionv)))

(defn- find-consecutive-distances
  "Make sure to have distances that share a common point. targetd is a seq [dist point]"
  [distset targetd]
  (let [matches (filter (fn [[d p]] (distset d)) targetd)
        [_ p] (first matches)]
    (take 2 (map first (filter (fn [[_ pp]] (= p pp)) matches)))))

(defn- next-diff
  "indexed diffs, first having at least 66 common is returned"
  [shuffle-fn perm targetd s]
  (let [si   (map-indexed (fn [idx itm] [idx itm]) s)
        td   (into #{} (map first targetd))
        resl (keep (fn keepfnx [[idx itm]]
                     (let [perms (clojure.set/intersection (into #{} (map shuffle-fn itm)) td)
                           c     (count perms)]
                       (when (<= 66 c) {:idx         idx
                                        :c c
                                        :permutation perm
                                        :examples    (into [] (find-consecutive-distances perms targetd))})
                       )) si)]
    (first resl)))

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(def perm-compensation {[0 1 2] [0 1 2]
                        [0 2 1] [0 2 1]
                        [1 0 2] [1 0 2]
                        [1 2 0] [2 0 1]
                        [2 0 1] [1 2 0]
                        [2 1 0] [2 1 0]})

(defn un-perm [compmap perm v]
  (shuffle-vec (compmap perm) v))

(defn- process
  [acc readings perms]
  (if (empty? perms)
    (println "error, empty perms")
    (let [permutation (first perms)
          acc-rel-dists (rel-distances vec-diff identity (fn fx1 [x _ d] [d x]) acc)
          rds-rel-dists (map #(rel-distances vec-diff identity (fn fx2 [_ _ d] d) %) readings)
          diff          (next-diff (partial shuffle-vec permutation) permutation acc-rel-dists rds-rel-dists)]
      (if (nil? diff)
        (process acc readings (rest perms))
        (let [{:keys [idx permutation examples]} diff
              s0                                 (find-points acc examples)
              rx                                 (nth readings idx)
              sx                                 (find-points (map #(shuffle-vec permutation %) rx) examples)
              fv                                 (first examples)
              dirvec                             (directions (get s0 fv) (get sx fv))
              v                                  (subs-vec (:common s0) (op/* (:common sx) dirvec))
              rebased                            (map (fn [x] (rebase-point v dirvec (shuffle-vec permutation x))) rx)
              ]
          {:acc (into #{} (concat acc rebased))
           :r   (drop-nth idx readings)
           :vec v})))))

(defn- make-relative [readings]
  (loop [acc (first readings)
         r (rest readings)
         vecs []]
    (if (empty? r)
      {:acc acc :vecs vecs}
      (let [{:keys [acc r vec]} (process acc r (permutations [0 1 2]))]
        (recur acc r (conj vecs vec))))))

(defn- manhattan
  [v1 v2]
  (apply + (vec-diff v1 v2)))

(defn- find-max-manhattan [vs]
  (loop [f (first vs)
         r (rest vs)
         mv 0]
    (if (empty? r)
      mv
      (let [ mv (max mv (apply max (map (partial manhattan f) r)))]
        (recur (first r) (rest r) mv)))))

(defn p12 []
  (let [ins       (clojure.string/split-lines (slurp input))
        readings  (->readings ins)
        result (make-relative readings)
        maxman (find-max-manhattan (:vecs result))]
    (println "p1:" (count (:acc result)) " p2:" maxman)))
