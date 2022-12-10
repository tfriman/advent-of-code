(ns aoc2021.day03)

(def example "resources/day03-example.txt")
(def path "resources/day03.txt")
(def input example)
(def input path)

(defn map-to-intg-vector [l i]
  (mapv (fn [x] (if x 1 -1)) (map #(bit-test i %) (reverse (range l)))))

(defn convert-to-bits [i fil]
  (mapv (fn [x] (if (fil x) 1 0)) i))

(defn convert-to-bitset [len v]
  (let [mi (map-indexed (fn [idx itm] [idx itm]) (reverse v))
        mif (filter (fn [[_ x]] (pos? x)) mi)]
    (reduce (fn [acc [idx _]] (bit-set acc idx)) 0 mif)))

(defn p1 []
  (let [in       (clojure.string/split-lines (slurp input))
        len      (count (first in))
        fx       (partial map-to-intg-vector len)
        integers (map #(Integer/parseInt % 2) in)
        resx     (map fx integers)
        counts   (apply map + resx)
        bitv     (convert-to-bits counts pos?)
        _        (println "bitv:" bitv)
        gamma    (convert-to-bitset len bitv)
        epsilon  (bit-xor gamma (dec (bit-set 0 len)))]
    (println "result:" (* gamma epsilon))))

(defn find-matching-for-pos [filfn len integers pos]
  (let [fx     (partial map-to-intg-vector len)
        resx   (map fx integers)
        counts (apply map + resx)
        bitv   (convert-to-bits counts filfn)
        #_     (println "bitv:" bitv)
        res    (= 1 (nth bitv pos))]
    #_(println "res:" res " for pos:" pos)
    res))

(defn find-matching [in filfn len]
  (let [xran (range len)
        f-m  (partial find-matching-for-pos filfn len)]
    (loop [pos xran
           c   in]
      (println "c:" c)
      (if (= 1 (count c))
        (first c)
        (let [ma (f-m c (first pos))]
          (println "ma:" ma " and pos" pos)
          (recur (rest pos) (filter (fn [x] (= ma (bit-test x (- len (first pos) 1)))) c)))))))

(defn p2 []
  (let [in       (clojure.string/split-lines (slurp input))
        len      (count (first in))
        integers (map #(Integer/parseInt % 2) in)
        oxygen   (find-matching integers (fn [x] (> x -1)) len)
        scrubber (find-matching integers (fn [x] (println "filfn:" x) (or (neg? x))) len)]
    (println "result:" (* oxygen scrubber))))
