(ns aoc2021.t6)

(def example "resources/t6-example.txt")
(def path "resources/t6.txt")
(def input example)
(def input path)

(defn iterate-next [s]
  (reduce (fn [acc i]
            (let [newi (dec i)]
              (if (= -1 newi)
                (conj (conj acc 6) 8)
                (conj acc newi)))) [] s))

(defn do-until [n s fx]
  (loop [x n
         se s]
    (if (= x 0)
      se
      (recur (dec x) (fx se)))))

(defn p1 []
  (let [in (slurp input)
        _ (println "in:" in)
        x (clojure.string/split in  #",")
        ints (map #(Integer/parseInt %) x)
        result18 (do-until 18 ints iterate-next)
        _ (println "r18:" (count result18))
        result80 (do-until 80 ints iterate-next)
        _ (println "r80:" (count result80))
        ]))

(defn ->f [is]
  (let [f (frequencies is)
        hf (fn [x] (get f x 0))]
    {:0 (hf 0)
     :1 (hf 1)
     :2 (hf 2)
     :3 (hf 3)
     :4 (hf 4)
     :5 (hf 5)
     :6 (hf 6)
     :7 (hf 7)
     :8 (hf 8)}))

(defn iter-map [fs]
    {:0 (:1 fs)
     :1 (:2 fs)
     :2 (:3 fs)
     :3 (:4 fs)
     :4 (:5 fs)
     :5 (:6 fs)
     :6 (+ (:0 fs) (:7 fs))
     :7 (:8 fs)
     :8 (:0 fs)})

(defn p2 []
  (let [in (slurp input)
        x (clojure.string/split in  #",")
        ints (map #(Long/parseLong %) x)
        fm (->f ints)
        result18 (do-until 18 fm iter-map)
        _ (println "r18:" (apply + (vals result18)))
        result80 (do-until 80 fm iter-map)
        _ (println "r80:" (apply + (vals result80)))
        result256 (do-until 256 fm iter-map)
        _ (println "r256:" (apply + (vals result256)))]))
