(ns aoc2021.t15)

(def example "resources/t15-example.txt")
(def examplep2 "resources/t15-p2-example.txt")

(def path "resources/t15.txt")
(def input example)
(def input path)
#_(def input examplep2)

(comment
  (def raw (clojure.string/split-lines (slurp input)))
  (def rows (count raw))
  (def columns (count (first raw)))
  (def as-numbers (mapv #(Character/getNumericValue %) (apply concat raw)))
  (def distances (merge  {0 0} (zipmap (range 1 (* columns rows)) (repeat Integer/MAX_VALUE))))
  (def inputs (range 0 (* columns rows)))
  )

(defn neighbours [idx cols rows]
  (let [up (when (>= idx cols) (- idx cols))
        down (when (> (* cols rows) (+ idx cols) ) (+ idx cols))
        left (when (not= 0 (mod idx cols)) (dec idx))
        right (when (not= (dec cols) (mod idx cols)) (inc idx))]
    (filter some? [up down left right])))

(defn ->priority-queue
  "s elems are indices. distmap contains weights"
  [s distmap]
  (sort-by #(distmap %) s))

(comment
  (->priority-queue [1 2 3] {1 11 2 2 3 33})
  )


(defn update-distances [idx neighbouridxs distmap weighs]
  (loop [n (first neighbouridxs)
         r (rest neighbouridxs)
         dm distmap]
    (if (nil? n)
      dm
      (let [oldd (dm n)
            prospectd (+ (dm idx) (nth weighs n))
            ;;newd (min oldd prospectd)
            update? (> oldd prospectd)
            newdm (if update?
                    (assoc dm n prospectd)
                    dm)]
        (recur (first r) (rest r) newdm)))))

(defn djikstra [rows cols inputseq distancemap weighs target]
  (println "djikstra searching for " target)
  (loop [nodeidx (first (->priority-queue inputseq distancemap))
         inputs (rest (->priority-queue inputseq distancemap))
         distmap distancemap]
    (if (nil? nodeidx)
      (distmap target)
      (let [neighs (neighbours nodeidx rows cols)
            newdmap (update-distances nodeidx neighs distmap weighs)
            s (->priority-queue inputs newdmap)]
        (recur (first s) (rest s) newdmap)))))

(defn gen-row [s]
  (loop [y (range 0 5)
         syy s
         resy []]
    (if (empty? y)
      resy
      (let [resynew (conj resy syy)
            syynew (map (fn [e] (let [ne (inc e)]
                                  (if (> ne 9)
                                    1
                                    ne))) syy)]
        (recur (rest y)  syynew resynew)))))

(defn gen-rows [s collength]
  (flatten (apply concat (map gen-row (partition collength s)))))

(defn generate-map-row [in collen]
  (loop [n (partition collen in)
         resx []]
    (if (empty? n)
      resx
      (let [newresx (gen-rows (first n) collen)
            newresxs (conj resx newresx)
            sxnew (map (fn [e] (let [ne (inc e)]
                                 (if (> ne 9)
                                   1
                                   ne))) resx)
            ]
        (recur (rest n) newresxs))
      )))

(defn gen-inc-maps [in collen]
  (loop [x (range 0 5)
         s in
         res []]
    (if (empty? x)
      (flatten res)
      (recur (rest x) (map (fn [e]
                             (let [ne (inc e)]
                               (if (> ne 9)
                                 1
                                 ne))) s)
             (conj res (generate-map-row s collen) )))))

(defn ->distances [columns rows]
  (merge  {0 0} (zipmap (range 1 (* columns rows)) (repeat Integer/MAX_VALUE))))

(defn ->inputs [columns rows]
  (range 0 (* columns rows)))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        weights (mapv #(Character/getNumericValue %) (apply concat raw))
        rows (count raw)
        columns (count (first raw))
        inputs (->inputs columns rows)
        distances (->distances columns rows)
        result (djikstra rows columns inputs distances weights (dec (* columns rows)))]
    (println "result:" result)
    #_(println "djik: "(djikstra rows columns inputs distances weighs (dec (* columns rows))))
    result
    )
  )

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        nums (mapv #(Character/getNumericValue %) (apply concat raw))
        rows5 (* 5 (count raw))
        columns5 (* 5 (count (first raw)))
        inputs (->inputs columns5 rows5)
        weights (gen-inc-maps nums (count (first raw)))
        distances (->distances columns5 rows5)
        result (djikstra rows5 columns5 inputs distances weights (dec (* columns5 rows5)))
        ]
    (println "rows5" rows5)
    (println "cols5" columns5)
    (println "result p2:" result)
    result
    ))
