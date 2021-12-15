(ns aoc2021.t15
  (:require [clojure.data.priority-map :refer [priority-map]]))

(def example "resources/t15-example.txt")
(def examplep2 "resources/t15-p2-example.txt")

(def path "resources/t15.txt")
(def input example)
(def input path)
#_(def input examplep2)

(defn neighbours [idx cols rows]
  #_(println "idx:" idx " cols " cols " rows " rows)
  (let [up (when (>= idx cols) (- idx cols))
        down (when (> (* cols rows) (+ idx cols) ) (+ idx cols))
        left (when (not= 0 (mod idx cols)) (dec idx))
        right (when (not= (dec cols) (mod idx cols)) (inc idx))]
    (filterv some? [up down left right])))

(defn ->priority-queue
  "s elems are indices. distmap contains weights"
  [s distmap]
  (sort-by #(distmap % Integer/MAX_VALUE) (take 6000 s)))

(defn update-distances [idx neighbouridxs distmap priomap weighs]
  #_(println "update-distances:" idx neighbouridxs)
  (loop [n (first neighbouridxs)
         r (rest neighbouridxs)
         dm distmap
         pm priomap]
    (if (nil? n)
      {:dist dm
       :prio pm}
      (let [oldd (dm n Integer/MAX_VALUE)
            prospectd (+ (dm idx Integer/MAX_VALUE) (nth weighs n))
            update? (> oldd prospectd)
            newdm (if update?
                    (assoc dm n prospectd)
                    dm)
            newpm (if update?
                    (assoc pm n prospectd)
                    pm)]
        (recur (first r) (rest r) newdm newpm)))))

(defn djikstra [rows cols inputseq distancemap weighs target]
  (println "Djikstra searching for " target)
  (let [inputdistmap (->
                       (into (priority-map) (map (fn [x] [x Integer/MAX_VALUE]) inputseq))
                       (assoc 0 0))]
    (println "input 5 first " (take 5 inputdistmap))
    (loop [nodeidx (ffirst inputdistmap)
           inputs (pop inputdistmap)
           distmap distancemap]
      #_(println "nodeidx" nodeidx)
      (if (nil? nodeidx)
        (distmap target)
        (let [neighs (neighbours nodeidx rows cols)
              #_ (println "neighs" neighs)
              ud (update-distances nodeidx neighs distmap inputs weighs)
              prio (dissoc (:prio ud) nodeidx)
              #_ (println "first prio" (first prio))
              #_ (println "first prio rest" (first (pop  prio)))
              dist (:dist ud)]
          (recur (ffirst prio) (if (empty? prio) {} (pop prio)) dist))))))

(defn djikstra-old [rows cols inputseq distancemap weighs target]
  (println "Djikstra searching for " target)
  (loop [nodeidx (first (->priority-queue inputseq distancemap))
         inputs (rest (->priority-queue inputseq distancemap))
         distmap distancemap]
    (println "inputs:" (take 3 inputs))
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

  #_(merge  {0 0} (zipmap (range 1 (* columns rows)) (repeat Integer/MAX_VALUE)))
  {0 0}
  )

(defn ->inputs [columns rows]
  (range 0 (* columns rows)))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        weights ^ints (mapv #(Character/getNumericValue ^char %) (apply concat raw))
        rows (count raw)
        columns (count (first raw))
        inputs ^ints (->inputs columns rows)
        ;;     distances (->distances columns rows)
        ;;           result (djikstra rows columns inputs distances weights ^int (dec (* columns rows)))
        result (djikstra rows columns inputs {0 0} weights ^int (dec (* columns rows)))
        ]
    (println "result:" result)
    result))

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        nums (mapv #(Character/getNumericValue ^char %) (apply concat raw))
        rows5 (* 5 (count raw))
        columns5 (* 5 (count (first raw)))
        inputs (->inputs columns5 rows5)
        weights ^ints (gen-inc-maps nums (count (first raw)))
        distances (->distances columns5 rows5)
        result (djikstra rows5 columns5 inputs {0 0} weights (dec (* columns5 rows5)))
        ]
    (println "rows5" rows5)
    (println "cols5" columns5)
    (println "result p2:" result)
    result
    ))
