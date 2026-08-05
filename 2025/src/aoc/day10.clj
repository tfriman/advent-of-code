(ns aoc.day10
  (:require [clojure.math :as math]
            [clojure.test]))

(def input-example "resources/day10-example.txt")
(def input-real "resources/day10-gitignore.txt")

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

(defn- parse-lights-to-long-and-length
  "Parse [.##..].* to seq true/false on/off"
  [r]
  (let [[_ xs] (re-find #"\[(.*)\].*" r)]
    {:long
     (Long/parseLong
      (->>  xs
            (map #(condp = % \# 1 \. 0))
            (apply str))
      2)
     :length (count xs)
     }))

(defn- parse-buttons-to-long
  "Parse d+, series to long based on length. Most significant position is 0."
  [length bseq]
  (let [button-strings (clojure.string/split bseq #",")
        buttons (map Long/parseLong button-strings)
        original (vec (repeat length "0"))
        result (reduce (fn [acc i] (assoc acc i "1")) original buttons)]
    (Long/parseLong (apply str result) 2)))

(defn- parse-row
  "Map containing segments"
  [r]
  (let [[_ lights-raw buttons-raw weights-raw] (re-find #"(\[.*\]) (.*) \{(.*)\}" r)
        lights (parse-lights-to-long-and-length lights-raw)
        buttons-seq   (-> buttons-raw
                          (clojure.string/replace #"\(|\)" "")
                          (clojure.string/split #" "))
        buttons (map (partial parse-buttons-to-long (:length lights)) buttons-seq)
        ]
    {:lights lights
     :buttons buttons}))

(defn- apply-xor [buttons in]
  (map (fn [x] (bit-xor x in)) buttons))

(comment
  (apply-xor [20 24] 0)
  )

(defn- find-shortest
  "Finds target with shortest possible moves"
  [target buttons]
  (loop [seen #{0}
         round 0
         ]
    (if (seen target)
      round
      (let [new-ones (mapcat (fn [x] (apply-xor buttons x)) seen)]
        (recur (into seen new-ones) (inc round)))
      )))


(defn p1 [in]
  (let [input (->> in
                   get-input
                   (map parse-row)
                   )]
    (apply + (map (fn [pr] (find-shortest (-> pr :lights :long) (:buttons pr)) ) input))))

(p1 input-real)
