(ns aoc2022.day04)

(def path-example "resources/day04-example.txt")

(def path "resources/day04.txt")
;;(def input path-example)
(def input path)

(defn- parse-p1 [line]
  (let [sn (rest (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line))
        [a1 a2 b1 b2] (map #(Long. %) sn)
        [s1 e1 s2 e2] (if (< a1 b1) [a1 a2 b1 b2]
                          (if (not= a1 b1) [b1 b2 a1 a2]
                              (if (<= a2 b2)
                                [b1 b2 a1 a2]
                                [a1 a2 b1 b2]

                                )
                              ))
        result (<= e2 e1)
        ]
    (println line "" s1 e1 "/" s2 e2 " result " result)
    result
    ))


(defn- parse-p2 [line]
  (let [sn (rest (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" line))
        [a1 a2 b1 b2] (map #(Long. %) sn)
        [s1 e1 s2 e2] (if (< a1 b1) [a1 a2 b1 b2]
                          (if (not= a1 b1) [b1 b2 a1 a2]
                              (if (<= a2 b2)
                                [b1 b2 a1 a2]
                                [a1 a2 b1 b2]

                                )
                              ))
        result (< e1 s2)
        ]
    (println line "" s1 e1 "/" s2 e2 " result " result)
    result
    ))
(comment
  (parse "4-6,3-6")
  (parse "4-6,4-7")
  )

(defn p1 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (->> (map parse-p1 lines)
         (filter true?)
         count)))

(defn p2 []
  (let [lines (clojure.string/split-lines (slurp input))]
    (->> (map parse-p2 lines)
         (filter false?)
         count)))
