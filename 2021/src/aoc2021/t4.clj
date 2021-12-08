(ns aoc2021.t4)

(def example "resources/t4-example.txt")
(def path "resources/t4.txt")
(def input example)
#_(def input path)

(defn ->table
  "6 rows, first is always empty"
  [rows]
  (let [x (rest rows)
        mat (mapcat (fn [i] (clojure.string/split i #" ")) x)
        ]
    (println "mat:" mat " length:" (count mat))
    mat
    )
  )

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        input (clojure.string/split (first raw) #",")
        _ (println "input:" input)
        _ (println "count:" (count raw))
        _ (->table (take 6 (rest raw)))
        ]))
