(ns aoc2021.day02)

(def example "resources/day02-example.txt")
(def path "resources/day02.txt")
(def input path)

(defn get-by [in reg]
  (map #(Integer/parseInt (second %)) (filter not-empty (map #(re-find reg %) in))))

(defn p1 []
  (let [in (clojure.string/split-lines (slurp input))
        fwd (apply + (get-by in #"forward (\d+)"))
        down (apply + (get-by in #"down (\d+)"))
        up (apply + (get-by in #"up (\d+)"))
        result (* fwd (- down up))
        ]
    (println "result" result)))

(def inputmap {:aim 0
               :horizontal 0
               :depth 0})

(defn get-res [{:keys [aim horizontal depth]} v]
  (let [i (Integer/parseInt (re-find #"\d+" v))]
    (if (re-find #"forward" v)
        {:aim aim
         :horizontal (+ i horizontal)
         :depth (+ (* i aim) depth)}
      (if (re-find #"up" v)
          {:aim (- aim i)
           :horizontal horizontal
           :depth depth}
          {:aim (+ aim i)
           :horizontal horizontal
           :depth depth} )))
  )

(defn p2 []
  (let [in (clojure.string/split-lines (slurp input))
        result (reduce get-res inputmap in)]
    (println "result:" (* (:horizontal result) (:depth result)))))
