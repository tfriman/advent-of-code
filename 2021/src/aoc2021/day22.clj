(ns aoc2021.day22)

(def example "resources/day22-example-2.txt")
(def short-example "resources/day22-example-1.txt")
(def path "resources/day22.txt")
(def input short-example)
(def input example)
(def input path)

(defn- ->num [s] (Long/parseLong s))

(defn- parse-instruction
  "Parse instruction row"
  [s]
  #_(println "parsing" s)
  (let [[_ action x1 x2 y1 y2 z1 z2] (re-matches #"(\w+) x=([-]{0,1}\d+)\.\.([-]{0,1}\d+),y=([-]{0,1}\d+)\.\.([-]{0,1}\d+),z=([-]{0,1}\d+)\.\.([-]{0,1}\d+)" s)]
    {:action (keyword action)
     :x1 (->num x1)
     :x2 (->num x2)
     :y1 (->num y1)
     :y2 (->num y2)
     :z1 (->num z1)
     :z2 (->num z2)}))

(defn- filter-area [{:keys [x1 x2 y1 y2 z1 z2]}]
  (every? #(<= -50 % 50) [x1 x2 y1 y2 z1 z2]))

(defn- new-on
  "Set in containing those on, then adds or removes new set to that"
  [startset {:keys [action x1 x2 y1 y2 z1 z2]}]
  #_(println "action" action x2 y1)
  (let [new (for [x (range x1 (inc x2))
                  y (range y1 (inc y2))
                  z (range z1 (inc z2))]
              [x y z])
        _ (def new new)
        new (into #{} new)
        ]
    (if (= :on action)
      (clojure.set/union startset new)
      (clojure.set/difference startset new))))

(defn p1 []
  (let [inputs (clojure.string/split-lines (slurp input))
        _ (def inputs inputs)
        ins (->> inputs
                 (map parse-instruction)
                 (filter filter-area))
        result (reduce (fn [a x] (new-on a x)) #{} ins)
        ]
    result)
  )
