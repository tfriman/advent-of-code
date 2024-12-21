(ns aoc2022.day05)

(def path-example "resources/day05-example.txt")

(def path "resources/day05.txt")
(def input path-example)
(def input path)

(defn- pad [input len]
  (let [origlen (count input)
        diff    (- len origlen)]
    (if (pos? diff)
      (str input (->> (repeat diff " ") (apply str)))
      input)))

(defn- parse-container [len row]
  (let [line   (-> (clojure.string/replace row #"\[(\w+)\] " "$1")
                   (clojure.string/replace #" \[(\w+)\]" "$1")
                   (clojure.string/replace #"\[(\w+)\]" "$1")
                   (clojure.string/replace #"\s{2,4}" " ")
                   (pad len))
        result (mapv (fn [x] (if (= \space x) nil x)) line)]
    result))

(comment
  (= [nil "D" nil] (parse-container 3  "    [D]" ))
  (parse-container 3 "    [D] [C]    " )
  (parse-container 3 "[Z] [M] [P]")
  (parse-container 9 "[N]     [F] [M]     [D] [V] [R] [N]")
  (= [\P nil \L nil nil \T nil nil nil] (parse-container 9 "[P]     [L]         [T]")))

(defn- parse-all [len parsedrows]
  (reduce (fn [a i]
            (->>
             (mapv (fn [x] (nth x i)) parsedrows)
             reverse
             (remove nil?)
             (assoc a (inc i)))
            )
          {}
          (range 0 len)))

(defn- parse-containers [inputs]
  (let [rev        (reverse inputs) ;; reverse
        len        (apply max (map #(Character/getNumericValue %) (clojure.string/replace (first rev) #" " "")))
        parser     (partial parse-container len)
        parsedrows (map parser (rest rev))
        parsed     (parse-all len parsedrows)]
    parsed))

(defn- parse-instruction [ifn input]
  (let [[amount from to] (->> (re-matches #"move (\d+) from (\d) to (\d)" input)
                              rest
                              (map #(Integer. %)))]
    (fn [m]
;;      (println "amount" amount "from" from "to" to "input" input)
      (let [fromcol    (get m from)
            tomove     (ifn (take amount fromcol))
            ;; p1 tomove (reverse (take amount fromcol))
            newfromcol (drop amount fromcol)
            tocol      (get m to)
            newtocol   (concat tomove tocol)]
        (-> (assoc m from newfromcol)
            (assoc to newtocol))))))

(comment
  (parse-instruction "move 1 from 2 to 1")
  "move 3 from 1 to 3"
  "move 2 from 2 to 1"
  "move 1 from 1 to 2"
  )

(defn solve [ifn]
  (let [[containerinput instructioninput] (split-with (fn [x] (not (empty? x))) (clojure.string/split-lines (slurp input)))
        containers                        (parse-containers containerinput)
        pif                               (partial parse-instruction ifn)
        instructions                      (map pif (rest instructioninput))
        resultmap                         (reduce (fn [a f] (f a)) containers instructions)
        _                                 (def resultmap resultmap)
        result                            (->> (map #(first (get resultmap %)) (range 1 10) )
                                               (apply str))
        ]
    (def cont containers)
    result
    ))

(defn p1 []
  (solve reverse)
  )

(defn p2 []
  (solve identity)
  )
