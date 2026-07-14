(ns aoc.day03)

(def input-example "resources/day03-example.txt")
(def input-real "resources/day03-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(def in (get-input input-example))

(defn- ->int [s]
  (if (char? s)
    (Integer/parseInt (str s))
    (if (string? s)
      (Integer/parseInt s)
      (throw (RuntimeException. "nopes")))))

(defn- calc-joltage
  "Calc joltage for a row"
  [s]
  {:pre [(string? s)]}
  (:maxseen
   (reduce (fn [{:keys [freqs a b maxseen] :or {a :notseen b :notseen}} v]
             (if (= :notseen a)
               {
                :a (->int v)
                :b :notseen
                :maxseen (->int v)
                }
               (if (= :notseen b)
                 {
                  :a a
                  :b (->int v)
                  :maxseen (->int (str a v))
                  }
                 (let [maxv (max a b)
                       newmax (->int (str maxv v))]
                   (if (> newmax maxseen)
                     {
                      :a maxv
                      :b (->int v)
                      :maxseen newmax
                      }
                     {
                      :a a
                      :b b
                      :maxseen maxseen
                      }
                     )
                   )
                 )
               )
             )
           {
            :maxseen 0
            }
           s
           )))

(comment
  (do
    (assert (= 98 (calc-joltage "987654321111111")))
    (assert (= 89 (calc-joltage "811111111111119")))
    (assert (= 78 (calc-joltage "234234234234278")))
    (assert (= 92 (calc-joltage "818181911112111")))
    )
  )

(defn p1 [in]
  (let [input (get-input in)]
    (->> (map calc-joltage input)
         (apply +)
         )
    ))

(p1 input-example)
(p1 input-real)
