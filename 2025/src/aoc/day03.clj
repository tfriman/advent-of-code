(ns aoc.day03)

(def input-example "resources/day03-example.txt")
(def input-real "resources/day03-gitignore.txt")

(defn get-input [in]
  (-> in
      slurp
      clojure.string/split-lines))

(def in (get-input input-example))

(defn- update-freqs
  "Decreases freqs, if 0, removes key too."
  [f k]
  {:pre [(clojure.test/is (char? k))]}
  (println "got k" k)
  (if (= 1 (get f k))
    (dissoc f k)
    (update-in f [k] dec)))

(comment
  (do
    (assert (= {\8 3, \1 10, \2 1} (update-freqs (frequencies "818181911112111") \9)))
    (assert (= {\8 2, \1 10, \9 1, \2 1}) (update-freqs (frequencies "818181911112111") \8)))
  )

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
  ;; freqs, find largest, then 2nd largest behind that?
  (let [freqs-init (frequencies s)]
    ;; (get (frequencies "818181911112111") \8)
    (reduce (fn [{:keys [freqs a b maxseen] :or {a :notseen b :notseen}} v]
              (let [new-freqs (update-freqs freqs v)]
                (if (= :notseen a)
                  {
                   :freq new-freqs
                   :a (->int v)
                   :b :notseen
                   :maxseen (->int v)
                   }
                  (if (= :notseen b)
                    {
                     :freq new-freqs
                     :a a
                     :b (->int v)
                     :maxseen (->int (str a v))
                     }
                    (let [maxv (max a b)
                          newmax (->int (str maxv v))]
                      (if (> newmax maxseen)
                        {:freqs new-freqs
                         :a maxv
                         :b (->int v)
                         :maxseen newmax
                         }
                        {
                         :freqs new-freqs
                         :a a
                         :b b
                         :maxseen maxseen
                         }
                        )
                      )
                    )
                  ))
              )
            {
             :freqs freqs-init
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

(defn p1 []
  )
