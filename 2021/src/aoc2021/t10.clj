(ns aoc2021.t10)

(def example "resources/t10-example.txt")
(def path "resources/t10.txt")
(def input example)
(def input path)

(def score {\) 3
            \] 57
            \} 1197
            \> 25137})

(defn check-closing [entry s]
  (if (empty? s)
    entry
    ((first s) entry s)))

(defn accept
  "return either coll or value"
  [entryin s]
  #_(println "accepting" entryin " s " s)
  (condp = entryin
    \< (conj s (fn close1 [entry ss] (if (= entry \>) (rest ss) entry)))
    \> (check-closing entryin s)
    \[ (conj s (fn close2 [entry ss] (if (= entry \]) (rest ss) entry)))
    \] (check-closing entryin s)
    \{ (conj s (fn close3 [entry ss] (if (= entry \}) (rest ss) entry)))
    \} (check-closing entryin s)
    \( (conj s (fn close4 [entry ss] (if (= entry \)) (rest ss) entry)))
    \) (check-closing entryin s)
    ))

(defn illegal [entry]
  (loop [input (seq entry)
         check '()]
    (if ((complement coll?) check)
      check
      (if (empty? input)
        nil
        (recur (rest input) (accept (first input) check))))))

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))
        illegals (filter some? (map illegal raw))
        result (apply + (map score illegals))]
    (println "result:" result)))

(defn ->fixfn [fnname target]
  (fn fnname
    ([]
     target
     )
    ([entry s]
     (if (= entry target) (rest s) entry)
     )))

(defn fixer
  "Either accept or return fixing one"
  [entryin s]
  #_(println "accepting" entryin " s " s)
  (condp = entryin
    \< (conj s (->fixfn "close1" \>))
    \> (check-closing entryin s)
    \[ (conj s (->fixfn "close2" \]))
    \] (check-closing entryin s)
    \{ (conj s (->fixfn "close3" \}))
    \} (check-closing entryin s)
    \( (conj s (->fixfn "close4" \)))
    \) (check-closing entryin s)
    ))

(defn ->legal [entry]
  (loop [input (seq entry)
         fixes '()]
    (if (empty? input)
      (map #(%) fixes)
      (recur (rest input) (fixer (first input) fixes)))))

(def fixscore {\) 1
               \] 2
               \} 3
               \> 4})

(defn countfixscore [acc c]
  (+ (* 5 acc) (fixscore c)))

(defn p2 []
  (let [raw (clojure.string/split-lines (slurp input))
        legals (filter (complement illegal) raw)
        middleidx (int (Math/floor (/ (count legals) 2)))
        result (nth (sort (map #(reduce countfixscore 0 (->legal %)) legals)) middleidx)]
    (println "result:" result)))
