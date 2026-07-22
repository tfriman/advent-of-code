(ns aoc.day07)

(def input-example "resources/day07-example.txt")
(def input-real "resources/day07-gitignore.txt")

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

(defn- parse-start-pos
  "0 based indexing"
  [row]
  (clojure.string/index-of row "S"))


(defn- parse-split-positions
  "Seq of 0 based positions for ^ chars"
  [row]
  (loop [skip 0
         indices []]
    (let [result-index (clojure.string/index-of row "^" skip)]
      (if (nil? result-index)
        indices
        (recur (inc result-index) (conj indices result-index))))))

(comment
  (def first-row ".......S.......")
  (count first-row) ;; 15 total
  (parse-start-pos first-row) ;;
  (parse-split-positions "....^.^...^....")
  (parse-split-positions "^.^")
  (parse-split-positions "....^.")
  (parse-split-positions ".")
  )

(defn- parse-split-rows [rows]
  (remove empty? (map parse-split-positions rows)))

(defn- split-beam
  "Split to both sides"
  [pos]
  [(dec pos) (inc pos)])

(defn- process-splitter-row
  "Check which beams hit the splitters and split. Calculate splits and return new beams seq"
  [beams splitters]
  (let [original-beams (count beams)
        beamset (into #{} beams)
        splitterset (into #{} splitters)
        hits (clojure.set/intersection beamset splitterset)
        split-count (count hits)
        new-beams (mapcat split-beam hits)
        all-beams-super (into #{} (concat new-beams beams))
        all-beams (clojure.set/difference all-beams-super hits)]
    {:split-count split-count
     :beams all-beams}
    ))

(defn p1 [in]
  (let [input (get-input in)
        splitters (parse-split-rows input)
        start-pos (parse-start-pos (first input))]
    ;;[start-pos splitters]
    (reduce (fn [{orig-beams :beams  orig-count :split-count :as acc} splitter-row]
              (let [{:keys [beams split-count]} (process-splitter-row orig-beams splitter-row)]
                {
                 :beams beams
                 :split-count (+ split-count orig-count)
                 }
                ))
            {
             :beams [start-pos]
             :split-count 0
             }
            splitters
            )))

(p1 input-example)
(p1 input-real)


;; ----- p2 -----

(defn- add-to-map
  "Adds values to map under key, if found, added to existing"
  [m k v]
  {:pre [(clojure.test/is (map? m)) (clojure.test/is (keyword? k)) (clojure.test/is (number? v))]}
  (update-in m [k] (fnil + 0) v))

(comment
  (do
    (add-to-map {} :a 3)
    (add-to-map {:b 0} :a 33)
    (add-to-map {:a 2} :a 3))
  )

(defn- process-splitter-row-p2
  "Check which beams hit the splitters and split and add those to the input map. First remove all matching ones before adding! Calculate splits and return new beams map"
  [beams-map splitters]
  {:pre [(clojure.test/is (map? beams-map)) (clojure.test/is (vector? splitters))]}
  (reduce
   (fn [beams splitter]
     {:pre [(clojure.test/is (map? beams)) (clojure.test/is (number? splitter))]}
     (if-let [has-match (get beams-map (keyword (str splitter)))]
       (let [hits (split-beam splitter)
             _ (def hits hits)]
         (reduce (fn [a v] (add-to-map a (keyword (str v)) has-match)) beams hits))
       beams))
   {}
   splitters
   ))

(defn- clear-map
  [m k]
  {:pre [(clojure.test/is (map? m)) (clojure.test/is (keyword? k))]}
  (dissoc m k))

(defn p2 [in]
  (let [input (get-input in)
        splitters (parse-split-rows input)
        start-pos (parse-start-pos (first input))]
    (->>
     (reduce (fn [acc splitter-seq]
               (let [new-hits (process-splitter-row-p2 acc splitter-seq)
                     hits-removed (reduce (fn [a x] (dissoc a (keyword (str x)))) acc splitter-seq)]
                 (merge-with + hits-removed new-hits)))
             {(keyword (str start-pos)) 1}
             splitters
             )
     vals
     (apply +))))

(p2 input-real)
