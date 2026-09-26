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
  (let [[_ lights-raw buttons-raw joltages-raw] (re-find #"(\[.*\]) (.*) \{(.*)\}" r)
        lights (parse-lights-to-long-and-length lights-raw)
        buttons-seq   (-> buttons-raw
                          (clojure.string/replace #"\(|\)" "")
                          (clojure.string/split #" "))
        buttons (map (partial parse-buttons-to-long (:length lights)) buttons-seq)
        joltages (mapv ->long (clojure.string/split joltages-raw #","))
        ]
    {:lights lights
     :buttons buttons
     :buttons-p2 (map (fn [s] (map ->long (clojure.string/split s #","))) buttons-seq)
     :joltages joltages}))

(defn- apply-xor [buttons in]
  (map (fn [x] (bit-xor x in)) buttons))

(defn- find-shortest
  "Finds target with shortest possible moves"
  [target buttons]
  (loop [seen #{0}
         round 0]
    (if (seen target)
      round
      (let [new-ones (mapcat (fn [x] (apply-xor buttons x)) seen)]
        (recur (into seen new-ones) (inc round))))))

(defn p1 [in]
  (let [input (->> in
                   get-input
                   (map parse-row)
                   )]
    (apply + (map (fn [pr] (find-shortest (-> pr :lights :long) (:buttons pr)) ) input))))

;; (p1 input-real)

;; p2 starts ========================================================

(defn- apply-inc
  "Increases in with all buttons applied."
  [buttons in]
  {:pre [(clojure.test/is (sequential? buttons))
         (or (sequential? in )
             (throw (Exception. (format "Pre-condition failed; %s is not sequential." in))))
         ]}
  (map (fn [x] (reduce (fn [acc i] (update acc i inc)) in x)) buttons))

(defn- remove-out-of-bounds
  "When one of the increments overflows, don't add that to the output set"
  [target joltages]
  {:pre [(clojure.test/is (sequential? joltages))
         (clojure.test/is (sequential? target))]}
  (remove (fn [x]
            ;; check number by number. There could be a better way too here.
            (if (= (count x) (count target))
              (let [res (some (fn [[a b]] (> a b)) (map (fn [a b] [a b]) x target))]
                res)
              (do (println "x" x) false)))
          joltages))

(comment
  (remove-out-of-bounds [1 2 3 4 5] (apply-inc [[3] [1 3]] [0 0 0 0 0]))
  (remove-out-of-bounds [1 2 3 4 5] (apply-inc [[3] [1 3]] [1 2 1 1 1]))
  )

(comment
  (defn- find-joltages-min-max
    "For debugging purposes find the min and max of the joltages"
    [joltages]
    (map (fn [[a b]] (- b a)) (map (juxt (partial apply min) (partial apply max)) joltages)))
  (defn- button-distributions
    "For debugging purposes, find which is the rarest button wiring"
    [buttons]
    (->> buttons
         (mapcat identity)
         frequencies
         (map (fn [[k v]] [v k]))
         sort
         first
         second))
  (do
    (def buttons-x [[0 1 5 8]
                    [1 6 7]
                    [3 6 8]
                    [1 3 6 7]
                    [0 1 2 6 7]
                    [1 2 3 5 7]
                    [0 1 3 4 5 6 7]
                    [1 2 4 5 7 8]
                    [0 2 5 7 8]
                    [1 2 3 5 7 8]])
    (button-distributions buttons-x)
    )

  (->> input-example get-input (map parse-row))
  (def inx (->> input-example get-input (map parse-row)))
  (find-joltages-min-max (map :joltages inx))
  (def inx-real (->> input-real get-input (map parse-row)))
  (apply max (find-joltages-min-max (map :joltages inx-real)))
  (first (->> input-real get-input (map parse-row)))
  )

(comment
  (p2 input-real)
  )

;; ideas:
;; DISCARD THIS 2 find the rarest button and it's levers. use only those to get the required amount? exclude them after that.
;; - first find those which affect the rarest and take them and create all combinations. then apply others to those?
;; 3 find the smallest joltage and its buttons, apply those and then remove them from the pool.

(defn find-smallest-joltage-with-index
  "Find the smallest joltage from those not seen, [joltage index]"
  [joltages consumed-indices]
  {:pre [(clojure.test/is (set? consumed-indices))]}
  (let [
        indexed (map-indexed (fn [idx itm] [itm idx] ) joltages)
        remaining (remove (fn [[itm idx ]] (consumed-indices idx)) indexed)]
    (first (sort remaining))))

(comment
  (do
    (assert (= [1 5] (find-smallest-joltage-with-index [6 5 4 3 2 1 5] #{})))
    (assert (= [1 6] (find-smallest-joltage-with-index [6 5 4 3 2 1 1] #{5})))
    (assert (= [2 4] (find-smallest-joltage-with-index [6 5 4 3 2 1 2] #{5})))
    ))

(defn- find-matching-buttons
  "Finds matching buttons for index
  ;; TODO could return two sets here, one with matching and one without.
  "
  [idx buttons]
  (filter (fn [x] ((set x) idx)) buttons))

(comment
  (do
    (find-matching-buttons 2 [[0] [2] [1 2 3]])
    (find-matching-buttons 0 [[0] [2] [1 2 3]])
    ))

(defn- remove-matching-ones
  "Removes second list from the first"
  [all matching]
  (let [m (set matching)]
    (remove (fn [x] (m x)) all)))

(comment
  (do
    (assert (= [] (remove-matching-ones [[1]] [[1]])))
    (assert (= [[1]] (remove-matching-ones [[1]] [[2]])))
    (assert (= [[1]] (remove-matching-ones [[1] [2]] [[2]])))))

(defn- merge-map-by-smallest-key
  "merges map so that smallest key wins in ties"
  [input-map [k v]]
  {:pre [(clojure.test/is (map? input-map))
         (clojure.test/is (sequential? k))
         (or (pos-int? v)
             (throw (Exception. (format "Pre-condition failed; %s is not pos-int." v))))
         ]}
  (if-let [x (get input-map k)]
    (if (> x v)
      (assoc input-map k v)
      input-map)
    (assoc input-map k v)))

(comment
  (do
    (assert (= {[1] 2} (merge-map-by-smallest-key {} [[1] 2])))
    (assert (= {[1] 11} (merge-map-by-smallest-key {[1] 33} [[1] 11])))
    (assert (= {[1] 11 [2] 3} (merge-map-by-smallest-key {[1] 33 [2] 3} [[1] 11])))
    (assert (= {[1] 11 [2] 3} (merge-map-by-smallest-key {[2] 3} [[1] 11]))))
  )

(defn- exhaust-apply-until-filled
  "Apply until the limit has been hit, return setups and their counts until there.
   Must return all combinations because this provides interim values for the next steps.
   Returns [list-of-joltages-with-target count]
  "
  [target-joltage-index target-all-joltages current-joltages matching-buttons]
  {:pre [(or (> target-joltage-index -1)
             (throw (AssertionError. (str "not pos-int:" target-joltage-index))))
         (clojure.test/is (sequential? target-all-joltages))
         (clojure.test/is (sequential? current-joltages))
         (clojure.test/is (sequential? matching-buttons))]}
  (if-let [target (get target-all-joltages target-joltage-index)]
    (loop [current (if (empty? current-joltages)
                     [(vec (repeat (count target-all-joltages) 0))]
                     current-joltages)
           c 0]
      (do
        (println (str "size of current:" (count current) " and c:" c))
        (let [new-possibles (->> (mapcat (fn [x] (apply-inc matching-buttons x)) current)
                                 (remove-out-of-bounds target-all-joltages))]
          (if (or (zero? (count new-possibles))
                  ;; if matches, then return immediately
                  ((set new-possibles) target-all-joltages))
            [current c]
            (recur new-possibles (inc c))))))
    (do
      (println "no index " target-joltage-index " found")
      (throw (AssertionError. "Wrong input")))))

(comment
  (exhaust-apply-until-filled 1 [3 2 4] [] [[1] [0 1 2]])
  )

(defn add-round
  [li rounds]
  {:pre [(clojure.test/is (sequential? li))
         (clojure.test/is (pos-int? rounds))]}
  (mapcat (fn [{:keys [l c]}] (map (fn [z] {:l (conj l z) :c (+ c z)} ) (range rounds))) li))

(defn- remove-too-fars [li target] (remove (fn [{:keys [c]}] (> c target)) li))

(defn- get-all-combinations-fixed
  "Target is button presses needed.
   Button count is how many of those are involved.
   Treshold is how many can be missing.
   "
  [target button-count treshold]
  (let [initial [{:l [] :c 0}]
        source (inc target)]
    (->>
     (reduce (fn [acc index]
               (-> (add-round acc source)
                   (remove-too-fars target)))
             initial
             (range button-count)
             )
     (filter (fn [{:keys [c]}]  (>= treshold (- target c) 0)))
     (map :l)
     )))
(comment
  (get-all-combinations-fixed 5 3 1)
  )

(defn- exhaust-apply-until-filled-new
  "Apply until the limit has been hit, return setups and their counts until there.
   Must return all combinations because this provides interim values for the next steps.
   Returns [list-of-joltages-with-target count]
  "
  [target-joltage-index target-all-joltages current-joltages matching-buttons treshold]
  {:pre [(or (> target-joltage-index -1)
             (throw (AssertionError. (str "not pos-int:" target-joltage-index))))
         (clojure.test/is (sequential? target-all-joltages))
         (clojure.test/is (sequential? current-joltages))
         (clojure.test/is (sequential? matching-buttons))
         (clojure.test/is (> treshold -1))
         ]}
  (if-let [target (get target-all-joltages target-joltage-index)]
    (let [combs (get-all-combinations-fixed target (count matching-buttons) treshold)
          empty-joltage-vec (vec (repeat (count target-all-joltages) 0))
          count-button-pairs (map (fn [combination]
                                    (map (fn [c button] [c button]) combination matching-buttons))
                                  combs)
          additions (map (fn [z]
                           (reduce (fn [acc [c button]]
                                     (if (zero? c)
                                       acc
                                       (reduce (fn [a b] (update a b (fn [x] (+ x c)))) (vec acc) button)
                                       )
                                     )
                                   empty-joltage-vec
                                   z
                                   ))
                         count-button-pairs)
          new-joltages (mapcat (fn [in]
                                 (map (fn [addition]
                                        (mapv (fn [a b] (+ a b)) in addition))
                                      additions
                                      ))
                               current-joltages
                               )
          matches (remove (fn [x] (not= (get x target-joltage-index) target)) new-joltages)
          res       (remove-out-of-bounds target-all-joltages matches)
          ]
      [res 1] ;; TODO hardcoded count to 1
      )
    (do
      (println "no index " target-joltage-index " found")
      (throw (AssertionError. "Wrong input")))))

(comment
  (def x (exhaust-apply-until-filled-new 1 [3 2 4] [[0 0 0]] [[1] [0 1 2]] 0))
  (def x2 (exhaust-apply-until-filled-new 0 [3 2 4] (first x) [[0] [0 2]] 2))
  (def x3 (exhaust-apply-until-filled-new 2 [3 2 4] (first x2) [[2]] 3))
  )

(defn find-smallest-joltage-and-apply-new
  "Finds the smallest joltage, it's buttons and create all the variants for that using those only.
  Then remove the used buttons from the pool.
  This can be looped again and again until solution is reached!
  Remember to check that joltages are not exceeded!
  "
  [buttons joltages-so-far target-joltage seen-joltage-indices-set treshold]
  (let [[joltage idx] (find-smallest-joltage-with-index target-joltage seen-joltage-indices-set)
        matching-buttons (find-matching-buttons idx buttons)
        [new-joltages c] (exhaust-apply-until-filled-new idx target-joltage joltages-so-far matching-buttons treshold)]
    {:buttons (remove-matching-ones buttons matching-buttons)
     :candidates new-joltages
     :count c
     :idx idx
     :treshold joltage
     }))

(defn- do-p2-loop-new
  "Loop until all indices are through. OR the solution found."
  [target-joltage buttons-in]
  (let [r (find-smallest-joltage-and-apply-new buttons-in [(vec (repeat (count target-joltage) 0))] target-joltage #{} 0)
        all-indices (into #{} (range 0 (count target-joltage)))]
    (loop [buttons (:buttons r)
           candidates (:candidates r)
           c (:count r)
           treshold (:treshold r)
           seen-indices #{(:idx r)}]
      (do
        (println "c" c "seen" seen-indices "candidates" (count candidates) "buttons" (count buttons) "treshold" treshold)
        (if (or (= all-indices seen-indices)
                ((set candidates) target-joltage))
          [c candidates]
          (let [res (find-smallest-joltage-and-apply-new buttons candidates target-joltage seen-indices treshold)]
            (recur (:buttons res) (:candidates res) (+ c (:count res)) (:treshold res) (conj seen-indices (:idx res)))))))))

(comment
  (get-all-combinations 5 3)
  (def test-data (->> input-example
                      get-input
                      (map parse-row)))
  (def test1 (first test-data))
  (do-p2-loop-new (:joltages test1) (:buttons-p2 test1))
  (def real (->> input-real
                 get-input
                 (map parse-row)
                 ))
  (def second-real (nth real 4))
  (do-p2-loop-new (:joltages second-real) (:buttons-p2 second-real))

  )
