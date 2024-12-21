(ns aoc2021.day18
  (:require [clojure.zip :as z]
            [clojure.data :as data]))

(def example "resources/day18-example.txt")
(def example-pt2 "resources/day18-example-pt2.txt")

(def simple-example "resources/day18-example-simple.txt")
(def path "resources/day18.txt")
(def input simple-example)
(def input example)
(def input example-pt2)
(def input path)

(defn- find-5th-level-loc
  "Finds 5th level loc or nil"
  [z]
  (loop [z z]
    (if (or (z/end? z) (= 5 (-> z z/path count)))
      (if (z/end? z) nil (z/up z))
      (recur (z/next z) ))))

(defn- find-up
  "Goes up until root or left/right found or root meaning nil"
  [loc matchfn]
  (loop [loc loc]
    (if (nil? loc)
      nil
      (if (matchfn loc)
        (matchfn loc)
        (recur (z/up loc))))))

(defn- explode
  "Left side added to first regular number in left if any. Same for right. Always ends up with [x 0] or [0 x] pair
  Returns root node."
  [loc]
  (let [node   (z/node loc)
        lleft  (first node)
        lright (second node)
        res    (if-let [left (find-up loc z/left)] ;; todo can this be removed?
                 (-> (first (drop-while #(z/branch? %) (iterate z/prev loc)))
                     (z/edit (fn xleft [x & args] (+ x lleft)))
                     find-5th-level-loc
                     (z/replace 0))
                 (z/replace loc 0))
        result (if-let [right (find-up res z/right)]
                 (let [noderight (first (drop-while #(z/branch? %) (lazy-cat [right] (iterate z/next right))))
                       resu      (z/edit noderight (fn nobranchx [x & args] (+ x lright)))]
                   resu)
                 res)]
    (z/root result)))

(defn- needs-split?
  "loc needs split?"
  [loc]
  (if (z/branch? loc)
    nil
    (> (z/node loc) 9)))

(defn- find-split-loc
  "Returns loc where there is a pair to split or nil if none found"
  [loc]
  (loop [loc loc]
    (if (z/end? loc)
      nil
      (if (needs-split? loc)
        loc
        (recur (z/next loc))))))

(defn split
  "Gets loc pointing to a reg number needing to be split. Returns root loc.
  To split a regular number, replace it with a pair; the left element of the
   pair should be the regular number divided by two and rounded down, while
   the right element of the pair should be the regular number divided by two
   and rounded up.
   For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on."
  [loc]
  (let [v (z/node loc)]
    (z/root (z/replace loc [(int (Math/floor (/ v 2))) (int (Math/ceil (/ v 2)))]))))

(defn- add
  "Adds two vectors"
  [left right]
  (-> (z/vector-zip [])
      (z/append-child left)
      (z/append-child right)
      z/root))

(defn notpure?
  "Checks if loc !branch or branch without seq children."
  [loc]
  (or (not (z/branch? loc)) (some sequential? (z/children loc))))

(defn magnitude
  "pt2 calculation"
  [loc]
  (loop [loc (z/vector-zip loc)]
    (if (not (z/branch? loc))
      (z/node loc)
      (let [purenode (first (drop-while notpure? (lazy-seq [loc] (iterate z/next loc))))
            [l r]    (z/node purenode)
            resu     (z/replace purenode (+ (* 3 l) (* 2 r)))]
        (recur (z/vector-zip (z/root resu)))))))

(defn process-loc
  "Does explode / split until no changes needed. Takes vec in."
  [loc]
  (loop [loc (z/vector-zip loc)]
    (let [nxt (if-let [fifth (find-5th-level-loc loc)]
                (explode fifth)
                (if-let [splitloc (find-split-loc loc)]
                  (split splitloc)
                  nil))]
      (if (nil? nxt)
        (z/root loc)
        (recur (z/vector-zip nxt))))))

(defn- process-pair [a b] (process-loc (add a b)))

(defn do-it [inp]
  (let [ins    (map read-string (clojure.string/split-lines (slurp inp)))
        result (reduce process-pair ins)]
    (doto result (#(println "result" %)))))

(defn p1 [] (println "p1" (magnitude (do-it input))))

(defn- find-max-mag
  "Loop each pair and find the max"
  [s]
  (loop [[f & r] s
         res     0]
    (if (empty? r)
      res
      (let [newres (reduce (fn redufn [acc x]
                             (let [fx (magnitude (process-pair f x))
                                   xf (magnitude (process-pair x f))
                                   m  (max acc fx xf)]
                               m)) res r)]
        (recur r newres)))))

(defn p2 []
  (let [ins    (map read-string (clojure.string/split-lines (slurp input)))
        maxmag (find-max-mag ins)]
    (doto maxmag (#(println "p2:" %)))))
