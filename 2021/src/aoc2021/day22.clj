(ns aoc2021.day22)

;; Part 1 could be brute forced with each on coordinate in memory but part 2 was too hard.
;; I had to check https://www.reddit.com/r/adventofcode/comments/rlxhmg/2021_day_22_solutions/ for ideas.
;; Ended up using signed cubes.
;; Idea is to sign each volume (intersection or original) with the opposite sign than what was found already.
;; That prevents double calculations when in the end one calculates the volumes and uses the sign either to add or remove led count.
(def example "resources/day22-example-2-only50x50.txt")
(def short-example "resources/day22-example-1.txt")
(def example-pt2 "resources/day22-pt2-example.txt")
(def path "resources/day22.txt")

(defn- ->num [s] (Long/parseLong s))

(defn- parse-instruction
  "Parse instruction row"
  [s]
  (let [[_ action x1 x2 y1 y2 z1 z2] (re-matches #"(\w+) x=([-]{0,1}\d+)\.\.([-]{0,1}\d+),y=([-]{0,1}\d+)\.\.([-]{0,1}\d+),z=([-]{0,1}\d+)\.\.([-]{0,1}\d+)" s)]
    {:sign (if (= "on" action) 1 -1)
     :x1   (->num x1)
     :x2   (->num x2)
     :y1   (->num y1)
     :y2   (->num y2)
     :z1   (->num z1)
     :z2   (->num z2)}))

(defn- filter-area [{:keys [x1 x2 y1 y2 z1 z2]}]
  (every? #(<= -50 % 50) [x1 x2 y1 y2 z1 z2]))

(defn- intersects-box
  "Return intersection bounding box or nil if no intersection.
  {:x1 1 :x2 2...}"
  [{v1x1 :x1 v1x2 :x2 v1y1 :y1 v1y2 :y2 v1z1 :z1 v1z2 :z2} {v2x1 :x1 v2x2 :x2 v2y1 :y1 v2y2 :y2 v2z1 :z1 v2z2 :z2}]
  ;; if v1 x/y/z max coord is smaller than v2 min, then boxes do not overlap. and vice versa.
  ;; This doesn't expect coords to be ordered (but they probably are)
  (if (or (nil? v1x1)
          (nil? v2x1)
          (or (< (max v1x1 v1x2) (min v2x1 v2x2)) (< (max v2x1 v2x2) (min v1x1 v1x2)))
          (or (< (max v1y1 v1y2) (min v2y1 v2y2)) (< (max v2y1 v2y2) (min v1y1 v1y2)))
          (or (< (max v1z1 v1z2) (min v2z1 v2z2)) (< (max v2z1 v2z2) (min v1z1 v1z2))))
    nil                                                     ;; no intersection
    ;; Find the bounding box if there was a hit
    {:x1 (max (min v1x1 v1x2) (min v2x1 v2x2))
     :x2 (min (max v1x1 v1x2) (max v2x1 v2x2))
     :y1 (max (min v1y1 v1y2) (min v2y1 v2y2))
     :y2 (min (max v1y1 v1y2) (max v2y1 v2y2))
     :z1 (max (min v1z1 v1z2) (min v2z1 v2z2))
     :z2 (min (max v1z1 v1z2) (max v2z1 v2z2))}))

(defn- volume
  [{:keys [x1 x2 y1 y2 z1 z2]}]
  {:pre [(and (<= x1 x2) (<= y1 y2) (<= z1 z2))]}
  (* (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1)))

(defn- calc-intersections-with-sign
  "Calculates volumes and counts based on that"
  [boxes-in]
  (loop [boxes boxes-in
         seen-volumes {}]
    (if (empty? boxes)
      seen-volumes
      (let [vol (first boxes)
            new (map (fn [[k c]] [(intersects-box vol k) (* -1 c)]) seen-volumes)
            all (remove (fn [[k _]] (nil? k)) new)
            new-volume-count-pairs (into (if (= 1 (:sign vol)) [[(dissoc vol :sign) 1]] []) all)
            accumulator (reduce (fn [acc [k v]] (update-in acc [k] (fnil + 0) v)) seen-volumes new-volume-count-pairs)]
        (recur (rest boxes) accumulator)))))

(defn- calculate [p filter-fn]
  (let [volumes (->> (slurp p)
                     clojure.string/split-lines
                     (map parse-instruction)
                     (filter filter-fn))
        vol-counts (calc-intersections-with-sign volumes)
        led-volumes (map (fn [[k c]] (* c (volume k))) vol-counts)
        result (apply + led-volumes)]
    result))

(defn p1and2 []
  (println "p1" (calculate path filter-area))
  (println "p2" (calculate path identity)))



