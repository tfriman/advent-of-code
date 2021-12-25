(ns aoc2021.day20)

(def example "resources/day20-example.txt")
(def actual "resources/day20.txt")
(def input example)
#_(def input actual)

(defn ->bits [c]
  (if (= \# c) 1 0))

(defn input->map [in]
  (let [lines       (clojure.string/split-lines (slurp in))
        mask        (map ->bits (first lines))
        image       (map ->bits (mapcat identity (drop 2 lines)))
        columnsrows (int (Math/sqrt (count image)))]
    {:mask  mask
     :image image
     :cols  columnsrows}))

(defn neighbours
  [cols idx]
  (let [n  (- idx cols)
        s  (+ idx cols)
        w  (dec idx)
        e  (inc idx)
        se (inc s)
        sw (dec s)
        ne (inc n)
        nw (dec n)]
    [nw n ne w idx e sw s se]))

(defn ->newval
  "Calculate the new value for index bit."
  [mask image idx]
  (let [cols    (int (Math/sqrt (count image)))
        neighs  (neighbours cols idx)
        maskidx (Long/parseLong (apply str (map (fn [x] (nth image x)) neighs)) 2)
        newval  (nth mask maskidx)]
    newval))

(defn pad
  "Expect square n*n, pad it to be (n+4)^2"
  [image bg]
  (let [cols     (int (Math/sqrt (count image)))
        rows     (partition cols image)
        extrarow (repeat (+ 4 cols) bg)
        newrows  (map (fn [x] (concat [bg bg] x [bg bg])) rows)
        result   (flatten (concat extrarow extrarow newrows extrarow extrarow))]
    (def r result)
    result))

(defn newbackground [mask bg]
  (nth mask (if (= bg 0) 0 511)))

(defn ->indices [npow]
  (let [n       (int (Math/sqrt npow))
        or      (range npow)
        pr      (partition n or)
        middles (drop-last (drop 1 pr))
        res     (flatten (map (fn [x] (drop-last (drop 1 x))) middles))]
    res))

(defn enhance
  [in-image mask in-bg n]
  (loop [i     0
         bg    in-bg
         image in-image]
    (if (= i n)
      image
      (let [padded   (pad image bg)
            cols     (int (Math/sqrt (count padded)))
            newimage (flatten
                      (mapv (fn [x]
                              (let [nei   (neighbours cols x)
                                    idstr (apply str
                                                 (map (fn [y] (nth padded y)) nei))
                                    id    (Long/parseLong idstr 2)]
                                (nth mask id)))
                            (->indices (count padded))))
            newbg    (newbackground mask bg)]
        (recur (inc i) newbg newimage)))))

(comment
  (use 'clojure.pprint)
  (def in (input->map input))
  (count (filter #(= % 1) (enhance (:image in) (:mask in) 0 2)))
  (def foofn (partial ->newval (:mask in)))

  (def padded (pad (:image in) 0))
  (foofn padded 24)
  (Long/parseLong (apply str (map (fn [x] (nth padded x)) (neighbours 7 24))) 2)

  )

(defn p1 []
  (let [{:keys [mask image] :as in} (input->map input)
        result (count (filter #(= % 1) (enhance (:image in) (:mask in) 0 2)))]
    (println "p1 result:" result)
    result
    ))

(defn p2 []
  (let [{:keys [mask image] :as in} (input->map input)
        result (count (filter #(= % 1) (enhance (:image in) (:mask in) 0 50)))]
    (println "p2 result:" result)
    result
    ))
