(ns aoc2021.day20)

;; Good training for perf tuning. added to-arrays and aget instead of nth and alength instead of count.
(def example "resources/day20-example.txt")
(def actual "resources/day20.txt")
(def input example)
(def input actual)

(defn ->bits [c]
  (if (= \# c) 1 0))

(defn input->map [in]
  (let [lines       (clojure.string/split-lines (slurp in))
        mask        (to-array (map ->bits (first lines)))
        image       (to-array (map ->bits (mapcat identity (drop 2 lines))))
        columnsrows (int (Math/sqrt (count image)))]
    {:mask  mask
     :image image
     :cols  columnsrows}))

(defn neighbours
  [^long cols ^long idx]
  (let [n  (- idx cols)
        s  (+ idx cols)
        w  (dec idx)
        e  (inc idx)
        se (inc s)
        sw (dec s)
        ne (inc n)
        nw (dec n)]
    [nw n ne w idx e sw s se]))

(defn pad
  "Expect square n*n, pad it to be (n+4)^2"
  [image bg]
  (let [cols      (int (Math/sqrt (count image)))
        rows      (partition cols image)
        extrarows (repeat 2 (repeat (+ 4 cols) bg))
        rowpad    [bg bg]
        newrows   (map (fn [x] (concat rowpad x rowpad)) rows)
        result    (long-array (flatten (concat extrarows newrows extrarows)))]
    result))

(defn newbackground-o [mask bg]
  (aget mask (if (= bg 0) 0 511)))

(defn ->indices [npow]
  (let [n       (int (Math/sqrt npow))
        or      (range npow)
        pr      (partition n or)
        middles (drop-last (drop 1 pr))
        res     (flatten (map (fn [x] (drop-last (drop 1 x))) middles))]
    res))

(defn idcol->newval [mask idstr]
  (aget mask (Long/parseLong idstr 2)))

(defn enhance
  [in-image id-fn in-bg bg-fn n]
  (loop [i     0
         bg    in-bg
         image in-image]
    (if (= i n)
      image
      (let [padded   ^longs (pad image bg)
            padcount (alength padded)
            cols     (int (Math/sqrt padcount))
            newimage (mapv (fn [x]
                            (let [nei   (neighbours cols x)
                                  ;; performance tuning part: change aget to nth to see performance gone much worse.
                                  idcol (apply str (map (fn [y] (aget padded y)) nei))]
                              (id-fn idcol)))
                          (->indices padcount))
            newbg    (bg-fn bg)]
        (recur (inc i) newbg newimage)))))

(defn do-it [n]
  (let [{:keys [mask image]} (input->map input)
        bg-fn                (memoize (partial newbackground-o mask))
        id-fn                (memoize (partial idcol->newval mask))
        result               (count (filter #(= % 1) (enhance image id-fn 0 bg-fn n)))]
    result))

(defn p1 []
  (let [result (do-it 2)]
    (println "p1 result:" result)
    result))

(defn p2 []
  (let [result (do-it 50)]
    (println "p2 result:" result)
    result))
