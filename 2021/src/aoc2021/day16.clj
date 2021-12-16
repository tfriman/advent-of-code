(ns aoc2021.day16)
(def example "resources/day16-example.txt")
(def path "resources/day16.txt")
(def input example)
#_(def input path)

(defn strbin->int [s]
  (Integer/parseInt s 2))

(defn fix-leading [st fi]
  (let [pf (Long/parseLong (str fi) 16)
        pad (cond
              (= 0 pf) "0000"
              (= 1 pf) "000"
              (<= 2 pf 3) "00"
              (<= 4 pf 7) "0"
              :else "")]
    (str pad st)))

(defn parse-hex-str
  "Returns version (int), type (int) and payload (string)"
  [hs]
  (println "parse-hex-str" hs)
  (let [bso (Long/toBinaryString (Long/parseLong hs 16))
        bs (fix-leading bso (first hs))
        pversion (apply str (take 3 bs))
        ptype (apply str (take 3 (drop 3 bs)))
        payload (apply str (drop 6 bs))]
    {:version (strbin->int pversion)
     :type (strbin->int ptype)
     :payload payload
     :original bs}))

(defn literal? [{:keys [type]}] (= type 4))

(defn parse-literal
  "5 bit segments, drop first bit, concat and return as int val"
  [s]
  (def sx s)
  (let [segs (partition 5 s)
        binstrs (apply str (apply concat (map #(rest %) segs)))]
    (strbin->int binstrs)))

(defn parse-op-15
  "operator payload in, 15 first bits tell the length of the payload"
  [s]
  (prn "15:" s)
  (let [payload-length-bits (->> (take 15 s)
                                 (apply str))
        _ (def sb payload-length-bits)
        plen (strbin->int payload-length-bits)
        actual-payload (drop plen s)]
    actual-payload))

(defn parse-operator
  "Returns {:payload x}"
  [s]
  (let [len-type (first s)]
    (println "len-type:" len-type)
    (if (= \0 len-type)
      (do
        (println "15 bits")
        (parse-op-15 (rest s)))
      (do
        (println "11 bits ")
        ))))

(comment
  (def t4 (parse-hex-str "D2FE28") )
  (literal? t4)
  (def t6 (parse-hex-str "38006F45291200" ) )
  (literal? t6)
  (parse-literal (:payload t4))
  (parse-operator (:payload t6))
  )

(defn p1 []
  (let [raw (clojure.string/split-lines (slurp input))]))
