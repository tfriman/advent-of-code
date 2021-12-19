(ns aoc2021.day16)

;; Task was https://adventofcode.com/2021/day/16/
;;
;; This was a horrible one for a person who has never made any complicated parsers.
;;
;; To see how this is properly made with Lisp, see for example https://github.com/atgreen/advent-of-code-2021/blob/main/16.lisp :)

(def input "020D708041258C0B4C683E61F674A1401595CC3DE669AC4FB7BEFEE840182CDF033401296F44367F938371802D2CC9801A980021304609C431007239C2C860400F7C36B005E446A44662A2805925FF96CBCE0033C5736D13D9CFCDC001C89BF57505799C0D1802D2639801A900021105A3A43C1007A1EC368A72D86130057401782F25B9054B94B003013EDF34133218A00D4A6F1985624B331FE359C354F7EB64A8524027D4DEB785CA00D540010D8E9132270803F1CA1D416200FDAC01697DCEB43D9DC5F6B7239CCA7557200986C013912598FF0BE4DFCC012C0091E7EFFA6E44123CE74624FBA01001328C01C8FF06E0A9803D1FA3343E3007A1641684C600B47DE009024ED7DD9564ED7DD940C017A00AF26654F76B5C62C65295B1B4ED8C1804DD979E2B13A97029CFCB3F1F96F28CE43318560F8400E2CAA5D80270FA1C90099D3D41BE00DD00010B893132108002131662342D91AFCA6330001073EA2E0054BC098804B5C00CC667B79727FF646267FA9E3971C96E71E8C00D911A9C738EC401A6CBEA33BC09B8015697BB7CD746E4A9FD4BB5613004BC01598EEE96EF755149B9A049D80480230C0041E514A51467D226E692801F049F73287F7AC29CB453E4B1FDE1F624100203368B3670200C46E93D13CAD11A6673B63A42600C00021119E304271006A30C3B844200E45F8A306C8037C9CA6FF850B004A459672B5C4E66A80090CC4F31E1D80193E60068801EC056498012804C58011BEC0414A00EF46005880162006800A3460073007B620070801E801073002B2C0055CEE9BC801DC9F5B913587D2C90600E4D93CE1A4DB51007E7399B066802339EEC65F519CF7632FAB900A45398C4A45B401AB8803506A2E4300004262AC13866401434D984CA4490ACA81CC0FB008B93764F9A8AE4F7ABED6B293330D46B7969998021C9EEF67C97BAC122822017C1C9FA0745B930D9C480")

(declare general-parser-fn)

(defn strbin->int [s]
  (Long/parseLong s 2))

(defn fix-leading [st fi]
  (let [pf (Long/parseLong (str fi) 16)
        pad (cond
              (= 0 pf) "0000"
              (= 1 pf) "000"
              (<= 2 pf 3) "00"
              (<= 4 pf 7) "0"
              :else "")]
    (str pad st)))

(defn oper-fn-math [op name]
  (fn ([s] (apply op s))
    ([] name)))

(defn oper-fn-comp [op name]
  (fn ([[a b]] (if (op a b) 1 0))
    ([] name)))

(def opers {0 ["sum" (oper-fn-math +' "sum")]
            1 ["product" (oper-fn-math *' "product")]
            2 ["min" (oper-fn-math min "min")]
            3 ["max "(oper-fn-math max "max")]
            4 ["value" (fn value [s e] (conj s (:value e)))]
            5 ["gt" (oper-fn-comp > "gt")]
            6 ["lt" (oper-fn-comp < "lt")]
            7 ["equal" (oper-fn-comp = "equal")]})

(defn add-clear-name [s]
  (map (fn [e] (assoc e :operation (first (get opers (:type e))))) s))

(defn read-wrapper
  "Used for debugging this pile."
  [orig eout]
  (let [il (count orig)
        ol (count (:payload eout))]
    #_(println "Read bits:" (- il ol) " left " ol " orig:" il " and ol value:" (:value ol))
    eout))

(defn parse-binary-str
  "Read version and type from the beginning. Return payload as is. Obsolete"
  [bs]
  (let [pversion (apply str (take 3 bs))
        ptype (apply str (take 3 (drop 3 bs)))
        payload (apply str (drop 6 bs))]
    (read-wrapper bs
                  {:version (strbin->int pversion)
                   :type (strbin->int ptype)
                   :payload payload})))

(defn hex->binary-str
  [hs]
  (let [result (mapcat (fn [x]
                         (let [l (Character/getNumericValue x)
                               pad (cond
                                     (= 0 l) "000"
                                     (= 1 l) "000"
                                     (<= 2 l 3) "00"
                                     (<= 4 l 7) "0"
                                     :else "")]
                           (str pad (Long/toBinaryString l)))) hs)]
    (apply str result)))

(defn parse-hex-str
  "Returns version (int), type (int) and payload (string)"
  [hs]
  (let [bso (Long/toBinaryString (Long/parseLong hs 16))
        bs (fix-leading bso (first hs))]
    (parse-binary-str bs)))

(defn literal? [{:keys [type]}] (= type 4))

(defn not-parseable? [payload]
  (< (count payload) 10))

(defn parse-literal
  "5 bit segments, drop first bit, concat and return as int val"
  [{:keys [payload type version]}]
  (let [segs-all   (partition 5 payload)
        first-segs (map #(rest %) (take-while #(not= \0 (first %)) segs-all))
        segs       (concat (flatten first-segs) (rest (first (drop (count first-segs) segs-all))))
        binstrs    (apply str segs)
        dropcount  (+ (* 5 (count first-segs)) 5)]
    (read-wrapper payload
                  {:version   version
                   :type      type
                   :value     (strbin->int binstrs)
                   :payload   (apply str (drop dropcount payload))
                   })))

(defn counter-parser [targetrest id parser-fn]
  #_(println "counter-parser created parser" targetrest "id" id)
  (fn ([binstr]
       (let [parsed (parser-fn binstr)
             lenrest (count (:payload parsed))
             diff (- lenrest targetrest)]
         #_(println "counter-parser:" targetrest " and current:" lenrest " and id" id (when (> targetrest lenrest) " bbb") " and parsed:" parsed)
         (when (> targetrest lenrest)
           (println "ERROR counter-parser:" targetrest " and current:" lenrest " and id" id))
         (read-wrapper binstr
                       (if (pos? diff)
                         (do
                           (let [existingp (:parsers parsed)]
                             #_(println "existingp:" existingp)
                             (if (empty? existingp)
                               (merge parsed  {:parsers [(counter-parser targetrest id (general-parser-fn "noups"))]})
                               (merge parsed  {:parsers
                                               (concat (drop-last existingp)  (list (counter-parser targetrest id (last existingp)))) })
                               )
                             )
                           )
                         parsed
                         ))))
    ([] (str "counter-parser len: " targetrest " and id:" id))))

(defn marker-emitter-fn [id]
  (fn ([binstr]
       {:marker true
        :origin-id id
        :version 0
        :payload binstr
        :parsers '()})
    ([] (str "marker-emitter-fn:" id))))

(defn parse-op-15
  "Operator payload in, 15 first bits tell the length of the payload."
  [version type s]
  (let [payload-length-bits (->> (take 15 s)
                                 (apply str))
        prest (->> (drop 15 s)
                   (apply str))
        paylen (strbin->int payload-length-bits)
        uid (count prest)]
    #_(println "payload length" paylen " and rest length:" uid " created parser" (- uid paylen))
    {:version version
     :type type
     :payload prest
     :id uid
     :parsers [(counter-parser (- (count prest) paylen) uid (general-parser-fn uid)) (marker-emitter-fn uid)]}))

(defn parse-op-11
  "Operator payload in, 11 first bits tell how many packets"
  [version type s]
  (let [payload-length-bits (->> (take 11 s)
                                 (apply str))
        prest (->> (drop 11 s)
                   (apply str))
        uid (count prest)
        packets (strbin->int payload-length-bits)]
    #_(println "Found packets:" packets)
    {:version version
     :type type
     :payload prest
     :id uid
     :parsers (concat (repeat packets (general-parser-fn (str "op11-uid-" uid))) (list (marker-emitter-fn uid)) )}))

(defn parse-operator
  "Returns {:payload x}"
  [{:keys [payload version type]}]
  (let [len-type (Character/getNumericValue (first payload))]
    (def lx len-type)
    #_(println "length encoding type:" (if (= 0 len-type) "bits" "packets"))
    (read-wrapper payload
                  (if (= 0 len-type)
                    (parse-op-15 version type (rest payload))
                    (parse-op-11 version type (rest payload))))))

(defn general-parser-fn [id]
  (fn ([binstr]
       (if (empty? binstr)
         (do
           (println "Empty binstr!")
           (read-wrapper binstr
                         {:ignore true
                          :payload ""
                          :version 0
                          :parsers '()}))
         (let [parsed (parse-binary-str binstr)]
           (if (literal? parsed)
             (parse-literal parsed)
             (parse-operator parsed)))))
    ([] "general-parser:" id)))

(defn parse-all
  "Take hex in and loop parse"
  [binstr]
  (loop [bstr binstr
         parsers [(general-parser-fn "first")]
         elements []
         versions []]
    (if (empty? parsers)
      {:elements elements
       :versionsum (apply + versions)}
      (if (not-parseable? bstr)
        (do
          ;; there are parsers to be utilized still, hopefully only markers...
          (println "---------- This is about the end! rest length:" (count bstr) "and parsers" parsers)
          {:elements (filter #((complement :ignore) %) (reduce (fn [a e] (conj a (e ""))) elements parsers))
           :versionsum (apply + versions)
           :extraparsers parsers})
        (let [par (first parsers)
              nxt (par bstr)
              nxtparsers (concat (:parsers nxt) (rest parsers))]
          #_(println "Added element:" (dissoc nxt :payload))
          (recur (:payload nxt) nxtparsers (conj elements nxt) (conj versions (:version nxt))))))))

(defn loop-parse [s]
  (loop [e (first s)
         r (rest s)
         idstack '()
         accu '()]
    (if (nil? e)
      accu
      (if-let [oid (:origin-id e)]
        (recur (first r) (rest r) (conj idstack oid) accu)
        (if-let [id (:id e)]
          (let [new-idstack (rest idstack)
                operation (second (get opers (:type e)))
                params (map second (take-while (fn [[x _]] (x id)) accu))
                newval (operation params)
                new-accu (conj (drop (count params) accu) [(set idstack)newval])]
            (recur (first r) (rest r) new-idstack new-accu))
          (let [val (:value e)
                new-accu (conj accu [(set idstack) val])]
            (recur (first r) (rest r) idstack new-accu)))))))

(defn solve [hexstr]
  (->> hexstr
       hex->binary-str
       parse-all
       :elements
       reverse
       add-clear-name
       loop-parse
       first
       second
       ))

(defn p1 []
  (let [result (:versionsum (parse-all (hex->binary-str input)))]
    (println "p1 result:" result)
    result))

(defn p2 []
  (let [result (solve input)]
    (println "p2 result:" result)))
