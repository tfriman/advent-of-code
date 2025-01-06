(ns aoc2021.day24)

;; TODO clean me, horrible mess.
;; Idea was to filter as early as possible. each step has been decoded to mod this and that
;; recursive calls to earlier stages to get filtered input candidates.
;; wish was that those ending up in the alternative branches would be the ones containing valid ones.

;; (def zeroes (remove (fn [x] (not= 0 (:z x))) mapped))
;; (def mapped (map (fn [i] (testbench alup i)) candidates))
;; aoc2021.day24> (apply max (map (fn [x] (Long/parseLong x)) (map :orig zeroes)))
;; 41299994879959
;; aoc2021.day24> (apply min (map (fn [x] (Long/parseLong x)) (map :orig zeroes)))
;;               11189561113216


;; i1: z = w + 16, y = z, x = 1
;; i2: 26w1 + w2 + 432 , eli min: 27 + 432 = 459, max: 675
;; i3: z = 676w1 + 26w2 + w3 + 11244
;; i4:


(defn i4 [w1 w2 w3 w4]
  (when (= (- (mod (i3 w1 w2 w3) 26) 5) w4)
    ;; w4 does not matter!
    (int (Math/floor (/ (i3 w1 w2 w3) 26.0)))))

(def w4b (for [w1 (range 1 10)
               w2 (range 1 10)
               w3 (range 1 10)
               w4 (range 1 10)
               w5 (range 1 10)
               :let [r (i4 w1 w2 w3 w4)]
               :when (and (not (nil? r))
                          (= (- (mod r 26) 3) w5))
               ]
           [w1 w2 w3 w4 w5]
           ))
(count w4b)

(defn i5 [w1 w2 w3 w4 w5]
  (when (= (- (mod (i4 w1 w2 w3 w4) 26) 3) w5)
    ;; w5 does not matter!
    (-> (/ (i4 w1 w2 w3 w4) 26.0)
        Math/floor
        int)))

(defn i6 [w1 w2 w3 w4 w5 w6]
  (+ (* 26 (i5 w1 w2 w3 w4 w5)) w6 2))

(defn i7 [w1 w2 w3 w4 w5 w6 w7]
  (+ (* 26 (i6 w1 w2 w3 w4 w5 w6)) w7 11))

(defn i8 [w1 w2 w3 w4 w5 w6 w7 w8]
  ;; w8 does not matter, if i7 mod 26 is between 17-25
  (-> (/ (i7 w1 w2 w3 w4 w5 w6 w7) 26.0)
      Math/floor
      int))

(defn i9 [w1 w2 w3 w4 w5 w6 w7 w8 w9]
  (+ (* 26 (i8 w1 w2 w3 w4 w5 w6 w7 w8)) w9 12))

(defn i10 [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10]
  (+ (* 26 (i9 w1 w2 w3 w4 w5 w6 w7 w8 w9)) w10 9))

(def i11candidates
  (for [[w1 w2 w3 w4 w5 w6 w7] w7b
        w8 (range 1 10)
        w9 (range 1 10)
        w10 (range 1 10)
        w11 (range 1 10)
        :let [r (i10 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10)]
        :when (and (not (nil? r))
                   (= (- (mod r 26) 7) w11))
        ]
    [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11]
    ))

(defn i11 [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11]
  ;; w11 does not count if mod 26 i10 8-16
  (if (< 7 (mod (i10 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10) 26) 17)
    (-> (/ (i10 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10) 26.0)
        Math/floor
        int)))

(def i12candidates
  (for [[w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11] i11candidates
        w12 (range 1 10)
        :let [r (i11 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11)]
        :when (and (not (nil? r))
                   (= (- (mod r 26) 11) w12))]
    [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12]
    ))

(defn i12 [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12]
  ;; w12 does not matter
  (when (< 10 (mod (i11 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11) 26) 21)
    (-> (/ (i11 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11) 26.0)
        Math/floor
        int)))

(def i13candidates
  (for [[w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12] i12candidates
        w12 (range 1 10)
        w13 (range 1 10)
        :let [r (i12 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12)]
        :when (and (not (nil? r))
                   (= (- (mod r 26) 6) w13))]
    [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13]
    ))

(defn i13 [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13]
  ;; w13 does not count
  (when (< 6 (mod (i12 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12) 26) 16)
    (-> (/ (i12 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12) 26.0)
        Math/floor
        int)))

(def i14candidates
  (for [[w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13] i13candidates
        w14 (range 1 10)
        :let [r (i13 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13)]
        :when (and (not (nil? r))
                   (= (- (mod r 26) 11) w14))]
    [w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14]
    )
  )

;; modulo calc rules:
;; (mod (+ 17 21) 12) = (mod (+ (mod 17 12) (mod 21 12)) 12)
;; (mod (* 17 21) 12) = (mod (* (mod 17 12) (mod 21 12)) 12)

(def inputcounter (atom 1))
(def skipcounter (atom 1))
;; The ALU is a four-dimensional processing unit: it has integer variables w, x, y, and z. These variables all start
;; with the value 0. The ALU also supports six instructions:
;; inp a - Read an input value and write it to variable a.
;; add a b - Add the value of a to the value of b, then store the result in variable a.
;; mul a b - Multiply the value of a by the value of b, then store the result in variable a.
;; div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in
;; variable a. (Here, "truncate" means to round the value toward zero.)
;; mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the
;; modulo operation.)
;; eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in
;; variable a.
;; In all of these instructions, a and b are placeholders; a will always be the variable where the result of the
;; operation is stored (one of w, x, y, or z), while b can be either a variable or a number. Numbers can be positive
;; or negative, but will always be integers

(def state {:w     0
            :x     0
            :y     0
            :z     0
            :input 0})

(defn- gen-b
  "Generate function based on b"
  [b]
  (if-let [int-b (re-matches #"[-]{0,1}\d+" b)]
    (fn idenfn [_] (Integer/parseInt int-b))
    (fn mapfn [m] ((keyword b) m))))

(defn ->parsed-instruction
  "Parses to map, {:op :a :b (optional, is a function)}"
  [x]
  (let [[_ op a b] (re-matches #"(\w+) (\w) (.*)" x)]
    (if op
      {:op (keyword op)
       :a  (keyword a)
       :b  (gen-b b)}
      (let [[_ op a] (re-matches #"(\w+) (\w)" x)]
        {:op (keyword op)
         :a  a}
        ))))

(defn- handle-eql
  [a b state]
  (let [newa (if (= (a state) (b state)) 1 0)]
    (assoc state a newa)))

(defn- handle-inp
  [a {:keys [input] :as state}]
  ;;(println "inp " a "z" (:z state))
  (if (= 0 (swap! inputcounter dec))
    state
    (let [next-input (Character/getNumericValue (first input))
          rest-input (rest input)]
      (-> state
          (assoc :input rest-input)
          (assoc (keyword a) next-input)))))

(defn- handle-mul
  [a b state]
  (let [mult (* (a state) (b state))]
    (assoc state a mult)))

(defn- handle-add [a b state]
  (let [newv (+ (a state) (b state))]
    (assoc state a newv)))

(defn- handle-div [a b state]
  (let [newv (-> (/ (a state) (b state))
                 Math/floor
                 int)]
    (assoc state a newv)))

(defn- handle-mod [a b state]
  (let [newv (mod (a state) (b state))]
    (assoc state a newv)))

(defn- process
  "Process given alu instruction, instruction and state are passed and state is returned"
  [{:keys [op a b] :as input} state]

  (def xxx-input input)
  (def xxx-state state)

  (if (= @inputcounter 0)
    state
    (if (not= 0 @skipcounter)
      (do
        (when (= :inp op) (swap! skipcounter dec))
        state)
      (condp = op
        :inp (handle-inp a state)
        :add (handle-add a b state)
        :mul (handle-mul a b state)
        :div (handle-div a b state)
        :mod (handle-mod a b state)
        :eql (handle-eql a b state)
        ))))

(defn- alu
  [instructions state]
  (let [parsed (map ->parsed-instruction instructions)]
    (alu-parsed parsed state)))

(defn- alu-parsed
  [parsed state]
  (reduce (fn [alustate instruction]
            ;;           (println alustate)
            (process instruction alustate))
          state
          parsed))



(def inpr2-9 {:1 {:w 1, :x 1, :y 17, :z 667}
              :2 {:w 2, :x 1, :y 18, :z 668}
              :3 {:w 3, :x 1, :y 19, :z 669}
              :4 {:w 4, :x 1, :y 20, :z 670}
              :5 {:w 5, :x 1, :y 21, :z 671}
              :6 {:w 6, :x 1, :y 22, :z 672}
              :7 {:w 7, :x 1, :y 23, :z 673}
              :8 {:w 8, :x 1, :y 24, :z 674}
              :9 {:w 9, :x 1, :y 25, :z 675}
              })

(def inpr3-2-1 {:1 {:w 1, :x 1, :y 13, :z 328185}
                :2 {:w 2, :x 1, :y 14, :z 328186}
                :3 {:w 3, :x 1, :y 15, :z 328187}
                :4 {:w 4, :x 1, :y 16, :z 328188}
                :5 {:w 5, :x 1, :y 17, :z 328189}
                :6 {:w 6, :x 1, :y 18, :z 328190}
                :7 {:w 7, :x 1, :y 19, :z 328191}
                :8 {:w 8, :x 1, :y 20, :z 328192}
                :9 {:w 9, :x 1, :y 21, :z 328193}
                })
;; round 2: input :z 459 + w + (* 26 (w-1))
(defn- r2
  "i - inputw j previous round"
  [i j]
  (+ (+ 458 i) (* 26 (dec j))))

;; round 3 ... z starts from 310609. between rounds 694 diff
;; 1-9 to 2-1: 316233 -> 328185 diff is 11952 = 16*9*83 = 3*3*4*4*83

(defn p1-after1 []
  (let [inputs (clojure.string/split-lines (slurp "resources/day24.txt"))
        parsed (map ->parsed-instruction inputs)
        alu-p (partial alu-parsed parsed)
        origin (:2 inpr3-2-2)]
    ;; mielenkiintoinen kombo n'ytt'isi olevan 221 ja 222 alut
    (println origin)
    (map (fn [i]
           (reset! skipcounter 3)
           (reset! inputcounter 4)
           (println (keyword (str i))
                    (->
                      (alu-p (-> origin
                                 (assoc :input (str i "2345678911239"))
                                 ))
                      (dissoc :input)))) (range 1 10))))

(defn do-it-p1 []
  (let [inputs (clojure.string/split-lines (slurp "resources/day24.txt"))
        parsed (map ->parsed-instruction inputs)
        alu-p (partial alu-parsed parsed)]
    (def alup alu-p)
    (def candis (map (fn [[w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14]]
                       (str w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14)
                       ) i14candidates))
    (comment
      (def all (->> (remove (fn [x] (clojure.string/index-of x "0")) (map str (range 11 100)))
                    (map (fn [suffix]
                           (map #(str % suffix) candis)
                           ))
                    flatten
                    )))
    (def resu (map (fn [i] (testbench alu-p i)) candis))
    ))

(defn p1 []
  (let [inputs (clojure.string/split-lines (slurp "resources/day24.txt"))
        parsed (map ->parsed-instruction inputs)
        alu-p (partial alu-parsed parsed)]

    (def x (map (fn [i] (testbench alu-p i)) (remove (fn [x] (clojure.string/index-of x "0")) (map #(str "1129114" %) (range 1 10)))))
    ;;    (def x (map (fn [i] (testbench alu-p i)) (remove (fn [x] (clojure.string/index-of x "0")) (map #(str "111891611352" %) (range 1 10)))))
    ;; (def x (map (fn [i] (testbench alu-p i)) (remove (fn [x] (clojure.string/index-of x "0")) (map #(str "11299194113294" %) (range 1111 10000)))))
    (def minimi (apply min (map :z x)))
    (filter #(> (+ 40 minimi) (:z %)) x)

    (comment
      (map (fn [i]
             (reset! skipcounter 0)
             (reset! inputcounter 15)
             (println (keyword (str i))
                      (alu-p (assoc state :input (str "1129919411329" i))))) (range 1 10)))
    x

    ;; ({:w 1, :x 1, :y 3, :z 445, :input (), :orig "112991"})
    ;; "11299194113294" That's not the right answer; your answer is too low.
    ;;    (get (frequencies (map :z x)) 454)

    ))
(def x12 '({:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611132"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611242"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611352"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611462"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611572"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611682"}
           {:w 2, :x 0, :y 0, :z 445, :input (), :orig "111891611792"}
           ))

(def x8 '({:w 1, :x 0, :y 0, :z 445, :input (), :orig "11189161"}
          {:w 2, :x 0, :y 0, :z 445, :input (), :orig "11189172"}
          ))

(def x1111 '({:w 8, :x 0, :y 0, :z 454, :input (), :orig "1118"}
             {:w 9, :x 0, :y 0, :z 454, :input (), :orig "1129"}
             {:w 8, :x 0, :y 0, :z 455, :input (), :orig "1218"}
             {:w 9, :x 0, :y 0, :z 455, :input (), :orig "1229"}
             {:w 8, :x 0, :y 0, :z 456, :input (), :orig "1318"}
             ))

(def x11111 '({:w 9, :x 0, :y 0, :z 17, :input (), :orig "11189"}
              {:w 9, :x 0, :y 0, :z 17, :input (), :orig "11299"}
              {:w 9, :x 0, :y 0, :z 18, :input (), :orig "21189"}
              {:w 9, :x 0, :y 0, :z 18, :input (), :orig "21299"}
              {:w 9, :x 0, :y 0, :z 19, :input (), :orig "31189"}
              {:w 9, :x 0, :y 0, :z 19, :input (), :orig "31299"}
              {:w 9, :x 0, :y 0, :z 20, :input (), :orig "41189"}
              {:w 9, :x 0, :y 0, :z 20, :input (), :orig "41299"}
              {:w 9, :x 0, :y 0, :z 21, :input (), :orig "51189"}
              {:w 9, :x 0, :y 0, :z 21, :input (), :orig "51299"}
              {:w 9, :x 0, :y 0, :z 22, :input (), :orig "61189"}
              {:w 9, :x 0, :y 0, :z 22, :input (), :orig "61299"}
              {:w 9, :x 0, :y 0, :z 23, :input (), :orig "71189"}
              {:w 9, :x 0, :y 0, :z 23, :input (), :orig "71299"}
              {:w 9, :x 0, :y 0, :z 24, :input (), :orig "81189"}
              {:w 9, :x 0, :y 0, :z 24, :input (), :orig "81299"}
              {:w 9, :x 0, :y 0, :z 25, :input (), :orig "91189"}
              {:w 9, :x 0, :y 0, :z 25, :input (), :orig "91299"}))

(def xxxx '({:w 8, :x 0, :y 0, :z 454, :input (), :orig "1118"}
            {:w 9, :x 0, :y 0, :z 454, :input (), :orig "1129"}
            {:w 8, :x 0, :y 0, :z 455, :input (), :orig "1218"}
            {:w 9, :x 0, :y 0, :z 455, :input (), :orig "1229"}
            {:w 8, :x 0, :y 0, :z 456, :input (), :orig "1318"}
            {:w 9, :x 0, :y 0, :z 456, :input (), :orig "1329"}
            {:w 8, :x 0, :y 0, :z 457, :input (), :orig "1418"}
            {:w 9, :x 0, :y 0, :z 457, :input (), :orig "1429"}
            {:w 8, :x 0, :y 0, :z 458, :input (), :orig "1518"}
            {:w 9, :x 0, :y 0, :z 458, :input (), :orig "1529"}
            {:w 8, :x 0, :y 0, :z 459, :input (), :orig "1618"}
            {:w 9, :x 0, :y 0, :z 459, :input (), :orig "1629"}
            {:w 8, :x 0, :y 0, :z 460, :input (), :orig "1718"}
            {:w 9, :x 0, :y 0, :z 460, :input (), :orig "1729"}
            {:w 8, :x 0, :y 0, :z 461, :input (), :orig "1818"}
            {:w 9, :x 0, :y 0, :z 461, :input (), :orig "1829"}
            {:w 8, :x 0, :y 0, :z 462, :input (), :orig "1918"}
            {:w 9, :x 0, :y 0, :z 462, :input (), :orig "1929"}))

(def bests ["1118" "1129"])

(defn testbench [alu-p inpx]
  (reset! skipcounter 0)
  (reset! inputcounter (inc (count inpx)))
  (alu-p (-> (assoc state :input inpx)
             (assoc :orig inpx)))
  )

(def prog-14 ["mul x 0"
              "add x z"
              "mod x 26"
              "div z 26"
              "add x -11"
              "eql x w"
              "eql x 0"
              "mul y 0"
              "add y 25"
              "mul y x"
              "add y 1"
              "mul z y"
              "mul y 0"
              "add y w"
              "add y 15"
              "mul y x"
              "add z y"
              ])

(defn d14 [input-map]
  (let [parsed (map ->parsed-instruction prog-14)
        alu-p (partial alu-parsed parsed)]
    (reset! skipcounter 0)
    (reset! inputcounter (inc (count (:input input-map))))
    ;;(-> (assoc state :input input) (assoc :orig input))
    (alu-p input-map)
    ))

(comment
  (def test-prog-1
    "negate input x"
    ["inp x"
     "mul x -1"])

  (def test-prog-2
    "z = 1 if second input = 3*first input"
    ["inp z"
     "inp x"
     "mul z 3"
     "eql z x"])

  (def test-prog-3-bits
    ["inp w"
     "add z w"
     "mod z 2"
     "div w 2"
     "add y w"
     "mod y 2"
     "div w 2"
     "add x w"
     "mod x 2"
     "div w 2"
     "mod w 2"])
  (assert (= -3 (:x (alu test-prog-1 (assoc state :input "3")))))
  (assert (= 1 (:z (alu test-prog-2 (assoc state :input "26")))))
  (assert (= 1 (:z (alu test-prog-3-bits (assoc state :input "4")))))
  )

