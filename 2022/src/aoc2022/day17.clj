(ns aoc2022.day17)

(def right 1)
(def left -1)

(def chambers
  "Heights of chambers"
  [0 0 0 0 0 0 0])

(def example-pattern ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(defn- parse-pattern
  "Return a lazy seq of 1/-1 right/left"
  [p]
  (let [p (map (fn [x] (if (= \> x) right left)) p)]
    (cycle p)))

(comment
  (parse-pattern example-pattern)
  (take 10  (parse-pattern "<<<<>")))

(defn- push-jet
  "Calc new left coord 0-6 for given width and current wind, check if collides with chambers too"
  [width currentpos wind currentheight chamberheights]
  (let [newleft        (+ currentpos wind)
        newright       (dec (+ newleft width))
        withinborders? (and (< -1 newleft) (< newright 7))]
    (if-not withinborders? currentpos
            (if-let [chamberhit? (if (= left wind)
                                   (< currentheight (get-in chamberheights [newleft]))
                                   (< currentheight (get-in chamberheights [newright])))]
              currentpos
              newleft))))

(comment
  (do
    (assert (= 3 (push-jet 4 3 right  3 (vec (repeat 7 0)))) "x1")
    (assert (= 4 (push-jet 2 3 right  3 (vec (repeat 7 0)))) "x2")
    (assert (= 1 (push-jet 4 1 left  10 [666 0 0 0 0 0 0])) "x3")
    (assert (= 2 (push-jet 4 1 right 10 [666 0 0 0 0 0 0])) "x4")
    (assert (= 2 (push-jet 4 1 right 10 [0 666 0 0 0 0 0])) "x5")
    (assert (= 2 (push-jet 4 1 right 10 [0 666 0 0 0 0 0])) "x6")
    (assert (= 0 (push-jet 4 0 right 10 [0 0 0 0 666 0 0])) "x7")
    (assert (= 1 (push-jet 4 0 right 10 [0 0 0 0 0 666 0])) "x8"))
  )

(defn- cannot-drop?
  "Check if drop is not possible"
  ;; idea on se, että tiputusta ennen katsotaan onko seuraavalla tasolla tilaa tippua, tai siis onko nykyinen taso sama kuin seuraavan ylin siinä kohdin
  ;; height on korkeus millä elementti on nyt
  ;; pos on 0 6 jossa on vasen reuna
  ;; width on leveys ko kohdassa
  ;; chamberheights on vektori jossa on korkeudet samassa korkeustasossa kuin height on
  [height pos width chamberheights]
  (let [x1 (select-keys chamberheights (range pos (+ pos width)))
        _ (def x1 x1)
        x2 (vals (select-keys chamberheights (range pos (+ pos width))))
        _ (def x2 x2)
        ]
    (<= height (apply max (vals (select-keys chamberheights (range pos (+ pos width)))))))
  )

(comment
  (assert
   (cannot-drop? 3 0 3 [2 2 2 2 2 2 2])
   )
  (cannot-drop? 3 3 3 [2 2 2 2 2 2 2])
  (cannot-drop? 3 3 3 [2 2 2 3 2 2 2])
  (cannot-drop? 4 0 3 [2 2 2 2 2 2 2])
  (cannot-drop? 2 0 3 [2 2 2 2 2 2 2])
  )

(defn- new-chambers [chambs startpos width height]
  (let [newlevel (->> (drop startpos chambs)
                      (take width)
                      (apply max)
                      (+ height))
        newchambs (concat (take startpos chambs) (repeat width newlevel) (drop (+ startpos width) chambs))]
    newchambs))

(comment
  (assert (= [1 1 1 0 0 0 0]
             (new-chambers [0 0 0 0 0 0 0] 0 3 1)
             ))
  (assert (= [0 1 1 1 0 0 0]
             (new-chambers [0 0 0 0 0 0 0] 1 3 1)
             ))
  (assert (= [0 0 0 2 2 2 2]
             (new-chambers [0 0 0 0 0 0 0] 3 4 2)
             ))
  (assert (= [1 2 6 4 5 6 7]
             (new-chambers [1 2 3 4 5 6 7] 2 1 3)
             ))
  )

(defn- ->dropheight [chambers]
  (-> (apply max chambers)
      (+ 3)))

(defn- drop-x-y [chambers winds width height]
  (loop [dh                (->dropheight chambers)
         [wind & windrest] winds
         currentpos        2]
    (let [newpos (push-jet width currentpos wind dh chambers)]
      (if (cannot-drop? dh newpos width chambers)
        (new-chambers chambers newpos width height)
        (recur (dec dh) windrest newpos)))))

(defn- drop-minus
  "Returns new chamber state and wind seq"
  [chambers winds]
  (drop-x-y chambers winds 4 1))

(comment
  (assert
   (= [1 1 2 2 2 2 1]
      (drop-minus [1 1 1 1 1 1 1] (take 10 (parse-pattern example-pattern)))
      ))
  (assert
   (= [4 4 4 4 3 1 1]
      (drop-minus [1 1 1 3 3 1 1] (take 10 (parse-pattern "<")))
      ))
  (assert
   (= [6 0 0 1 1 1 1]
      (drop-minus [6 0 0 0 0 0 0] (take 10 (parse-pattern ">")))
      ))
  (assert
   (= [7 7 7 7 0 0 0]
      (drop-minus [6 0 0 0 0 0 0] (take 10 (parse-pattern "<")))
      )))

(defn- drop-i [chambers winds]
  (drop-x-y chambers winds 1 4))

(comment
  (do
    (assert
     (= [4 0 0 0 0 0 0]
        (drop-i [0 0 0 0 0 0 0] (take 10 (parse-pattern "<")))
        ))
    (assert
     (= [0 0 4 0 0 0 0]
        (drop-i [0 0 0 0 0 0 0] (take 10 (parse-pattern "><")))
        ))
    (assert
     (= [1 1 1 1 1 1 5]
        (drop-i [1 1 1 1 1 1 1] (take 10 (parse-pattern ">")))
        )))
  )

(defn- drop-plus [chambers winds])

(defn- drop-l [chambers winds]
    (drop-x-y chambers winds 1 4))

(defn- drop-square [chambers winds]
  (drop-x-y chambers winds 2 2))

(comment
  (do
    (assert (= [0 0 0 0 0 2 2]
               (drop-square [0 0 0 0 0 0 0] (parse-pattern ">"))
               ))
    ))
