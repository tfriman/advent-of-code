(ns aoc2021.day18
  (:require [clojure.zip :as z]
            [clojure.zip :as zip]))

(def example "resources/day18-example.txt")
(def simple-example "resources/day18-example-simple.txt")
(def path "resources/day18.txt")
(def input simple-example)
#_(def input example)
#_(def input path)

(comment
  (defn update-all [z f]
    (loop [z z]
      (if (z/end? z)
        (z/root z)
        (recur (z/next (fn [loc] (if (z/branch? loc) loc (zip/edit loc f))))))))

  (defn zip-walk [f z]
    (if (zip/end? z)
      (zip/root z)
      (recur f (zip/next (f z)))))

  (zip-walk
   (fn [loc]
     (if (zip/branch? loc)
       loc
       (zip/edit loc * 2)))
   (zip/vector-zip [1 2 [3 4]]))

  (def zp (z/zipper (fn [x] (:up (meta x))) seq (fn [_ c] c) (with-meta [[1 2 3] [2 2 2]] {:up true})))
  (z/edit (z/next (z/next zp)) (fn [[a b c]] [a b (dec c)]))

  (-> (z/vector-zip [[1 1 0]])
      (z/append-child [2 2 0])
      (z/root)
      #_(z/next)
      #_(z/edit (fn [x] [(first x) (second x) (inc (last x))]))
      ))

;; explode examples
(def ex1 [[[[[9,8],1],2],3],4])
(def ex1-result [[[[0,9],2],3],4])
(def ex2 [7,[6,[5,[4,[3,2]]]]])
(def ex2-result [7,[6,[5,[7,0]]]])
(def ex3 [[6,[5,[4,[3,2]]]],1])
(def ex3-result [[6,[5,[7,0]]],3])
(def nex1 [[1,9],[8,5]])

(def nz1 (z/vector-zip nex1))
(def z1 (z/vector-zip ex1))
(def z2 (z/vector-zip ex2))
(def z3 (z/vector-zip ex3))

(defn- find-5th-level-loc
  "Finds 5th level loc or nil"
  [z]
  (loop [z z
         bc 0]
    (if (or (z/end? z) (= 5 bc))
      (if (z/end? z) nil (z/up z))
      (recur (z/next z) (if (z/branch? z) (inc bc) bc)))))

(def l1 (find-5th-level-loc z1))
(def l2 (find-5th-level-loc z2))
(def l3 (find-5th-level-loc z3))

(comment
  (def l1-left (z/left l1))
  (def l1-right (z/right l1))
  (z/root (z/replace l1 [6 6]))
  (z/right (z/up (z/up (z/up (z/next l3))))))

(defn find-up
  "Goes up until root or left/right found or root meaning nil"
  [loc matchfn]
  (loop [loc loc]
    (if (nil? loc)
      nil
      (if (matchfn loc)
        (matchfn loc)
        (recur (z/up loc))))))

;; z/remove found node, then it moves up.
;; talleta z/left ja z/right ensin.


;; hackish way to find the levels... trusts internals of zip location. not recommended. 5 means too much!
#_(map (fn [[a b]] (count (:pnodes b)))
                      (take-while #(not= :end (second %)) (iterate z/next z3)))



;; explode: four deep, left is added to first regular number on right if any
;; and left to first regular number on left if any. result is always x,0 or 0,x pair.
;; so one pair is exploded.

(defn p1 []
  (let [in (clojure.string/split-lines (slurp input))]
    in
    ))
