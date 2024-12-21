(ns aoc2022.day07)

(def ex-map {"/" {:dirs  {"a" {:dirs  {"e" {:files {"i" 584}}}
                               :files {"f"     29116
                                       "g"     2557
                                       "h.lst" 62596}}
                          "d" {:dirs  {}
                               :files {"j"     4060174
                                       "d.log" 8033020
                                       "d.ext" 5626152
                                       "k"     7214296}}
                          }
                  :files {"b.txt" 14848514
                          "c.dat" 8504156}}})

(def input-example (clojure.string/split-lines (slurp "resources/day07-example.txt")))
(def input (clojure.string/split-lines (slurp "resources/day07.txt")))

(defn- parse-line [line]
  (condp re-seq line
    #"\$ cd (.*)"    :>> (fn [xs] {:op :cd :to (-> xs first (nth 1))})
    #"\$ ls"         :>> (fn [xs] {:op :ls})
    #"dir (\w+)"     :>> (fn [xs] {:op :dir :name (-> xs first (nth 1))})
    #"(\d+) (.*)"    :>> (fn [xs] {:op :file :size (Long/parseLong (-> xs first (nth 1)))})
    ))

(defn- remove-ops [ops-to-remove parsedseq]
  (let [f (set ops-to-remove)]
    (remove (fn [x] (f (:op x))) parsedseq)))

(defn- parse-fs
  "Parse fs line stream"
  [lines]
  (loop [[line & rl] lines
         acc []]
    (if (nil? line)
      (remove-ops [:ls :dir] acc)
      (recur rl (conj acc (parse-line line))))))


(defn- update-cd [loc to]
  (if (= ".." to)
    (update-in loc [:current-path] (fn [o & _] (drop-last o)))
    (update-in loc [:current-path] (fn [o & _] (conj (vec o) to)))
    ))

(defn- update-file [loc size]
  (let [path (-> (:current-path loc)
                 (interleave (repeat :dirs))
                 drop-last
                 vec
                 (conj :files))]
    (update-in loc path (fn [o n] (+ (or o 0) n)) size)))

(defn- update-tree
  "Add entry to tree or update loc"
  [loc {:keys [op] :as entry}]
  (if-let [result (condp = op
                    :cd   (let [res (update-cd loc (:to entry))] #_(println "cd res:" res) res)
                    :file (update-file loc (:size entry))
                    )]
    result
    (do
      (def xloc [loc entry])
      {:error entry :reason "result nil"})
    ))

(defn- filesystem-visitor
  "Custom zipper creation"
  [parsed-lines]
  (loop [[pl & r] parsed-lines
         rz {:current-path []}]
    (if (nil? pl)
      rz
      (if (nil? rz)
        "RZ was nil"
        (recur r (update-tree rz pl))))))

(comment
  (-> (parse-fs input-example) filesystem-visitor)
  )

(def testresult {:current-path ["/" "d"],
                 "/"
                 {:files 23352670,
                  :dirs
                  {"a" {:files 94269, :dirs {"e" {:files 584}}},
                   "d" {:files 24933642}}}})

(def treeres (get testresult "/"))

(defn- remove-dirs [p]
  [:all (vec (remove (fn [x] (or (= :dirs x) (:files x))) p))]  )

(defn update-fs-tree [tm]
  (loop [[path & rpaths] [["/"]];;(map (fn [p] ["/" :dirs p]) (-> (get tm "/") :dirs keys))
         acc             {}
         i               0]
;;    (println ["path:" path])
    (if (or (= i 5000) (nil? path))
      acc
      (let [np       (vec path)
            newpaths (->> (remove nil? (-> (get-in tm np) :dirs keys))
                          (map (fn [p] (-> np (conj :dirs) (conj p))) )
                          (into rpaths))
            current  (get-in tm (conj np :files) 0)
            newacc   (-> acc
                         (update-in [:paths] (fn [o & _] (conj (or o #{}) np)))
                         (update-in (conj np :files) (fn [o & _] (+ (or o 0) current)))
                         (update-in (remove-dirs np) (fn [o & _] (+ (or o 0) current)))
                         )]
        (recur newpaths newacc (inc i))))))

(defn- update-sizes [sizes]
  (reduce (fn blergh [acc k]
            (update-in acc [(vec (drop-last k))] (fn xorb [o & _] (+ (or o 0) (get acc k))))
            )
          sizes
          (sort-by count > (keys sizes))))

(defn p1 []
  (let [parsed  (parse-fs input)
        visited (filesystem-visitor parsed)
        res     visited ;;(get visited "/")
        upsizes (update-sizes (:all (update-fs-tree res)))
        ]
    (->> upsizes
        vals
        (remove (fn [x] (> x 100000)))
        (apply +))))

(comment
  (p1)
  )

(defn p2 []
  (let [parsed  (parse-fs input)
        visited (filesystem-visitor parsed)
        res     (select-keys visited ["/"])
        updated (update-fs-tree res)
        upsizes (update-sizes (:all updated))
        diff    (- (get upsizes [])  40000000)]
    (first (drop-while (fn [x] (> diff x)) (sort (vals upsizes))))))

(comment
  (p2)
  )
