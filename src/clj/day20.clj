(ns clj.day20)

(def input "         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z       ")

(defn read-input [input]
  (reduce
   (fn [acc [x y c]]
     (if (= c \.)
       (update acc :way-points conj [x y])
       (update acc :characters assoc [x y] c)))
   {:way-points #{}
    :characters {}}
   (->> (clojure.string/split-lines input)
        (map-indexed vector)
        (mapcat (fn [[y line]]
                  (map-indexed (fn [x c] [x y c]) line)))
        (remove (fn [[_ _ c]] (or (= c \#) (= c \space)))))))

(defn neighbours [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) (for [dx (range -1 2) dy (range -1 2) :when (not (= 0 dx dy))] [dx dy])))

(defn make-portal [[x1 y1] c1 [x2 y2] c2]
  (if (and (<= x1 x2)
           (<= y1 y2))
    (str c1 c2)
    (str c2 c1)))

(defn find-portals [{:keys [characters way-points]}]
  (loop [chars   characters
         portals []]
    (if (not-empty chars)
      (let [[xy1 c] (first chars)
            xys       (neighbours xy1)
            xy2       (first (filter #(chars %) xys))
            portal    (make-portal xy1 c
                                   xy2 (chars xy2))
            loc       (first (filter #(way-points %) (concat xys (neighbours xy2))))]
        (recur (dissoc chars xy1 xy2)
               (conj portals [portal loc])))
      {:way-points way-points
       :portals portals})))

(defn build-portals [{:keys [portals way-points]}]
  (reduce
   (fn [acc [p xy]]
     (case p
       "AA" (assoc acc :start xy)
       "ZZ" (assoc acc :end xy)
       (-> acc
           (update-in [:portals p] (fnil conj []) xy)
           (update-in [:portals :all] (fnil assoc {}) xy p))))
   {:way-points way-points}
   portals))

(defn step [[x y]]
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]])

(defn solve [{:keys [way-points start end portals]}]
  (let [portal-locations (:all portals)
        queue            (conj (clojure.lang.PersistentQueue/EMPTY)
                               [start 0])]
    (loop [q queue
           v #{}]
      (if (empty? q)
        -1
        (let [[xy n] (peek q)
              n'     (inc n)
              q'     (apply conj (pop q) (->> (step xy)
                                              (filter #(way-points %))
                                              (filter #(not (contains? v %)))
                                              (map #(vector % n'))))
              portal (portal-locations xy)
              q''    (if (not (nil? portal))
                       (conj q' [(first (remove #(= xy %) (portals portal))) n'])
                       q')]
          (if (= xy end)
            n
            (recur q'' (conj v xy))))))))

(comment
  (-> (read-input input)
      find-portals
      build-portals)
  (-> (slurp "/home/eike/Projects/aoc-2019/resources/day19.txt")
      read-input
      find-portals
      build-portals)
  ,)

(defn solve2 [{:keys [way-points start end portals]}]
  (let [portal-locations (:all portals)
        queue            (conj (clojure.lang.PersistentQueue/EMPTY)
                               [start 0 0])
        max-x (first (apply max-key first way-points))
        max-y (second (apply max-key second way-points))
        is-outer? (fn [[x y]] (or (= x 2) (>= x max-x)
                                  (= y 2) (>= y max-y)))]
    (loop [q queue
           v #{}]
      (if (empty? q)
        -1
        (let [[xy n lvl] (peek q)]
          (if (contains? v [xy lvl])
            (recur (pop q) v)
            (let [n'         (inc n)
                  q'         (apply conj (pop q) (->> (step xy)
                                                      (filter #(way-points %))
                                                      (filter #(not (contains? v [% lvl])))
                                                      (map #(vector % n' lvl))))
                  portal     (portal-locations xy)
                  q''        (if (not (nil? portal))
                               (let [outside (is-outer? xy)
                                     lvl'    (if outside (dec lvl) (inc lvl))]
                                 (if (and (= 0 lvl) outside)
                                   q'
                                   (conj q' [(first (remove #(= xy %) (portals portal))) n' lvl'])))
                               q')]
              (if (and (= xy end) (zero? lvl))
                n
                (recur q'' (conj v [xy lvl]))))))))))

(comment
  (-> (read-input input)
      find-portals
      build-portals
      solve2)
  (-> (slurp "/home/eike/Projects/aoc-2019/resources/day19.txt")
      read-input
      find-portals
      build-portals
      solve2)
  )