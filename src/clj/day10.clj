(ns clj.day10)

(defn y-values [line]
  (keep-indexed (fn [idx itm] (when (= itm \#) idx)) line))

(defn line->coords [[y line]]
  (->> line
       (y-values)
       (map (fn [x] [x y]))))

(defn str->coords [str]
  (->> (map-indexed vector (clojure.string/split-lines str))
       (map line->coords)
       (apply concat)))

(def test-system 
  (str->coords
   ".#..#
.....
#####
....#
...##"))

(def input1
  (-> "./resources/input10.txt"
      (slurp)
      (str->coords)))

(defn normalized [[x y]]
  (let [l (Math/sqrt (+ (* x x) (* y y)))]
    [(/ x l)
     (/ y l)]))

(defn almost-equal [[x y] [x' y']]
  (let [epsilon 0.0001
        dx      (Math/abs (- x x'))
        dy      (Math/abs (- y y'))]
    (and (< dx epsilon)
         (< dy epsilon))))

(defn same-direction [v1 v2]
  (let [nv1 (normalized v1)
        nv2 (normalized v2)]
    (almost-equal nv1 nv2)))

(defn vectors [[x y] system]
  (into #{}
        (for [[x' y'] system :when (or (not= x x') (not= y y'))]
          [(- x' x)
           (- y' y)])))

(defn remove-duplicates [vectors]
  (reduce (fn [acc itm]
            (if (some (partial same-direction itm) acc)
              acc
              (conj acc itm)))
          #{}
          vectors))

(defn best-from [system]
  (apply max
         (reduce (fn [acc itm]
                   (conj acc (-> itm
                                 (vectors system)
                                 (remove-duplicates)
                                 (count))))
                 #{}
                 system)))

(best-from test-system)
