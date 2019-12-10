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
  (apply max-key first
         (reduce (fn [acc itm]
                   (conj acc [(-> itm
                                   (vectors system)
                                   (remove-duplicates)
                                   (count))
                              itm]))
                 #{}
                 system)))

; part 1
(println
 (best-from input1)) ; [288 [17 22]]

; part 2
(defn dot [[x y] [x' y']]
  (+ (* x x') (* y y')))

(defn abs [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn angle [v1 v2]
  (Math/acos (/ (dot v1 v2) (* (abs v1) (abs v2)))))

; create list of 288 seen obstacles
(def seen-directions (-> [17 22] (vectors input1) (remove-duplicates)))
(def left (filter #(< (first %) 0) seen-directions))
(def right (remove #(< (first %) 0) seen-directions))
(count right) ; => 129

(count left) ; => 159
; find the direction of the 71st largest in left
; find the direction of the 88st smallest in left

(nth
 (into []
       (sort-by first
                (for [v left] [(angle [0 -1] v) v])))
 88); [-11 -6]

; [17 22] -> [6 16]
