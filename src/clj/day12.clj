(ns clj.day12)

(def x-pos [13 8 -5 2])
(def y-pos [9 14 4 -6])
(def z-pos [5 -2 11 1])
(def vel [0 0 0 0])

(defn apply-step [[pos vel]]
  (let [v+ (for [x pos] (count (filter #(> x %) pos)))
        v- (for [x pos] (count (filter #(< x %) pos)))
        v' (mapv - vel (mapv - v+ v-))
        p' (mapv + pos v')]
    [p' v']))

(defn apply-steps [p v steps]
  (nth (iterate apply-step [p v]) steps))

(defn abs [coll] (map #(Math/abs %) coll))

(let [[x-pos x-vel] (apply-steps x-pos vel 1000)
      [y-pos y-vel] (apply-steps y-pos vel 1000)
      [z-pos z-vel] (apply-steps z-pos vel 1000)]
  (reduce +
          (map * 
               (map + (abs x-pos) (abs y-pos) (abs z-pos))
               (map + (abs x-vel) (abs y-vel) (abs z-vel)))))

(defn period [pos vel]
  (loop [p     pos
         v     vel
         count 0
         seen  #{}]
    (if (contains? seen [p v])
      count
      (let [[p' v'] (apply-step p v)]
        (recur p' v' (inc count) (conj seen [p v]))))))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn solve []
  (let [p1 (period x-pos vel)
        p2 (period y-pos vel)
        p3 (period z-pos vel)]
    (println (reduce lcm [p1 p2 p3]))))

