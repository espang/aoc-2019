(ns clj.day12)

(defn apply-step [pos vel]
  (let [v+ (for [x pos] (count (filter #(> x %) pos)))
        v- (for [x pos] (count (filter #(< x %) pos)))
        v' (mapv - vel (mapv - v+ v-))
        p' (mapv + pos v')]
    [p' v']))

(defn apply-steps [pos vel n]
  (loop [p pos
         v vel
         n n]
    (if (zero? n)
      [p v]
      (let [[p' v'] (apply-step p v)]
        (recur p' v' (dec n))))))

(def x-pos [13 8 -5 2])
(def y-pos [9 14 4 -6])
(def z-pos [5 -2 11 1])
(def vel [0 0 0 0])

(let [[x-pos x-vel] (apply-steps x-pos vel 1000)
      [y-pos y-vel] (apply-steps y-pos vel 1000)
      [z-pos z-vel] (apply-steps z-pos vel 1000)]
  (reduce +
          (map * 
               (map + (map #(Math/abs %) x-pos) (map #(Math/abs %) y-pos) (map #(Math/abs %) z-pos))
               (map + (map #(Math/abs %) x-vel) (map #(Math/abs %) y-vel) (map #(Math/abs %) z-vel)))))

(defn period [pos vel]
  (loop [p     pos
         v     vel
         count 0
         seen  #{}]
    (if (contains? seen [p v])
      count
      (let [[p' v'] (apply-step p v)]
        (recur p' v' (inc count) (conj seen [p v]))))))

(period x-pos vel)
; 268296
(period y-pos vel)
; 161428
(period z-pos vel)
; 102356

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(println (reduce lcm [268296 161428 102356]))

