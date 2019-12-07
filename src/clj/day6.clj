(ns clj.day6)

(def test-input ["COM)B" "B)C"])

(def part1-input
  (-> "./resources/input6.txt"
      slurp
      clojure.string/split-lines))

(defn parse-input [input]
  (let [[inner outer] (clojure.string/split input #"\)")]
    [inner outer]))

(parse-input "COM)B")

(defn system [inputs]
  (loop [inputs inputs
         m      {}]
    (if (empty? inputs)
      m
      (let [[inner outer] (parse-input (first inputs))]
        (recur (rest inputs)
               (update m inner (fnil conj []) outer))))))

(defn orbits-of [system-map object]
  (if-not (contains? system-map object)
    0
    (+ (count (system-map object))
       (reduce +
               (map (partial orbits-of system-map) (system-map object))))))

(defn checksum [system-map]
  (reduce +
          (for [object (keys system-map)]
            (orbits-of system-map object))))

; part 1
(checksum (system part1-input))

; part 2
(defn invert-map-of-coll [m]
   (reduce (fn [a [k v]] (assoc a k v)) {} (for [[k s] m v s] [v k])))

(defn distance-map [system-map object]
  (loop [current   object
         m         (invert-map-of-coll system-map)
         ret       {}
         transfers 1]
    (if-not (contains? m current)
      ret
      (recur (m current)
             m
             (assoc ret (m current) transfers)
             (inc transfers)))))

(def path-you (distance-map (system part1-input) "YOU"))
(def path-san (distance-map (system part1-input) "SAN"))

(def intersection (select-keys path-you (keys path-san)))

(apply min (for [object (keys intersection)]
             (+ (path-you object) (path-san object))))



