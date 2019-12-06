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

(checksum (system part1-input))
