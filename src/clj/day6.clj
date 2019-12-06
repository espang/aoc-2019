(ns clj.day6)


(def test-input ["COM)B" "B)C"])

(defn parse-input [input]
  (let [[inner outer] (clojure.string/split input #"\)")]
    [inner outer]))

(parse-input "COM)B")

(defn system [inputs]
  (loop [inputs inputs
         m      {}]
    (println inputs)
    (if (empty? inputs)
      m
      (let [[inner outer] (parse-input (first inputs))]
        (println inner)
        (recur (rest inputs)
               (update m inner (fnil conj []) outer))))))
