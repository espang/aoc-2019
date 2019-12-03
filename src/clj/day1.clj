(ns clj.day1)


(defn fuel-required [mass]
  (- (int (/ mass 3))
     2))

; testcases:
(assert (= (fuel-required 12) 2))
(assert (= (fuel-required 14) 2))
(assert (= (fuel-required 1969) 654))
(assert (= (fuel-required 100756) 33583))

; part 1
(->> "./resources/input_day1_1.txt"
    (slurp)
    (clojure.string/split-lines)
    (map read-string)
    (map fuel-required)
    (reduce +)) ; => 3239890 

; part 2
(defn fuel-required-with-fuel-mass
  ([mass] (fuel-required-with-fuel-mass mass 0))
  ([mass acc]
   (let [fuel-mass (fuel-required mass)]
     (if (<= fuel-mass 0)
       acc
       (recur fuel-mass (+ acc fuel-mass))))))

(assert (= (fuel-required-with-fuel-mass 12) 2))
(assert (= (fuel-required-with-fuel-mass 1969) 966))
(assert (= (fuel-required-with-fuel-mass 100756) 50346))

(->> "./resources/input_day1_1.txt"
    (slurp)
    (clojure.string/split-lines)
    (map read-string)
    (map fuel-required-with-fuel-mass)
    (reduce +)) ; => 4856963
