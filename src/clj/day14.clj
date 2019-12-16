(ns clj.day14)

(defn parse-term [term]
  (let [[n e] (clojure.string/split term #" ")]
    [e (read-string n)]))

(defn parse-lines [lines]
  (loop [lines     lines
         reactions {}
         from-ores {}]
    (if (empty? lines)
      [reactions from-ores]
      (let [[lhs rhs] (clojure.string/split (first lines) #"=> ")
            [ret n]   (parse-term rhs)
            terms     (if (clojure.string/includes? lhs ", ")     
                        (mapv parse-term (clojure.string/split lhs #", "))
                        [(parse-term lhs)])]
        (println terms)
        (if (and (= 1 (count terms))
                 (= "ORE" (first (first terms))))
          (do
            (println "from-ores")
            (recur (rest lines)
                   reactions
                   (conj from-ores [ret [n (second (first terms))]])))
          (recur (rest lines)
                 (conj reactions 
                       [ret [n (into {} terms)]])
                 from-ores))))))

(def input 
  (-> "./resources/input14.txt"
      (slurp)
      (clojure.string/split-lines)
      (parse-lines)))

(defn factor [amount steps]
  (if (zero? (mod amount steps))
    (quot amount steps)
    (inc (quot amount steps))))

(defn resolve [[n wanted] reactions from-ore additional]
  (if (contains? from-ore wanted)
    [{wanted n} additional]
    (if (contains? additional wanted)
      (if (> n (additional wanted))
        (recur [(- n (additional wanted)) wanted] reactions from-ore (dissoc additional wanted))
        [{} (update additional wanted - n)])
      (let [[m input]  (reactions wanted)
            f          (factor n m)
            adds       (- (* f m) n)]
        (reduce (fn [[r' leftovers] [w' n']]
                  (let [[receipt leftovers] (resolve [(* f n') w'] reactions from-ore leftovers)]
                    [(merge-with + r' receipt)
                     leftovers]))
                [{} (update additional wanted (fnil + 0) adds)]
                input)))))

(defn solve [n]
  (let [[r f] input]
    (resolve [n "FUEL"] r f {})))

(defn ore [solution from-ore]
  (reduce +
          (for [[k v] solution]
            (let [[n ores] (from-ore k)]
              (* ores (factor v n))))))

(ore (first (solve 1)) (second input))
; part 1: 1582325

; part 2
(defn find [amount start end]
  (let [middle (quot (+ start end) 2)
        ret    (ore (first (solve middle)) (second input))]
    (if (< (Math/abs (- amount ret)) 2000000)
      middle
      (if (< ret amount)
        (recur amount middle end)
        (recur amount start middle)))))

; find a good estimate
(find 1000000000000 1000000 3000000)

; print range around the estimates
(for [x (range 2267480 2267495)]
  (println x (ore (first (solve x)) (second input))))
