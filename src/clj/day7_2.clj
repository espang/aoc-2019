
(ns clj.day7-2
  (:require [clojure.math.combinatorics :as combo]))

(def input [3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99])


(defn first-idx [arr idx v]
  (if (odd? (quot v 100))
    (inc idx)
    (aget arr (inc idx))))

(defn second-idx [arr idx v]
  (if (odd? (quot v 1000))
    (+ 2 idx)
    (aget arr (+ 2 idx))))

(defn third-idx [arr idx v]
  (if (odd? (quot v 10000))
    (+ 3 idx)
    (aget arr (+ 3 idx))))

(defn add [arr idx v]
  (aset arr (third-idx arr idx v)
        (+ (aget arr (first-idx arr idx v))
           (aget arr (second-idx arr idx v)))))

(defn multi [arr idx v]
  (aset arr (third-idx arr idx v)
        (* (aget arr (first-idx arr idx v))
           (aget arr (second-idx arr idx v)))))

(defn start-until-no-inputs [arr idx inputs] 
  (loop [idx     idx
         inputs  inputs]
    (let [parameter-value (aget arr idx)
          optcode         (mod parameter-value 100)]
      (case optcode
        1 (do (add arr idx parameter-value)
              (recur (+ idx 4) inputs))
        2 (do (multi arr idx parameter-value)
              (recur (+ idx 4) inputs))
        3 (do (aset arr (first-idx arr idx parameter-value) (first inputs))
              (recur (+ idx 2) (rest inputs)))
        4 [arr (+ 2 idx) inputs (aget arr (first-idx arr idx parameter-value))]
        5 (if (zero? (aget arr (first-idx arr idx parameter-value)))
            (recur (+ idx 3) inputs)
            (recur (aget arr (second-idx arr idx parameter-value)) inputs))
        6 (if-not (zero? (aget arr (first-idx arr idx parameter-value)))
            (recur (+ idx 3) inputs)
            (recur (aget arr (second-idx arr idx parameter-value)) inputs))
        7 (let [to-store (if (< (aget arr (first-idx arr idx parameter-value))
                                (aget arr (second-idx arr idx parameter-value))) 1 0)]
            (aset arr (third-idx arr idx parameter-value) to-store)
            (recur (+ idx 4) inputs))
        8 (let [to-store (if (= (aget arr (first-idx arr idx parameter-value))
                                (aget arr (second-idx arr idx parameter-value))) 1 0)]
            (aset arr (third-idx arr idx parameter-value) to-store)
            (recur (+ idx 4) inputs))
        99 [arr idx inputs :stop]
        (throw (Exception. (str "unexpected command " optcode " received.")))))))

(defn initialise [input phases]
  (loop [phases     phases
         names      [:A :B :C :D :E]
         amplifiers []]
    (if (empty? phases)
      amplifiers
      (recur (rest phases)
             (rest names)
             (conj amplifiers
                   {:state  (int-array input)
                    :inputs [(first phases)]
                    :index  0
                    :name   (first names)})))))

(defn execute-amplifier [more-inputs {:keys [state index inputs] :as amplifier}]
  (let [[new-state new-index inputs output] (start-until-no-inputs state index (concat inputs more-inputs))]
    [output (assoc amplifier
                   :state new-state
                   :index new-index
                   :inputs inputs
                   :output (if-not (= :stop output) output (:output amplifier)))]))

(defn output-of [amplifiers label]
  (:output (first (filter #(= (:name %) label) amplifiers))))

(defn run [input phases]
  (let [amplifiers (initialise input phases)]
    (loop [amplifiers amplifiers
           inputs     [0]]
      (if (empty? inputs)
        (output-of amplifiers :E)
        (let [[o a] (execute-amplifier inputs (first amplifiers))]
          (if (= :stop o)
            (output-of amplifiers :E)
            (recur (concat (rest amplifiers) [a]) [o])))))))

(apply max (for [phases (combo/permutations [5 6 7 8 9])]
             (run input phases))) ; => 30872528
