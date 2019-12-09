(ns clj.day9)

(def test-input [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])

(def input
  (->> "./resources/input9.txt"
       (slurp)
       (as-> x (clojure.string/split x #","))
       (map read-string)
       (map bigint)
       (into-array)))

(bigint 1)

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
         inputs  inputs
         base    0]
    (let [parameter-value (aget arr idx)
          optcode         (mod parameter-value 100)]
      (case optcode
        1 (do (add arr idx parameter-value)
              (recur (+ idx 4) inputs))
        2 (do (multi arr idx parameter-value)
              (recur (+ idx 4) inputs))
        3 (do (aset arr (first-idx arr idx parameter-value) (first inputs))
              (recur (+ idx 2) (rest inputs)))
        4 (do (println (aget arr (first-idx arr idx parameter-value)))
              (recur (+ idx 2) inputs)) 
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
        99 (into [] arr)
        (throw (Exception. (str "unexpected command " optcode " received.")))))))

(into-array [1N 2N])
