(ns clj.day9)

(def input
  (->> (-> "./resources/input9.txt"
           (slurp)
           (clojure.string/split #","))
       (map read-string)
       (map bigint)))

(def extended-input 
  (into-array (concat input (repeat 1000 0N))))

(defn first-idx [{:keys [arr idx val base]}]
  (case (mod (quot val 100) 10)
    0 (aget arr (inc idx))
    1 (inc idx)
    2 (+ base (aget arr (inc idx)))))
  
(defn second-idx [{:keys [arr idx val base]}]
  (case (mod (quot val 1000) 10)
    0 (aget arr (+ 2 idx))
    1 (+ 2 idx)
    2 (+ base (aget arr (+ 2 idx)))))
 
(defn third-idx [{:keys [arr idx val base]}]
  (case (mod (quot val 10000) 10)
    0 (aget arr (+ 3 idx))
    1 (+ 3 idx)
    2 (+ base (aget arr (+ 3 idx)))))

(defn add [{:keys [arr] :as state}]
  (aset arr (third-idx state)
        (+ (aget arr (first-idx state))
           (aget arr (second-idx state)))))

(defn multi [{:keys [arr] :as state}]
  (aset arr (third-idx state)
        (* (aget arr (first-idx state))
           (aget arr (second-idx state)))))

(defn run [arr inputs] 
  (loop [idx     0
         inputs  inputs
         base    0N]
    (let [parameter-value (aget arr idx)
          optcode         (mod parameter-value 100)
          state           {:arr  arr
                           :idx  idx
                           :val  parameter-value
                           :base base}]
      (case optcode
        1 (do (add state)
              (recur (+ idx 4) inputs base))
        2 (do (multi state)
              (recur (+ idx 4) inputs base))
        3 (do (aset arr (first-idx state) (first inputs))
              (recur (+ idx 2) (rest inputs) base))
        4 (do (println (aget arr (first-idx state)))
              (recur (+ idx 2) inputs base)) 
        5 (if (zero? (aget arr (first-idx state)))
            (recur (+ idx 3) inputs base)
            (recur (aget arr (second-idx state)) inputs base))
        6 (if-not (zero? (aget arr (first-idx state)))
            (recur (+ idx 3) inputs base)
            (recur (aget arr (second-idx state)) inputs base))
        7 (let [to-store (if (< (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (+ idx 4) inputs base))
        8 (let [to-store (if (= (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (+ idx 4) inputs base))
        9 (recur (+ idx 2) inputs (+ base (aget arr (first-idx state))))
        99 (into [] arr)
        (throw (Exception. (str "unexpected command " optcode " received.")))))))

; part 1
(run extended-input [1N])

; part 2
(run extended-input [2N])
