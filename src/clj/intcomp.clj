(ns clj.intcomp)

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

(defn set [{:keys [arr] :as state} value]
  (aset arr (first-idx state) value))

(defprotocol InOutput
  (input-from  [x])
  (output-into [x v]))

(defn run [arr thing] 
  (loop [state  {:arr arr
                 :idx 0
                 :base 0N
                 :thing thing}]
    (let [idx             (:idx state)
          arr             (:arr state)
          parameter-value (aget arr idx)
          optcode         (mod parameter-value 100)
          state           (assoc state :val parameter-value)]
      (case optcode
        1 (do (add state)
              (recur (update state :idx + 4)))
        2 (do (multi state)
              (recur (update state :idx + 4)))
        3 (let [input (input-from (:thing state))]
            (if (= :stop input)
              (:thing state)
              (do (set state input)
                  (recur (update state :idx + 2)))))
        4 (recur (assoc state
                        :thing (output-into (:thing state) (aget arr (first-idx state)))
                        :idx   (+ 2 (:idx state))))
        5 (if (zero? (aget arr (first-idx state)))
            (recur (update state :idx + 3))
            (recur (assoc state :idx (aget arr (second-idx state)))))
        6 (if-not (zero? (aget arr (first-idx state)))
            (recur (update state :idx + 3))
            (recur (assoc state :idx (aget arr (second-idx state)))))
        7 (let [to-store (if (< (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (update state :idx + 4)))
        8 (let [to-store (if (= (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (update state :idx + 4)))
        9 (recur (assoc state
                        :idx  (+ 2 idx)
                        :base (+ (:base state) (aget arr (first-idx state)))))
        99 (:thing state)
        (throw (Exception. (str "unexpected command " optcode " received.")))))))
