(ns clj.day13)

(def input
  (->> (-> "./resources/input13.txt"
           (slurp)
           (clojure.string/split #","))
       (map read-string)
       (map bigint)))

(def extended-input 
  (concat input (repeat 1000 0N)))

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
        3 (do (set state (input-from (:thing state)))
              (recur (update state :idx + 2)))
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


; part 1
(deftype P1 [a]
  InOutput
  (input-from  [x] 0N)
  (output-into [x v] (P1. (conj a v))))

(def output
  (run (into-array extended-input) (P1. [])))

(.a output)

(->> (.a  output)
     (partition 3)
     (filter #(= 2N (nth % 2)))
     (count))

; part 2
(deftype P2 [state]
  InOutput
  (input-from [x]
    (let [ball-x (:ball-x state)
          pad-x  (:pad-x state)]
      (bigint (compare ball-x pad-x))))
  (output-into [x v]
    (let [buffer (:buffer state [])
          buffer (conj buffer v)]
      (if (= 3 (count buffer))
        (let [[x y t] buffer
              blocks  (:blocks state #{})]
          (if (and (contains? blocks [x y]) (not= t 2N) (= 1 (count blocks)))
            (println "game done with score: " (:score state)) 
            (if (and (= x -1N) (= y 0N))
              (P2. (assoc state :buffer [] :score t))
              (case t
                ; either empty or balls are used to remove blocks
                2N (P2. (assoc state
                               :blocks (conj blocks [x y])
                               :buffer []))
                3N (P2. (assoc state :pad-x x
                               :blocks (disj blocks [x y])
                               :buffer []))
                4N (P2. (assoc state :ball-x x
                               :buffer []))
                (P2. (assoc state :buffer []))))))
        (P2. (assoc state :buffer buffer))))))

(defn replace-first [coll val]
  (concat [val] (drop 1 coll)))

(def output (run (into-array (replace-first extended-input 2N)) (P2. {})))
(.state output)
