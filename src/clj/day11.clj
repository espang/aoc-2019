(ns clj.day11)


(def input [3,8,1005,8,337,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,29,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,51,1,1008,18,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,76,1006,0,55,1,1108,6,10,1,108,15,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,110,2,1101,13,10,1,101,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,139,1006,0,74,2,107,14,10,1,3,1,10,2,1104,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,177,2,1108,18,10,2,1108,3,10,1,109,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,210,1,1101,1,10,1,1007,14,10,2,1104,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,244,1,101,3,10,1006,0,31,1006,0,98,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,277,1006,0,96,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,302,1,3,6,10,1006,0,48,2,101,13,10,2,2,9,10,101,1,9,9,1007,9,1073,10,1005,10,15,99,109,659,104,0,104,1,21101,937108976384,0,1,21102,354,1,0,1105,1,458,21102,1,665750077852,1,21101,0,365,0,1105,1,458,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,21478178856,0,1,21101,412,0,0,1105,1,458,21102,3425701031,1,1,21102,1,423,0,1106,0,458,3,10,104,0,104,0,3,10,104,0,104,0,21102,984458351460,1,1,21102,1,446,0,1105,1,458,21101,0,988220908388,1,21101,457,0,0,1105,1,458,99,109,2,22101,0,-1,1,21102,1,40,2,21101,489,0,3,21101,479,0,0,1105,1,522,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,484,485,500,4,0,1001,484,1,484,108,4,484,10,1006,10,516,1102,0,1,484,109,-2,2105,1,0,0,109,4,1201,-1,0,521,1207,-3,0,10,1006,10,539,21102,1,0,-3,21201,-3,0,1,21202,-2,1,2,21101,1,0,3,21101,558,0,0,1105,1,563,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,586,2207,-4,-2,10,1006,10,586,22102,1,-4,-4,1106,0,654,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,1,605,0,1106,0,563,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,624,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,646,22101,0,-1,1,21102,646,1,0,106,0,521,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0])

(def extended-input 
 (concat (map bigint input) (repeat 1000 0N)))

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

(defn input-from [{:keys [pos whites]}]
  (if (contains? whites pos)
    1N
    0N))

(defn update-robot [{:keys [pos paint? dir whites painted] :as robot} output]
  (if paint?
    (if (zero? output)
      ; black
      (assoc robot
             :paint? false
             :painted (conj painted pos)
             :whites  (disj whites pos))
      (assoc robot
             :paint? false
             :painted (conj painted pos)
             :whites  (conj whites pos)))
    (let [robot (assoc robot :paint? true)
          [x y] pos]
      (if (zero? output)
        (case dir
          :north (assoc robot
                        :dir :west
                        :pos [(dec x) y])
          :south (assoc robot
                        :dir :east
                        :pos [(inc x) y])
          :east (assoc robot
                       :dir :north
                       :pos [x (inc y)])
          :west (assoc robot
                       :dir :south
                       :pos [x (dec y)]))
        (case dir
          :north (assoc robot
                        :dir :east
                        :pos [(inc x) y])
          :south (assoc robot
                        :dir :west
                        :pos [(dec x) y])
          :east (assoc robot
                       :dir :south
                       :pos [x (dec y)])
          :west (assoc robot
                       :dir :north
                       :pos [x (inc y)]))))))

(defn run [arr] 
  (loop [idx      0
         base     0N
         robot    {:pos     [0 0]
                   :paint?  true
                   :dir     :north
                   :whites  #{[0 0]}
                   :painted #{}}]
    (let [parameter-value (aget arr idx)
          optcode         (mod parameter-value 100)
          state           {:arr  arr
                           :idx  idx
                           :val  parameter-value
                           :base base}]
      (case optcode
        1 (do (add state)
              (recur (+ idx 4) base robot))
        2 (do (multi state)
              (recur (+ idx 4) base robot))
        3 (do (aset arr (first-idx state) (input-from robot))
              (recur (+ idx 2) base robot))
        4 (let [output (aget arr (first-idx state))]
            (recur (+ idx 2) base (update-robot robot output))) 
        5 (if (zero? (aget arr (first-idx state)))
            (recur (+ idx 3) base robot)
            (recur (aget arr (second-idx state)) base robot))
        6 (if-not (zero? (aget arr (first-idx state)))
            (recur (+ idx 3) base robot)
            (recur (aget arr (second-idx state)) base robot))
        7 (let [to-store (if (< (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (+ idx 4) base robot))
        8 (let [to-store (if (= (aget arr (first-idx state))
                                (aget arr (second-idx state))) 1N 0N)]
            (aset arr (third-idx state) to-store)
            (recur (+ idx 4) base robot))
        9 (recur (+ idx 2) (+ base (aget arr (first-idx state))) robot)
        99 robot
        (throw (Exception. (str "unexpected command " optcode " received.")))))))

(def robot
  (run (into-array extended-input)))

(count (:painted robot))
robot

(defn bounds [whites]
  [(first  (apply min-key first whites))
   (second (apply min-key second whites))
   (first  (apply max-key first whites))
   (second (apply max-key second whites))])

(println
 (bounds (:whites robot))) ; => [1 -5 39 0]

(def pixels (for [x (range 0 40)]
              (for [y (range 0 -6 -1)]
                (if (contains? (:whites robot) [x y])
                  "#"
                  " "))))

(->> pixels
     (flatten)
     (partition 6) ; list of columns
     (apply map list)
     (map #(apply str %))
     (map println)
     (doall))

; BLULZJLZ
