(ns clj.day7)

(def input [3,8,1001,8,10,8,105,1,0,0,21,34,59,68,89,102,183,264,345,426,99999,3,9,102,5,9,9,1001,9,5,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,5,9,9,1002,9,3,9,1001,9,5,9,4,9,99,3,9,101,5,9,9,4,9,99,3,9,102,4,9,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99])

(defn command-with-op
  [arr idx1 idx2 to-idx op]
  (aset arr to-idx
        (op (aget arr idx1)
            (aget arr idx2))))

(defn process [coll inputs] 
  (let [arr (int-array coll)]
    (loop [idx    0
           inputs inputs]
      (let [parameter-value (aget arr idx)
            optcode         (mod parameter-value 100)
            ; check if it is immediate (im) or position mode
            first-im?       (odd? (quot parameter-value 100))
            first-idx       (if first-im? (+ 1 idx) (aget arr (+ 1 idx)))
            second-im?      (odd? (quot parameter-value 1000))
            second-idx      (if second-im? (+ 2 idx) (aget arr (+ 2 idx)))
            third-im?       (odd? (quot parameter-value 10000))
            third-idx       (if third-im? (+ 3 idx) (aget arr (+ 3 idx)))]
        (case optcode
          1 (do (command-with-op arr first-idx second-idx third-idx +)
                (recur (+ idx 4) inputs))
          2 (do (command-with-op arr first-idx second-idx third-idx *)
                (recur (+ idx 4) inputs))
          3 (do (aset arr first-idx (first inputs))
                (recur (+ idx 2) (rest inputs)))
          4 (aget arr first-idx)
          5 (if (zero? (aget arr first-idx))
              (recur (+ idx 3) inputs)
              (recur (aget arr second-idx) inputs))
          6 (if-not (zero? (aget arr first-idx))
              (recur (+ idx 3) inputs)
              (recur (aget arr second-idx) inputs))
          7 (let [to-store (if (< (aget arr first-idx)
                                  (aget arr second-idx)) 1 0)]
              (aset arr third-idx to-store)
              (recur (+ idx 4) inputs))
          8 (let [to-store (if (= (aget arr first-idx)
                                  (aget arr second-idx)) 1 0)]
              (aset arr third-idx to-store)
              (recur (+ idx 4) inputs))
          99 (throw (Exception. (str "end command received")))
          (throw (Exception. (str "unexpected command " optcode " received."))))))))

(defn run [input phases]
  (loop [phases phases
         last   0]
    (if (empty? phases)
      last
      (recur (rest phases) (process input [(first phases) last])))))

(def test-input [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

(run input [4 3 2 1 0])


(process input [4 0])
(process input [3 2])
(process input [2 59])
(process input [1 64])
(process input [0 1025])


; 43210
