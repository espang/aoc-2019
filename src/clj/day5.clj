(ns clj.day5)


(def input-file "./resources/input5.txt")

(defn input-to-coll [filename]
  (-> filename
      slurp
      (clojure.string/split #",")
      (as-> xs
          (map clojure.string/trim xs)
          (map read-string xs))
      (doall)))

(def input-coll (input-to-coll input-file))

(defn command-with-op
  [arr idx1 idx2 to-idx op]
  (aset arr to-idx
        (op (aget arr idx1)
            (aget arr idx2))))

;Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
;Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
;Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.

(defn process [coll inputs] 
  (let [arr (int-array coll)]
    (loop [idx    0
           inputs inputs]
      (let [parameter-value (aget arr idx)
            optcode         (mod parameter-value 100)
            ; check if it is immediate or position mode (pm)
            first-pm        (odd? (quot parameter-value 100))
            first-idx       (if first-pm (+ 1 idx) (aget arr (+ 1 idx)))
            second-pm       (odd? (quot parameter-value 1000))
            second-idx      (if second-pm (+ 2 idx) (aget arr (+ 2 idx)))
            third-pm        (odd? (quot parameter-value 10000))
            third-idx       (if third-pm (+ 3 idx) (aget arr (+ 3 idx)))]
        (case optcode
          1 (do (command-with-op arr first-idx second-idx third-idx +)
                (recur (+ idx 4) inputs))
          2 (do (command-with-op arr first-idx second-idx third-idx *)
                (recur (+ idx 4) inputs))
          3 (do (aset arr first-idx (first inputs))
                (recur (+ idx 2) (rest inputs)))
          4 (do (println (aget arr first-idx))
                (recur (+ idx 2) inputs))
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
          99 (into [] arr)
          (throw (Exception. (str "unexpected command " optcode " received."))))))))

; part 1
(process input-coll [1]) ; => 7259358

; part 2
(process input-coll [5]) ; =? 11826654
