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
; returns ArrayIndexOutOfBoundsException after printing the result.
(process input-coll [5]) ; => 11826654
