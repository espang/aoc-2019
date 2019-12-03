(ns clj.day2)


(def input-file "/home/eike/projects/aoc-2019/resources/input_2_1.txt")

(defn input-to-coll [filename]
  (-> filename
      slurp
      (clojure.string/split #",")
      (as-> xs
          (map clojure.string/trim xs)
          (map #(Integer/parseInt %) xs))
      (doall)))

(def input-coll (input-to-coll input-file))

(defn command-with-op
  [arr idx op]
  (let [idx1   (aget arr (+ idx 1))
        idx2   (aget arr (+ idx 2))
        to-idx (aget arr (+ idx 3))]
    (aset arr to-idx
          (op (aget arr idx1)
              (aget arr idx2)))))

(defn process [coll]
  (let [arr (int-array coll)]
    (loop [idx 0]
      (let [command (aget arr idx)]
        (case command
          1 (do (command-with-op arr idx +)
                (recur (+ idx 4)))
          2 (do (command-with-op arr idx *)
                (recur (+ idx 4)))
          99 (into [] arr)
          (throw (Exception. (str "unexpected command " command " received.")))))xs)))

(assert (= (process [1 0 0 0 99]) [2 0 0 0 99]))
(assert (= (process [2 3 0 3 99]) [2 3 0 6 99]))
(assert (= (process [2 4 4 5 99 0]) [2 4 4 5 99 9801]))
(assert (= (process [1 1 1 4 99 5 6 0 99]) [30 1 1 4 2 5 6 0 99]))

(first (process input-coll)) ; => 6087827

; part 2
(def first-entry (first input-coll))
(def other-entries (drop 3 input-coll))

(defn result-for [noun verb]
  (first (process (concat [first-entry noun verb] other-entries))))

(doall
 (for [noun (range 0 100)
       verb (range 0 100)
       :when (= (result-for noun verb) 19690720)]
   {:noun noun
    :verb verb})) ; => ({:noun 53 :verb 79})

