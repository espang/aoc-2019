(ns clj.day17
  (:require [clj.intcomp :as ic]))

(def input
  (->> (-> "./resources/input17.txt"
           (slurp)
           (clojure.string/split #","))
       (map read-string)
       (map bigint)))

(def extended-input 
  (concat input (repeat 3000 0N)))

(deftype Robot [state]
  ic/InOutput
  (input-from [x]
    0N)
  (output-into [x v]
    (Robot. (conj state v))))

(def ret (ic/run (into-array extended-input) (Robot. [])))

(defn scaffold [chars]
  (loop [x     0
         y     0
         chars chars
         ret   #{}]
    (if (empty? chars)
      ret
      (case (first chars)
        \# (recur (inc x) y (rest chars) (conj ret [x y]))
        \newline (recur 0 (inc y) (rest chars) ret)
        (recur (inc x) y (rest chars) ret)))))

(def s (scaffold (map char (.state ret))))

(defn crosses [s]
  (loop [scaffolds     s
         intersections #{}]
    (if (empty? scaffolds)
      intersections
      (if (every? #(contains? s %) (map #(mapv + (first scaffolds) %) [[1 0] [-1 0] [0 1] [0 -1]]))
        (recur (rest scaffolds) (conj intersections (first scaffolds)))
        (recur (rest scaffolds) intersections)))))

(reduce + (map #(apply * %) (crosses s)))
