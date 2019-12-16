(ns clj.day15
  (:require [clj.intcomp :as ic]
            [ubergraph.core :as uber]
            [ubergraph.alg  :as alg]))

(def input
  (->> (-> "./resources/input15.txt"
           (slurp)
           (clojure.string/split #","))
       (map read-string)
       (map bigint)))

(def extended-input 
  (concat input (repeat 1000 0N)))

(defn cmd->move [cmd]
  (case cmd
    1N [0  1]
    2N [0 -1]
    3N [1  0]
    4N [-1 0]))

(defn move->cmd [move]
  (case move
    [0  1] 1N
    [0 -1] 2N
    [1  0] 3N
    [-1 0] 4N
    nil))

(defn robot-state []
  {:graph   (uber/graph)
   ; current position. exploring parts around it
   :pos     [0 0]
   :explore (list [1 0] [-1 0] [0 -1])
   :next    [0 1]
   ; queue contains nodes to explore next
   :queue   (clojure.lang.PersistentQueue/EMPTY)
   ; keep track of walls and nodes
   :walls   #{}
   :nodes   #{}})

(defn select-new-target [{:keys [pos explore queue graph] :as state}]
  (if (empty? explore)
    ; move somewhere else
    (let [target (peek queue)
          next   (:dest (first alg/edges-in-path
                               (alg/shortest-path graph)))]
      (assoc state
             :state :move
             :target (peek queue)
             :queue  (pop queue)
             :next   next))
    ; explore more
    (assoc state
           :next    (first explore)
           :explore (rest explore))))

(defn hit-wall [{:keys [walls next-step] :as state}]
  (select-new-target
   (assoc state :walls (conj walls next-step))))

(defn moved [{:keys [pos next target graph nodes] :as state}]
  (if (= target next)
    
    
    ;
    )
  (let [g (uber/add-edges graph [pos next])])
  (let [p' (mapv + pos (movement next-cmd))]
    (if (contains? distance p')
      ; moving to a new field along existing paths
      (assoc state 
             :pos   p'
             :next-cmd (next-step p' next-to state))
      ; found a new field
      (select-new-target
       (assoc state
              :pos p'
              :distance (assoc distance p' (inc (distance pos))))))))

(defn found-target [{:keys [pos distance] :as state}]
  (println (pos distance))
  (throw (Exception. "done")))

(deftype BFSRobot [state]
  ic/InOutput
  (input-from [x] (move->cmd (:next-step state)))
  (output-into [x v]
    (case v
      ; hit wall
      0N (hit-wall state)
      ; did step
      1N (moved state)
      ; found target
      2N (found-target state))))

(defn to-pixel [p]
  (if (= p [0 0])
    "S"
    (if (= p (:ox-sys (.state result)))
      "T"
      (if (contains? (:barriers (.state result)) p)
        "#"
        (if (contains? (:ways (.state result)) p)
          "."
          " ")))))

(def pixels (for [x (range -30 30)
                  y (range -30 35)]
              (to-pixel [x y])))

(->> pixels 
     (flatten)
     (partition 65)
     (apply map list)
     (map #(apply str %))
     (map println)
     (doall))
