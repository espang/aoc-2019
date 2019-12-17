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
    3N [-1  0]
    4N [1 0]))

(defn move->cmd [move]
  (case move
    [0  1] 1N
    [0 -1] 2N
    [-1 0] 3N
    [ 1 0] 4N
    nil))

(defn cmd->inverse [cmd]
  (case cmd
    1N 2N
    2N 1N
    3N 4N
    4N 3N))

(defn robot-state []
  {:graph    (uber/graph)
   :mode     :explore
   :cmds     [1N 2N 3N 4N]
   :pos      [0 0]
   :frontier (clojure.lang.PersistentQueue/EMPTY)
   ; keep track of walls and nodes
   :walls   #{}
   :nodes   #{}})

(defn path->cmds [start end graph]
  (let [path  (alg/shortest-path graph start end)
        edges (alg/edges-in-path path)
        moves (map #(mapv - (:dest %) (:src %)) edges)
        cmds  (map move->cmd moves)]
    cmds))

(defmulti hit-wall :mode)

(defmethod hit-wall :explore [state]
  (let [[cmd & cmds] (:cmds state)
        walls        (conj (:walls state) (mapv + (:pos state)
                                                  (cmd->move cmd)))]
    (if (empty? cmds)
      (assoc state
             :mode     :move
             :cmds     (path->cmds (:pos state) (peek (:frontier state)) (:graph state))
             :frontier (pop (:frontier state))
             :walls walls)
      (assoc state
             :cmds cmds
             :walls walls))))

(defmethod hit-wall :move [state]
  (throw (Exception. "shouldn't hit walls in move mode!")))

(defmulti moved :mode)

(defmethod moved :explore [state]
  (let [[cmd & cmds] (:cmds state)
        moved-to     (mapv + (:pos state) (cmd->move cmd))]
    (if (:step-back state)
      ; step back to continue exploring from moved-to
      (if (empty? cmds)
        (if (empty? (:frontier state))
          (assoc state
                 :cmds (list :stop))
          (assoc state
                 :pos       moved-to
                 :step-back false
                 :mode      :move
                 :cmds      (path->cmds moved-to (peek (:frontier state)) (:graph state))
                 :frontier  (pop (:frontier state))))
        (assoc state
               :step-back false
               :pos  moved-to
               :cmds cmds))
      ; new point
      (let [frontier (if (contains? (:nodes state) moved-to)
                       (:frontier state)
                       (conj (:frontier state) moved-to))]
        (assoc state
               :pos moved-to
               :graph (uber/add-edges (:graph state) [(:pos state) moved-to])
               :cmds  (conj cmds (cmd->inverse cmd))
               :step-back true
               :nodes (conj (:nodes state) moved-to)
               :frontier frontier)))))

(defmethod moved :move [state]
  (let [[cmd & cmds] (:cmds state)
        moved-to (mapv + (:pos state) (cmd->move cmd))]
    (if (empty? cmds)
      (assoc state
             :mode :explore
             :cmds (list 1N 2N 3N 4N)
             :pos  moved-to)
      (assoc state
             :cmds cmds
             :pos  moved-to))))

(defn found-target [state]
  (let [[cmd & cmds] (:cmds state)
        moved-to (mapv + (:pos state) (cmd->move cmd))]
    (println "target " moved-to)
    (println "steps: " (count (path->cmds [0 0] (:pos state) (:graph state))))
    (moved (assoc state :ox-sys moved-to))))

(deftype BFSRobot [state]
  ic/InOutput
  (input-from [x] 
    (first (:cmds state)))
  (output-into [x v]
    (case v
      ; hit wall
      0N (BFSRobot. (hit-wall state))
      ; did step
      1N (BFSRobot. (moved state))
      ; found target
      2N (BFSRobot. (found-target state)))))

(robot-state)
(def ret
  (ic/run (into-array extended-input) (BFSRobot. (robot-state))))

(into []
      (:frontier
       (.state ret)))
(:cmds (.state ret))

(defn to-pixel [p]
  (if (= p [0 0])
    "S"
    (if (= p (:ox-sys (.state ret)))
      "T"
      (if (contains? (:walls (.state ret)) p)
        "#"
        (if (contains? (:nodes (.state ret)) p)
          "."
          " ")))))

(def pixels (for [x (range -40 20)
                  y (range -20 25)]
              (to-pixel [x y])))

(->> pixels 
     (flatten)
     (partition 45)
     (apply map list)
     (map #(apply str %))
     (map println)
     (doall))

