(ns clj.graph
  (:require [clojure.zip :as z]
            [ubergraph.core :as uber]
            [ubergraph.alg  :as alg]))

(def graph (uber/graph [[0 0] [0 1]] [[0 0] [0 -1]] [[0 1] [0 2]]))

(uber/pprint graph)

(alg/pprint-path
 (alg/shortest-path graph [0 0] [0 2]))
(def path (alg/shortest-path graph [0 0] [0 2]))

(:dest (first (alg/edges-in-path path)))

(uber/add-edges graph [[0 0] [0 1]])

graph



(defn zp [data]
 (z/zipper vector? seq (fn [_ c] c) data))

(-> (zp [[0 0]])
    z/down
    z/down
    first)



[[0 0] ]



