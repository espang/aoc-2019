(ns clj.day8)

(def input
  (->> "./resources/input8.txt"
       slurp
       (map #(- (int %) (int \0)))
       (reduce conj [])))

; part 1
(->> input
     (partition 150)
     (map frequencies)
     (apply min-key #(% 0)))

; part 2
(defn resolve-pixel [pixel]
  (let [pixel (drop-while #(= % 2) pixel)]
    (if (nil? pixel)
      " " ; transparent -> black
      (if (= 1 (first pixel)) "#" " "))))

(->> input
     ; into list of layers
     (partition 150)
     ; transpose into list of pixels
     (apply map list)
     (map resolve-pixel)
     ; partition pixel into rows
     (partition 25)
     (map #(apply str %))
     (map println)
     (doall))
