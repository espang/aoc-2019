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
(def pixels
  (->> input
       (partition 150)
       (apply map list)))

(defn resolve-pixel [pixel]
  (let [pixel (drop-while #(= % 2) pixel)]
    (if (nil? pixel)
      2
      (if (= 1 (first pixel)) "#" " "))))

(defn print-pixels [pixels w t]
  (loop [pixels pixels
         x      0]
    (if (empty? pixels)
      nil
      (do
        (when (zero? (mod x w))
          (println ""))
        (print (resolve-pixel (first pixels)))
        (recur (rest pixels) (inc x))))))

(print-pixels pixels 25 6)
