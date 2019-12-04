(ns clj.day3)

(def file-wire1 "./resources/input3.txt")

(def file-wire2 "./resources/input3_2.txt")

(defn read-input [file-name]
  (-> file-name
      slurp
      (clojure.string/split #",")))

(def wire1 (read-input file-wire1))

(def wire2 (read-input file-wire2))

(defn parse-nbr [chars]
  (read-string (apply str chars)))

(defn parse-dir [dir]
  (case dir
    \U :up
    \D :down
    \L :left
    \R :right
    (throw (Exception. (str "unexpected direction " dir " received, expectiong one of {U|D|L|R}.")))))

(defn parse-command [[dir & number]]
  [(parse-dir dir)
   (parse-nbr number)])

(defn move-one-step [dir [x y]]
  (case dir
    :up    [x (inc y)]
    :down  [x (dec y)]
    :left  [(dec x) y]
    :right [(inc x) y]))

(defn move [command pos steps-done]
  (let [[dir steps] (parse-command command)]
    (loop [steps      steps
           pos'       pos
           points     {}
           steps-done steps-done]
      (if (zero? steps)
        [points pos' steps-done]
        (let [new-pos (move-one-step dir pos')]
          (recur (dec steps)
                 new-pos
                 (conj points [new-pos (inc steps-done)])
                 (inc steps-done)))))))

(defn points [wire-commands]
  (loop [commands   wire-commands
         points     {}
         pos        [0 0]
         steps-done 0]
    (if (nil? (first commands))
      points
      (let [[new-points new-pos steps-done] (move (first commands) pos steps-done)]
        (recur (rest commands)
               (merge new-points points)
               new-pos
               steps-done)))))

(def points-wire1 (points wire1))

(def points-wire2 (points wire2))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(def intersections (select-keys points-wire1 (keys points-wire2)))

; part 1
(apply min-key #(manhattan-distance [0 0] %) (keys intersections)) ; => [-34 -352]
; part 2
(apply min (for [pos (keys intersections)]
             (+ (points-wire1 pos) (points-wire2 pos))))
