(ns clj.day3)

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

(defn move [command pos]
  (let [[dir steps] (parse-command command)]
    
    (into #{} (for [_ range])))
  )

(read-string (apply str (rest "U100")))
