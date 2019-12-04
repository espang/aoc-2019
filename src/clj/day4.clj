(ns clj.day4)
 
; input 273025-767253
(def input (range 273025 767254))

(defn number->digits 
  [n]  
  (loop [digits '()
         n n]
    (if (= 0 n)
      digits
      (recur (conj digits (mod n 10))
             (quot n 10)))))

(defn test-input [possible-password]
  (let [digits (number->digits possible-password)
        freqs  (frequencies digits)
        dup?   (boolean (some #(> (val %) 1) freqs))
        inc?   (apply <= digits)]
    (and dup? inc?)))

(assert (= (test-input 111111) true))
(assert (= (test-input 223450) false))
(assert (= (test-input 123789) false))

()
