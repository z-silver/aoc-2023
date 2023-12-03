#!/usr/bin/env janet

(var last-digit-grammar @{})

(defn last-digit [] last-digit-grammar)

(def grammar*
 ~{:main (any :calibration-number)

   :non-delimiter (* (! "\n") 1)

   :first-calibration-digit
   (+ :calibration-digit (* :non-delimiter :first-calibration-digit))

   :last-calibration-digit
   (+ (* :non-delimiter :last-calibration-digit) :calibration-digit)

   :calibration-number
   (cmt (group '(* :first-calibration-digit (thru "\n")))
     ,|(parse (string (first $)
                (last (peg/match (last-digit) (last $))))))

   :calibration-digit
   (+ ':d
     (* "one" (constant "1"))
     (* "two" (constant "2"))
     (* "three" (constant "3"))
     (* "four" (constant "4"))
     (* "five" (constant "5"))
     (* "six" (constant "6"))
     (* "seven" (constant "7"))
     (* "eight" (constant "8"))
     (* "nine" (constant "9")))})

(set last-digit-grammar
  (-> grammar*
    struct/to-table
    (update :main (fn [_] '(* :last-calibration-digit (thru "\n"))))
    table/to-struct))

(def grammar (peg/compile grammar*))

(defn main [& _]
  (->> (:read stdin :all)
    (peg/match grammar)
    (reduce + 0)
    pp))
