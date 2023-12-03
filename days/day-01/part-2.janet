#!/usr/bin/env janet
(def grammar*
 ~{:main
   (replace (group (any :calibration-number))
     ,|(reduce + 0 $))

   :calibration-number
   (replace
     (group (* (some (+ :calibration-digit (* (! "\n") 1)))
              "\n"))
     ,|(parse (string (first $) (last $))))

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

(def grammar (peg/compile grammar*))

(defn main [& _]
  (->> (:read stdin :all)
    (peg/match grammar)
    first
    pp))
