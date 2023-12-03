#!/usr/bin/env janet

(def max-red 12)
(def max-green 13)
(def max-blue 14)

(defn valid? [game]
  (and (<= (game :red) max-red)
    (<= (game :green) max-green)
    (<= (game :blue) max-blue)))

(defn add-results [results]
  (->> results
    (filter valid?)
    (map |($ :game-id))
    (reduce + 0)))

(defn capture->game-max [entries]
  (reduce
    (fn [game [key val]]
      (update game key |(max $ val)))
    @{:game-id 0 :red 0 :blue 0 :green 0}
    entries))

(def grammar*
  ~{:main (replace :all-games ,add-results)
    :all-games (group (* (any :game-max) -1))

    :color-sample
    (replace
      (group (* " " (replace ':d+ ,parse) " " (replace ':a+ ,keyword)))
      ,(fn [[val key]] [key val]))

    :game-id
    (* "Game "
      (group
        (* (constant :game-id)
          (replace ':d+ ,parse)))
      ":")

    :game-samples
    (any (* :color-sample (+ (set ",;\n") -1)))

    :game-max
    (replace
      (group (* :game-id :game-samples))
      ,capture->game-max)})

(def grammar (peg/compile grammar*))

(defn main [& _]
  (->> (:read stdin :all)
    (peg/match grammar)
    first
    pp))
