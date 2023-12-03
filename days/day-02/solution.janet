#!/usr/bin/env janet

(def max-red 12)
(def max-green 13)
(def max-blue 14)

(defn valid? [game]
  (and (<= (game :red) max-red)
    (<= (game :green) max-green)
    (<= (game :blue) max-blue)))

(defn add-ids [games]
  (->> games
    (filter valid?)
    (map |($ :game-id))
    (reduce + 0)))

(defn power [game]
  (* (game :red) (game :green) (game :blue)))

(defn add-powers [games]
  (->> games
    (map power)
    (reduce + 0)))

(defn capture->game-max [entries]
  (reduce
    (fn [game [key val]]
      (update game key |(max $ val)))
    @{:game-id 0 :red 0 :blue 0 :green 0}
    entries))

(def grammar*
  ~{:main (* (any :game-max) -1)

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

(defn part-1 [_ input] (->> input (peg/match grammar) add-ids))
(defn part-2 [_ input] (->> input (peg/match grammar) add-powers))

(defn main [_ part & _]
  (->> (:read stdin :all)
    ((keyword part) {:1 part-1 :2 part-2})
    pp))
