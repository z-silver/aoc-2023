#!/usr/bin/env janet

(defn line->meaning [line]
  (peg/match
    ~(any (+ (/ ':d ,first)
            (* "." (constant :space))
            (* "*" (constant :asterisk))
            (* 1 (constant :symbol))))
    line))

(defn sum-values [schematic positions]
  (defn maybe-add-value [acc position]
    (def value (get-in schematic position))
    (if (number? value)
      (+ acc value)
      acc))
  (reduce maybe-add-value 0 positions))

(defn schematic->positions [include? schematic]
  (def height (length schematic))
  (def width (-> schematic first length))
  (def positions @{})
  (loop [row :range [0 height]
         col :range [0 height]]
    (when (include? (get-in schematic [row col]))
      (put positions [row col] [row col])))
  positions)

(defn schematic->symbol-positions [schematic]
  (schematic->positions
    |(or (= :asterisk $) (= :symbol $))
    schematic))

(defn schematic->asterisk-positions [schematic]
  (schematic->positions |(= :asterisk $) schematic))

(defn position->neighbors [rows cols [row* col*]]
  (def neighborhood @[])
  (loop [row :range [(dec row*) (-> row* inc inc)]
         col :range [(dec col*) (-> col* inc inc)]]
    (when (and (not (and (= row row*) (= col col*)))
            (<= 0 row) (< row rows)
            (<= 0 col) (< col cols))
      (array/push neighborhood [row col])))
  neighborhood)

(defn number-details [[start number]]
  (def width (length number))
  @{:width width
    :col-start start
    :col-end (+ start width)
    :source number
    :value (parse number)})

(defn map-indexed [f coll]
  (map f (-> coll length range) coll))

(defn input->schematic-size [input]
  (def newline 10)
  (def cols (length (peg/match '(any '(* (! "\n") 1)) input)))
  (def rows (reduce (fn [acc char]
                      (if (= newline char) (inc acc) acc))
              0 input))
  [rows cols])

(defn input->schematic [input]
    (peg/match
      ~(any (/ '(thru "\n")
              ,|(->> $ string/trim line->meaning)))
      input))

(defn number-position? [num [row col]]
  (and (= row (num :row))
    (<= (num :col-start) col) (< col (num :col-end))))

(defn input->numbers [input]
  (def [rows cols] (input->schematic-size input))
  (defn find-number-neighbors [num]
    (->> (range (num :col-start) (num :col-end))
      (map |(tuple (num :row) $))
      (mapcat |(position->neighbors rows cols $))
      (filter |(not (number-position? num $)))
      (put num :neighbors)))

  (-> (string/split "\n" input)
    (array/remove -2)
    (->>
      (map |(peg/match ~(any (+ (/ (group (* ($) ':d+)) ,number-details) 1)) $))
      (map-indexed (fn [i num] (map |(put $ :row i) num)))
      (mapcat |(map find-number-neighbors $)))))

(defn adjacent-to? [symbols num]
  (defn step [acc position]
    (or acc
      (symbols position)))
  (reduce step false (num :neighbors)))

(defn solve-1 [_ input]
  (def numbers (input->numbers input))
  (def symbols
    (-> input
      input->schematic
      schematic->symbol-positions))
  (def parts (filter |(adjacent-to? symbols $) numbers))
  (->> parts
    (map |(get $ :value))
    (reduce + 0)))

(defn gear->ratio [schematic position]
  (def rows (length schematic))
  (def cols (-> schematic first length))
  (cond
    (not= :asterisk (get-in schematic position)) 0
    :else
    (->> position
      (position->neighbors rows cols)
      (reduce
        (fn [acc position*]
          (def value (get-in schematic position*))
          (if (number? value)
            (put acc position* value)
            acc))
        @{})
      pp
      ((fn [_] 0)))))

(defn solve-2 [_ input]
  (def schematic (input->schematic input))
  (->> schematic
    schematic->asterisk-positions
    (map |(gear->ratio schematic $))))
#pp
#(mapcat values)
#(reduce + 0)

(def problems {:1 solve-1 :2 solve-2})

(defn main [_ part & _]
  (->> (:read stdin :all)
    ((keyword part) problems)
    pp))
