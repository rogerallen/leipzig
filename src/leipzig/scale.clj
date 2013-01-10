(ns leipzig.scale 
    (:use
       [clojure.math.numeric-tower :only [floor]]))

(defmacro defs {:private true} [names values]
  `(do ~@(map
     (fn [name value] `(def ~name ~value))
     names (eval values))))

(defn- sum-n [series n] (apply + (take n series)))

(defmulti scale-of
  (fn [intervals degree]
    (cond 
      (not= degree (floor degree)) :fraction
      (neg? degree)                :negative
      :otherwise                   :natural)))

(defn scale [intervals] (partial scale-of intervals))

(defmethod scale-of :natural [intervals degree]
  (sum-n (cycle intervals) degree))
(defmethod scale-of :negative [intervals degree]
  (->> degree - (scale-of (reverse intervals)) -))
(defmethod scale-of :fraction [intervals degree]
  (->> degree floor (scale-of intervals) inc))

(def major (scale [2 2 1 2 2 2 1]))
(def blues (scale [3 2 1 1 3 2]))
(def pentatonic (scale [3 2 2 3 2]))
(def chromatic (scale [1]))

(defn- from [base] (partial + base))

(defs [sharp ♯ flat ♭] [inc inc dec dec])

(defs [C D E F G A B]
  (map
    (comp from (from 60) major)
    (range)))

;; all the midi pitches
(defs [C-1 D-1 E-1 F-1 G-1 A-1 B-1
       C0 D0 E0 F0 G0 A0 B0
       C1 D1 E1 F1 G1 A1 B1
       C2 D2 E2 F2 G2 A2 B2
       C3 D3 E3 F3 G3 A3 B3
       C4 D4 E4 F4 G4 A4 B4
       C5 D5 E5 F5 G5 A5 B5
       C6 D6 E6 F6 G6 A6 B6
       C7 D7 E7 F7 G7 A7 B7
       C8 D8 E8 F8 G8 A8 B8
       C9 D9 E9 F9 G9 A9 B9 ;; note A9 & B9 are > 127
       ]
  (map
    (comp from (from 0) major)
    (range)))

(defn mode [scale n] (comp #(- % (scale n)) scale (from n)))

(defs
  [ionian dorian phrygian lydian mixolydian aeolian locrian]
  (map (partial mode major) (range)))

(def minor aeolian)

(def low #(- % 7))
(def high #(+ % 7))
