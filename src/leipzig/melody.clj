(ns leipzig.melody
  (:use
    [overtone.live :only [at now]]))

(defn bpm
  "Returns a function that translates a beat number into milliseconds.
  e.g. ((bpm 90) 5)" 
  [beats] (fn [beat] (-> beat (/ beats) (* 60) (* 1000))))

(defn- sum-n [series n] (reduce + (take n series)))
(defn phrase
  "Translates a sequence of durations and pitches into a melody.
  e.g. (phrase [1 1 2] [7 6 4])" 
  [durations pitches]
  (let [timings (map (partial sum-n durations) (range))]
    (map #(zipmap [:time :pitch :duration] [%1 %2 %3])
         timings pitches durations)))

(defn where
  "Applies f to the k key of each note in notes.
  e.g. (->> notes (where :time (bpm 90)))"
  [k f notes] (map #(update-in % [k] f) notes))

(def is
  "Synonym for constantly.
  e.g. (->> notes (where :part (is :bass)))" 
  constantly)

(defn- from [base] (partial + base))

(defn after
  "Delay notes by wait.
  e.g. (->> melody (after 3))"
  [wait notes] (where :time (from wait) notes))

(defn then 
  "Sequences second after first.
  e.g. (->> call (then response))"
  [second first]
    (let [{time :time duration :duration} (last first)
          shifted (after (+ duration time) second)]
      (concat first shifted)))

(defn times
  "Repeats notes n times.
  e.g. (->> bassline (times 4))"
  [n notes] (reduce then (repeat n notes)))

(defn- before? [a b] (<= (:time a) (:time b)))
(defn with
  "Accompanies two melodies with each other.
  e.g. (->> melody (with bass))"
  [[a & other-as :as as] [b & other-bs :as bs]]
  (cond
    (empty? as) bs
    (empty? bs) as
    (before? a b) (cons a (lazy-seq (with other-as bs)))
    :otherwise    (cons b (lazy-seq (with as other-bs)))))

(defmulti play-note
  "Plays a note according to its :part.
  e.g. (play-note {:part :bass :time _})"
  :part)

(defmulti note-on
  "Start a note according to its :part."
  :part)

(defmulti note-off
  "Stop a note according to its :part."
  :part)

(defn- trickle [notes]
  (if-let [{epoch :time :as note} (first notes)]
    (do
      (Thread/sleep (max 0 (- epoch (+ 100 (now))))) 
      (cons note 
        (lazy-seq (trickle (rest notes)))))))

(defn play
  "Plays notes now.
  e.g. (->> melody play)"
  [notes] 
  (->>
    notes
    (after (now))
    trickle
    (map (fn [{epoch :time :as note}] (at epoch (play-note note))))
    dorun))

(defn play2
  "Plays notes now for an instrument that takes note-on/note-off combos
  e.g. (->> melody play)"
  [notes] 
  (->>
    notes
    (after (now))
    trickle
    (map (fn [{epoch :time
              duration :duration
              :as note}]
           (let [cur-inst (at epoch (note-on note))]
             (at (+ epoch duration) (note-off note cur-inst)))))
    dorun))
