(ns audio.midi

  (:import (java.io File)
           (javax.sound.midi MidiSystem Sequence MidiMessage MidiEvent ShortMessage Track))
  (:require [overtone.live :as overtone]))



;; (piano)
;; (piano 67)
;; (Thread/sleep 256)
;; (piano 64)
;; (Thread/sleep 256)
;; (piano 60)

(defn ts [ms] (Thread/sleep ms))


(defn matmusic [f x y t]
  (loop [i 40]
    (if (< i 120)
      (do (play_piano i)
        (recur (int (f i x)))))))


(def c0 8.1757989156)
(def halfTone 1.05946309435941)
(def quaterTone 1.029302236643495)
(def b1 15.4338531643)
(def c0bound (* c0 quaterTone))

;; (matmusic * 1.03 0 120)

;; (doseq [note [60 64 67 72]] (piano note))

(def c0 8.1757989156)


(defn hz->midiTable []
  (loop [i 0 note c0 acc []]
    (if (> i 127) acc
      (recur (inc i) (* note halfTone) (conj acc {:midi i :hz note}))
      )))


(defn midi->hz [midiNote]
  (loop [note midiNote freq c0]
    (if (zero? note) freq
      (recur (dec note) (* halfTone freq)))))

(midi->hz 60)


(defn hz->midi
  "converts frequency to midi note"
  [herz]
  (loop [i 0 hz herz]
    (if (< hz c0bound) i
      (recur (inc i) (/ hz halfTone))
      )))

(hz->midi 17.3239144360)


(defn matmusic [fun t]
  (loop [i 40]
    (if (< i 120)
      (do (play_piano i)
        (recur (int (fun i 1.05)))))))



(def pokerface "/Users/michal/hits/2008_119_8_m_ladygaga_pokerface.mid")
(def m0 "/Users/michal/m0.mid")
(def midi5 "/Users/michal/61.mid")
(def midi6 "/Users/michal/62.mid")
(def midi7 "/Users/michal/63.mid")
(def midi8 "/Users/michal/60-126.mid")
(def midi9 "/Users/michal/60-120.mid")
(def smpl "/Users/michal/Desktop/bounces/Ralphi Rosario VS Bojan - You used to hold me Instrumental.wav")


(defn track->midi-events
  "takes midi track and returns vector of midi events"
  [track]
  (loop [i 0 events []]
    (if (= (inc i) (.size track)) events
      (recur (inc i) (conj events (.get track i))))))


(defn midiMessage
  "takes midi event and returns set of position of this event in ticks
  and midi note plus info if it's note on (0x90) or note off (0x80)"
  [event]
  (let [position (.getTick event)
        noteEvent (.getData1 (.getMessage event))
        command (.getCommand (.getMessage event))]
    {:pos position :note noteEvent :onOff command}))



(defn midi-parse
  "parses midi file, returns sequence of set: position, note.
  ignores note off, and velocity"
  ([file-name] (midi-parse file-name 0))
  ([file-name track]
   (let [note-on 0x90
         note-off 0x80
         sequence (MidiSystem/getSequence (File. file-name))
         track  (-> sequence .getTracks (aget track))]
     (->>
       track
       (track->midi-events)
       (filter #(instance? ShortMessage (.getMessage %)))
       (map midiMessage)

       (filter #(or (= (get % :onOff) 0x90) (= (get % :onOff) 0x80)))
;;        (map #(dissoc % :onOff))
       ))))




tt


(defn mapMidi [events]
  (loop [ev events acc {}]
    (if (empty? ev) acc
      (let [e (first ev)
            {:keys [note pos onOff]} e
            newEvent {:pos pos :onOff onOff}
            setNote (get acc note)
            newAcc (if (nil? setNote)
                     (assoc acc note [newEvent])
                     (assoc acc note(conj setNote newEvent)))]
      (recur (rest ev) newAcc)))))



(defn durations [noteEvents]
  (loop [notes (get noteEvents 1) acc []]
    (if (empty? notes) acc
      (let [pos1 (:pos (get notes 0))
            pos2 (:pos (get notes 1))
            duration (- pos2 pos1)]
        (recur (subvec notes 2) (conj  acc {:note (get noteEvents 0) :pos pos1 :dur duration}))))))


(defn parseMidiDurations [midiFile]
  (->> (midi-parse midiFile)
       (mapMidi)
       (map durations)
       (reduce #(into %1 %2))))









(defn playMidi [xs]
  (doseq [event xs]
    (play_piano (get event :notes) (get event :duration))))

;; (repeatedly 1 #(playMidi (events->seq tt)))



(def sampl (overtone/sample smpl))

(def sample1 (overtone/load-sample smpl))
(def dry (overtone/play-buf 1 sample1))


sample1
(overtone/defsynth lpf []
  (let [dry (overtone/play-buf 1 sample1)
        wet (overtone/lpf18 dry 300)]
      (overtone/out 1 [wet dry])))


(repeatedly 1 #(do (lpf)))





  ; (defn f1 [x]

;;   (do (println "started f1") (Thread/sleep 4000) (println "done f1 " x) x))

;; (defn f2 [y]
;;   (do (println "started f2") (Thread/sleep 5000) (println "done f2 " y) y))



;; (time (do (f1 1) (f2 2)))
;; (time (pmap #(%) [f1 f2]))
;; (time (pvalues (f1 1) (f2 2)))

;; (def letters (mapv (comp str char (partial + 65)) (range 26)))
;; (defn random-string [n]
;;   (apply str (take n (repeatedly #(rand-nth letters)))))

;; (defn rand-string [ll sl]
;;   (doall (take ll (repeatedly (partial random-string sl)))))



;; (def llist (rand-string 2000 10000))

;; (time (dorun (map clojure.string/lower-case llist)))

;; (time (dorun (map sort llist)))

;; (time (dorun (pmap sort llist)))

;; (time (doall (pmap (fn [x] (doall (pmap sort x))) (partition-all (/ (count llist) 16) llist))))

;; (defn multia
;;   ([x] (multia x "zero"))
;;   ([x y]
;;    (println x y)

;;    ))

;; (def v ["a" "b"])
;; (def vv [["a" "b"]["c" "d"]["e" "f"]["g" "h"]["i" "j"]])
;; (multia "a")
;; (multia "a" "b")
;; (apply multia v)

;;  (map (fn [x] (apply multia x)) vv)
;; (defn retry [x]
;;   (println "future available and the answer is:")
;;   )
;; (defn when-done [future-to-watch function-to-call]
;;           (future (function-to-call @future-to-watch)))

;;          (let [f (future (do (println "started ")(Thread/sleep 2000)) 42)]
;;             (when-done f #(retry %)))
