(ns audio.try
(:import (javax.sound.sampled AudioSystem DataLine$Info SourceDataLine
                              AudioFormat AudioFormat$Encoding)))

(def popular-format
  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                48000 ; sample rate
                16    ; bits per sample
                2     ; channels
                4     ; frame size 2*16bits [bytes]
                48000 ; frame rate
                false ; little endian
                ))

(defn open-line [audio-format]
  (doto (AudioSystem/getLine (DataLine$Info. SourceDataLine audio-format))
    (.open audio-format)
    (.start)))

(defn sine [sample-rate freq]
  (let [term (/ 1 freq)
        samples (* term sample-rate)
        factor (/ (* 2 Math/PI) samples)]
    (map #(Math/sin (* % factor))
         (range samples))))

(defn amplitude [sample-size]
  (Math/pow 2 (- sample-size 1.1)))

(defn quantize [amplitude value]
  (int (* amplitude value)))

(defn unsigned-byte [x]
  (byte (if (> x 127) (- x 256) x)))

(defn little-endian [size x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range size)))

(defn big-endian [size x]
  (reverse (little-endian size x)))

(defn sine-bytes [format freq]
  (let [{:keys [sampleSizeInBits frameSize
                sampleRate bigEndian]} (bean format)
        sample-size (/ sampleSizeInBits 8)
        ampl (amplitude sampleSizeInBits)
        tear (if bigEndian big-endian little-endian)]
    (->> (sine sampleRate freq)
         (map (partial quantize ampl))
         (map (partial tear sample-size))
         (map cycle)
         (map (partial take frameSize))
         (apply concat)
         byte-array)))

(defn recalc-data [{:keys [line] :as state} freq]
  (assoc state :data (sine-bytes (.getFormat line) freq)))

(defn play-data [{:keys [line data playing] :as state} agent]
  (when (and line data playing)
    (.write line data 0 (count data))
    (send-off agent play-data agent))
  state)

(defn pause [agent]
  (send agent assoc :playing false)
  (doto (:line @agent)
    .stop .flush))

(defn play [agent]
  (.start (:line @agent))
  (send agent assoc :playing true)
  (send-off agent play-data agent))

(defn change-freq [agent freq]
  (doto agent
    pause
    (send recalc-data freq)
    play))

(defn line-agent [line freq]
  (let [agent (agent {:line line})]
    (send agent recalc-data freq)
    (play agent)))

(defn tone-freq [x]
  (-> (Math/pow 2 (/ x 11)) (* 440) (/ 512)))

(def mountain-king
  [[0 1] [2 1] [3 1] [5 1]
   [7 1] [3 1] [7 2]
   [6 1] [2 1] [6 2]
   [5 1] [1 1] [5 2]
   [0 1] [2 1] [3 1] [5 1]
   [7 1] [3 1] [7 1] [12 1]
   [10 1] [7 1] [3 1] [7 1]
   [10 2] [-2 2]])

(defn play-melody [agent base-tone base-duration melody]
  (doseq [[tone duration] melody]
    (change-freq agent (tone-freq (+ base-tone tone)))
    (Thread/sleep (* base-duration duration)))
  (pause agent))

(def a (line-agent (open-line popular-format) 60))
(play-melody a  99 10 mountain-king)
