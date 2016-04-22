(ns audio.core

  (require [clojure.java.io :refer [file output-stream input-stream]]
           [audio.io :as io])
  (:import
   [ java.net URL]
    [java.io ByteArrayInputStream]
    [java.io InputStream OutputStream]
    [javax.sound.sampled AudioFormat AudioFileFormat AudioFormat$Encoding
     Line Clip LineEvent LineListener LineEvent$Type TargetDataLine DataLine$Info
     SourceDataLine  Line$Info
     Port AudioFileFormat$Type AudioInputStream AudioSystem Mixer]
    [java.nio ByteBuffer IntBuffer CharBuffer ShortBuffer LongBuffer]
     (java.io RandomAccessFile FileInputStream FileOutputStream )))


(def dir "/Users/michal/Desktop/bounces/")

(defn short-double
  "return value in range -1 to +1 as in matlab"
  [^Short sht]
  (double (/ sht 0x8000)))

(defn double-short [^double dbl]
  (short (* dbl 0x8000)))

(defn mult-ceiling [^double x ^double y]
  (let [result (* x y)]
    (cond (> result 0.9999999) 0.98
          (< result -0.9999999) -0.98
          :else result)))

(defn gain [^doubles xs ^double gain]
  (map #(mult-ceiling gain %) xs))

(def audioData (io/read-wav-data  (str dir "0000.wav") 0 44100))
(def wnoise (io/read-wav-data  "/Users/michal/wnoise.wav" 0 441000))
(def l (audioData :left))
(def lw (wnoise :left))

(defn sin [x] (Math/sin x))

(defn sinGenerator
  "generates 2channel mono seq {left: xs, right: xs} of sinusoid of given freq and length (ms) fs = 44100"
  ([freq length] (sinGenerator freq length 1))
  ([freq length amplitude]
  (let [fs 44100
        wavelengthInSamples (* 441/10 length)
        pi2 (* 2 (Math/PI))
        cycl (/ fs freq)
        piCycles (/ pi2 cycl)
        xs (map #(* amplitude (Math/sin (*  piCycles %))) (range wavelengthInSamples))]
    {:left xs, :right xs})))


(defn getR
  "helper function uses iterator i and order n"
  [^Integer i ^Integer n]
  (Math/sin (/ (* Math/PI (+ (* 2 i) 1)) (* 4 n))))

(defn coeffsLP
  "calclulate coefficients for sampling freq s, half power frequency f and order n"
  [^double f ^Integer n ^Integer s]
  (let [a (Math/tan (* (/ f s) Math/PI))
        a2 (* a a)
    coeffs (for [i (range n)]
      (let [r (getR i n)
            s (+ a2 (* 2.0 a r) 1)
            ai (/ a2 s)
            d1i (/ (* 2.0 (- 1 a2)) s)
            d2i (* -1.0 (/ (+ (- a2 (* 2.0 a r)) 1) s))]
        [ai d1i d2i]))]
    (vec coeffs)))

(defn getLPx
  "calculates filter transform over one sample x"
  [x coeffs w0i w1i w2i i n]
    (if (= i n) [x w0i w1i w2i]
      (let [[^double a ^double d1 ^double d2] (get coeffs i)
             w0 (get w0i i)
             w1 (get w1i i)
             w2 (get w2i i)
            w0new (+ (* d1 w1) (* d2 w2) x)
            w0i (assoc w0i i w0new)
            x (* a (+ w0new (* 2.0 w1) w2))
            w2new w1
            w2i (assoc w2i i w2new)
           w1new w0new
            w1i (assoc w1i i w0new)
            ]
        (recur x coeffs w0i w1i w2i (inc i) n))))

(defn generateWs [n]
  (vec (for [x (range 3)] (vec (for [x (range n)] 0.0)))))

(defn butterLP
  "butterworth filter f - filter freq, n - order, s - sampling frequency"
  ([^doubles samples ^double f ^Integer n] (butterLP samples f n 44100))
  ([^doubles samples ^double f ^Integer n ^Integer s]
  (let [n (/ n 2)
        coefficients (coeffsLP f n s)
        [^doubles initW0 ^doubles initW1 ^doubles initW2] (generateWs n)
        ]
    (loop [xs samples acc [] w0 initW0 w1 initW1 w2 initW2 n n]
      (if (empty? xs) acc
        (let [
              [^double x ^doubles w0 ^doubles w1 ^doubles w2] (getLPx (first xs) coefficients w0 w1 w2 0 n)
              ]
          (recur (rest xs) (conj acc x) w0 w1 w2 n)))))))

;; (butterLP (io/getDataFromText "/Users/michal/sines2.txt") 200 4 44100)


(defn bpf [s f b samples]
  (let [t (/ (* 2 Math/PI f) s)
        r (- 1 (/ (* 2 Math/PI b) s))
        twoSinT (* 2 (Math/sin t))
        r2 (* r r)
        twoRCosT (* 2 r (Math/cos (* 2 t)))
        A (/ (* (- 1 r) (Math/sqrt (+ (- 1 twoRCosT) r2))) twoSinT)
        t (* 2 r (Math/cos t))
        r (* r r)]
    (loop [xs samples acc [] a0 0 a1 0 a2 0]
      (if (empty? xs) acc
        (let [x (first xs)
              a0 (+ (- (* t a1) (* r a2)) x)
              a2 a1
              a1 a0]
          (recur (rest xs) (conj acc (* A (- a0 a2))) a0 a1 a2))))))
;; (io/play-mono-seq (bpf 44100 120 1 l) 8)
;; (io/play-mono-seq (butterLP l 100 16) 0.8)
;; (io/play-mono-seq (io/getDataFromText "/Users/michal/sines2a.txt"))
;; (apply max (butterLP l 1000 14 44100))
;; (time (butterLP lw 1000 4))

;; (time (do (butterLP (:left wnoise )1000 4) (butterLP (:right wnoise )1000 4)))


;; different concurent solutions:
(defn LP [stereoSamples f n s]
  (let [l (:left stereoSamples)
        r (:right stereoSamples)
        lf (future (butterLP l f n s))
        rf (future (butterLP r f n s))
        lfdone @lf
        rfdone @rf]
    {:left lfdone, :right rfdone}
    ))

(time (:left (LP wnoise 1000 4 44100)))

(defn LP1 [stereoSamples f n s]
  (let [l (:left stereoSamples)
        r (:right stereoSamples)
        lrf (pmap (fn [x] (butterLP x f n s)) [l r])]

    {:left (first lrf) , :right (second lrf)}
    ))

(time (:left (LP1 wnoise 1000 4 44100)))

(defn LP2 [stereoSamples f n s]
  (let [l (:left stereoSamples)
        r (:right stereoSamples)
        lrf (pvalues (butterLP l f n s) (butterLP r f n s)) ]

    {:left (first lrf) , :right (second lrf)}
    ))
(io/play-seq (LP2 (io/read-wav-data "/Users/michal/Desktop/_refs2015/126 don diablo adair.wav" 88100) 200 16 44100))
;; (io/write-wav-data (LP2 wnoise 100 16 44100) "/Users/michal/wno100.wav")
