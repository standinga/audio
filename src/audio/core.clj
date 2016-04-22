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
(def wnoise (io/read-wav-data  "/Users/michal/wnoise.wav" 0 4410000))
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


(deftype Coefficients [^Double ai ^Double d1i ^Double d2i])
(deftype Ws [^Double w0 ^Double w1 ^Double w2])

(defn coeffsLP
  "calclulate coefficients for sampling freq s, half power frequency f and order n"
  [^double f ^Integer n ^Integer s]
  (let [a (Math/tan (* (/ f s) Math/PI))
        a2 (* a a)
    coeffs (for [i (range n)]
      (let [r (getR i n)
            s (+ a2 (* 2.0 a r) 1)
            ai (/ a2 s)
            d1i (/ (* 2.0 (- 1.0 a2)) s)
            d2i (* -1.0 (/ (+ (- a2 (* 2.0 a r)) 1.0) s))]
        [ai d1i d2i]))]
    (vec coeffs)))

(type (get (get (coeffsLP 200.0 4 44000) 1) 2))

(defn coeffsHP
  "calclulate coefficients for sampling freq s, half power frequency f and order n"
  [^double f ^Integer n ^Integer s]
  (let [a (Math/tan (* (/ f s) Math/PI))
        a2 (* a a)
    coeffs (for [i (range n)]
      (let [r (getR i n)
            s (+ a2 (* 2.0 a r) 1)
            ai (/ 1.0 s) ; this is different from coeffsLP
            d1i (/ (* 2.0 (- 1.0 a2)) s)
            d2i (* -1.0 (/ (+ (- a2 (* 2.0 a r)) 1.0) s))]
        [ai d1i d2i]))]
    (vec coeffs)))

(defn coeffsBP
  "calclulate coefficients for sampling freq s, half power upper frequency f1, half power lower frequency f2 and order n"
  [^double f1 ^double f2 ^Integer n ^Integer s]
  (let [
        PIdivS (/ Math/PI s)
        a (/ (Math/cos (* PIdivS (+ f1 f2)))
        (Math/cos (* PIdivS (- f1 f2) )))
        ;a = cos(M_PI*(f1+f2)/s)/cos(M_PI*(f1-f2)/s);
        a2 (* a a)
        b (Math/tan (* PIdivS (- f1 f2)))
        b2 (* b b)
    coeffs (for [i (range n)]
      (let [r (getR i n)
            br (* b r)
            s (+ b2 (* 2.0 br) 1)
            ai (/ b2 s)
            d1i (/ (* 4.0 a (+ 1.0 br)) s)
            d2i (/ (* 2.0 (- b2 (* 2.0 a2) 1.0)) s)
            d3i (/ (* 4.0 a (- 1.0 br)) s)
            d4i (/ (- (+ (- b2 (* 2.0 br)) 1)) s)
            ]
        [ai d1i d2i d3i d4i]))]
    (vec coeffs)))

(defn getBPx
  "calculates filter transform over one sample x"
  [x coeffs w0i w1i w2i w3i w4i i n]
    (if (= i n) [x w0i w1i w2i w3i w4i]
      (let [[a d1 d2 d3 d4] (get coeffs i)
             w0 (get w0i i)
             w1 (get w1i i)
             w2 (get w2i i)
             w3 (get w3i i)
            w4 (get w4i i)
            w0new (+ (* d1 w1) (* d2 w2) (* d3 w3) (* d4 w4) x)
            w0i (assoc w0i i w0new)
            x (* a (+ (- w0new (* 2.0 w2)) w4))
            w4new w3
            w4i (assoc w4i i w4new)
            w3new w2
            w3i (assoc w3i i w3new)
            w2new w1
            w2i (assoc w2i i w2new)
           w1new w0new
            w1i (assoc w1i i w0new)
            ]
        (recur x coeffs w0i w1i w2i w3i w4i(inc i) n))))



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


(defn getHPx
  "calculates filter transform over one sample x"
  [x coeffs w0i w1i w2i i n]
    (if (= i n) [x w0i w1i w2i]
      (let [[^double a ^double d1 ^double d2] (get coeffs i)
             w0 (get w0i i)
             w1 (get w1i i)
             w2 (get w2i i)
            w0new (+ (* d1 w1) (* d2 w2) x)
            w0i (assoc w0i i w0new)
            x (* a (+ (- w0new (* 2.0 w1)) w2)) ; different from getLPx
            w2new w1
            w2i (assoc w2i i w2new)
           w1new w0new
            w1i (assoc w1i i w0new)
            ]
        (recur x coeffs w0i w1i w2i (inc i) n))))



(defn generateWs [n ws]
  (vec (for [x (range ws)] (vec (for [x (range n)] 0.0)))))

(defn butterLP_old
  "butterworth filter f - filter freq, n - order, s - sampling frequency"
  ([^doubles samples ^double f ^Integer n] (butterLP samples f n 44100))
  ([^doubles samples ^double f ^Integer n ^Integer s]
  (let [n (/ n 2)
        coefficients (coeffsLP f n s)
        [^doubles initW0 ^doubles initW1 ^doubles initW2] (generateWs n 3)
        ]
    (time (loop [xs samples acc [] w0 initW0 w1 initW1 w2 initW2 n n]
      (if (empty? xs) acc
        (let [
              [^double x ^doubles w0 ^doubles w1 ^doubles w2] (getLPx (first xs) coefficients w0 w1 w2 0 n)
              ]
          (recur (rest xs) (conj acc x) w0 w1 w2 n))))))))

(defn butterLP_1
  "butterworth filter f - filter freq, n - order, s - sampling frequency"
  ([^doubles samples ^double f ^Integer n] (butterLP samples f n 44100))
  ([^doubles samples ^double f ^Integer n ^Integer s]
  (let [n (/ n 2)
        coefficients (coeffsLP f n s)
        [^doubles initW0 ^doubles initW1 ^doubles initW2] (generateWs n 3)
        ]
   (time (loop [xs samples acc (transient []) w0 initW0 w1 initW1 w2 initW2 n n]
      (if (empty? xs) (persistent! acc)
        (let [
              [^double x ^doubles w0 ^doubles w1 ^doubles w2] (getLPx (first xs) coefficients w0 w1 w2 0 n)
              ]
          (recur (rest xs) (conj! acc x) w0 w1 w2 n))))))))

(defn butterLP
  "butterworth filter f - filter freq, n - order, s - sampling frequency"
  ([^doubles samples ^double f ^Integer n] (butterLP samples f n 44100))
  ([^doubles samples ^double f ^Integer n ^Integer s]
  (let [n (/ n 2)
        coefficients (coeffsLP f n s)
        [^doubles initW0 ^doubles initW1 ^doubles initW2] (generateWs n 3)
        ]
   (time (loop [xs samples acc (transient []) w0 initW0 w1 initW1 w2 initW2 n n]
      (if (empty? xs) (persistent! acc)
        (let [
              [^double x ^doubles w0 ^doubles w1 ^doubles w2] (getLPx (first xs) coefficients w0 w1 w2 0 n)
              ]
          (recur (rest xs) (conj! acc x) w0 w1 w2 n))))))))





(defn butterBP
  "butterworth filter f1 - upper filter freq,f2 - lower filter freq, n - order, s - sampling frequency"
  ([samples f1 f2 n] (butterBP samples f1 f2 n 44100))
  ([samples f1 f2 n s]
  (let [n (/ n 2)
        coefficients (coeffsBP f1 f2 n s)
        [initW0 initW1 initW2 initW3 initW4] (generateWs n 5)
        ]
    (loop [xs samples acc [] w0 initW0 w1 initW1 w2 initW2 w3 initW3 w4 initW4 n n]
      (if (empty? xs) acc
        (let [
              [x w0 w1 w2 w3 w4] (getBPx (first xs) coefficients w0 w1 w2 w3 w4 0 n)
              ]
          (recur (rest xs) (conj acc x) w0 w1 w2 w3 w4 n)))))))




(defn butterHP
  "butterworth filter f - filter freq, n - order, s - sampling frequency"
  ([^doubles samples ^double f ^Integer n] (butterHP samples f n 44100))
  ([^doubles samples ^double f ^Integer n ^Integer s]
  (let [n (/ n 2)
        coefficients (coeffsHP f n s) ; different from butterLP
        [^doubles initW0 ^doubles initW1 ^doubles initW2] (generateWs n 3)
        ]
    (loop [xs samples acc [] w0 initW0 w1 initW1 w2 initW2 n n]
      (if (empty? xs) acc
        (let [
              [^double x ^doubles w0 ^doubles w1 ^doubles w2] (getHPx (first xs) coefficients w0 w1 w2 0 n) ;; different from butterLP
              ]
          (recur (rest xs) (conj acc x) w0 w1 w2 n)))))))

(io/play-mono-seq (butterBP l 2800 1000 4) 0.8)
(butterLP lw 400 16 44100)
(butterHP (io/getDataFromText "/Users/michal/sines2.txt") 200 4 44100)
(io/writeSamplesToText lw "/Users/michal/lw.txt")

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
(io/play-mono-seq (bpf 44100 120 1 l) 8)
(io/play-mono-seq (butterHP l 400 16) 0.8)
(io/play-mono-seq (butterLP l 400 16) 0.8)
;; (io/play-mono-seq (io/getDataFromText "/Users/michal/sines2a.txt"))
;; (apply max (butterLP l 1000 14 44100))
;; (time (butterLP lw 1000 4))

;; (io/writeSamplesToText l "/Users/michal/l.txt")

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

;; (time (:left (LP1 wnoise 1000 4 44100)))

(defn LP2 [stereoSamples f n s]
  (let [l (:left stereoSamples)
        r (:right stereoSamples)
        lrf (pvalues (butterLP l f n s) (butterLP r f n s)) ]

    {:left (first lrf) , :right (second lrf)}
    ))
;; (io/play-seq (LP2 (io/read-wav-data "/Users/michal/Desktop/_refs2015/126 don diablo adair.wav" 88100) 200 16 44100))
(io/write-wav-data (LP2 wnoise 200 16 44100) "/Users/michal/wno200.wav")

(io/writeSamplesToText (io/read-wav-data "/Users/michal/Desktop/_refs2015/126 don diablo adair.wav" 44100) "/Users/michal/ddiablo.txt")
(io/writeSamplesToText (io/read-wav-data "/Users/michal/wnoise.wav" 882000) "/Users/michal/noise.txt")


;; (:left (io/read-wav-data "/Users/michal/Desktop/_refs2015/126 don diablo adair.wav" 881))
;; (io/play-mono-seq (io/getDataFromText "/Users/michal/sines2a.txt"))
(io/write-wav-data (io/getDataFromText "/Users/michal/sines2a .txt") "/Users/michal/cwno200.wav")


