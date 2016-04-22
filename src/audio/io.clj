(ns audio.io
  (require [clojure.java.io :refer [file output-stream input-stream]])
  (:import
    [java.io InputStream OutputStream]
    [java.nio ByteBuffer IntBuffer CharBuffer ShortBuffer LongBuffer]
    [javax.sound.sampled AudioFormat AudioFileFormat AudioFormat$Encoding
                                AudioFileFormat$Type AudioInputStream AudioSystem]
    [java.io ByteArrayInputStream]
           [java.nio ByteBuffer]
           (java.io RandomAccessFile FileInputStream FileOutputStream )

            ))


(def dir "/Users/michal/Desktop/bounces/")

(defn short-double
  "return value in range -1 to +1 as in matlab"
  [^Short sht]
  (double (/ sht 0x8000)))

(defn double-short [^double dbl]
  (short (* dbl 0x8000)))

(defn mult-ceiling [x ^double y]
  (let [result (* x y)]
    (cond (> result 0.9999999) 0.98
          (< result -0.9999999) -0.98
          :else result)))

(defn read-wav-data
  "loads wav file fname from sample and given length"
  ([fname length] (read-wav-data fname 0 length))
  ([fname from length]
  (let [file (java.io.RandomAccessFile. fname "r")
      channel (.getChannel file)
      fbuffer (.map channel java.nio.channels.FileChannel$MapMode/READ_ONLY (+ 44 (* from 4)) (+ 44 (* length 4) (* from 4)))
       _ (.order fbuffer java.nio.ByteOrder/LITTLE_ENDIAN)
        shortbuffer (.asShortBuffer fbuffer)
        _ (.close channel)
    Lseq  (for [x (range 0 (* length 2) 2)]
                   (.get shortbuffer x))
     Rseq  (for [x (range 1 (* length 2) 2)]
                   (.get shortbuffer x))]
     {:left (map short-double Lseq) :right (map short-double Rseq)})))


(defn interlave-samples [samples]
  (interleave (:left samples) (:right samples)))

(defn write-wav [fname samples fs bps channels]
  (let [header [18770 17990 8838 604 16727 17750 28006 8308 bps 0 1 channels fs 0 45328 2 4 16 24932 24948 9999 9999] ;last 4 bytes will be replaced later
        header-and-data (concat  header samples)
        size (* (count samples) (/ bps 8))
        total-size (+ 44 size) ; header is 44 bytes long
        file (java.io.RandomAccessFile. fname, "rw")
        channel (.getChannel file)
        buffer (.map channel java.nio.channels.FileChannel$MapMode/READ_WRITE 0 total-size)
    _ (.order buffer java.nio.ByteOrder/LITTLE_ENDIAN)
         fbuffer (.asShortBuffer buffer)]
    (.put fbuffer (short-array header-and-data) 0 (/ total-size 2))
    (.putLong buffer  40 size)
         (.close channel)))


(time (def audioData (read-wav-data  (str dir "01L.wav") 0 98784)))


;; (def fff (file (str dir "286.wav")))

(defn fill-data-buffer!
  [^ByteBuffer b-data data sample-bytes]
  (loop [idx 0
         data data]
    (when-not (empty? data)
      (let [b-idx (* sample-bytes idx)]
        (.putShort b-data b-idx (short (* (first data) Short/MAX_VALUE))))
      (recur (inc idx) (rest data))))

  b-data)

(defn *ceiling [^double x]
    (cond (> x 0.9999999) 0.999
          (< x -0.9999999) -0.999
          :else x))

(defn celing [xs]
  (map *ceiling xs))

(defn gain [^doubles xs gain]
  (map #(mult-ceiling gain %) xs))

(defn write-wav-data
  "Writes contents of data to a new wav file with path. Adds frame-rate and
  n-channels as file metadata for appropriate playback/consumption of the new
  audio file."
  ([samples path] (write-wav-data samples path 44100 2))
  ([samples path frame-rate n-channels]
  (let [interlaved (interlave-samples samples)
        data (map *ceiling interlaved)
        frame-rate   (float frame-rate)
        n-channels   (int n-channels)
        sample-bytes (/ Short/SIZE 8)
        frame-bytes  (* sample-bytes n-channels)
        a-format     (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   frame-rate
                                   Short/SIZE
                                   n-channels
                                   frame-bytes
                                   frame-rate
                                   true)
        data-size    (count data)
        n-bytes      (* data-size sample-bytes)
        b-data       (ByteBuffer/allocate n-bytes)
        b-data       (fill-data-buffer! b-data data sample-bytes)
        stream       (AudioInputStream. (ByteArrayInputStream. (.array b-data))
                                        a-format
                                        data-size)
        f            (file path)
        f-type       AudioFileFormat$Type/WAVE]
    (AudioSystem/write stream f-type f))))

(defn wave-header [fname]
  (let [file (java.io.RandomAccessFile. fname "r")
      channel (.getChannel file)
      fileSize (.size channel)
      fbuffer (.map channel java.nio.channels.FileChannel$MapMode/READ_ONLY 0 44)
      _ (.order fbuffer java.nio.ByteOrder/LITTLE_ENDIAN)
        _ (.close channel)
        fs (.getInt fbuffer 24)
        size (.getInt fbuffer 40)
        channels (.getShort fbuffer 22)
        bps (.getShort fbuffer 34)
        samples (int (* (/ size bps channels) 8))]
  [fs samples channels bps fileSize]))

(defn fill-data-buffer!
  [^ByteBuffer b-data data sample-bytes]
  (loop [idx 0
         data data]
    (when-not (empty? data)
      (let [b-idx (* sample-bytes idx)]
        (.putShort b-data b-idx (short (* (first data) Short/MAX_VALUE))))
      (recur (inc idx) (rest data))))

  b-data)


(defn play-seq
  "Writes contents of data to a new wav file with path. Adds frame-rate and
  n-channels as file metadata for appropriate playback/consumption of the new
  audio file."
  ([inputData] (play-seq inputData 44100 2))
  ([inputData frame-rate n-channels]
  (let [data (-> (interlave-samples inputData) celing)
        frame-rate   (float frame-rate)
        n-channels   (int n-channels)
        sample-bytes (/ Short/SIZE 8)
        frame-bytes  (* sample-bytes n-channels)
        a-format     (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   frame-rate
                                   Short/SIZE
                                   n-channels
                                   frame-bytes
                                   frame-rate
                                   true)
        data-size    (count data)
        n-bytes      (* data-size sample-bytes)
        b-data       (ByteBuffer/allocate n-bytes)
        b-data       (fill-data-buffer! b-data data sample-bytes)
        stream       (AudioInputStream. (ByteArrayInputStream. (.array b-data))
                                        a-format
                                        data-size)
        clip         (AudioSystem/getClip)]
    (.open clip stream)
     (.start clip))))

(defn play-mono-seq
  "plays one channel at fs 44100"
  ([input] (play-mono-seq input 1))
  ([input level]
  (play-seq {:left (gain input level) :right (gain input level)})))

(defn toString
  "helper to writeSamplesToText"
  [data]
  (map str data))

(defn writeSamplesToText
  "write samples as text each line double sample"
  [fname samples]
  (with-open [w (clojure.java.io/writer fname)]
  (doseq [line (toString samples)]
    (.write w line)
    (.newLine w))))

(defn getDataFromText
  "parse doubles from text fromated file"
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (->> (reduce  conj [] (line-seq rdr))
         (map read-string)
         )))
