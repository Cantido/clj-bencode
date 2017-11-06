(ns clj-bencode.core
  "Encodes and decodes between BitTorrent b-encoding and
   Clojure data structures."
  (:require [clojure.java.io :as io])
  (:import (java.util Collection Map)
           (java.nio.charset StandardCharsets)
           (clojure.lang Keyword Ratio Named)
           (java.nio ByteBuffer CharBuffer)
           (java.io InputStream Reader ByteArrayOutputStream)
           (org.apache.commons.io IOUtils)
           (java.util.concurrent.atomic AtomicInteger AtomicLong)
           (java.io InputStream Reader ByteArrayOutputStream IOException)
           (org.apache.commons.io IOUtils)))

(def ^:private byte-array-class (class (byte-array 1)))

(defn- int-base-10 [x]
  (->> x int str seq (map byte) byte-array))

(defprotocol BencodeWriter
  (-write [object ^ByteArrayOutputStream out]
    "Print object to writer out as bencoding"))

(defn- write-bytes [xs ^ByteArrayOutputStream out]
  (.toByteArray
    (doto out
      (.write (bytes (int-base-10 (count xs))))
      (.write (byte \:))
      (.write (bytes xs)))))

(defn- write-string [s ^ByteArrayOutputStream out]
  (write-bytes
    (.getBytes
      (str s)
      StandardCharsets/UTF_8)
    out))

(defn- write-boolean [x ^ByteArrayOutputStream out])

(defn- write-named [x ^ByteArrayOutputStream out]
  (write-string (name x) out))

(defn- write-integer [x ^ByteArrayOutputStream out]
  (.toByteArray
    (doto out
      (.write (byte \i))
      (.write (byte-array (.getBytes (str (bigint x)) StandardCharsets/UTF_8)))
      (.write (byte \e)))))

(defn- write-seq-contents
  "Converts the given sequence into a byte array.
   Not the same as represent-collection; this function does
   not put characters at the start and end of the array."
  [s ^ByteArrayOutputStream  out]
  (.toByteArray
    (doto out
      (.write (byte-array (mapcat #(-write % out) (seq s)))))))

(defn- write-collection
  [xs ^ByteArrayOutputStream out]
  (.write out (byte \l))
  (write-seq-contents (seq xs) out)
  (.write out (byte \e))
  (.toByteArray out))

(defn- write-map [m ^ByteArrayOutputStream out]
  (.write out (byte \d))
  (write-seq-contents (apply concat (sort (seq m))) out)
  (.write out (byte \e))
  (.toByteArray out))



(defn encode
  "Encodes x into a b-encoded byte array.

   Strings are encoded to UTF-8, and byte arrays are stored untouched."
  [x]
  (-write x (ByteArrayOutputStream.)))

(extend Boolean       BencodeWriter {:-write write-boolean})
(extend Byte          BencodeWriter {:-write write-integer})
(extend Short         BencodeWriter {:-write write-integer})
(extend Integer       BencodeWriter {:-write write-integer})
(extend Long          BencodeWriter {:-write write-integer})
(extend Float         BencodeWriter {:-write write-integer})
(extend Double        BencodeWriter {:-write write-integer})
(extend Ratio         BencodeWriter {:-write write-integer})
(extend BigInteger    BencodeWriter {:-write write-integer})
(extend BigDecimal    BencodeWriter {:-write write-integer})
(extend AtomicInteger BencodeWriter {:-write write-integer})
(extend AtomicLong    BencodeWriter {:-write write-integer})

;; Symbols, Keywords, and Strings
(extend Named        BencodeWriter {:-write write-named})
(extend CharSequence BencodeWriter {:-write write-string})

;; Collections
(extend Map          BencodeWriter {:-write write-map})
(extend Collection   BencodeWriter {:-write write-collection})

;; Maybe a Java array, otherwise fail
(extend java.lang.Object       BencodeWriter {:-write write-string})




(declare ^:private decode-next)

(defn- decode-int [^InputStream stream delimeter & ch]
  (loop [i (if (nil? ch) (.read stream) (first ch)), result ""]
    (when (= -1 (int i)) (throw (IOException. "End of input stream reached earlier than expected.")))
    (let [c (char i)]
      (if (= c delimeter)
        (BigInteger. result)
        (recur (.read stream) (str result c))))))

(defn- in-ascii-range? [x] (<= 0 (int x) 127))

(defn- ascii? [xs]
  (every? in-ascii-range? (seq xs)))

(defn- decode-bytes
  [^InputStream rdr start]
  {:pre [(<= 0 (int start) 255)]}
  (let [length (decode-int rdr \: start)
        buffer (bytes (make-array Byte/TYPE length))]
    (.read rdr (bytes buffer))
    (if (ascii? buffer)
      (String. buffer StandardCharsets/US_ASCII)
      buffer)))

(defn- decode-list
  [^InputStream rdr]
  (loop [result []]
    (let [c (char (.read rdr))]
      (if (= c \e)
        result
        (recur (conj result (decode-next rdr (int c))))))))

(defn- decode-dict
  [^InputStream rdr]
  (apply hash-map (decode-list rdr)))

(defn- numeric? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (char c))))

(defn- decode-next [stream & i]
  (let [indicator (if (nil? i) (.read stream) (first i))]
    (cond
      (numeric? indicator) (decode-bytes stream indicator)
      (= (char indicator) \i) (decode-int stream \e)
      (= (char indicator) \l) (decode-list stream)
      (= (char indicator) \d) (decode-dict stream))))

(defn decode
  "Decodes b-encoded data.

  Accepts any data type which can satisfy clojure.java.io/IOFactory,
  which includes files, input streams, bytes arrays, and
  strings (which are assumed to be URIs, then file paths if that won't work)."
  [x]
  (-> x io/input-stream decode-next))
