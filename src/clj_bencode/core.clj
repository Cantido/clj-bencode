(ns clj-bencode.core
  "Encodes and decodes between BitTorrent b-encoding and
   Clojure data structures."
  (:require [clojure.java.io :as io])
  (:import (java.util Collection Map)
           (java.nio.charset StandardCharsets)
           (clojure.lang Keyword)
           (java.nio ByteBuffer CharBuffer)
           (java.io InputStream Reader ByteArrayOutputStream IOException)
           (org.apache.commons.io IOUtils)))

(def ^:private byte-array-class (class (byte-array 1)))

(defn- int-base-10 [x]
  (->> x int str seq (map byte) byte-array))

(declare ^:private represent)

(defn- represent-integer
  "Converts the given integer into a b-encoded byte array."
  [x]
  (.toByteArray
    (doto (ByteArrayOutputStream.)
      (.write (byte \i))
      (.write (byte-array (map byte (seq (str (bigint x))))))
      (.write (byte \e)))))

(defn- represent-seq
  "Converts the given sequence into a byte array.
   Not the same as represent-collection; this function does
   not put characters at the start and end of the array."
  [s]
  (.toByteArray
    (doto (ByteArrayOutputStream.)
      (.write (byte-array (mapcat represent (seq s)))))))

(defn- represent-collection [coll]
  (.toByteArray
    (doto (ByteArrayOutputStream.)
      (.write (byte \l))
      (.write (bytes (represent-seq (seq coll))))
      (.write (byte \e)))))

(defn- represent-map [m]
  (.toByteArray
    (doto (ByteArrayOutputStream.)
      (.write (byte \d))
      (.write (bytes (represent-seq (apply concat (sort (seq m))))))
      (.write (byte \e)))))

(defn- represent-bytes [xs]
  (.toByteArray
    (doto (ByteArrayOutputStream.)
      (.write (bytes (int-base-10 (count xs))))
      (.write (byte \:))
      (.write (bytes xs)))))

(defn- represent-string [s]
  (represent-bytes (.getBytes (str s) StandardCharsets/UTF_8)))

(defmulti ^:private represent class :default String)

(defmethod represent Number [x] (represent-integer x))
(defmethod represent String [x] (represent-string x))
(defmethod represent byte-array-class [x] (represent-bytes x))
(defmethod represent Keyword [x] (represent (name x)))
(defmethod represent Collection [x] (represent-collection x))
(defmethod represent Map [x] (represent-map x))


(defn encode
  "Encodes x into a b-encoded byte array.

   Strings are encoded to UTF-8, and byte arrays are stored untouched."
  [x]
  (represent x))



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
