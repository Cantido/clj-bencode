(ns clj-bencode.core
  (:require [clojure.java.io :as io])
  (:import (java.util Collection Map)
           (java.nio.charset StandardCharsets)
           (clojure.lang Keyword)
           (java.nio ByteBuffer CharBuffer)
           (java.io InputStream Reader ByteArrayOutputStream)
           (org.apache.commons.io IOUtils)))

(def ^:private byte-array-class (class (byte-array 1)))

(defmulti ^:private  byte-buffer
  "Coerces the arg into a java.nio.ByteBuffer"
  class)

(defmethod byte-buffer ByteBuffer [x] x)
(defmethod byte-buffer byte-array-class [x] (ByteBuffer/wrap x))

(defn- bb-array
  "Extracts the contents of the byte buffer, truncating unused buffer space.
   Hides gross mutability."
  [^ByteBuffer b]
  (let [arr (byte-array (.limit b))]
    (.get b arr)
    arr))

(defn- cb-array
  [^CharBuffer b]
  (let [arr (char-array (.limit b))]
    (.get b arr)
    arr))

(defn to-utf8
  "Encodes a string into a UTF-8 byte array."
  [& more]
  (.getBytes (apply str more) StandardCharsets/UTF_8))

(defn- stringify
  "Stringifies then byteifies an integer."
  [x]
  (byte-array (map byte (seq (str x)))))


(defn from-utf8
  "Decodes the given byte array into a string, assuming it is UTF-8-encoded."
  ^String [x]
  (String. (bytes x) StandardCharsets/UTF_8))

(defmulti ^:private ^ByteArrayOutputStream represent class :default String)

(defn- represent-long
  ^ByteArrayOutputStream [x]
  (doto (ByteArrayOutputStream.)
    (.write (byte \i))
    (.write (byte-array (map byte (seq (str x)))))
    (.write (byte \e))))

(defn represent-seq
  ^ByteArrayOutputStream [s]
  (doto (ByteArrayOutputStream.)
    (.write (byte-array (mapcat (comp #(.toByteArray %) represent) s)))))

(defn- represent-collection
  ^ByteArrayOutputStream [coll]
  (doto (ByteArrayOutputStream.)
    (.write (byte \l))
    (.write (.toByteArray (represent-seq coll)))
    (.write (byte \e))))


(defn- represent-map ^ByteArrayOutputStream [m]
  (doto (ByteArrayOutputStream.)
    (.write (byte \d))
    (.write (.toByteArray (represent-seq (apply concat (sort (seq m))))))
    (.write (byte \e))))

(defn- represent-string
  ^ByteArrayOutputStream [s]
  (let [encoded-str (.getBytes (str s) StandardCharsets/UTF_8)]
    (doto (ByteArrayOutputStream.)
      (.write (bytes (stringify (count encoded-str))))
      (.write (byte \:))
      (.write (bytes encoded-str)))))

(defmethod represent Number [x] (represent-long x))
(defmethod represent String [x] (represent-string x))
(defmethod represent Keyword [x] (represent (name x)))
(defmethod represent Collection [x] (represent-collection x))
(defmethod represent Map [x] (represent-map x))


(defn encode [x]
  (.toByteArray (represent x)))



(declare decode-next)

(defn- decode-int [stream delimeter & ch]
  (loop [i (if (nil? ch) (.read stream) (first ch)), result ""]
    (let [c (char i)]
      (if (= c delimeter)
        (BigInteger. result)
        (recur (.read stream) (str result c))))))

(defn- decode-string
  [^InputStream rdr start]
  {:pre [(<= 0 (int start) 255)]}
  (let [length (decode-int rdr \: start)
        buffer (make-array Byte/TYPE length)]
    (.read rdr (bytes buffer))
    (String. (bytes buffer) StandardCharsets/UTF_8)))

(defn- decode-list
  [^InputStream rdr]
  (loop [result []]
    (let [c (char (.read rdr))]
      (if (= c \e)
        result
        (recur (conj result (decode-next rdr (int c))))))))

(defn- decode-dict
  "Splits the next value off of x and interprets it as a map, and returns
   [({\"key\" val}) (rest-of-coll)]."
  [^InputStream rdr]
  (apply hash-map (decode-list rdr)))

(defn- numeric? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (char c))))

(defn decode-next [stream & i]
  (let [indicator (if (nil? i) (.read stream) (first i))]
    (cond
      (numeric? indicator) (decode-string stream indicator)
      (= (char indicator) \i) (decode-int stream \e)
      (= (char indicator) \l) (decode-list stream)
      (= (char indicator) \d) (decode-dict stream))))

(defn decode
  "Decodes b-encoded data.

  Accepts any data type which can satisfy clojure.java.io/IOFactory,
  which includes files, input streams, bytes arrays, and strings."
  [x]
  (-> x io/input-stream decode-next))
