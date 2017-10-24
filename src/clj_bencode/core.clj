(ns clj-bencode.core
  (:require [clojure.java.io :as io])
  (:import (java.util Collection Map)
           (java.nio.charset StandardCharsets)
           (clojure.lang Keyword)
           (java.nio ByteBuffer CharBuffer)
           (java.io InputStream)
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
  (->> more
    (apply str)
    (str) ;; Casts the following .encode call correctly. Makes compiler happy.
    (.encode StandardCharsets/UTF_8)
    (bb-array)))

(defn- stringify
  "Stringifies then byteifies an integer."
  [x]
  (map byte (seq (str x))))

(defn from-utf8
  "Decodes the given byte array into a string, assuming it is UTF-8-encoded."
  ^String [x]
  (->> x
    (byte-buffer)
    (.decode StandardCharsets/UTF_8)
    (cb-array)
    (apply str)))

(defmulti ^:private represent class :default String)

(defn- represent-long [x]
  (byte-array
    (concat (list (byte \i))
            (stringify x)
            (list (byte \e)))))

(defn- represent-collection [coll]
  (byte-array
    (if (seq coll)
      (concat (list (byte \l))
              (mapcat represent coll)
              (list (byte \e)))
      (list (byte \l) (byte \e)))))


(defn- reduce-map-represent [init k v]
  (concat (seq init)
          (represent k)
          (represent v)))

(defn- represent-map [m]
  (byte-array
    (if (seq m)
      (concat (list (byte \d))
              (reduce-kv reduce-map-represent [] m)
              (list (byte \e)))
      (list (byte \d) (byte \e)))))

(defn- represent-string [s]
  (let [encoded-str (to-utf8 s)]
    (byte-array
      (concat (stringify (count encoded-str))
              (list (byte \:))
              (seq encoded-str)))))

(defmethod represent Long [x] (represent-long x))
(defmethod represent String [x] (represent-string x))
(defmethod represent Keyword [x] (represent (name x)))
(defmethod represent Collection [x] (represent-collection x))
(defmethod represent Map [x] (represent-map x))


(defn encode [x]
  (byte-array (represent x)))

(defn- seq-or-nil? [x]
  (or
    (seq? x)
    (nil? x)))

(defn- in-char-range? [x] (<= 0 (int x) 255))
(defn- is-char? [expected actual]
  (and (some? actual)
       (in-char-range? (int actual))
       (= (char expected) (char actual))))

(defn- bytestr
  "Converts a collection of bytes into a string by applying char to each byte."
  ^String [x]
  (apply str (map char x)))

(defn- numeric? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (char c))))

(defn- numeric-or-sign? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \-} (char c))))

(defn- split-next-int
  "Splits the given collection into [(123) (rest-of-coll)]."
  [x]
  {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
  (let [[tag-char after-tag] (split-at 1 x)
        [numbers after-numbers] (split-with numeric-or-sign? after-tag)
        [end-tag after-end] (split-at 1 after-numbers)]
    (assert (is-char? \i (first tag-char)))
    (assert (is-char? \e (first end-tag)))
    [(list (BigInteger. (bytestr numbers)))
     after-end]))

(defn- split-next-string
  "Splits the given collection into [(\"next string\") (rest-of-coll)]."
  [x]
  {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
  (let [[length-bytes after-length] (split-with numeric? x)
        length (BigInteger. (bytestr length-bytes))
        [colon-bytes after-colon] (split-at 1 after-length)
        [str-contents-bytes after-contents] (split-at length after-colon)
        str-contents (from-utf8 (byte-array str-contents-bytes))]
    (assert (is-char? \: (first colon-bytes)))
    ;(println "my string is" str-contents)
    [(list str-contents)
     after-contents]))

(declare extract-list-contents)

(defn- split-next-list
  "Splits the given collection into [([next list]) (rest-of-coll)].
   Note that the first element of the result vector is a seq containing
   the desired list."
  [x]
  {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (is-char? \l (first tag-char)))
    (assert (is-char? \e (first end-tag)))
    [(list (vec list-contents))
     after-end]))

(defn- split-next-dict
  "Splits the next value off of x and interprets it as a map, and returns
   [({\"key\" val}) (rest-of-coll)]."
  [x]
  {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (is-char? \d (first tag-char)) (str "dict start tag was byte " (first tag-char) " instead of d"))
    (assert (is-char? \e (first end-tag)) (str "dict end tag was byte " (first end-tag) " instead of e"))
    [(list (apply hash-map list-contents))
     (seq after-end)]))

(defn- split-next
  "Splits the given collection into [(next-token) (rest-of-coll)]."
  [x]
  {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
  (let [next-char (first x)]
    (cond
      (not (in-char-range? next-char)) [[] x]
      (numeric? next-char) (split-next-string x)
      (is-char? \i next-char) (split-next-int x)
      (is-char? \l next-char) (split-next-list x)
      (is-char? \d next-char) (split-next-dict x)
      :else [(list) (seq x)])))


(defn- end-of-list? [x] (or (nil? x)
                           (not (in-char-range? x))
                           (is-char? \e x)))

(defn- extract-list-contents
  "Splits tokens off of the given collection until the collection runs out
   or we hit an end character 'e'. Returns a vector of
   [(1, 2, 3) (rest-of-coll)], and removes the final 'e' from rest-of-coll."
  ([coll] (extract-list-contents [] coll))
  ([list-so-far coll]
   {:post [(vector? %) (seq-or-nil? (first %)) (seq-or-nil? (second %))]}
   (loop [list-so-far (seq list-so-far)
          coll coll]
     (if (end-of-list? (first coll))
         (do
           [list-so-far (seq coll)])
       (let [[next-list-element rest-of-coll] (split-next coll)
             new-list (concat list-so-far next-list-element)]
         (recur new-list rest-of-coll))))))

(defn decode
  "Decodes b-encoded data.

  Accepts any data type which can satisfy clojure.java.io/IOFactory,
  which includes files, input streams, bytes arrays, and strings."
  [x]
  (-> x
    io/input-stream
    IOUtils/toByteArray
    split-next
    ffirst))
