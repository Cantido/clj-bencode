(ns clj-bencode.core
  (:import (java.util Collection Map)
           (java.nio.charset StandardCharsets)
           (clojure.lang MapEntry Keyword)
           (java.nio ByteBuffer)))

(defn- to-utf8 [& more]
  (byte-array (.array (.encode StandardCharsets/UTF_8 (apply str more)))))

(defn- from-utf8 [^ByteBuffer x]
  (str (.decode StandardCharsets/UTF_8 x)))

(defmulti represent class :default String)

(defn- represent-collection [coll]
  (if (seq coll)
    (concat (seq "l")
            (mapcat represent coll)
            (seq "e"))
    "le"))


(defn- reduce-map-represent [init k v]
  (concat (seq init)
          (represent k)
          (represent v)))

(defn- represent-map [m]
  (if (seq m)
    (concat (seq "d")
            (reduce-kv reduce-map-represent [] m)
            (seq "e"))
    "de"))

(defn- represent-string [s]
  (concat [(count s) ":"]
          (seq s)))

(defmethod represent Long [x] ["i" x "e"])
(defmethod represent String [x] (represent-string x))
(defmethod represent Keyword [x] (represent (name x)))
(defmethod represent Collection [x] (represent-collection x))
(defmethod represent Map [x] (represent-map x))


(defn encode [x]
  (apply to-utf8 (represent x)))


(def fchar (comp char first))
(def chstr (comp str char))

(defn in-char-range? [x] (<= 0 x 255))
(defn is-char? [expected actual]
  (and (some? actual)
       (in-char-range? (int actual))
       (= (char expected) (char actual))))

(defn bytestr ^String [x] (apply str (map char x)))

(defn- numeric? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} (char c))))

(defn- numeric-or-sign? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \-} (char c))))

(defn- split-next-int [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [numbers after-numbers] (split-with numeric-or-sign? after-tag)
        [end-tag after-end] (split-at 1 after-numbers)]
    (assert (is-char? \i (first tag-char)))
    (assert (is-char? \e (first end-tag)))
    [(BigInteger. (bytestr numbers))
     after-end]))

(defn- split-next-string [x]
  (let [[length-bytes after-length] (split-with numeric? x)
        length (BigInteger. (bytestr length-bytes))
        [colon-bytes after-colon] (split-at 1 after-length)
        [str-contents-bytes after-contents] (split-at length after-colon)
        str-contents (from-utf8 (ByteBuffer/wrap (byte-array str-contents-bytes)))]
    (assert (is-char? \: (first colon-bytes)))
    [str-contents
     after-contents]))

(declare extract-list-contents)

(defn- split-next-list [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (is-char? \l (first tag-char)))
    (assert (is-char? \e (first end-tag)))
    [list-contents
     after-end]))

(defn- split-next-dict [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (is-char? \d (first tag-char)) (str "dict start tag was byte " (first tag-char) " instead of d"))
    (assert (is-char? \e (first end-tag)) (str "dict end tag was byte " (first end-tag) " instead of e"))
    [(apply hash-map list-contents)
     after-end]))

(defn- split-next [x]
  (let [next-char (first x)]
    (cond
      (not (in-char-range? next-char)) [[] x]
      (numeric? next-char) (split-next-string x)
      (is-char? \i next-char) (split-next-int x)
      (is-char? \l next-char) (split-next-list x)
      (is-char? \d next-char) (split-next-dict x)
      :else [[] x])))


(defn end-of-list? [x] (or (nil? x)
                           (not (in-char-range? x))
                           (is-char? \e x)))

(defn- extract-list-contents [list-so-far coll]
  (if (end-of-list? (first coll))
    [list-so-far coll]
    (let [[next-list-element rest-of-coll] (split-next coll)
          new-list (concat list-so-far next-list-element)]
      (recur new-list rest-of-coll))))


(defn decode
  "Decodes a b-encoded byte array."
  [x]
  (let [inputarr (bytes x)
        [decoded _] (split-next inputarr)]
    decoded))
