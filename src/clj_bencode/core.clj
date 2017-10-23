(ns clj-bencode.core
  (:import (java.util Collection Map)
    (java.nio.charset StandardCharsets)
    (clojure.lang MapEntry Keyword)))

(defn- to-utf8 [^String x]
  (.encode StandardCharsets/UTF_8 x))

(defn- from-utf8 [^String x]
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

(defmethod represent Long [x] ["i" x "e"])
(defmethod represent String [x] [(count x) ":" x])
(defmethod represent Keyword [x] (represent (name x)))
(defmethod represent Collection [x] (represent-collection x))
(defmethod represent Map [x] (represent-map x))


(defn encode [x]
  (to-utf8 (apply str (represent x))))

(defn- numeric? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} c)))

(defn- numeric-or-sign? [c]
  (boolean (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \-} c)))

(defn- split-next-int [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [numbers after-numbers] (split-with numeric-or-sign? after-tag)
        [end-tag after-end] (split-at 1 after-numbers)]
    (assert (= \i (first tag-char)))
    (assert (= \e (first end-tag)))
    [(Integer/parseInt (apply str numbers))
     after-end]))

(defn- split-next-string [x]
  (let [[length-str after-length] (split-with numeric? x)
        length (Integer/parseInt (apply str length-str))
        [colon after-colon] (split-at 1 after-length)
        [str-contents after-contents] (split-at length after-colon)]
    (assert (= \: (first colon)))
    [(apply str str-contents)
     after-contents]))

(declare extract-list-contents)

(defn- split-next-list [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (= \l (first tag-char)))
    (assert (= \e (first end-tag)))
    [list-contents
     after-end]))

(defn- split-next-dict [x]
  (let [[tag-char after-tag] (split-at 1 x)
        [list-contents after-contents] (extract-list-contents [] after-tag)
        [end-tag after-end] (split-at 1 after-contents)]
    (assert (= \d (first tag-char)))
    (assert (= \e (first end-tag)))
    [(apply hash-map list-contents)
     after-end]))

(defn- split-next [x]
  (cond
    (numeric? (first x)) (split-next-string x)
    (= \i (first x)) (split-next-int x)
    (= \l (first x)) (split-next-list x)
    (= \d (first x)) (split-next-dict x)))

(defn- extract-list-contents [coll s]
  (if (not= \e (first s))
    (let [[next-token rest-of-coll] (split-next s)
          new-coll (conj (vec coll) next-token)]
      (recur new-coll rest-of-coll))
    [coll s]))

(defn decode [x]
  (first (split-next (from-utf8 x))))
