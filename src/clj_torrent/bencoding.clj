(ns clj-torrent.bencoding
  (:require [instaparse.core :as insta])
  (:require [instaparse.transform :as transform])
  (:import (java.util Collection)
           (java.nio.charset StandardCharsets)
           (java.nio ByteBuffer)))

(defn to-utf8 [^String x]
  (.encode StandardCharsets/UTF_8 x))

(defn from-utf8 [^String x]
  (str (.decode StandardCharsets/UTF_8 x)))

(defmulti represent class)

(defmethod represent Long [x] (str "i" x "e"))
(defmethod represent String [x] (str (count x) ":" x))
(defmethod represent Collection [x] (str "l" (apply str (map represent x)) "e"))

(defn encode [x]
  (to-utf8 (represent x)))

(defn- numeric? [c]
  (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0} c))

(defn- numeric-or-sign? [c]
  (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \-} c))

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
     after-contents]))

(defn- split-next [x]
  (cond
    (= \i (first x)) (split-next-int x)
    (numeric? (first x)) (split-next-string x)
    (= \l (first x)) (split-next-list x)))

(defn- extract-list-contents [coll s]
  (if (not= \e (first s))
    (let [[next-token rest-of-coll] (split-next s)
          new-coll (conj (vec coll) next-token)]
      (recur new-coll rest-of-coll))
    [coll s]))

(defn decode [x]
  (first (split-next (from-utf8 x))))



