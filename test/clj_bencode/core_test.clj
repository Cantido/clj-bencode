(ns clj-bencode.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-bencode.core :as b]
            [clojure.java.io :as io])
  (:import (org.apache.commons.io IOUtils)
           (java.net URL)
           (java.nio.charset StandardCharsets)))

(def truncated-file-url (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent-test"))
(def full-file-url (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent"))

(def torrentstring "d8:announce43:https://torrents.linuxmint.com/announce.php10:created by25:Transmission/2.84 (14307)13:creation datei1499021259e8:encoding5:UTF-84:infod6:lengthi1676083200e4:name33:linuxmint-18.2-cinnamon-64bit.iso12:piece lengthi1048576e6:pieces1:a7:privatei0eee")

(defn strbytes [x] (IOUtils/toByteArray (str x)))
(defn filebytes [^URL x] (let [file (io/file x)
                               length (.length file)
                               buf (byte-array length)]
                           (IOUtils/readFully (io/input-stream x) buf)
                           buf))

(deftest decode-real-string-test
  (let [result (b/decode (strbytes torrentstring))]
    (is (= "https://torrents.linuxmint.com/announce.php" (get result "announce")))
    (is (= "Transmission/2.84 (14307)" (get result  "created by")))
    (is (= 1499021259 (get result  "creation date")))
    (is (= "UTF-8" (get result "encoding")))
    (let [info (get result "info")]
      (is (= 1676083200 (get info "length")))
      (is (= 1048576 (get info "piece length")))
      (is (= "linuxmint-18.2-cinnamon-64bit.iso" (get info "name")))
      (is (= "a" (get info "pieces")))
      (is (= 0 (get info "private"))))))

(deftest decode-truncated-file-test
  (let [result (b/decode (filebytes truncated-file-url))]
    (is (= "https://torrents.linuxmint.com/announce.php" (get result "announce")))
    (is (= "Transmission/2.84 (14307)" (get result  "created by")))
    (is (= 1499021259 (get result  "creation date")))
    (is (= "UTF-8" (get result "encoding")))
    (let [info (get result "info")]
      (is (= 1676083200 (get info "length")))
      (is (= 1048576 (get info "piece length")))
      (is (= "linuxmint-18.2-cinnamon-64bit.iso" (get info "name")))
      (is (= "a" (get info "pieces")))
      (is (= 0 (get info "private"))))))

(deftest decode-full-file-test
  (let [result (b/decode (filebytes full-file-url))]
    (is (= "https://torrents.linuxmint.com/announce.php" (get result "announce")))
    (is (= "Transmission/2.84 (14307)" (get result  "created by")))
    (is (= 1499021259 (get result  "creation date")))
    (is (= "UTF-8" (get result "encoding")))
    (let [info (get result "info")]
      (is (= 1676083200 (get info "length")))
      (is (= 1048576 (get info "piece length")))
      (is (= "linuxmint-18.2-cinnamon-64bit.iso" (get info "name")))
      (is (= 0 (get info "private")))
      (let [pieces (get info "pieces")]
        (is (= 31980 (count pieces)))))))


(deftest encode-full-file-test
  (let [file (filebytes full-file-url)
        decoded (b/decode file)
        encoded (b/encode decoded)]
    (is (= 32244
           (count (seq file))
           (count (seq encoded))))))

(deftest encode-test
  (testing "encode"
    (testing "integers"
      (is (= "i3e" (String. (bytes (b/encode 3)))))
      (is (= "i0e" (String. (bytes (b/encode 0)))))
      (is (= "i-1e" (String. (bytes (b/encode -1))))))
    (testing "strings"
      (is (= "1:a" (String. (bytes (b/encode "a")))))
      (is (= "3:foo" (String. (bytes (b/encode "foo")))))
      (is (= "7:foo bar" (String. (bytes (b/encode "foo bar")))))
      (testing "containing characters above U+FFFF"
        ; "\uD801\uDC37")
        (is (= "4:𐐷" (String. (bytes (b/encode "𐐷")))))
        (is (= "6:𐐷bc" (String. (bytes (b/encode "𐐷bc")))))
        (is (= "6:a𐐷c" (String. (bytes (b/encode "a𐐷c")))))
        (is (= "6:ab𐐷" (String. (bytes (b/encode "ab𐐷")))))))
    (testing "lists"
      (testing "of integers"
        (is (= "li1ei2ei3ee" (String. (bytes (b/encode [1 2 3]))))))
      (testing "of strings"
        (is (= "l1:a1:b1:ce" (String. (bytes (b/encode ["a" "b" "c"])))))
        (is (= "l1:00:e" (String. (bytes (b/encode ["0" ""]))))))
      (testing "in edge cases"
        (is (= "le" (String. (bytes (b/encode [])))))
        (is (= "llee" (String. (bytes (b/encode [[]])))))))
    (testing "maps"
      (testing "that are empty"
        (is (= "de" (String. (bytes (b/encode {}))))))
      (testing "that contains string keys and values"
        (is (= "d0:lee" (String. (bytes (b/encode {"" []})))))
        (is (= "d3:cow3:mooe" (String. (bytes (b/encode {:cow "moo"})))))
        (is (= "d8:cow says3:mooe" (String. (bytes (b/encode {"cow says" "moo"})))))
        (is (= "d3:cow3:moo4:spam4:eggse" (String. (bytes (b/encode {"cow" "moo" "spam" "eggs"}))))))
      (testing "should sort their keys"
        (is (= "d1:ai1e1:bi2ee" (String. (bytes (b/encode {"b" 2 "a" 1})))))))))

(defn byte-seq [s]
  (seq (.getBytes (str s))))

(deftest decode-test
  (testing "decode"
    (testing "integers"
      (is (= 1 (b/decode (.getBytes "i1e"))))
      (is (= 10 (b/decode (.getBytes  "i10e"))))
      (is (= -10 (b/decode (.getBytes  "i-10e")))))
    (testing "strings"
      (is (= "foo" (b/decode (.getBytes  "3:foo"))))
      (is (= "foo bar" (b/decode (.getBytes  "7:foo bar"))))
      (testing "including characters above U+FFFF"
        (is (= (byte-seq "𐐷") (seq (b/decode (.getBytes  "4:𐐷")))))
        (is (= (byte-seq "𐐷bc") (seq (b/decode (.getBytes  "6:𐐷bc")))))
        (is (= (byte-seq "a𐐷c") (seq (b/decode (.getBytes  "6:a𐐷c")))))
        (is (= (byte-seq "ab𐐷") (seq (b/decode (.getBytes  "6:ab𐐷")))))))
    (testing "lists"
      (is (= [] (b/decode (.getBytes  "le"))))
      (is (= [1] (b/decode (.getBytes  "li1ee"))))
      (testing "of integers"
        (is (= [1 2 3] (b/decode (.getBytes  "li1ei2ei3ee")))))
      (testing "of strings"
        (is (= ["a" "b" "c"] (b/decode (.getBytes  "l1:a1:b1:ce"))))
        (is (= ["0" ""] (b/decode (.getBytes  "l1:00:e")))))
      (testing "of mixed contents"
        (is (= [{} 0] (b/decode (.getBytes  "ldei0ee"))))))
    (testing "decode a dict"
      (is (= {"" []} (b/decode (.getBytes  "d0:lee"))))
      (is (= {"cow" "moo" "spam" "eggs"} (b/decode (.getBytes  "d3:cow3:moo4:spam4:eggse"))))
      (is (= {"cow says" "moo" "spam" "eggs"} (b/decode (.getBytes  "d8:cow says3:moo4:spam4:eggse")))))))


(def gen-primitives (gen/one-of [gen/int gen/string-ascii]))
(def gen-list (gen/list gen-primitives))
(def gen-dict (gen/map gen/string-ascii gen-primitives))
(def gen-leaf (gen/one-of [gen-primitives gen-list gen-dict]))

;; Just to make an editor shut up about these defspec's
(declare
  x
  encode-ints encode-strings
  encode-int-lists encode-string-lists encode-mixed-lists
  encode-string-dicts encode-int-dicts encode-mixed-dicts
  encode-shallow-dicts encode-dict-in-dict
  encode-mixed-nested-dicts encode-mixed-nested-lists
  unicode-handling)

(defspec encode-ints
         (prop/for-all [x gen/int]
                       (= x (-> x b/encode b/decode))))

(defspec encode-strings
         (prop/for-all [x gen/string-ascii]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-lists
         (prop/for-all [x (gen/list gen/int)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-lists
         (prop/for-all [x (gen/list gen/string-ascii)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-mixed-lists
         (prop/for-all [x (gen/list gen-leaf)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-shallow-dicts
         (prop/for-all [x (gen/map gen/string-ascii (gen/return []))]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-dicts
         (prop/for-all [x (gen/map gen/string-ascii gen/string-ascii)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-dicts
  (prop/for-all [x (gen/map gen/string-ascii gen/int)]
    (= x (-> x b/encode b/decode))))


(defspec encode-dict-in-dict
  (prop/for-all [x (gen/map gen/string-ascii (gen/map gen/string-ascii gen/int))]
    (= x (-> x b/encode b/decode))))

(defspec encode-mixed-dicts
  (prop/for-all [x (gen/map gen/string-ascii (gen/return {"a" 1}))]
    (= x (-> x b/encode b/decode))))
