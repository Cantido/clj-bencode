(ns clj-bencode.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-bencode.core :as b]
            [clojure.java.io :as io])
  (:import (java.nio.charset StandardCharsets)
           (java.io File)
           (org.apache.commons.io IOUtils)
           (java.net URL)
           (java.nio ByteBuffer)
           (java.util Map)))

(defn truncated-file ^URL []
  (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent-test"))

(defn full-file ^URL []
  (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent"))

(defn full-file-string ^String []
  (slurp (io/file (full-file))))

(defn full-file-buffer []
  (ByteBuffer/wrap (IOUtils/toByteArray (full-file-string))))

(def torrentstring "d8:announce43:https://torrents.linuxmint.com/announce.php10:created by25:Transmission/2.84 (14307)13:creation datei1499021259e8:encoding5:UTF-84:infod6:lengthi1676083200e4:name33:linuxmint-18.2-cinnamon-64bit.iso12:piece lengthi1048576e6:pieces1:a7:privatei0eee")


(defn utf8 [^String x]
  (byte-array (.array (.encode StandardCharsets/UTF_8 x))))

(defn utf8d [x]
  (str (.decode StandardCharsets/UTF_8 (ByteBuffer/wrap (byte-array x)))))

(defn strbytes [x] (IOUtils/toByteArray x))
(defn filebytes [^URL x] (IOUtils/toByteArray x))

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
  (let [result (b/decode (filebytes (truncated-file)))]
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
  (let [result (b/decode (filebytes (full-file)))]
    (is (= "https://torrents.linuxmint.com/announce.php" (get result "announce")))
    (is (= "Transmission/2.84 (14307)" (get result  "created by")))
    (is (= 1499021259 (get result  "creation date")))
    (is (= "UTF-8" (get result "encoding")))
    (let [info (get result "info")]
      (is (= 1676083200 (get info "length")))
      (is (= 1048576 (get info "piece length")))
      (is (= "linuxmint-18.2-cinnamon-64bit.iso" (get info "name")))
      ;(is (= "a" (get info "pieces")))f
      (is (= 0 (get info "private"))))))

(deftest encode-test
  (testing "encode an integer"
    (is (= "i3e" (utf8d (b/encode 3))))
    (is (= "i0e" (utf8d (b/encode 0))))
    (is (= "i-1e" (utf8d (b/encode -1)))))
  (testing "encode a string"
    (is (= "1:a" (utf8d (b/encode "a"))))
    (is (= "3:foo" (utf8d (b/encode "foo"))))
    (is (= "7:foo bar" (utf8d (b/encode "foo bar")))))
  (testing "encode a list of integers"
    (is (= "li1ei2ei3ee" (utf8d (b/encode [1 2 3])))))
  (testing "encode a list of strings"
    (is (= "l1:a1:b1:ce" (utf8d (b/encode ["a" "b" "c"]))))
    (is (= "l1:00:e" (utf8d (b/encode ["0" ""]))))
    (testing "including characters above U+FFFF"
      (is (= "2:𐐷" (utf8d (b/encode "𐐷"))))
      (is (= "4:𐐷bc" (utf8d (b/encode "𐐷bc"))))
      (is (= "4:a𐐷c" (utf8d (b/encode "a𐐷c"))))
      (is (= "4:ab𐐷" (utf8d (b/encode "ab𐐷"))))))
  (testing "encode a dict"
    (testing "that is empty"
      (is (= "de" (utf8d (b/encode {})))))
    (testing "that contains string keys and values"
      (is (= "d3:cow3:mooe" (utf8d (b/encode {:cow "moo"}))))
      (is (= "d8:cow says3:mooe" (utf8d (b/encode {"cow says" "moo"}))))
      (is (= "d3:cow3:moo4:spam4:eggse" (utf8d (b/encode {"cow" "moo" "spam" "eggs"})))))))


(deftest decode-test
  (testing "decode an integer"
    (is (= 1 (b/decode (utf8 "i1e"))))
    (is (= 10 (b/decode (utf8 "i10e"))))
    (is (= -10 (b/decode (utf8 "i-10e")))))
  (testing "decode a string"
    (is (= "foo" (b/decode (utf8 "3:foo"))))
    (is (= "foo bar" (b/decode (utf8 "7:foo bar"))))
    (testing "handles unicode characters above U+FFFF"
      (is (= "𐐷" (b/decode (utf8 "2:𐐷"))))
      (is (= "𐐷bc" (b/decode (utf8 "4:𐐷bc"))))
      (is (= "a𐐷c" (b/decode (utf8 "4:a𐐷c"))))
      (is (= "ab𐐷" (b/decode (utf8 "4:ab𐐷"))))))
  (testing "decode a list of integers"
    (is (= [1 2 3] (b/decode (utf8 "li1ei2ei3ee")))))
  (testing "decode a list of strings"
    (is (= ["a" "b" "c"] (b/decode (utf8 "l1:a1:b1:ce"))))
    (is (= ["0" ""] (b/decode (utf8 "l1:00:e")))))
  (testing "decode a mixed list"
    (is (= [{} 0] (b/decode (utf8 "ldei0ee")))))
  (testing "decode a dict"
    (is (= {"cow" "moo" "spam" "eggs"} (b/decode (utf8 "d3:cow3:moo4:spam4:eggse"))))
    (is (= {"cow says" "moo" "spam" "eggs"} (b/decode (utf8 "d8:cow says3:moo4:spam4:eggse"))))))

(def gen-primitives (gen/one-of [gen/int gen/string]))
(def gen-list (gen/list gen-primitives))
(def gen-dict (gen/map gen/string gen-primitives))
(def gen-leaf (gen/one-of [gen-primitives gen-list gen-dict]))

;; Just to make an editor shut up about these defspec's
(declare
  x
  encode-ints encode-strings
  encode-int-lists encode-string-lists encode-mixed-lists
  encode-string-dicts encode-int-dicts encode-mixed-dicts
  encode-shallow-dicts encode-dict-in-dict
  encode-mixed-nested-dicts encode-mixed-nested-lists)

(defspec encode-ints
         (prop/for-all [x gen/int]
                       (= x (-> x b/encode b/decode))))

(defspec encode-strings
         (prop/for-all [x gen/string]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-lists
         (prop/for-all [x (gen/list gen/int)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-lists
         (prop/for-all [x (gen/list gen/string)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-mixed-lists
         (prop/for-all [x (gen/list gen-leaf)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-shallow-dicts
         (prop/for-all [x (gen/map gen/string-alphanumeric (gen/return []))]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-dicts
         (prop/for-all [x (gen/map gen/string gen/string)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-dicts
  (prop/for-all [x (gen/map gen/string gen/int)]
    (= x (-> x b/encode b/decode))))


(defspec encode-dict-in-dict
  (prop/for-all [x (gen/map gen/string (gen/map gen/string gen/int))]
    (= x (-> x b/encode b/decode))))

(defspec encode-mixed-dicts
  (prop/for-all [x (gen/map gen/string (gen/return {"a" 1}))]
    (= x (-> x b/encode b/decode))))
