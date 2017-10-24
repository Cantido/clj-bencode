(ns clj-bencode.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-bencode.core :as b]
            [clojure.java.io :as io])
  (:import (org.apache.commons.io IOUtils)
           (java.net URL)))

(defn truncated-file ^URL []
  (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent-test"))

(defn full-file ^URL []
  (io/resource "linuxmint-18.2-cinnamon-64bit.iso.torrent"))

(def torrentstring "d8:announce43:https://torrents.linuxmint.com/announce.php10:created by25:Transmission/2.84 (14307)13:creation datei1499021259e8:encoding5:UTF-84:infod6:lengthi1676083200e4:name33:linuxmint-18.2-cinnamon-64bit.iso12:piece lengthi1048576e6:pieces1:a7:privatei0eee")

(defn strbytes [x] (IOUtils/toByteArray (str x)))
(defn filebytes [^URL x] (IOUtils/toByteArray x))

(def endec (comp b/decode b/encode))
(def bytestr (comp str char int))

(def weird-str (bytestr 128))

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
  (testing "encode"
    (testing "integers"
      (is (= "i3e" (b/from-utf8 (b/encode 3))))
      (is (= "i0e" (b/from-utf8 (b/encode 0))))
      (is (= "i-1e" (b/from-utf8 (b/encode -1)))))
    (testing "strings"
      (is (= "1:a" (b/from-utf8 (b/encode "a"))))
      (is (= "3:foo" (b/from-utf8 (b/encode "foo"))))
      (is (= "7:foo bar" (b/from-utf8 (b/encode "foo bar"))))
      (is (= (str "2:" weird-str) (b/from-utf8 (b/encode weird-str))))
      (testing "containing characters above U+FFFF"
        (is (= "4:𐐷" (b/from-utf8 (b/encode "𐐷"))))
        (is (= "6:𐐷bc" (b/from-utf8 (b/encode "𐐷bc"))))
        (is (= "6:a𐐷c" (b/from-utf8 (b/encode "a𐐷c"))))
        (is (= "6:ab𐐷" (b/from-utf8 (b/encode "ab𐐷"))))))
    (testing "lists"
      (testing "of integers"
        (is (= "li1ei2ei3ee" (b/from-utf8 (b/encode [1 2 3])))))
      (testing "of strings"
        (is (= "l1:a1:b1:ce" (b/from-utf8 (b/encode ["a" "b" "c"]))))
        (is (= "l1:00:e" (b/from-utf8 (b/encode ["0" ""])))))
      (testing "in edge cases"
        (is (= "le" (b/from-utf8 (b/encode []))))
        (is (= "llee" (b/from-utf8 (b/encode [[]]))))))
    (testing "maps"
      (testing "that are empty"
        (is (= "de" (b/from-utf8 (b/encode {})))))
      (testing "that contains string keys and values"
        (is (= "d0:lee" (b/from-utf8 (b/encode {"" []}))))
        (is (= "d3:cow3:mooe" (b/from-utf8 (b/encode {:cow "moo"}))))
        (is (= "d8:cow says3:mooe" (b/from-utf8 (b/encode {"cow says" "moo"}))))
        (is (= "d3:cow3:moo4:spam4:eggse" (b/from-utf8 (b/encode {"cow" "moo" "spam" "eggs"}))))))))


(deftest decode-test
  (testing "decode"
    (testing "integers"
      (is (= 1 (b/decode (b/to-utf8 "i1e"))))
      (is (= 10 (b/decode (b/to-utf8 "i10e"))))
      (is (= -10 (b/decode (b/to-utf8 "i-10e")))))
    (testing "strings"
      (is (= "foo" (b/decode (b/to-utf8 "3:foo"))))
      (is (= "foo bar" (b/decode (b/to-utf8 "7:foo bar"))))
      (is (=  weird-str (b/decode (b/to-utf8 (str "2:" weird-str)))))
      (testing "including characters above U+FFFF"
        (is (= "𐐷" (b/decode (b/to-utf8 "4:𐐷"))))
        (is (= "𐐷bc" (b/decode (b/to-utf8 "6:𐐷bc"))))
        (is (= "a𐐷c" (b/decode (b/to-utf8 "6:a𐐷c"))))
        (is (= "ab𐐷" (b/decode (b/to-utf8 "6:ab𐐷"))))))
    (testing "lists"
      (is (= [] (b/decode (b/to-utf8 "le"))))
      (testing "of integers"
        (is (= [1 2 3] (b/decode (b/to-utf8 "li1ei2ei3ee")))))
      (testing "of strings"
        (is (= ["a" "b" "c"] (b/decode (b/to-utf8 "l1:a1:b1:ce"))))
        (is (= ["0" ""] (b/decode (b/to-utf8 "l1:00:e")))))
      (testing "of mixed contents"
        (is (= [{} 0] (b/decode (b/to-utf8 "ldei0ee"))))))
    (testing "decode a dict"
      (is (= {"" []} (b/decode (b/to-utf8 "d0:lee"))))
      (is (= {"cow" "moo" "spam" "eggs"} (b/decode (b/to-utf8 "d3:cow3:moo4:spam4:eggse"))))
      (is (= {"cow says" "moo" "spam" "eggs"} (b/decode (b/to-utf8 "d8:cow says3:moo4:spam4:eggse")))))))


(deftest encode-decode-test
  (is (= weird-str (endec weird-str)))
  (is (= {(-> 129 char str) {"a" 1}} (b/decode (b/encode {(-> 129 char str) {"a" 1}})))))

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
  encode-mixed-nested-dicts encode-mixed-nested-lists
  unicode-handling)

(defspec unicode-handling 1000
  (prop/for-all [x gen/string]
    (= x (-> x b/to-utf8 b/from-utf8))))

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


(defspec encode-dict-in-dict 50
  (prop/for-all [x (gen/map gen/string (gen/map gen/string gen/int))]
    (= x (-> x b/encode b/decode))))

(defspec encode-mixed-dicts
  (prop/for-all [x (gen/map gen/string (gen/return {"a" 1}))]
    (= x (-> x b/encode b/decode))))
