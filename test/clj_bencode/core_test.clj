(ns clj-bencode.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-bencode.core :as b])
  (:import (java.nio.charset StandardCharsets)))

(defn utf8 [^String x]
  (.encode StandardCharsets/UTF_8 x))

(defn utf8d [x]
  (str (.decode StandardCharsets/UTF_8 x)))

(deftest encode-test
  (testing "encode an integer"
    (is (= (utf8 "i3e") (b/encode 3)))
    (is (= (utf8 "i0e") (b/encode 0)))
    (is (= (utf8 "i-1e") (b/encode -1))))
  (testing "encode a string"
    (is (= (utf8 "1:a") (b/encode "a")))
    (is (= (utf8 "3:foo") (b/encode "foo"))))
  (testing "encode a list of integers"
    (is (= (utf8 "li1ei2ei3ee") (b/encode [1 2 3]))))
  (testing "encode a list of strings"
    (is (= (utf8 "l1:a1:b1:ce") (b/encode ["a" "b" "c"])))
    (is (= (utf8 "l1:00:e") (b/encode ["0" ""])))
    (testing "including characters above U+FFFF"
      (is (= (utf8 "2:𐐷") (b/encode "𐐷")))
      (is (= (utf8 "4:𐐷bc") (b/encode "𐐷bc")))
      (is (= (utf8 "4:a𐐷c") (b/encode "a𐐷c")))
      (is (= (utf8 "4:ab𐐷") (b/encode "ab𐐷")))))
  (testing "encode a dict"
    (testing "that is empty"
      (is (= "de") (utf8d (b/encode {}))))
    (testing "that contains string keys and values"
      (is (= "d3:cow3:mooe" (utf8d (b/encode {:cow "moo"}))))
      (is (= "d3:cow3:moo4:spam4:eggse" (utf8d (b/encode {:cow "moo" :spam "eggs"})))))))


(deftest decode-test
  (testing "decode an integer"
    (is (= 1 (b/decode (utf8 "i1e"))))
    (is (= 10 (b/decode (utf8 "i10e"))))
    (is (= -10 (b/decode (utf8 "i-10e")))))
  (testing "decode a string"
    (is (= "foo" (b/decode (utf8 "3:foo"))))
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
    (is (= {:cow "moo" :spam "eggs"} (b/decode (utf8 "d3:cow3:moo4:spam4:eggse"))))))

(def gen-primitives (gen/one-of [gen/int gen/string]))
(def gen-list (gen/list gen-primitives))
(def gen-dict (gen/map gen/keyword gen-primitives))
(def gen-leaf (gen/one-of [gen-primitives gen-list gen-dict]))

;; Just to make an editor shut up about these defspec's
(declare
  x
  encode-ints encode-strings
  encode-int-lists encode-string-lists encode-mixed-lists
  encode-string-dicts encode-int-dicts encode-mixed-dicts
  encode-mixed-nested-dicts encode-mixed-nested-lists)

(defspec encode-ints 1000
         (prop/for-all [x gen/int]
                       (= x (-> x b/encode b/decode))))

(defspec encode-strings 1000
         (prop/for-all [x gen/string]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-lists
         (prop/for-all [x (gen/list gen/int)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-lists
         (prop/for-all [x (gen/list gen/string)]
                       (= x (-> x b/encode b/decode))))

;; the complexity of mixed collections balloons rapidly,
;; so setting this much higher than 50 tests will
;; take way too long
(defspec encode-mixed-lists 50
         (prop/for-all [x (gen/list gen-leaf)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-string-dicts
         (prop/for-all [x (gen/map gen/keyword gen/string)]
                       (= x (-> x b/encode b/decode))))

(defspec encode-int-dicts
         (prop/for-all [x (gen/map gen/keyword gen/int)]
                       (= x (-> x b/encode b/decode))))

;; the complexity of mixed collections balloons rapidly,
;; so setting this much higher than 50 tests will
;; take way too long
(defspec encode-mixed-dicts 50
         (prop/for-all [x (gen/map gen/keyword gen-leaf)]
                       (= x (-> x b/encode b/decode))))
