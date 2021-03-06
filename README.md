# clj-torrent

[![Clojars Project](https://img.shields.io/clojars/v/clj-bencode.svg)](https://clojars.org/clj-bencode)
[![Travis](https://img.shields.io/travis/Cantido/clj-bencode.svg)]()

A Clojure library for BitTorrent bencoding.

## Table of Contents

1. [Usage](#usage)
   1. [Encoding data](#encoding-data)
      1. [Examples](#encoding-examples)
   1. [Decoding data](#decoding-data)
      1. [Examples](#decoding-examples)
1. [License](#license)

## Usage

Clj-torrent's tests require Clojure v1.7 or later,
so it has only been tested back to that version.
If you want to be a pioneer, you can try using it on earlier versions.

Add the following line to your leiningen dependencies:

```clojure
[clj-bencode "4.0.0"]
```

Require clj-bencode in your namespace header:

```clojure
(ns example.core
  (:require [clj-bencode.core :as b]))
```
### Encoding data

The `encode` function accepts a Clojure data structure and returns a b-encoded byte array.
Strings are encoded into UTF-8.
Note that a string containing non-ASCII characters cannot be encoded and then decoded without a custom step,
because this library does not assume non-ASCII bytes are characters in a string.

#### Encoding examples

```clojure
(require '[clj-bencode.core :as b])
=> nil

;; Encode returns a byte-array
(b/encode "hi!")
=> #object["[B" 0x23cca7d5 "[B@23cca7d5"]

;; To see the byte values, convert it to a seq
(seq (b/encode "hi!"))
=> (51 58 104 105 33)

;; java.lang.String has a constructor that accepts a byte-array, so you can
;; do this conversion if you don't have to worry about string encodings.
(String. (b/encode "hi!"))
=> "3:hi!"

;; Floating-point values are truncated
(String. (b/encode 1.2))
=> "i1e"

;; Map keywords become normal strings
(String. (b/encode {:keyword 9000}))
=> "d7:keywordi9000ee"

;; Since encoding always yields a byte-array, you can encode arbitrary binary data
(seq (b/encode (byte-array [0x01])))
=> (49 58 1)

;; Converting to a string in this case can be a mistake
(String. (b/encode (byte-array [0x01])))
=> "1:□"
```

### Decoding data

The `decode` function accepts any argument that can satisfy `clojure.java.io/IOFactory`, and tries to extract an input stream from it.
This means that byte-arrays or files are the ideal arguments.

To decode a string, it needs to be converted to a byte-array via `.getBytes` or `(map byte (seq "foo"))` depending on your use-case.
Handling string encodings are beyond the scope of this library, because contents of a byte-string are specific to each use-case.
The standard encoding for strings is UTF-8, however string fields are also used to store binary data.

Data is decoded from the b-encoding format into one of the following clojure types:

| bencode form | Clojure type |
|----|----|
| integer      | bigint |
| string       | string or byte-array<sup>1</sup> |
| list         | seq |
| dictionary   | map<sup>2</sup> |

<sup>1</sup> A string is first decoded into a byte array.
If the array consists only of characters in the ASCII range, then it is formed into a string.
Otherwise, the byte array we read is left untouched.

<sup>2</sup> Map keys are not made into keywords, so a decoded map will have string keys, not keyword keys.
Note that given <sup>1</sup> that this could result in a byte-array map key if your map keys contain non-ASCII characters.

#### Decoding examples

```clojure
(require '[clj-bencode.core :as b])
=> nil

;; Strings need to be explicitly converted into byte arrays before decoding,
;; since clojure.java.io/input-stream assumes a string means a file name or URL.
(b/decode "hi!")
FileNotFoundException hi! (No such file or directory)  java.io.FileInputStream.open0 (FileInputStream.java:-2)

(b/decode (.getBytes "3:hi!"))
=> "hi!"

;; Map keys are always strings, even if you encoded a keyword.
;; String-to-keyword conversion is lossy (since we need to encode spaces into
;; hyphens, but what about decoding?) so this library avoids that.
(b/decode (.getBytes "d8:cow says3:mooe"))
=> {"cow says" "moo"}
```


## License

Copyright © 2019 Rosa Richter

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the Software
is furnished to do so, subject to the following conditions:


* The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

* The software may not be used by individuals, corporations, governments, or
other groups for systems or activities that actively and knowingly endanger,
harm, or otherwise threaten the physical, mental, economic, or general
well-being of underprivileged individuals or groups.


THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.

This license is derived from the MIT License, as amended to limit the impact
of the unethical use of open source software.

