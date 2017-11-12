# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [4.0.1] - 2017-11-12
### Added
- Added tests under Clojure 1.7 and 1.9, in addition to the current 1.8
- Usage guide added to README

### Changed
- `decode` throws a more useful exception message when the input stream runs out unexpectedly.
- `test.check` and `commons-io` moved to dev dependencies

## [4.0.0] - 2017-10-25
### Changed
- Unicode and byte strings are now supported
  - Decoding
    - If a byte string contains only bytes in the ASCII range, they are decoded into Strings.
      Otherwise, the byte string is returned as a byte-array.
    - Map keys are decoded into strings (or potentially byte-arrays), not keywords
  - Encoding
    - Strings are encoded as UTF-8
    - Byte arrays are encoded untouched

## [3.1.0] - 2017-10-23
### Added
- `decode` now accepts other implementations of `clojure.java.io/IOFactory`,
  including files and URLs. Byte arrays are still supported.

## [3.0.0] - 2017-10-23
### Changed
- Tries to handle Unicode correctly (still doesn't)
- Decoding a map inserts hyphens into keys before making them keywords

## [2.0.0] - 2017-10-22
### Changed
- All functions have moved into `clj-bencoding.core` from `clj-bencoding.bencoding`

## 1.0.0 - 2017-10-22
### Added
- All data types, excluding byte arrays, can be encoded into strings
- Structures can be decoded from strings (no Unicode or byte-string support)

### Known Issues
- Byte-array encoding and decoding is not supported.
  Data becomes corrupted if you try this.

[Unreleased]: https://github.com/cantido/clj-bencode/compare/4.0.1...HEAD
[4.0.1]: https://github.com/cantido/clj-bencode/compare/4.0.0...4.0.1
[4.0.0]: https://github.com/cantido/clj-bencode/compare/3.1.0...4.0.0
[3.1.0]: https://github.com/cantido/clj-bencode/compare/3.0.0...3.1.0
[3.0.0]: https://github.com/cantido/clj-bencode/compare/2.0.0...3.0.0
[2.0.0]: https://github.com/cantido/clj-bencode/compare/1.0.0...2.0.0
