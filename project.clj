(defproject clj-bencode "3.0.0-SNAPSHOT"
  :description "A Clojure library for BitTorrent bencoding."
  :url "https://github.com/Cantido/clj-bencode"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/gpl.txt"}
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:resource-paths ["test-resources"]}}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/test.check "0.9.0"]
                 [commons-io/commons-io "2.5"]])

