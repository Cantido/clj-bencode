(defproject clj-bencode "4.0.1-SNAPSHOT"
  :description "A Clojure library for BitTorrent bencoding."
  :url "https://github.com/Cantido/clj-bencode"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/gpl.txt"}
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:resource-paths ["test-resources"]
                   :dependencies [[org.clojure/test.check "0.9.0"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0-beta4"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}}
  :dependencies [[commons-io/commons-io "2.5"]])

