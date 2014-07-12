(defproject outlierdetection "0.1.0-SNAPSHOT"
  :description "My implementation to Problem 1: Outlier Detection from Factual's Devblog"
  :url "https://github.com/phillipgreenii/factual-data-testing-challenge"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :maintainer {:email "phillip.green.ii@gmail.com"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter-core "1.5.5"]
                 [instaparse "1.3.2"]]
  :main ^:skip-aot outlierdetection.bin
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
