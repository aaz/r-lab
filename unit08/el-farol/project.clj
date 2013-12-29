(defproject el-farol "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [incanter/incanter-core "1.5.5-SNAPSHOT"]
                 [incanter/incanter-charts "1.5.5-SNAPSHOT"]]
  :main ^:skip-aot el-farol.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
