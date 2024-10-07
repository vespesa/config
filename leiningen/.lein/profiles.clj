{:user {:dependencies [[im.chit/vinyasa "0.4.7"]
                       [zprint "1.2.9"]]
        :plugins      [[cider/cider-nrepl "LATEST"]]
        :aliases      {"outdated" ["with-profile" "root-antq" "run" "-m" "antq.core" "--skip=pom" "--no-changes"]}
        :injections
        [(require '[vinyasa.inject :as inject])
         (inject/in ;; the default injected namespace is `.`
           clojure.core >
           [clojure.pprint pprint]
           [zprint.core zprint]
           [zprint.core czprint])]}
 :root-antq    {:dependencies [[com.github.liquidz/antq "RELEASE"]
                               [org.slf4j/slf4j-nop "RELEASE"]]}}
