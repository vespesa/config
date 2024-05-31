{:user {:dependencies [[im.chit/vinyasa "0.4.7"]
                       [zprint "1.2.8"]]
        :injections
        [(require '[vinyasa.inject :as inject])
         (inject/in ;; the default injected namespace is `.`
           clojure.core >
           [clojure.pprint pprint]
           [zprint.core zprint]
           [zprint.core czprint])]}}
