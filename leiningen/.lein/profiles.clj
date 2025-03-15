{:user {:dependencies        [[com.github.vespesa/taplica "0.1.0"]]
        :plugins             [[com.gfredericks/lein-shorthand "0.4.1"]]
        :shorthand           {tap [taplica.core/tap>> taplica.core/tap!
                                   taplica.core/values taplica.core/value
                                   taplica.core/fvalue taplica.core/lvalue
                                   taplica.core/pause taplica.core/stop
                                   taplica.core/resume taplica.core/reset]}
        :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                          :username :env/clojars_username
                                          :password :env/clojars_password}]]}}
