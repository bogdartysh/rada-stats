(defproject rada "0.1.0-SNAPSHOT"
  :description "Верховна Рада аналіз: Розподіл депутатів ВРУ по результатах голосувань"
  :url "https://github.com/bogdartysh/rada-stats"
  :license {:name "GNY Affero General Public License v3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.hml"}
  :dependencies [
        [org.clojure/clojure "1.8.0"] 
        [uawa/uawa "0.1.1-SNAPSHOT"] 
    	[clojure-soup "0.0.1"]
	    [com.github.haifengl/smile-core "1.5.0"]
        [clj-http "3.7.0"]]
  :plugins [[lein-gorilla "0.4.0"]]
  :profiles {:uberjar {:aot :all}  :dev {:dependencies [[alembic "0.3.2"]]}}
  
  )
