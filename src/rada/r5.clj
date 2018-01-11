;; gorilla-repl.fileformat = 1

;; **
;;; # Розподіл депутатів Верховної Ради України за результатами голосувань
;;; 
;;; 
;; **

;; **
;;; Необхідні функції для считування та розпізнання результатів голосувань з 2015 р.
;;; Доступна за адресою http://viewer.gorilla-repl.org/view.html?source=github&user=bogdartysh&repo=rada-stats&path=src/rada/r4.clj
;; **

;; @@
(use 'jsoup.soup)
(use 'uawa.core)
(use '[gorilla-plot core vega])
(use '[gorilla-repl table html])

(функція прочти-всі-тексти [текстові-елементи]
         (->> текстові-елементи (відобрази #(-> % .childNodes перший .text .trim (.replace "  " " ") )))
)

(функція прочти-документ [номер депутати]
 (спробуй (let [
        лінка (str "http://w1.c1.rada.gov.ua/pls/radan_gs09/ns_golos?g_id=" номер)
        документ  (get! лінка)
        титул  (-> ($ документ ".head_gol") .text .trim  )
        результати          (zipmap 
                              (прочти-всі-тексти  ($ документ ".dep"))
                              (прочти-всі-тексти  ($ документ ".golos")) 
                              )
        ] 
        (defn- gt [sn res] (or (->> res (filter #(= sn (first %))) first second) "Нема даних"))
       (відобрази #(геш-таблиця :фіо  %
                                :вибір (gt % результати)
                                :титул титул 
                                :лінка лінка 
                                :номер номер)
                  (відобрази :фіо  депутати))
       )
  (catch Exception e {} )
 )
)

(функція знайди-за-фіо [фіо колекція]  (фільтруй #(= фіо (:фіо %)) колекція))

(функція голосування-депутата [депутат результати]
  (відобрази #(геш-таблиця (:номер %)                   
                  (condp = (:вибір %)
                    "За"                  1.0
                    "Проти"              -1.0
                    "Не голосував"       -0.1
                    "Не голосувала"      -0.1
                    "Відсутній"           0.0
                    "Відсутня"            0.0
                    "Утримався"          -0.1
                    "Утрималась"         -0.1   
                    "Нема даних"          0.0
                  )) 
             (знайди-за-фіо депутат результати)
           ;  (фільтруй #(.equals депутат (:фіо %)) результати)
   )
)


(функція результати-по-депутатах [прізвища результати]
  (відобрази #(геш-таблиця :фіо % :результати (голосування-депутата % результати)) прізвища)
)

(функція результати-на-масив [результати прізвища]
         (into-array (відобрази #(double-array (->> % (відобрази вибери-значення) (відобрази перший) ))  
                                (відобрази :результати (результати-по-депутатах прізвища результати))))
)

(функція кластери-по-результатах [результати-голосувань прізвища кількість-фракцій]
         (smile.clustering.KMeans. (результати-на-масив результати-голосувань прізвища) кількість-фракцій))

(функція депутати-по-кластерах [результати-голосувань прізвища кількість-фракцій ]
 (хай [фракції (кластери-по-результатах результати-голосувань прізвища кількість-фракцій )]
      (відобрази #(геш-таблиця  :фіо % :фракція-за-результатами-голосувань (.predict фракції (перший (результати-на-масив результати-голосувань [%]))))   прізвища)     
))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-кластерах</span>","value":"#'user/депутати-по-кластерах"}
;; <=

;; @@
(функція покажи-розподіл-фракцій-по-кластерах
         [кількість-кластерів текст-заголовку депутати-по-кдастерах ]
         (хай [номера-груп (проміжок 0 кількість-кластерів)]
              (table-view 
                (відобрази (fn [n]   [
                                        n
                                       ( bar-chart
                                         фракції-ВР-10012018_1
                                         (для-всіх [f фракції-ВР-10012018_1]  
                                                   (/ 
                                                     (->> депутати-по-кдастерах
                                                          (фільтруй #(= n (:фракція-за-результатами-голосувань %)) )
                                                          (фільтруй #(= f (:фракція (перший (знайди-за-фіо (:фіо %) депутати-по-фракціях-ВР-04012018)))))
                                                          розмір      )
                                                     (->>  депутати-по-2-кластерах-згідно-голосувань-10012018_1
                                                           (фільтруй #(= f (:фракція (перший (знайди-за-фіо (:фіо %) депутати-по-фракціях-ВР-04012018)))))
                                                           розмір)
                                                     0.01
                                                     )
                                                   )
                                         ) 
                                       ]) 
                           номера-груп)
                :columns [(html-view текст-заголовку) (html-view "розподіл по фракціям")])
              )
         )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/покажи-розподіл-фракцій-по-кластерах</span>","value":"#'user/покажи-розподіл-фракцій-по-кластерах"}
;; <=

;; @@
(функція депутати-по-фракціях [результати-голосувань прізвища кількість-фракцій ]
 (хай [   
       фракції (депутати-по-кластерах  результати-голосувань прізвища  кількість-фракцій)
       ]
     (відобрази #(геш-таблиця  :фіо % :фракція-за-результатами-голосувань (.predict фракції (перший (результати-на-масив результати-голосувань [%]))))   прізвища)     
))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-фракціях</span>","value":"#'user/депутати-по-фракціях"}
;; <=

;; **
;;; вичитка та розпізнання даних з вікіпедії та сайте ВРУ
;; **

;; @@
(постійна депутати-по-фракціях-ВР-11012018 
          (хай  [
                  документ  (get! "https://uk.wikipedia.org/wiki/%D0%92%D0%B5%D1%80%D1%85%D0%BE%D0%B2%D0%BD%D0%B0_%D0%A0%D0%B0%D0%B4%D0%B0_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D0%B8_VIII_%D1%81%D0%BA%D0%BB%D0%B8%D0%BA%D0%B0%D0%BD%D0%BD%D1%8F")
                  таблиця  (-> (.select документ  "table")   (.отримай 3) (.select "tr"))                 
                  номера-депутатів (проміжок 1 (.size таблиця) )
                  ]
                (->> номера-депутатів 
                     (відобрази 
                       #(хай[рядок (-> таблиця (.отримай %) (.select "td"))]    
                            {:суб'єкт-висування (-> рядок (.отримай 1) .text) 
                             :фіо (хай [повне (-> рядок (.отримай 2) .text)
                                        прізвище  (-> повне (.split " ") перший)
                                        імя (-> повне (.split " ") другий)
                                        по-батькові (-> повне (.split " ") останній)              
                                        ]
                                       (str прізвище " " (-> імя (.charAt 0)) "." (-> по-батькові (.charAt 0)) "."))
                             :номер-у-списку (-> рядок (.отримай 3) .text)
                             :округ (-> рядок (.отримай 4) .text)
                             :фракція (-> рядок (.отримай 5) .text)
                             })
                       )
                     )
                )
)
(друкуй "зчитано дані з вікіпедіі дані про " (розмір депутати-по-фракціях-ВР-04012018 ) "  депутатів")
;; @@
;; ->
;;; зчитано дані з вікіпедіі дані про  423   депутатів
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(постійна фракції-ВР-10012018_1 (distinct (відобрази :фракція розподіл-фракцій-10012018_1)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/фракції-ВР-10012018_1</span>","value":"#'user/фракції-ВР-10012018_1"}
;; <=

;; @@
(постійна голосування-05012018_1 (застосуй concat (->> (проміжок 0 50000) (відобрази #(прочти-документ % депутати-по-фракціях-ВР-04012018)) (фільтруй #(-> % пусто? не)))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/голосування-05012018_1</span>","value":"#'user/голосування-05012018_1"}
;; <=

;; @@
(постійна прізвища-депутатів-05012018_1 (дай-різні (відобрази :фіо депутати-по-фракціях-ВР-04012018)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/прізвища-депутатів-05012018_1</span>","value":"#'user/прізвища-депутатів-05012018_1"}
;; <=

;; @@
(друкуй "зчитано " (розмір голосування-05012018_1) " персональних голосувань " (розмір прізвища-депутатів-05012018_1) " депутатів")
;; @@
;; ->
;;; зчитано  2228787  персональних голосувань  422  депутатів
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; нац. гвардієць Тимошенко Юрій Володимирович та Тимошенко Юлія Володимірівна мають однакові ініціали, нажаль у системі голосувань їх важко розрізнити
;; **

;; @@
(постійна прізвища-депутатів-без-Тимошенко-10012018 (фільтруй #(не (= % "Тимошенко Ю.В.")) прізвища-депутатів-05012018_1))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/прізвища-депутатів-без-Тимошенко-10012018</span>","value":"#'user/прізвища-депутатів-без-Тимошенко-10012018"}
;; <=

;; @@
(розмір прізвища-депутатів-без-Тимошенко-10012018)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>421</span>","value":"421"}
;; <=

;; **
;;; розподіл депутатів на кластери згідно з голосуваннями
;; **

;; @@
(постійна депутати-по-2-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 2))
(фільтруй #(= "Артюшенко І.А." (:фіо %)) депутати-по-2-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:фракція-за-результатами-голосувань 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(покажи-розподіл-фракцій-по-кластерах 2 "розподіл на дві групи, \n Номер групи:" депутати-по-2-кластерах-згідно-голосувань-10012018_1)

;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:фракція-за-результатами-голосувань 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(постійна депутати-по-3-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 3))
(фільтруй #(= "Артюшенко І.А." (:фіо %)) депутати-по-3-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:фракція-за-результатами-голосувань 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 1, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(покажи-розподіл-фракцій-по-кластерах 3 "розподіл на три групи, \n Номер групи:" депутати-по-3-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"розподіл на три групи, \n Номер групи:","value":"#gorilla_repl.html.HtmlView{:content \"розподіл на три групи, \\n Номер групи:\"}"},{"type":"html","content":"розподіл по фракціям","value":"#gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"розподіл на три групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"ed49b951-43d8-47e9-b6c0-77afdd428881","values":[{"x":"БПП","y":11.594202898550721},{"x":"НФ","y":10.0},{"x":"ВН","y":88.8888888888889},{"x":"ПФ","y":42.30769230769231},{"x":"ОБ","y":100.0},{"x":"ПВ","y":88.46153846153845},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":15.789473684210531},{"x":"РПОЛ","y":5.0}]}],"marks":[{"type":"rect","from":{"data":"ed49b951-43d8-47e9-b6c0-77afdd428881"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"ed49b951-43d8-47e9-b6c0-77afdd428881","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"ed49b951-43d8-47e9-b6c0-77afdd428881","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :values ({:x \"БПП\", :y 11.594202898550721} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 42.30769230769231} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :values ({:x \"БПП\", :y 11.594202898550721} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 42.30769230769231} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"67f37b8f-1e52-4b39-8224-fac103d07ca5","values":[{"x":"БПП","y":60.14492753623188},{"x":"НФ","y":82.5},{"x":"ВН","y":0.0},{"x":"ПФ","y":7.692307692307692},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"67f37b8f-1e52-4b39-8224-fac103d07ca5"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"67f37b8f-1e52-4b39-8224-fac103d07ca5","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"67f37b8f-1e52-4b39-8224-fac103d07ca5","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :values ({:x \"БПП\", :y 60.14492753623188} {:x \"НФ\", :y 82.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :values ({:x \"БПП\", :y 60.14492753623188} {:x \"НФ\", :y 82.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c97e9611-0c8b-418f-99ae-a106168b8dcb","values":[{"x":"БПП","y":28.260869565217387},{"x":"НФ","y":7.5},{"x":"ВН","y":11.11111111111111},{"x":"ПФ","y":50.0},{"x":"ОБ","y":0.0},{"x":"ПВ","y":11.53846153846154},{"x":"ОСП","y":100.0},{"x":"ВОБ","y":84.21052631578947},{"x":"РПОЛ","y":95.0}]}],"marks":[{"type":"rect","from":{"data":"c97e9611-0c8b-418f-99ae-a106168b8dcb"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c97e9611-0c8b-418f-99ae-a106168b8dcb","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c97e9611-0c8b-418f-99ae-a106168b8dcb","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :values ({:x \"БПП\", :y 28.260869565217387} {:x \"НФ\", :y 7.5} {:x \"ВН\", :y 11.11111111111111} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 11.53846153846154} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :values ({:x \"БПП\", :y 28.260869565217387} {:x \"НФ\", :y 7.5} {:x \"ВН\", :y 11.11111111111111} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 11.53846153846154} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :values ({:x \"БПП\", :y 11.594202898550721} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 42.30769230769231} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ed49b951-43d8-47e9-b6c0-77afdd428881\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :values ({:x \"БПП\", :y 60.14492753623188} {:x \"НФ\", :y 82.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"67f37b8f-1e52-4b39-8224-fac103d07ca5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :values ({:x \"БПП\", :y 28.260869565217387} {:x \"НФ\", :y 7.5} {:x \"ВН\", :y 11.11111111111111} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 11.53846153846154} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c97e9611-0c8b-418f-99ae-a106168b8dcb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]), :opts (:columns [#gorilla_repl.html.HtmlView{:content \"розподіл на три групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}])}"}
;; <=

;; @@
(постійна депутати-по-5-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 5))
(фільтруй #(= "Артюшенко І.А." (:фіо %)) депутати-по-5-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:фракція-за-результатами-голосувань 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 3, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 3, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(покажи-розподіл-фракцій-по-кластерах 5 "розподіл на п'ять групи, \n Номер групи:" депутати-по-5-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"розподіл на п'ять групи, \n Номер групи:","value":"#gorilla_repl.html.HtmlView{:content \"розподіл на п'ять групи, \\n Номер групи:\"}"},{"type":"html","content":"розподіл по фракціям","value":"#gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"розподіл на п'ять групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"5378f65b-7ca0-4a76-a54e-aaef03fd5a09","values":[{"x":"БПП","y":9.420289855072463},{"x":"НФ","y":8.75},{"x":"ВН","y":33.33333333333333},{"x":"ПФ","y":38.46153846153846},{"x":"ОБ","y":100.0},{"x":"ПВ","y":65.38461538461539},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":15.789473684210531},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"5378f65b-7ca0-4a76-a54e-aaef03fd5a09"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"5378f65b-7ca0-4a76-a54e-aaef03fd5a09","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"5378f65b-7ca0-4a76-a54e-aaef03fd5a09","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 65.38461538461539} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 65.38461538461539} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"13be2774-6186-422e-bb2f-a1ae759c2bf0","values":[{"x":"БПП","y":23.18840579710145},{"x":"НФ","y":17.5},{"x":"ВН","y":0.0},{"x":"ПФ","y":5.769230769230769},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":95.0}]}],"marks":[{"type":"rect","from":{"data":"13be2774-6186-422e-bb2f-a1ae759c2bf0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"13be2774-6186-422e-bb2f-a1ae759c2bf0","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"13be2774-6186-422e-bb2f-a1ae759c2bf0","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :values ({:x \"БПП\", :y 23.18840579710145} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :values ({:x \"БПП\", :y 23.18840579710145} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"6ca43e4f-6974-41dd-be76-dbd84b71fab5","values":[{"x":"БПП","y":10.8695652173913},{"x":"НФ","y":2.5},{"x":"ВН","y":66.66666666666667},{"x":"ПФ","y":15.38461538461538},{"x":"ОБ","y":0.0},{"x":"ПВ","y":34.61538461538462},{"x":"ОСП","y":4.0},{"x":"ВОБ","y":5.263157894736842},{"x":"РПОЛ","y":5.0}]}],"marks":[{"type":"rect","from":{"data":"6ca43e4f-6974-41dd-be76-dbd84b71fab5"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"6ca43e4f-6974-41dd-be76-dbd84b71fab5","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"6ca43e4f-6974-41dd-be76-dbd84b71fab5","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :values ({:x \"БПП\", :y 10.8695652173913} {:x \"НФ\", :y 2.5} {:x \"ВН\", :y 66.66666666666667} {:x \"ПФ\", :y 15.38461538461538} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 34.61538461538462} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :values ({:x \"БПП\", :y 10.8695652173913} {:x \"НФ\", :y 2.5} {:x \"ВН\", :y 66.66666666666667} {:x \"ПФ\", :y 15.38461538461538} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 34.61538461538462} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af","values":[{"x":"БПП","y":48.55072463768116},{"x":"НФ","y":71.25},{"x":"ВН","y":0.0},{"x":"ПФ","y":5.769230769230769},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :values ({:x \"БПП\", :y 48.55072463768116} {:x \"НФ\", :y 71.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :values ({:x \"БПП\", :y 48.55072463768116} {:x \"НФ\", :y 71.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"8e51ae3a-3068-42bd-9567-6fe04fb9b31c","values":[{"x":"БПП","y":7.971014492753622},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":34.61538461538462},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":96.0},{"x":"ВОБ","y":78.94736842105263},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"8e51ae3a-3068-42bd-9567-6fe04fb9b31c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"8e51ae3a-3068-42bd-9567-6fe04fb9b31c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"8e51ae3a-3068-42bd-9567-6fe04fb9b31c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :values ({:x \"БПП\", :y 7.971014492753622} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :values ({:x \"БПП\", :y 7.971014492753622} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 100.0} {:x \"ПВ\", :y 65.38461538461539} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"5378f65b-7ca0-4a76-a54e-aaef03fd5a09\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :values ({:x \"БПП\", :y 23.18840579710145} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"13be2774-6186-422e-bb2f-a1ae759c2bf0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :values ({:x \"БПП\", :y 10.8695652173913} {:x \"НФ\", :y 2.5} {:x \"ВН\", :y 66.66666666666667} {:x \"ПФ\", :y 15.38461538461538} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 34.61538461538462} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"6ca43e4f-6974-41dd-be76-dbd84b71fab5\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :values ({:x \"БПП\", :y 48.55072463768116} {:x \"НФ\", :y 71.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"85efb5cf-2fbb-4c7c-8cb9-5fbc267828af\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :values ({:x \"БПП\", :y 7.971014492753622} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e51ae3a-3068-42bd-9567-6fe04fb9b31c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]), :opts (:columns [#gorilla_repl.html.HtmlView{:content \"розподіл на п'ять групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}])}"}
;; <=

;; @@
(постійна депутати-по-7-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 7))
(фільтруй #(= "Артюшенко І.А." (:фіо %)) депутати-по-7-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:фракція-за-результатами-голосувань 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 3, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 3, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(покажи-розподіл-фракцій-по-кластерах 7 "розподіл на сім груп, \n Номер групи:" депутати-по-7-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"розподіл на сім груп, \n Номер групи:","value":"#gorilla_repl.html.HtmlView{:content \"розподіл на сім груп, \\n Номер групи:\"}"},{"type":"html","content":"розподіл по фракціям","value":"#gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"розподіл на сім груп, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"18ab7173-197b-4524-b084-52d14fefa556","values":[{"x":"БПП","y":12.31884057971014},{"x":"НФ","y":10.0},{"x":"ВН","y":88.8888888888889},{"x":"ПФ","y":50.0},{"x":"ОБ","y":48.837209302325576},{"x":"ПВ","y":88.46153846153845},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":15.789473684210531},{"x":"РПОЛ","y":5.0}]}],"marks":[{"type":"rect","from":{"data":"18ab7173-197b-4524-b084-52d14fefa556"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"18ab7173-197b-4524-b084-52d14fefa556","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"18ab7173-197b-4524-b084-52d14fefa556","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"18ab7173-197b-4524-b084-52d14fefa556\", :values ({:x \"БПП\", :y 12.31884057971014} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"18ab7173-197b-4524-b084-52d14fefa556\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"18ab7173-197b-4524-b084-52d14fefa556\", :values ({:x \"БПП\", :y 12.31884057971014} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"18ab7173-197b-4524-b084-52d14fefa556\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"73e60ad4-ce39-4ead-9e0c-94edb9eae139","values":[{"x":"БПП","y":34.05797101449275},{"x":"НФ","y":17.5},{"x":"ВН","y":5.555555555555556},{"x":"ПФ","y":7.692307692307692},{"x":"ОБ","y":0.0},{"x":"ПВ","y":3.846153846153846},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"73e60ad4-ce39-4ead-9e0c-94edb9eae139"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"73e60ad4-ce39-4ead-9e0c-94edb9eae139","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"73e60ad4-ce39-4ead-9e0c-94edb9eae139","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :values ({:x \"БПП\", :y 34.05797101449275} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :values ({:x \"БПП\", :y 34.05797101449275} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d","values":[{"x":"БПП","y":7.246376811594203},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":34.61538461538462},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":100.0},{"x":"ВОБ","y":84.21052631578947},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :values ({:x \"БПП\", :y 7.246376811594203} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :values ({:x \"БПП\", :y 7.246376811594203} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"bd97d708-cc8d-4d40-b154-f7ef61c98857","values":[{"x":"БПП","y":46.3768115942029},{"x":"НФ","y":72.5},{"x":"ВН","y":0.0},{"x":"ПФ","y":5.769230769230769},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"bd97d708-cc8d-4d40-b154-f7ef61c98857"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"bd97d708-cc8d-4d40-b154-f7ef61c98857","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"bd97d708-cc8d-4d40-b154-f7ef61c98857","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :values ({:x \"БПП\", :y 46.3768115942029} {:x \"НФ\", :y 72.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :values ({:x \"БПП\", :y 46.3768115942029} {:x \"НФ\", :y 72.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"62bb4401-4bbf-4035-bd9a-bf0598890f48","values":[{"x":"БПП","y":0.0},{"x":"НФ","y":0.0},{"x":"ВН","y":5.555555555555556},{"x":"ПФ","y":0.0},{"x":"ОБ","y":0.0},{"x":"ПВ","y":7.692307692307692},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"62bb4401-4bbf-4035-bd9a-bf0598890f48"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"62bb4401-4bbf-4035-bd9a-bf0598890f48","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"62bb4401-4bbf-4035-bd9a-bf0598890f48","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 7.692307692307692} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 7.692307692307692} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"e8796b09-5573-4207-a2d3-e18f69f00145","values":[{"x":"БПП","y":0.0},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":0.0},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":95.0}]}],"marks":[{"type":"rect","from":{"data":"e8796b09-5573-4207-a2d3-e18f69f00145"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"e8796b09-5573-4207-a2d3-e18f69f00145","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"e8796b09-5573-4207-a2d3-e18f69f00145","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"e8796b09-5573-4207-a2d3-e18f69f00145\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[6 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"e8796b09-5573-4207-a2d3-e18f69f00145\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f","values":[{"x":"БПП","y":0.0},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":1.923076923076923},{"x":"ОБ","y":51.16279069767442},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[7 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"18ab7173-197b-4524-b084-52d14fefa556\", :values ({:x \"БПП\", :y 12.31884057971014} {:x \"НФ\", :y 10.0} {:x \"ВН\", :y 88.8888888888889} {:x \"ПФ\", :y 50.0} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 88.46153846153845} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"18ab7173-197b-4524-b084-52d14fefa556\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"18ab7173-197b-4524-b084-52d14fefa556\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :values ({:x \"БПП\", :y 34.05797101449275} {:x \"НФ\", :y 17.5} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 7.692307692307692} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"73e60ad4-ce39-4ead-9e0c-94edb9eae139\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :values ({:x \"БПП\", :y 7.246376811594203} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 34.61538461538462} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 100.0} {:x \"ВОБ\", :y 84.21052631578947} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"93470c08-aeeb-40e8-9fcc-a0ef2e7c641d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :values ({:x \"БПП\", :y 46.3768115942029} {:x \"НФ\", :y 72.5} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"bd97d708-cc8d-4d40-b154-f7ef61c98857\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 7.692307692307692} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"62bb4401-4bbf-4035-bd9a-bf0598890f48\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [6 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"e8796b09-5573-4207-a2d3-e18f69f00145\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"e8796b09-5573-4207-a2d3-e18f69f00145\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [7 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"dfc7e038-a4f6-4d17-94ae-f74c486c8d4f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]), :opts (:columns [#gorilla_repl.html.HtmlView{:content \"розподіл на сім груп, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}])}"}
;; <=

;; @@
(постійна депутати-по-9-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 9))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-9-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-9-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(фільтруй #(= "Артюшенко І.А." (:фіо %)) депутати-по-9-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція-за-результатами-голосувань</span>","value":":фракція-за-результатами-голосувань"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"}],"value":"[:фракція-за-результатами-голосувань 5]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"}],"value":"{:фракція-за-результатами-голосувань 5, :фіо \"Артюшенко І.А.\"}"}],"value":"({:фракція-за-результатами-голосувань 5, :фіо \"Артюшенко І.А.\"})"}
;; <=

;; @@
(покажи-розподіл-фракцій-по-кластерах 9 "розподіл на дев'ять груп, \n Номер групи:" депутати-по-9-кластерах-згідно-голосувань-10012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"розподіл на дев'ять групи, \n Номер групи:","value":"#gorilla_repl.html.HtmlView{:content \"розподіл на дев'ять групи, \\n Номер групи:\"}"},{"type":"html","content":"розподіл по фракціям","value":"#gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"розподіл на дев'ять групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"7b034b20-2c4c-4979-a653-4d5386ddd138","values":[{"x":"БПП","y":9.420289855072463},{"x":"НФ","y":8.75},{"x":"ВН","y":33.33333333333333},{"x":"ПФ","y":38.46153846153846},{"x":"ОБ","y":51.16279069767442},{"x":"ПВ","y":69.23076923076923},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":15.789473684210531},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"7b034b20-2c4c-4979-a653-4d5386ddd138"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"7b034b20-2c4c-4979-a653-4d5386ddd138","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"7b034b20-2c4c-4979-a653-4d5386ddd138","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 69.23076923076923} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 69.23076923076923} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2","values":[{"x":"БПП","y":0.0},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":1.923076923076923},{"x":"ОБ","y":48.837209302325576},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"50ac67ee-9f86-4b96-8119-479d8603db95","values":[{"x":"БПП","y":13.04347826086957},{"x":"НФ","y":5.0},{"x":"ВН","y":61.11111111111111},{"x":"ПФ","y":13.46153846153846},{"x":"ОБ","y":0.0},{"x":"ПВ","y":26.92307692307692},{"x":"ОСП","y":4.0},{"x":"ВОБ","y":5.263157894736842},{"x":"РПОЛ","y":5.0}]}],"marks":[{"type":"rect","from":{"data":"50ac67ee-9f86-4b96-8119-479d8603db95"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"50ac67ee-9f86-4b96-8119-479d8603db95","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"50ac67ee-9f86-4b96-8119-479d8603db95","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"50ac67ee-9f86-4b96-8119-479d8603db95\", :values ({:x \"БПП\", :y 13.04347826086957} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 61.11111111111111} {:x \"ПФ\", :y 13.46153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 26.92307692307692} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"50ac67ee-9f86-4b96-8119-479d8603db95\", :values ({:x \"БПП\", :y 13.04347826086957} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 61.11111111111111} {:x \"ПФ\", :y 13.46153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 26.92307692307692} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"48b72ea6-a36d-47c6-8dda-b91aa8f235e7","values":[{"x":"БПП","y":3.6231884057971007},{"x":"НФ","y":5.0},{"x":"ВН","y":5.555555555555556},{"x":"ПФ","y":0.0},{"x":"ОБ","y":0.0},{"x":"ПВ","y":3.846153846153846},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"48b72ea6-a36d-47c6-8dda-b91aa8f235e7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"48b72ea6-a36d-47c6-8dda-b91aa8f235e7","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"48b72ea6-a36d-47c6-8dda-b91aa8f235e7","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :values ({:x \"БПП\", :y 3.6231884057971007} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :values ({:x \"БПП\", :y 3.6231884057971007} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"9eff61bb-00d0-4247-8bd9-15da099e5d5f","values":[{"x":"БПП","y":0.0},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":0.0},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":95.0}]}],"marks":[{"type":"rect","from":{"data":"9eff61bb-00d0-4247-8bd9-15da099e5d5f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"9eff61bb-00d0-4247-8bd9-15da099e5d5f","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"9eff61bb-00d0-4247-8bd9-15da099e5d5f","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"4799421c-7f9e-493a-9dab-0345f88512c6","values":[{"x":"БПП","y":29.71014492753623},{"x":"НФ","y":66.25},{"x":"ВН","y":0.0},{"x":"ПФ","y":5.769230769230769},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"4799421c-7f9e-493a-9dab-0345f88512c6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"4799421c-7f9e-493a-9dab-0345f88512c6","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"4799421c-7f9e-493a-9dab-0345f88512c6","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"4799421c-7f9e-493a-9dab-0345f88512c6\", :values ({:x \"БПП\", :y 29.71014492753623} {:x \"НФ\", :y 66.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[6 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"4799421c-7f9e-493a-9dab-0345f88512c6\", :values ({:x \"БПП\", :y 29.71014492753623} {:x \"НФ\", :y 66.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a","values":[{"x":"БПП","y":5.0724637681159415},{"x":"НФ","y":0.0},{"x":"ВН","y":0.0},{"x":"ПФ","y":32.69230769230769},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":96.0},{"x":"ВОБ","y":78.94736842105263},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :values ({:x \"БПП\", :y 5.0724637681159415} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 32.69230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[7 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :values ({:x \"БПП\", :y 5.0724637681159415} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 32.69230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08","values":[{"x":"БПП","y":5.797101449275361},{"x":"НФ","y":11.25},{"x":"ВН","y":0.0},{"x":"ПФ","y":3.846153846153846},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :values ({:x \"БПП\", :y 5.797101449275361} {:x \"НФ\", :y 11.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[8 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :values ({:x \"БПП\", :y 5.797101449275361} {:x \"НФ\", :y 11.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"fe63f74a-00a7-4fa6-bf50-2fc39dc82276","values":[{"x":"БПП","y":33.33333333333333},{"x":"НФ","y":3.75},{"x":"ВН","y":0.0},{"x":"ПФ","y":3.846153846153846},{"x":"ОБ","y":0.0},{"x":"ПВ","y":0.0},{"x":"ОСП","y":0.0},{"x":"ВОБ","y":0.0},{"x":"РПОЛ","y":0.0}]}],"marks":[{"type":"rect","from":{"data":"fe63f74a-00a7-4fa6-bf50-2fc39dc82276"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"fe63f74a-00a7-4fa6-bf50-2fc39dc82276","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"fe63f74a-00a7-4fa6-bf50-2fc39dc82276","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :values ({:x \"БПП\", :y 33.33333333333333} {:x \"НФ\", :y 3.75} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[9 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :values ({:x \"БПП\", :y 33.33333333333333} {:x \"НФ\", :y 3.75} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([1 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :values ({:x \"БПП\", :y 9.420289855072463} {:x \"НФ\", :y 8.75} {:x \"ВН\", :y 33.33333333333333} {:x \"ПФ\", :y 38.46153846153846} {:x \"ОБ\", :y 51.16279069767442} {:x \"ПВ\", :y 69.23076923076923} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 15.789473684210531} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7b034b20-2c4c-4979-a653-4d5386ddd138\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [2 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 1.923076923076923} {:x \"ОБ\", :y 48.837209302325576} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7f9c1402-d190-4f9f-baaf-1f84d66ed1b2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [3 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"50ac67ee-9f86-4b96-8119-479d8603db95\", :values ({:x \"БПП\", :y 13.04347826086957} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 61.11111111111111} {:x \"ПФ\", :y 13.46153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 26.92307692307692} {:x \"ОСП\", :y 4.0} {:x \"ВОБ\", :y 5.263157894736842} {:x \"РПОЛ\", :y 5.0})}], :marks [{:type \"rect\", :from {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"50ac67ee-9f86-4b96-8119-479d8603db95\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [4 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :values ({:x \"БПП\", :y 3.6231884057971007} {:x \"НФ\", :y 5.0} {:x \"ВН\", :y 5.555555555555556} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 3.846153846153846} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"48b72ea6-a36d-47c6-8dda-b91aa8f235e7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [5 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :values ({:x \"БПП\", :y 0.0} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 0.0} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 95.0})}], :marks [{:type \"rect\", :from {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9eff61bb-00d0-4247-8bd9-15da099e5d5f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [6 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"4799421c-7f9e-493a-9dab-0345f88512c6\", :values ({:x \"БПП\", :y 29.71014492753623} {:x \"НФ\", :y 66.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 5.769230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"4799421c-7f9e-493a-9dab-0345f88512c6\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [7 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :values ({:x \"БПП\", :y 5.0724637681159415} {:x \"НФ\", :y 0.0} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 32.69230769230769} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 96.0} {:x \"ВОБ\", :y 78.94736842105263} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0ca2b1dc-60cd-440a-bb0f-caaa2e682a8a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [8 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :values ({:x \"БПП\", :y 5.797101449275361} {:x \"НФ\", :y 11.25} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"76ac3586-5ee1-4cc7-bd61-b5b7a6fa3e08\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [9 #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :values ({:x \"БПП\", :y 33.33333333333333} {:x \"НФ\", :y 3.75} {:x \"ВН\", :y 0.0} {:x \"ПФ\", :y 3.846153846153846} {:x \"ОБ\", :y 0.0} {:x \"ПВ\", :y 0.0} {:x \"ОСП\", :y 0.0} {:x \"ВОБ\", :y 0.0} {:x \"РПОЛ\", :y 0.0})}], :marks [{:type \"rect\", :from {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"fe63f74a-00a7-4fa6-bf50-2fc39dc82276\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]), :opts (:columns [#gorilla_repl.html.HtmlView{:content \"розподіл на дев'ять групи, \\n Номер групи:\"} #gorilla_repl.html.HtmlView{:content \"розподіл по фракціям\"}])}"}
;; <=

;; @@
(постійна розподіл-фракцій-11012018_1 (->> 
     депутати-по-фракціях-ВР-11012018
     (фільтруй #(не (= % "Тимошенко Ю.В." (:фіо %))))
     (відобрази #(merge % { 
                    :на-два-кластери-за-голосуваннями       (:фракція-за-результатами-голосувань (перший (знайди-за-фіо  (:фіо %) 
                                                                                       депутати-по-2-кластерах-згідно-голосувань-10012018_1)))
                    :на-три-кластерів-за-голосуваннями      (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-3-кластерах-згідно-голосувань-10012018_1)))
                    :на-п'ять-кластерів-за-голосуваннями    (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-5-кластерах-згідно-голосувань-10012018_1)))
                    :на-сім-кластерів-за-голосуваннями      (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                   депутати-по-7-кластерах-згідно-голосувань-10012018_1)))
                    :на-дев'ять-кластерів-за-голосуваннями  (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-9-кластерах-згідно-голосувань-10012018_1)))
                })
     )
))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/розподіл-фракцій-11012018_1</span>","value":"#'user/розподіл-фракцій-11012018_1"}
;; <=

;; @@

(знайди-за-фіо "Артюшенко І.А." розподіл-фракцій-11012018_1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-сім-кластерів-за-голосуваннями</span>","value":":на-сім-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:на-сім-кластерів-за-голосуваннями 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-дев&#x27;ять-кластерів-за-голосуваннями</span>","value":":на-дев'ять-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"}],"value":"[:на-дев'ять-кластерів-за-голосуваннями 5]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:округ</span>","value":":округ"},{"type":"html","content":"<span class='clj-string'>&quot;75&quot;</span>","value":"\"75\""}],"value":"[:округ \"75\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-два-кластери-за-голосуваннями</span>","value":":на-два-кластери-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:на-два-кластери-за-голосуваннями 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:суб&#x27;єкт-висування</span>","value":":суб'єкт-висування"},{"type":"html","content":"<span class='clj-string'>&quot;Блок Петра Порошенка&quot;</span>","value":"\"Блок Петра Порошенка\""}],"value":"[:суб'єкт-висування \"Блок Петра Порошенка\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Артюшенко І.А.&quot;</span>","value":"\"Артюшенко І.А.\""}],"value":"[:фіо \"Артюшенко І.А.\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція</span>","value":":фракція"},{"type":"html","content":"<span class='clj-string'>&quot;БПП&quot;</span>","value":"\"БПП\""}],"value":"[:фракція \"БПП\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:номер-у-списку</span>","value":":номер-у-списку"},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""}],"value":"[:номер-у-списку \"\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-три-кластерів-за-голосуваннями</span>","value":":на-три-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:на-три-кластерів-за-голосуваннями 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-п&#x27;ять-кластерів-за-голосуваннями</span>","value":":на-п'ять-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:на-п'ять-кластерів-за-голосуваннями 3]"}],"value":"{:на-сім-кластерів-за-голосуваннями 3, :на-дев'ять-кластерів-за-голосуваннями 5, :округ \"75\", :на-два-кластери-за-голосуваннями 1, :суб'єкт-висування \"Блок Петра Порошенка\", :фіо \"Артюшенко І.А.\", :фракція \"БПП\", :номер-у-списку \"\", :на-три-кластерів-за-голосуваннями 1, :на-п'ять-кластерів-за-голосуваннями 3}"}],"value":"({:на-сім-кластерів-за-голосуваннями 3, :на-дев'ять-кластерів-за-голосуваннями 5, :округ \"75\", :на-два-кластери-за-голосуваннями 1, :суб'єкт-висування \"Блок Петра Порошенка\", :фіо \"Артюшенко І.А.\", :фракція \"БПП\", :номер-у-списку \"\", :на-три-кластерів-за-голосуваннями 1, :на-п'ять-кластерів-за-голосуваннями 3})"}
;; <=

;; @@
(функція на_csv [кол] (застосуй str (встав ", "  кол )))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/на_csv</span>","value":"#'user/на_csv"}
;; <=

;; @@
(на_csv (vals (first
        розподіл-фракцій-11012018_1)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;3, 8, 11, 1, Блок Петра Порошенка, Домбровський О.Г., БПП, , 1, 3&quot;</span>","value":"\"3, 8, 11, 1, Блок Петра Порошенка, Домбровський О.Г., БПП, , 1, 3\""}
;; <=

;; @@
(print (на_csv (keys (first розподіл-фракцій-11012018_1))))
;; @@
;; ->
;;; :на-сім-кластерів-за-голосуваннями, :на-дев&#x27;ять-кластерів-за-голосуваннями, :округ, :на-два-кластери-за-голосуваннями, :суб&#x27;єкт-висування, :фіо, :фракція, :номер-у-списку, :на-три-кластерів-за-голосуваннями, :на-п&#x27;ять-кластерів-за-голосуваннями
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

(nth розподіл-фракцій-11012018_1 1)


;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-сім-кластерів-за-голосуваннями</span>","value":":на-сім-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:на-сім-кластерів-за-голосуваннями 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-дев&#x27;ять-кластерів-за-голосуваннями</span>","value":":на-дев'ять-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"}],"value":"[:на-дев'ять-кластерів-за-голосуваннями 5]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:округ</span>","value":":округ"},{"type":"html","content":"<span class='clj-string'>&quot;12&quot;</span>","value":"\"12\""}],"value":"[:округ \"12\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-два-кластери-за-голосуваннями</span>","value":":на-два-кластери-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:на-два-кластери-за-голосуваннями 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:суб&#x27;єкт-висування</span>","value":":суб'єкт-висування"},{"type":"html","content":"<span class='clj-string'>&quot;Блок Петра Порошенка&quot;</span>","value":"\"Блок Петра Порошенка\""}],"value":"[:суб'єкт-висування \"Блок Петра Порошенка\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фіо</span>","value":":фіо"},{"type":"html","content":"<span class='clj-string'>&quot;Порошенко О.П.&quot;</span>","value":"\"Порошенко О.П.\""}],"value":"[:фіо \"Порошенко О.П.\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:фракція</span>","value":":фракція"},{"type":"html","content":"<span class='clj-string'>&quot;БПП&quot;</span>","value":"\"БПП\""}],"value":"[:фракція \"БПП\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:номер-у-списку</span>","value":":номер-у-списку"},{"type":"html","content":"<span class='clj-string'>&quot;&quot;</span>","value":"\"\""}],"value":"[:номер-у-списку \"\"]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-три-кластерів-за-голосуваннями</span>","value":":на-три-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>1</span>","value":"1"}],"value":"[:на-три-кластерів-за-голосуваннями 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:на-п&#x27;ять-кластерів-за-голосуваннями</span>","value":":на-п'ять-кластерів-за-голосуваннями"},{"type":"html","content":"<span class='clj-unkown'>3</span>","value":"3"}],"value":"[:на-п'ять-кластерів-за-голосуваннями 3]"}],"value":"{:на-сім-кластерів-за-голосуваннями 3, :на-дев'ять-кластерів-за-голосуваннями 5, :округ \"12\", :на-два-кластери-за-голосуваннями 1, :суб'єкт-висування \"Блок Петра Порошенка\", :фіо \"Порошенко О.П.\", :фракція \"БПП\", :номер-у-списку \"\", :на-три-кластерів-за-голосуваннями 1, :на-п'ять-кластерів-за-голосуваннями 3}"}
;; <=

;; @@
(System/getProperty "user.dir") 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;C:\\\\Users\\\\artyushe\\\\workspace\\\\personal\\\\rada-stats\\\\rada-stats&quot;</span>","value":"\"C:\\\\Users\\\\artyushe\\\\workspace\\\\personal\\\\rada-stats\\\\rada-stats\""}
;; <=

;; @@
(use 'clojure.java.io)

(with-open [wrtr (writer "rada_110118.csv")]
  (
    (.write wrtr (на_csv (keys (first розподіл-фракцій-11012018_1))))
    
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(let [data (take 3 розподіл-фракцій-11012018_1)]
(defn write-csv [path row-data]
  (let [columns [:a :b :c :d]
        headers (map name columns)
        rows (mapv #(mapv % columns) row-data)]
    (with-open [file (io/writer path)]
      (csv/write-csv file (cons headers rows)))))

;; @@

;; @@
(->  розподіл-фракцій-11012018_1 first keys на_csv print)
;; @@
;; ->
;;; :на-сім-кластерів-за-голосуваннями, :на-дев&#x27;ять-кластерів-за-голосуваннями, :округ, :на-два-кластери-за-голосуваннями, :суб&#x27;єкт-висування, :фіо, :фракція, :номер-у-списку, :на-три-кластерів-за-голосуваннями, :на-п&#x27;ять-кластерів-за-голосуваннями
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(->> розподіл-фракцій-11012018_1 (sort-by :фракція) (map vals) (map на_csv) (map #(str % "\n")) print)
;; @@
;; ->
;;; (3, 8, 11, 1, Блок Петра Порошенка, Домбровський О.Г., БПП, , 1, 3
;;;  3, 5, 12, 1, Блок Петра Порошенка, Порошенко О.П., БПП, , 1, 3
;;;  3, 5, 13, 1, Самовисування, Юрчишин П.В., БПП, , 1, 3
;;;  3, 5, 14, 1, Блок Петра Порошенка, Мельничук І.І., БПП, , 1, 3
;;;  3, 5, 15, 1, Блок Петра Порошенка, Спориш І.Д., БПП, , 1, 3
;;;  3, 5, 16, 1, Самовисування, Македон Ю.М., БПП, , 1, 3
;;;  3, 8, 17, 1, Самовисування, Кучер М.І., БПП, , 1, 3
;;;  3, 5, 18, 1, Блок Петра Порошенка, Демчак Р.Є., БПП, , 1, 3
;;;  3, 5, 25, 1, Блок Петра Порошенка, Курячий М.П., БПП, , 1, 3
;;;  0, 0, 27, 0, Самовисування, Ричкова Т.Б., БПП, , 0, 0
;;;  3, 5, 28, 1, Блок Петра Порошенка, Куліченко І.І., БПП, , 1, 3
;;;  1, 2, 33, 0, Блок Петра Порошенка, Усов К.Г., БПП, , 2, 2
;;;  0, 2, 38, 0, Самовисування, Нестеренко В.Г., БПП, , 0, 2
;;;  1, 8, 48, 1, Самовисування, Єфімов М.В., БПП, , 2, 1
;;;  1, 8, 53, 1, Самовисування, Недава О.А., БПП, , 1, 1
;;;  3, 7, 60, 1, Блок Петра Порошенка, Лубінець Д.В., БПП, , 1, 3
;;;  3, 5, 63, 1, Самовисування, Ревега О.В., БПП, , 1, 3
;;;  3, 5, 64, 1, Блок Петра Порошенка, Арешонков В.Ю., БПП, , 1, 3
;;;  3, 5, 68, 1, Блок Петра Порошенка, Горват Р.І., БПП, , 1, 3
;;;  1, 3, 74, 1, Блок Петра Порошенка, Сабашук П.П., БПП, , 2, 1
;;;  3, 5, 75, 1, Блок Петра Порошенка, Артюшенко І.А., БПП, , 1, 3
;;;  3, 5, 76, 1, Блок Петра Порошенка, Фролов М.О., БПП, , 1, 3
;;;  3, 7, 81, 1, Самовисування, Валентиров С.В., БПП, , 1, 3
;;;  1, 2, 82, 0, Блок Петра Порошенка, Кривохатько В.В., БПП, , 2, 2
;;;  3, 5, 84, 1, Блок Петра Порошенка, Довбенко М.В., БПП, , 1, 3
;;;  3, 8, 89, 1, Блок Петра Порошенка, Соловей Ю.І., БПП, , 1, 3
;;;  1, 8, 91, 1, Блок Петра Порошенка, Сольвар Р.М., БПП, , 2, 1
;;;  1, 3, 92, 1, Блок Петра Порошенка, Гудзенко В.І., БПП, , 2, 1
;;;  2, 6, 97, 1, Блок Петра Порошенка, Різаненко П.О., БПП, , 1, 4
;;;  1, 8, 99, 1, Блок Петра Порошенка, Яриніч К.В., БПП, , 2, 1
;;;  1, 8, 103, 1, Блок Петра Порошенка, Кузьменко А.І., БПП, , 1, 1
;;;  1, 8, 113, 1, Самовисування, Курило В.С., БПП, , 1, 1
;;;  2, 8, 117, 1, Блок Петра Порошенка, Юринець О.В., БПП, , 2, 4
;;;  1, 8, 118, 1, Блок Петра Порошенка, Дубневич Б.В., БПП, , 1, 1
;;;  3, 8, 120, 1, Блок Петра Порошенка, Дубневич Я.В., БПП, , 1, 3
;;;  1, 7, 125, 1, Самовисування, Лопушанський А.Я., БПП, , 2, 1
;;;  1, 8, 126, 1, Блок Петра Порошенка, Кіт А.Б., БПП, , 2, 1
;;;  3, 5, 127, 1, Блок Петра Порошенка, Козир Б.Ю., БПП, , 1, 3
;;;  3, 8, 129, 1, Блок Петра Порошенка, Жолобецький О.О., БПП, , 1, 3
;;;  1, 8, 130, 1, Блок Петра Порошенка, Вадатурський А.О., БПП, , 1, 1
;;;  3, 8, 131, 1, Блок Петра Порошенка, Лівік О.П., БПП, , 1, 3
;;;  1, 3, 132, 1, Блок Петра Порошенка, Корнацький А.О., БПП, , 2, 1
;;;  1, 2, 134, 0, Блок Петра Порошенка, Чекіта Г.Л., БПП, , 2, 2
;;;  0, 2, 136, 0, Блок Петра Порошенка, Голубов Д.І., БПП, , 0, 2
;;;  1, 8, 143, 1, Сильна Україна, Урбанський О.І., БПП, , 1, 3
;;;  1, 8, 144, 1, Блок Петра Порошенка, Каплін С.М., БПП, , 2, 1
;;;  3, 5, 148, 1, Блок Петра Порошенка, Іщейкін К.Є., БПП, , 1, 3
;;;  3, 5, 154, 1, Блок Петра Порошенка, Дехтярчук О.В., БПП, , 1, 3
;;;  3, 5, 155, 1, Блок Петра Порошенка, Яніцький В.П., БПП, , 1, 3
;;;  3, 8, 158, 1, Блок Петра Порошенка, Сугоняко О.Л., БПП, , 1, 3
;;;  3, 5, 161, 1, Блок Петра Порошенка, Лаврик М.І., БПП, , 1, 3
;;;  3, 5, 165, 1, Блок Петра Порошенка, Юрик Т.З., БПП, , 1, 3
;;;  3, 5, 166, 1, Блок Петра Порошенка, Люшняк М.В., БПП, , 1, 3
;;;  3, 5, 167, 1, Блок Петра Порошенка, Барна О.С., БПП, , 1, 3
;;;  3, 5, 182, 1, Блок Петра Порошенка, Співаковський О.В., БПП, , 1, 3
;;;  1, 8, 184, 1, Блок Петра Порошенка, Вінник І.Ю., БПП, , 1, 1
;;;  3, 8, 185, 1, Блок Петра Порошенка, Хлань С.В., БПП, , 1, 3
;;;  1, 2, 186, 0, Самовисування, Негой Ф.Ф., БПП, , 2, 1
;;;  3, 5, 187, 1, Самовисування, Мельник С.І., БПП, , 1, 3
;;;  3, 5, 189, 1, Самовисування, Шинькович А.В., БПП, , 1, 3
;;;  3, 5, 190, 1, Блок Петра Порошенка, Мацола Р.М., БПП, , 1, 3
;;;  3, 5, 193, 1, Самовисування, Мельниченко В.В., БПП, , 1, 3
;;;  2, 6, 198, 1, Самовисування, Рудик С.Я., БПП, , 2, 4
;;;  3, 8, 202, 1, Блок Петра Порошенка, Рибак І.П., БПП, , 1, 3
;;;  3, 5, 203, 1, Блок Петра Порошенка, Тіміш Г.І., БПП, , 1, 3
;;;  1, 2, 205, 0, Блок Петра Порошенка «Солідарність», Березенко С.І., БПП, , 2, 2
;;;  1, 8, 207, 1, Блок Петра Порошенка, Євлахов А.С., БПП, , 2, 1
;;;  1, 8, 208, 1, Заступ, Давиденко В.М., БПП, , 2, 1
;;;  3, 7, 210, 1, Блок Петра Порошенка, Дмитренко О.М., БПП, , 1, 3
;;;  0, 0, 218, 0, Блок Петра Порошенка, Ар&#x27;єв В.І., БПП, , 0, 0
;;;  3, 8, 219, 1, Блок Петра Порошенка, Третьяков О.Ю., БПП, , 1, 3
;;;  3, 8, 222, 1, Блок Петра Порошенка, Андрієвський Д.Й., БПП, , 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Богомолець О.В., БПП, 3, 2, 2
;;;  0, 0, , 0, Блок Петра Порошенка, Джемілєв М.М., БПП, 5, 0, 0
;;;  3, 5, , 1, Блок Петра Порошенка, Мамчур Ю.В., БПП, 6, 1, 3
;;;  0, 2, , 0, Блок Петра Порошенка, Матіос М.В., БПП, 7, 2, 2
;;;  3, 8, , 1, Блок Петра Порошенка, Князевич Р.П., БПП, 12, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Гринів І.О., БПП, 14, 2, 1
;;;  2, 6, , 1, Блок Петра Порошенка, Продан О.П., БПП, 15, 2, 4
;;;  2, 6, , 1, Блок Петра Порошенка, Новак Н.В., БПП, 16, 2, 4
;;;  2, 6, , 1, Блок Петра Порошенка, Пинзеник В.М., БПП, 17, 2, 4
;;;  2, 8, , 1, Блок Петра Порошенка, Заліщук С.П., БПП, 18, 1, 4
;;;  2, 8, , 1, Блок Петра Порошенка, Лещенко С.А., БПП, 19, 2, 4
;;;  0, 0, , 0, Блок Петра Порошенка, Найєм М.М., БПП, 20, 0, 0
;;;  3, 5, , 1, Блок Петра Порошенка, Черненко О.М., БПП, 21, 1, 3
;;;  3, 8, , 1, Блок Петра Порошенка, Іонова М.М., БПП, 23, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Палатний А.Л., БПП, 24, 1, 1
;;;  3, 5, , 1, Блок Петра Порошенка, Романюк Р.С., БПП, 25, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Заболотний Г.М., БПП, 27, 1, 3
;;;  3, 7, , 1, Блок Петра Порошенка, Южаніна Н.П., БПП, 28, 1, 3
;;;  1, 7, , 1, Блок Петра Порошенка, Кононенко І.В., БПП, 29, 1, 1
;;;  3, 5, , 1, Блок Петра Порошенка, Фріз І.В., БПП, 30, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Онуфрик Б.С., БПП, 31, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Матвієнко А.С., БПП, 32, 1, 1
;;;  3, 8, , 1, Блок Петра Порошенка, Павелко А.В., БПП, 33, 1, 3
;;;  1, 2, , 1, Блок Петра Порошенка, Король В.М., БПП, 35, 2, 1
;;;  1, 8, , 1, Блок Петра Порошенка, Паламарчук М.П., БПП, 36, 1, 1
;;;  1, 8, , 1, Блок Петра Порошенка, Іщенко В.О., БПП, 37, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Пацкан В.В., БПП, 38, 1, 3
;;;  1, 2, , 1, Блок Петра Порошенка, Антонищак А.Ф., БПП, 39, 2, 1
;;;  1, 7, , 1, Блок Петра Порошенка, Гончаренко О.О., БПП, 40, 1, 1
;;;  1, 2, , 1, Блок Петра Порошенка, Козаченко Л.П., БПП, 41, 2, 1
;;;  1, 8, , 1, Блок Петра Порошенка, Ткачук Г.В., БПП, 42, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Герасимов А.В., БПП, 43, 1, 3
;;;  0, 2, , 0, Блок Петра Порошенка, Агафонова Н.В., БПП, 44, 0, 2
;;;  3, 8, , 1, Блок Петра Порошенка, Денисенко В.І., БПП, 45, 1, 3
;;;  1, 2, , 0, Блок Петра Порошенка, Тригубенко С.М., БПП, 46, 2, 2
;;;  3, 8, , 1, Блок Петра Порошенка, Бєлькова О.В., БПП, 47, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Червакова О.В., БПП, 49, 1, 3
;;;  1, 7, , 1, Блок Петра Порошенка, Кобцев М.В., БПП, 50, 1, 1
;;;  3, 8, , 1, Блок Петра Порошенка, Загорій Г.В., БПП, 52, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Алєксєєв С.О., БПП, 55, 1, 3
;;;  0, 0, , 0, Блок Петра Порошенка, Чепинога В.М., БПП, 56, 0, 0
;;;  3, 5, , 1, Блок Петра Порошенка, Бакуменко О.Б., БПП, 57, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Грановський О.М., БПП, 58, 2, 1
;;;  1, 8, , 1, Блок Петра Порошенка, Мушак О.П., БПП, 60, 1, 1
;;;  1, 2, , 1, Блок Петра Порошенка, Брензович В.І., БПП, 62, 2, 2
;;;  3, 5, , 1, Блок Петра Порошенка, Кудлаєнко С.В., БПП, 63, 1, 3
;;;  3, 5, , 1, Блок Петра Порошенка, Матузко О.О., БПП, 64, 1, 3
;;;  0, 0, , 0, Блок Петра Порошенка, Макар&#x27;ян Д.Б., БПП, 66, 0, 0
;;;  3, 8, , 1, Блок Петра Порошенка, Побер І.М., БПП, 67, 1, 3
;;;  1, 3, , 1, Блок Петра Порошенка, Куніцин С.В., БПП, 68, 2, 1
;;;  3, 8, , 1, Блок Петра Порошенка, Лесюк Я.В., БПП, 69, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Луценко І.С., БПП, 70, 1, 1
;;;  1, 2, , 1, Блок Петра Порошенка, Чубаров Р.А., БПП, 71, 2, 2
;;;  1, 2, , 1, Блок Петра Порошенка, Шверк Г.А., БПП, 72, 2, 2
;;;  1, 2, , 1, Блок Петра Порошенка, Севрюков В.В., БПП, 73, 2, 2
;;;  0, 0, , 0, Блок Петра Порошенка, Бригинець О.М., БПП, 75, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Білоцерковець Д.О., БПП, 76, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Карпунцов В.В., БПП, 77, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Велікін О.М., БПП, 78, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Саврасов М.В., БПП, 79, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Білозір О.В., БПП, 80, 0, 0
;;;  0, 0, , 0, Блок Петра Порошенка, Буглак Ю.О., БПП, 81, 0, 0
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Кривенко В.М., БПП, 5, 2, 4
;;;  1, 2, , 1, Об&#x27;єднання «Самопоміч», Суслова І.М., БПП, 6, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Кишкар П.М., БПП, 7, 2, 4
;;;  1, 3, , 1, Об&#x27;єднання «Самопоміч», Немировський А.В., БПП, 33, 2, 2
;;;  0, 2, 20, 0, Самовисування, Мартиняк С.В., ВН, , 0, 2
;;;  0, 2, 21, 0, Самовисування, Івахів С.П., ВН, , 0, 2
;;;  0, 2, 72, 0, Самовисування, Петьовка В.В., ВН, , 2, 2
;;;  0, 0, 77, 0, Самовисування, Богуслаєв В.О., ВН, , 0, 0
;;;  4, 2, 78, 0, Самовисування, Пономарьов О.С., ВН, , 0, 2
;;;  0, 2, 79, 0, Самовисування, Бандуров В.В., ВН, , 0, 2
;;;  0, 0, 93, 0, Самовисування, Онищенко О.Р., ВН, , 0, 0
;;;  0, 2, 96, 0, Самовисування, Москаленко Я.М., ВН, , 0, 2
;;;  0, 2, 101, 0, Самовисування, Поплавський М.М., ВН, , 0, 2
;;;  0, 2, 102, 0, Самовисування, Довгий О.С., ВН, , 0, 2
;;;  0, 0, 114, 0, Наш край, Шахов С.В., ВН, , 0, 0
;;;  0, 0, 138, 0, Самовисування, Фурсін І.Г., ВН, , 0, 0
;;;  0, 2, 160, 0, Самовисування, Молоток І.Ф., ВН, , 0, 2
;;;  0, 0, 174, 0, Самовисування, Фельдман О.Б., ВН, , 0, 0
;;;  0, 0, 179, 0, Самовисування, Гіршфельд А.М., ВН, , 0, 0
;;;  0, 2, 188, 0, Самовисування, Лабазюк С.П., ВН, , 0, 2
;;;  1, 3, 211, 1, Блок Петра Порошенка, Рибчинський Є.Ю., ВН, , 2, 2
;;;  0, 2, , 0, Радикальна партія, Мельничук С.П., ВН, 3, 0, 2
;;;  0, 0, 151, 0, ВО «Батьківщина», Богдан Р.Д., ВОБ, , 0, 0
;;;  2, 6, 156, 1, ВО «Батьківщина», Євтушок С.М., ВОБ, , 2, 4
;;;  2, 6, 162, 1, ВО «Батьківщина», Бухарєв В.В., ВОБ, , 2, 4
;;;  0, 0, 183, 0, ВО «Батьківщина», Одарченко Ю.В., ВОБ, , 0, 0
;;;  , , , , ВО «Батьківщина», Тимошенко Ю.В., ВОБ, 2, , 
;;;  2, 6, , 1, ВО «Батьківщина», Луценко І.В., ВОБ, 3, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Соболєв С.В., ВОБ, 4, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Шкрум А.І., ВОБ, 5, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Івченко В.Є., ВОБ, 6, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Немиря Г.М., ВОБ, 7, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Крулько І.І., ВОБ, 8, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Рябчин О.М., ВОБ, 9, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Тарасюк Б.І., ВОБ, 12, 2, 4
;;;  0, 0, , 0, ВО «Батьківщина», Кожем&#x27;якін А.А., ВОБ, 13, 0, 0
;;;  2, 6, , 1, ВО «Батьківщина», Кириленко І.Г., ВОБ, 14, 2, 4
;;;  2, 6, , 0, ВО «Батьківщина», Власенко С.В., ВОБ, 15, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Кужель О.В., ВОБ, 16, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Абдуллін О.Р., ВОБ, 17, 2, 4
;;;  2, 6, , 1, ВО «Батьківщина», Кондратюк О.К., ВОБ, 18, 2, 4
;;;  2, 2, , 0, ВО «Батьківщина», Дубіль В.О., ВОБ, 19, 2, 2
;;;  3, 5, 19, 1, Народний фронт, Гузь І.В., НФ, , 1, 3
;;;  3, 5, 22, 1, Народний фронт, Лапін І.О., НФ, , 1, 3
;;;  3, 5, 34, 1, Самовисування, Кришин О.Ю., НФ, , 1, 3
;;;  3, 5, 66, 1, Народний фронт, Дзюблик П.В., НФ, , 1, 3
;;;  3, 5, 86, 1, Народний фронт, Дирів А.Б., НФ, , 1, 3
;;;  , , 88, , Народний фронт, Тимошенко Ю.В., НФ, , , 
;;;  3, 5, 94, 1, Народний фронт, Романюк В.М., НФ, , 1, 3
;;;  3, 5, 95, 1, Народний фронт, Гаврилюк М.В., НФ, , 1, 3
;;;  3, 5, 119, 1, Народний Фронт, Бондар М.Л., НФ, , 1, 3
;;;  3, 5, 149, 1, Народний Фронт, Река А.О., НФ, , 1, 3
;;;  3, 5, 153, 1, Народний Фронт, Вознюк Ю.В., НФ, , 1, 3
;;;  3, 5, 157, 1, Народний Фронт, Медуниця О.В., НФ, , 1, 3
;;;  3, 5, 169, 1, Народний Фронт, Кірш О.В., НФ, , 1, 3
;;;  3, 5, 201, 1, Народний Фронт, Федорук М.Т., НФ, , 1, 3
;;;  3, 5, 204, 1, Народний Фронт, Бурбак М.Ю., НФ, , 1, 3
;;;  3, 5, 209, 1, Народний Фронт, Кодола О.М., НФ, , 1, 3
;;;  3, 5, 212, 1, Народний Фронт, Сташук В.Ф., НФ, , 1, 3
;;;  1, 7, 221, 1, Народний Фронт, Ємець Л.О., НФ, , 1, 1
;;;  3, 5, , 1, Народний Фронт, Чорновол Т.М., НФ, 2, 1, 3
;;;  3, 5, , 1, Народний Фронт, Тетерук А.А., НФ, 5, 1, 3
;;;  1, 3, , 1, Народний Фронт, Сюмар В.П., НФ, 7, 1, 1
;;;  3, 5, , 1, Народний Фронт, Береза Ю.М., НФ, 10, 1, 3
;;;  1, 3, , 1, Народний Фронт, Пашинський С.В., НФ, 12, 1, 1
;;;  3, 7, , 1, Народний Фронт, Тимчук Д.Б., НФ, 13, 1, 3
;;;  3, 5, , 1, Народний Фронт, Денісова Л.Л., НФ, 15, 1, 3
;;;  1, 7, , 1, Народний Фронт, Іванчук А.В., НФ, 16, 2, 1
;;;  3, 5, , 1, Народний Фронт, Васюник І.В., НФ, 17, 1, 3
;;;  0, 0, , 0, Народний Фронт, Лук&#x27;янчук Р.В., НФ, 18, 0, 0
;;;  1, 7, , 1, Народний Фронт, Лунченко В.В., НФ, 19, 1, 1
;;;  0, 0, , 0, Народний Фронт, Донець Т.А., НФ, 20, 0, 0
;;;  1, 8, , 1, Народний Фронт, Геращенко А.Ю., НФ, 21, 1, 1
;;;  1, 3, , 1, Народний Фронт, Савчук Ю.П., НФ, 22, 2, 1
;;;  3, 5, , 1, Народний Фронт, Левус А.М., НФ, 23, 1, 3
;;;  0, 0, , 0, Народний Фронт, Дзензерський Д.В., НФ, 24, 0, 0
;;;  3, 5, , 1, Народний Фронт, Шкварилюк В.В., НФ, 25, 1, 3
;;;  1, 8, , 1, Народний Фронт, Матейченко К.В., НФ, 26, 1, 1
;;;  3, 5, , 1, Народний Фронт, Ледовських О.В., НФ, 27, 1, 3
;;;  3, 3, , 1, Народний Фронт, Колганова О.В., НФ, 28, 1, 1
;;;  3, 5, , 1, Народний Фронт, Фаєрмарк С.О., НФ, 29, 1, 3
;;;  3, 5, , 1, Народний Фронт, Сидорчук В.В., НФ, 30, 1, 3
;;;  3, 7, , 1, Народний Фронт, Заставний Р.Й., НФ, 31, 1, 3
;;;  1, 2, , 0, Народний Фронт, Дейдей Є.С., НФ, 32, 2, 2
;;;  3, 5, , 1, Народний Фронт, Княжицький М.Л., НФ, 34, 1, 3
;;;  3, 5, , 1, Народний Фронт, Бабенко В.Б., НФ, 35, 1, 3
;;;  3, 5, , 1, Народний Фронт, Ксенжук О.С., НФ, 36, 1, 3
;;;  1, 7, , 1, Народний Фронт, Логвинський Г.В., НФ, 37, 2, 1
;;;  3, 5, , 1, Народний Фронт, Єленський В.Є., НФ, 38, 1, 3
;;;  1, 2, , 1, Народний Фронт, Поляков М.А., НФ, 39, 2, 1
;;;  3, 5, , 1, Народний Фронт, Величкович М.Р., НФ, 40, 1, 3
;;;  3, 5, , 1, Народний Фронт, Горбунов О.В., НФ, 41, 1, 3
;;;  3, 5, , 1, Народний Фронт, Хміль М.М., НФ, 42, 1, 3
;;;  1, 8, , 1, Народний Фронт, Унгурян П.Я., НФ, 43, 1, 1
;;;  3, 5, , 1, Народний Фронт, Висоцький С.В., НФ, 44, 1, 3
;;;  3, 7, , 1, Народний Фронт, Войцеховська С.М., НФ, 46, 1, 3
;;;  3, 5, , 1, Народний Фронт, Бриченко І.В., НФ, 47, 1, 3
;;;  1, 2, , 1, Народний Фронт, Котвіцький І.О., НФ, 48, 2, 1
;;;  3, 5, , 1, Народний Фронт, Пинзеник П.В., НФ, 49, 1, 3
;;;  3, 5, , 1, Народний Фронт, Сочка О.О., НФ, 50, 1, 3
;;;  3, 5, , 1, Народний Фронт, Кадикало М.О., НФ, 51, 1, 3
;;;  3, 5, , 1, Народний Фронт, Масоріна О.С., НФ, 52, 1, 3
;;;  1, 7, , 1, Народний Фронт, Присяжнюк О.А., НФ, 53, 1, 1
;;;  3, 7, , 1, Народний Фронт, Кацер-Бучковська Н.В., НФ, 54, 1, 3
;;;  3, 5, , 1, Народний Фронт, Соляр В.М., НФ, 55, 1, 3
;;;  3, 5, , 1, Народний Фронт, Кремінь Т.Д., НФ, 59, 1, 3
;;;  3, 5, , 1, Народний Фронт, Кривенко В.В., НФ, 60, 1, 3
;;;  3, 5, , 1, Народний Фронт, Єфремова І.О., НФ, 63, 1, 3
;;;  3, 5, , 1, Народний Фронт, Алексєєв І.С., НФ, 64, 1, 3
;;;  3, 5, , 1, Народний Фронт, Бойко О.П., НФ, 66, 1, 3
;;;  3, 5, , 1, Народний Фронт, Корчик В.А., НФ, 67, 1, 3
;;;  3, 5, , 1, Народний Фронт, Помазанов А.В., НФ, 68, 1, 3
;;;  3, 5, , 1, Народний Фронт, Кривошея Г.Г., НФ, 69, 1, 3
;;;  3, 5, , 1, Народний фронт, Стеценко Д.О., НФ, 70, 1, 3
;;;  3, 5, , 1, Народний фронт, Драюк С.Є., НФ, 71, 1, 3
;;;  3, 5, , 1, Народний фронт, Романовський О.В., НФ, 72, 1, 3
;;;  3, 5, , 1, Народний фронт, Дроздик О.В., НФ, 74, 1, 3
;;;  3, 5, , 1, Народний фронт, Бабій Ю.Ю., НФ, 75, 1, 3
;;;  0, 2, , 0, Народний фронт, Підберезняк В.І., НФ, 76, 0, 2
;;;  0, 0, , 0, Народний фронт, Єдаков Я.Ю., НФ, 77, 0, 0
;;;  0, 0, , 0, Народний фронт, Мепарішвілі Х.Н., НФ, 78, 0, 0
;;;  0, 0, , 0, Народний фронт, Данілін В.Ю., НФ, 79, 0, 0
;;;  0, 0, , 0, Народний фронт, Бендюженко Ф.В., НФ, 80, 0, 0
;;;  6, 1, 31, 0, Самовисування, Павлов К.Ю., ОБ, , 0, 0
;;;  6, 1, 32, 0, Самовисування, Гальченко А.В., ОБ, , 0, 0
;;;  6, 1, 36, 0, Самовисування, Мартовицький А.В., ОБ, , 0, 0
;;;  6, 1, 37, 0, Самовисування, Шпенов Д.Ю., ОБ, , 0, 0
;;;  0, 0, 45, 0, Самовисування, Звягільський Ю.Л., ОБ, , 0, 0
;;;  0, 0, 47, 0, Опозиційний блок, Солод Ю.В., ОБ, , 0, 0
;;;  0, 0, 49, 0, Самовисування, Омельянович Д.С., ОБ, , 0, 0
;;;  6, 1, 57, 0, Самовисування, Матвієнков С.А., ОБ, , 0, 0
;;;  6, 1, 59, 0, Самовисування, Сажко С.М., ОБ, , 0, 0
;;;  6, 0, 80, 0, Самовисування, Балицький Є.В., ОБ, , 0, 0
;;;  0, 0, 106, 0, Опозиційний блок, Бакулін Є.М., ОБ, , 0, 0
;;;  6, 1, 107, 0, Самовисування, Дунаєв С.В., ОБ, , 0, 0
;;;  0, 0, 112, 0, Самовисування, Іоффе Ю.Я., ОБ, , 0, 0
;;;  0, 0, 135, 0, Самовисування, Ківалов С.В., ОБ, , 0, 0
;;;  0, 0, 176, 0, Самовисування, Шенцев Д.О., ОБ, , 0, 0
;;;  0, 0, 178, 0, Самовисування, Добкін Д.М., ОБ, , 0, 0
;;;  0, 0, , 0, Опозиційний блок, Бойко Ю.А., ОБ, 1, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Вілкул О.Ю., ОБ, 2, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Добкін М.М., ОБ, 3, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Рабінович В.З., ОБ, 4, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Білий О.П., ОБ, 5, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Ларін С.М., ОБ, 6, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Шуфрич Н.І., ОБ, 7, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Королевська Н.Ю., ОБ, 8, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Бахтеєва Т.Д., ОБ, 9, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Скорик М.Л., ОБ, 10, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Новинський В.В., ОБ, 11, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Льовочкін С.В., ОБ, 12, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Воропаєв Ю.М., ОБ, 13, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Козак Т.Р., ОБ, 14, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Гусак В.Г., ОБ, 15, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Льовочкіна Ю.В., ОБ, 16, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Кісельов А.М., ОБ, 17, 0, 0
;;;  0, 0, , 0, Опозиційний блок, Мирний І.М., ОБ, 18, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Мороко Ю.М., ОБ, 19, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Мірошниченко Ю.Р., ОБ, 20, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Колєсніков Д.В., ОБ, 21, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Папієв М.М., ОБ, 22, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Долженков О.В., ОБ, 23, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Павленко Ю.О., ОБ, 24, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Німченко В.І., ОБ, 25, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Нечаєв О.І., ОБ, 26, 0, 0
;;;  6, 1, , 0, Опозиційний блок, Шурма І.М., ОБ, 27, 0, 0
;;;  2, 6, 116, 1, Об&#x27;єднання «Самопоміч», Подоляк І.І., ОСП, , 2, 4
;;;  2, 6, 163, 1, Самовисування, Пастух Т.Т., ОСП, , 2, 4
;;;  2, 2, , 0, Об&#x27;єднання «Самопоміч», Семенченко С.І., ОСП, 2, 2, 2
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Скрипник О.О., ОСП, 3, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Бабак А.В., ОСП, 8, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Данченко О.І., ОСП, 10, 2, 4
;;;  2, 6, , 0, Об&#x27;єднання «Самопоміч», Сотник О.С., ОСП, 11, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Лаврик О.В., ОСП, 12, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Соболєв Є.В., ОСП, 13, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Маркевич Я.В., ОСП, 14, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Мірошніченко І.В., ОСП, 15, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Журжій А.В., ОСП, 16, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Діденко І.А., ОСП, 18, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Острікова Т.Г., ОСП, 19, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Опанасенко О.В., ОСП, 20, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Сидорович Р.М., ОСП, 21, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Войціцька В.М., ОСП, 22, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Сисоєнко І.В., ОСП, 25, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Підлісецький Л.Т., ОСП, 26, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Семенуха Р.С., ОСП, 27, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Березюк О.Р., ОСП, 28, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Костенко П.П., ОСП, 29, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Романова А.А., ОСП, 30, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Кіраль С.І., ОСП, 32, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Зубач Л.Л., ОСП, 34, 2, 4
;;;  0, 0, 35, 0, Самовисування, Шипко А.Ф., ПВ, , 0, 0
;;;  0, 0, 50, 0, Самовисування, Гєллєр Є.Б., ПВ, , 0, 0
;;;  0, 0, 52, 0, Самовисування, Шкіря І.М., ПВ, , 0, 0
;;;  4, 2, 67, 1, Самовисування, Развадовський В.Й., ПВ, , 2, 2
;;;  0, 0, 70, 0, Самовисування, Ланьо М.І., ПВ, , 0, 0
;;;  0, 2, 100, 0, Самовисування, Березкін С.С., ПВ, , 0, 2
;;;  0, 2, 128, 0, Самовисування, Ільюк А.О., ПВ, , 0, 2
;;;  0, 0, 137, 0, Самовисування, Клімов Л.М., ПВ, , 0, 0
;;;  0, 0, 139, 0, Самовисування, Пресман О.С., ПВ, , 0, 0
;;;  1, 2, 140, 0, Самовисування, Гуляєв В.О., ПВ, , 2, 2
;;;  0, 0, 141, 0, Самовисування, Барвіненко В.Д., ПВ, , 0, 0
;;;  0, 2, 142, 0, Самовисування, Кіссе А.І., ПВ, , 0, 2
;;;  4, 3, 146, 1, Самовисування, Шаповалов Ю.А., ПВ, , 2, 2
;;;  0, 2, 147, 0, Самовисування, Кулініч О.І., ПВ, , 0, 2
;;;  0, 0, 168, 0, Самовисування, Писаренко В.В., ПВ, , 0, 0
;;;  0, 0, 170, 0, Самовисування, Святаш Д.В., ПВ, , 0, 0
;;;  0, 0, 171, 0, Самовисування, Хомутиннік В.Ю., ПВ, , 0, 0
;;;  0, 0, 172, 0, Самовисування, Мисик В.Ю., ПВ, , 0, 0
;;;  0, 0, 175, 0, Самовисування, Кацуба В.М., ПВ, , 0, 0
;;;  0, 2, 177, 0, Самовисування, Остапчук В.М., ПВ, , 0, 2
;;;  0, 0, 180, 0, Самовисування, Біловол О.М., ПВ, , 0, 2
;;;  0, 0, 191, 0, Самовисування, Бондар В.В., ПВ, , 0, 0
;;;  0, 0, 195, 0, Самовисування, Зубик В.В., ПВ, , 0, 0
;;;  0, 0, 196, 0, Самовисування, Бобов Г.Б., ПВ, , 0, 0
;;;  0, 0, 199, 0, Самовисування, Ничипоренко В.М., ПВ, , 0, 0
;;;  0, 0, 200, 0, Самовисування, Яценко А.В., ПВ, , 0, 0
;;;  0, 0, 23, 0, Українське об&#x27;єднання патріотів — УКРОП, Констанкевич І.М., ПФ, , 0, 0
;;;  6, 1, 24, 0, Самовисування, Безбах Я.Я., ПФ, , 0, 0
;;;  0, 0, 26, 0, Блок Петра Порошенка, Денисенко А.С., ПФ, , 0, 0
;;;  0, 0, 29, 0, Блок Петра Порошенка, Купрій В.М., ПФ, , 0, 0
;;;  0, 2, 30, 0, Блок Петра Порошенка, Дубінін О.І., ПФ, , 2, 2
;;;  0, 0, 39, 0, Правий сектор, Ярош Д.А., ПФ, , 0, 0
;;;  2, 6, 40, 1, Блок Петра Порошенка, Дідич В.В., ПФ, , 2, 4
;;;  0, 0, 46, 0, Самовисування, Клюєв С.П., ПФ, , 0, 0
;;;  0, 0, 58, 0, Самовисування, Тарута С.О., ПФ, , 0, 0
;;;  3, 5, 62, 1, Блок Петра Порошенка, Розенблат Б.С., ПФ, , 1, 3
;;;  0, 2, 65, 0, Самовисування, Литвин В.М., ПФ, , 0, 2
;;;  0, 0, 69, 0, Самовисування, Балога В.І., ПФ, , 0, 0
;;;  0, 0, 71, 0, Самовисування, Балога П.І., ПФ, , 0, 0
;;;  0, 2, 73, 0, Самовисування, Балога І.І., ПФ, , 2, 2
;;;  1, 7, 83, 1, Блок Петра Порошенка, Шевченко О.Л., ПФ, , 2, 1
;;;  0, 0, 85, 0, Українське об&#x27;єднання патріотів — УКРОП, Шевченко В.Л., ПФ, , 0, 0
;;;  0, 0, 87, 0, Воля, Дерев&#x27;янко Ю.Б., ПФ, , 0, 0
;;;  2, 6, 90, 0, ВО «Свобода», Марченко О.О., ПФ, , 2, 4
;;;  0, 2, 98, 0, Самовисування, Міщенко С.Г., ПФ, , 2, 2
;;;  2, 6, 115, 1, Самовисування, Добродомов Д.Є., ПФ, , 2, 4
;;;  2, 8, 121, 1, Самовисування, Матківський Б.М., ПФ, , 2, 4
;;;  2, 6, 122, 0, Самовисування, Парасюк В.З., ПФ, , 2, 4
;;;  1, 7, 123, 1, Блок Петра Порошенка, Батенко Т.І., ПФ, , 2, 1
;;;  2, 6, 124, 1, Самовисування, Мусій О.С., ПФ, , 2, 4
;;;  0, 0, 133, 0, Самовисування, Матвійчук Е.Л., ПФ, , 0, 0
;;;  2, 6, 145, 0, ВО «Свобода», Бублик Ю.В., ПФ, , 2, 4
;;;  0, 0, 150, 0, Самовисування, Жеваго К.В., ПФ, , 0, 0
;;;  2, 6, 152, 0, ВО «Свобода», Осуховський О.І., ПФ, , 2, 4
;;;  0, 0, 159, 0, Самовисування, Деркач А.Л., ПФ, , 0, 0
;;;  2, 6, 164, 0, ВО «Свобода», Головко М.Й., ПФ, , 2, 4
;;;  0, 0, 173, 0, Самовисування, Денисенко А.П., ПФ, , 0, 0
;;;  0, 0, 181, 0, Самовисування, Мураєв Є.В., ПФ, , 0, 0
;;;  0, 0, 192, 0, Самовисування, Герега О.В., ПФ, , 0, 2
;;;  3, 5, 194, 1, Блок Петра Порошенка, Петренко О.М., ПФ, , 1, 3
;;;  2, 6, 197, 1, Блок Петра Порошенка, Голуб В.В., ПФ, , 2, 4
;;;  0, 0, 206, 0, Самовисування, Микитась М.В., ПФ, , 0, 0
;;;  0, 2, 213, 0, Самовисування, Береза Б.Ю., ПФ, , 2, 2
;;;  2, 6, 214, 1, Блок Петра Порошенка, Чумак В.В., ПФ, , 2, 4
;;;  2, 6, 215, 0, ВО «Свобода», Іллєнко А.Ю., ПФ, , 2, 4
;;;  0, 2, 216, 0, Самовисування, Супруненко О.І., ПФ, , 2, 2
;;;  0, 0, 217, 0, Самовисування, Білецький А.Є., ПФ, , 0, 0
;;;  1, 2, 220, 0, Народний Фронт, Константіновський В.Л., ПФ, , 2, 2
;;;  2, 6, 223, 0, ВО «Свобода», Левченко Ю.В., ПФ, , 2, 4
;;;  3, 5, , 1, Народний Фронт, Парубій А.В., ПФ, 4, 1, 3
;;;  1, 8, , 1, Блок Петра Порошенка, Геращенко І.В., ПФ, 9, 2, 1
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Гопко Г.М., ПФ, 1, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Сироїд О.І., ПФ, 4, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Веселова Н.В., ПФ, 9, 2, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Єднак О.В., ПФ, 17, 1, 4
;;;  2, 6, , 1, Об&#x27;єднання «Самопоміч», Пташник В.Ю., ПФ, 23, 2, 4
;;;  0, 0, , 0, Радикальна партія, Заружко В.Л., ПФ, 28, 0, 0
;;;  0, 0, , 0, ВО «Батьківщина», Савченко Н.В., ПФ, 1, 0, 0
;;;  5, 4, , 1, Радикальна партія, Ляшко О.В., РПОЛ, 1, 2, 1
;;;  5, 4, , 0, Радикальна партія, Лозовой А.С., РПОЛ, 2, 2, 1
;;;  5, 4, , 1, Радикальна партія, Шухевич Ю.Р., РПОЛ, 5, 2, 1
;;;  5, 4, , 1, Радикальна партія, Попов І.В., РПОЛ, 6, 2, 1
;;;  5, 4, , 1, Радикальна партія, Вітко А.Л., РПОЛ, 7, 2, 1
;;;  5, 4, , 1, Радикальна партія, Мосійчук І.В., РПОЛ, 9, 2, 1
;;;  5, 4, , 1, Радикальна партія, Галасюк В.В., РПОЛ, 10, 2, 1
;;;  5, 4, , 1, Радикальна партія, Ленський О.О., РПОЛ, 11, 2, 1
;;;  5, 4, , 1, Радикальна партія, Силантьєв Д.О., РПОЛ, 12, 2, 1
;;;  5, 4, , 1, Радикальна партія, Кошелєва А.В., РПОЛ, 13, 2, 1
;;;  5, 4, , 1, Радикальна партія, Купрієнко О.В., РПОЛ, 17, 2, 1
;;;  5, 4, , 1, Радикальна партія, Кириченко О.М., РПОЛ, 18, 2, 1
;;;  5, 4, , 1, Радикальна партія, Скуратовський С.І., РПОЛ, 19, 2, 1
;;;  5, 4, , 1, Радикальна партія, Лінько Д.В., РПОЛ, 20, 2, 1
;;;  5, 4, , 1, Радикальна партія, Амельченко В.В., РПОЛ, 21, 2, 1
;;;  5, 4, , 1, Радикальна партія, Рибалка С.В., РПОЛ, 22, 2, 1
;;;  5, 4, , 1, Радикальна партія, Вовк В.І., РПОЛ, 23, 2, 1
;;;  5, 4, , 1, Радикальна партія, Корчинська О.А., РПОЛ, 24, 2, 1
;;;  5, 4, , 1, Радикальна партія, Чижмарь Ю.В., РПОЛ, 25, 2, 1
;;;  0, 2, , 0, Радикальна партія, Юзькова Т.Л., РПОЛ, 26, 0, 2
;;; )
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
