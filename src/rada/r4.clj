;; gorilla-repl.fileformat = 1

;; **
;;; # Розподіл депутатів Верховної Ради України за результатами голосувань
;;; 
;;; 
;; **

;; **
;;; Необхідні функції для считування та розпізнання результатів голосувань з 2015 р.
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
(постійна депутати-по-фракціях-ВР-04012018 
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
                             :номер-у-списку (-> рядок (.отримай 4) .text)
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

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-2-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-2-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(постійна депутати-по-3-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-3-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-3-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(постійна депутати-по-5-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 5))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-5-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-5-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(постійна депутати-по-8-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 8))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-8-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-8-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(постійна депутати-по-10-кластерах-згідно-голосувань-10012018_1
           (депутати-по-кластерах голосування-05012018_1 прізвища-депутатів-без-Тимошенко-10012018 10))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/депутати-по-10-кластерах-згідно-голосувань-10012018_1</span>","value":"#'user/депутати-по-10-кластерах-згідно-голосувань-10012018_1"}
;; <=

;; @@
(постійна розподіл-фракцій-10012018_1 (->> 
     депутати-по-фракціях-ВР-04012018
     (фільтруй #(не (= % "Тимошенко Ю.В." (:фіо %))))
     (відобрази #(merge % { 
                    :на-два-кластери-за-голосуваннями   (:фракція-за-результатами-голосувань (перший (знайди-за-фіо  (:фіо %) 
                                                                                       депутати-по-2-кластерах-згідно-голосувань-10012018_1)))
                    :на-три-кластерів-за-голосуваннями    (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-3-кластерах-згідно-голосувань-10012018_1)))
                    :на-п'ять-кластерів-за-голосуваннями  (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-5-кластерах-згідно-голосувань-10012018_1)))
                    :на-вісім-кластерів-за-голосуваннями  (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                                       депутати-по-8-кластерах-згідно-голосувань-10012018_1)))
                    :на-десять-кластерів-за-голосуваннями (:фракція-за-результатами-голосувань (перший (знайди-за-фіо (:фіо %)
                                                                        депутати-по-10-кластерах-згідно-голосувань-10012018_1)))
                })
     )
))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/розподіл-фракцій-10012018_1</span>","value":"#'user/розподіл-фракцій-10012018_1"}
;; <=

;; @@
(постійна фракції-ВР-10012018_1 (distinct (відобрази :фракція розподіл-фракцій-10012018_1)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/фракції-ВР-10012018_1</span>","value":"#'user/фракції-ВР-10012018_1"}
;; <=

;; @@
(для-всіх [f фракції-ВР-10012018_1]  
    (->> депутати-по-2-кластерах-згідно-голосувань-10012018_1
         (фільтруй #(= 0 (:фракція-за-результатами-голосувань %)) )
         (фільтруй #(= f (:фракція (перший (знайди-за-фіо (:фіо %) депутати-по-фракціях-ВР-04012018)))))
         розмір
    ))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-unkown'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-unkown'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>2</span>","value":"2"}],"value":"(23 9 17 35 43 24 2 5 2)"}
;; <=

;; @@
(:фракція (перший (знайди-за-фіо "Домбровський О.Г." депутати-по-фракціях-ВР-04012018)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;БПП&quot;</span>","value":"\"БПП\""}
;; <=

;; @@
(let [nr (range 2)]
(table-view 
  (map (fn [n]
    [
       (html-view(str "Група номер " (+ 1 n)))
       ( bar-chart
         фракції-ВР-10012018_1
         (для-всіх [f фракції-ВР-10012018_1]  
                   (/ 
                     (->> депутати-по-2-кластерах-згідно-голосувань-10012018_1
                          (фільтруй #(= n (:фракція-за-результатами-голосувань %)) )
                          (фільтруй #(= f (:фракція (перший (знайди-за-фіо (:фіо %) депутати-по-фракціях-ВР-04012018)))))
                          розмір      )
                     (->>  депутати-по-2-кластерах-згідно-голосувань-10012018_1
                           (фільтруй #(= f (:фракція (перший (знайди-за-фіо (:фіо %) депутати-по-фракціях-ВР-04012018)))))
                           розмір)
                     )
                   )
         ) 
       ]) nr)
  :columns [(html-view "розподіл Фракцій на дві групи") (html-view "розподіл Фракцій")])
  )
;; @@
;; =>
;;; {"type":"list-like","open":"<center><table>","close":"</table></center>","separator":"\n","items":[{"type":"list-like","open":"<tr><th>","close":"</th></tr>","separator":"</th><th>","items":[{"type":"html","content":"розподіл Фракцій на дві групи","value":"#gorilla_repl.html.HtmlView{:content \"розподіл Фракцій на дві групи\"}"},{"type":"html","content":"розподіл Фракцій","value":"#gorilla_repl.html.HtmlView{:content \"розподіл Фракцій\"}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"розподіл Фракцій на дві групи\"} #gorilla_repl.html.HtmlView{:content \"розподіл Фракцій\"}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"Група номер 1","value":"#gorilla_repl.html.HtmlView{:content \"Група номер 1\"}"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"30429e67-8b5c-4db7-b8f2-136ce63a7381","values":[{"x":"БПП","y":0.1666666666666667},{"x":"НФ","y":0.1125},{"x":"ВН","y":0.9444444444444444},{"x":"ПФ","y":0.6730769230769231},{"x":"ОБ","y":1},{"x":"ПВ","y":0.9230769230769231},{"x":"ОСП","y":0.08},{"x":"ВОБ","y":0.2631578947368421},{"x":"РПОЛ","y":0.1}]}],"marks":[{"type":"rect","from":{"data":"30429e67-8b5c-4db7-b8f2-136ce63a7381"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"30429e67-8b5c-4db7-b8f2-136ce63a7381","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"30429e67-8b5c-4db7-b8f2-136ce63a7381","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :values ({:x \"БПП\", :y 1/6} {:x \"НФ\", :y 9/80} {:x \"ВН\", :y 17/18} {:x \"ПФ\", :y 35/52} {:x \"ОБ\", :y 1} {:x \"ПВ\", :y 12/13} {:x \"ОСП\", :y 2/25} {:x \"ВОБ\", :y 5/19} {:x \"РПОЛ\", :y 1/10})}], :marks [{:type \"rect\", :from {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"Група номер 1\"} #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :values ({:x \"БПП\", :y 1/6} {:x \"НФ\", :y 9/80} {:x \"ВН\", :y 17/18} {:x \"ПФ\", :y 35/52} {:x \"ОБ\", :y 1} {:x \"ПВ\", :y 12/13} {:x \"ОСП\", :y 2/25} {:x \"ВОБ\", :y 5/19} {:x \"РПОЛ\", :y 1/10})}], :marks [{:type \"rect\", :from {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<tr><td>","close":"</td></tr>","separator":"</td><td>","items":[{"type":"html","content":"Група номер 2","value":"#gorilla_repl.html.HtmlView{:content \"Група номер 2\"}"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"cd010a8d-209c-4a2c-8b82-45bdcfff43ad","values":[{"x":"БПП","y":0.8333333333333333},{"x":"НФ","y":0.8875},{"x":"ВН","y":0.05555555555555556},{"x":"ПФ","y":0.3269230769230769},{"x":"ОБ","y":0},{"x":"ПВ","y":0.07692307692307691},{"x":"ОСП","y":0.92},{"x":"ВОБ","y":0.7368421052631579},{"x":"РПОЛ","y":0.9}]}],"marks":[{"type":"rect","from":{"data":"cd010a8d-209c-4a2c-8b82-45bdcfff43ad"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"cd010a8d-209c-4a2c-8b82-45bdcfff43ad","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"cd010a8d-209c-4a2c-8b82-45bdcfff43ad","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :values ({:x \"БПП\", :y 5/6} {:x \"НФ\", :y 71/80} {:x \"ВН\", :y 1/18} {:x \"ПФ\", :y 17/52} {:x \"ОБ\", :y 0} {:x \"ПВ\", :y 1/13} {:x \"ОСП\", :y 23/25} {:x \"ВОБ\", :y 14/19} {:x \"РПОЛ\", :y 9/10})}], :marks [{:type \"rect\", :from {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[#gorilla_repl.html.HtmlView{:content \"Група номер 2\"} #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :values ({:x \"БПП\", :y 5/6} {:x \"НФ\", :y 71/80} {:x \"ВН\", :y 1/18} {:x \"ПФ\", :y 17/52} {:x \"ОБ\", :y 0} {:x \"ПВ\", :y 1/13} {:x \"ОСП\", :y 23/25} {:x \"ВОБ\", :y 14/19} {:x \"РПОЛ\", :y 9/10})}], :marks [{:type \"rect\", :from {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}],"value":"#gorilla_repl.table.TableView{:contents ([#gorilla_repl.html.HtmlView{:content \"Група номер 1\"} #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :values ({:x \"БПП\", :y 1/6} {:x \"НФ\", :y 9/80} {:x \"ВН\", :y 17/18} {:x \"ПФ\", :y 35/52} {:x \"ОБ\", :y 1} {:x \"ПВ\", :y 12/13} {:x \"ОСП\", :y 2/25} {:x \"ВОБ\", :y 5/19} {:x \"РПОЛ\", :y 1/10})}], :marks [{:type \"rect\", :from {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"30429e67-8b5c-4db7-b8f2-136ce63a7381\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}] [#gorilla_repl.html.HtmlView{:content \"Група номер 2\"} #gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :values ({:x \"БПП\", :y 5/6} {:x \"НФ\", :y 71/80} {:x \"ВН\", :y 1/18} {:x \"ПФ\", :y 17/52} {:x \"ОБ\", :y 0} {:x \"ПВ\", :y 1/13} {:x \"ОСП\", :y 23/25} {:x \"ВОБ\", :y 14/19} {:x \"РПОЛ\", :y 9/10})}], :marks [{:type \"rect\", :from {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"cd010a8d-209c-4a2c-8b82-45bdcfff43ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]), :opts (:columns [#gorilla_repl.html.HtmlView{:content \"розподіл Фракцій на дві групи\"} #gorilla_repl.html.HtmlView{:content \"розподіл Фракцій\"}])}"}
;; <=

;; @@
(range 2)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(0 1)</span>","value":"(0 1)"}
;; <=

;; @@

;; @@
