(ns t.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def characters
  [\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z])

(def words
  (->> (io/resource "words.txt")
       io/reader
       line-seq
       (map (fn [^String s] (.toLowerCase s)))
       (into #{})))

(def strips
  {247 {:key "ambecudlebfig?hyiojvkpltmtnro?peqhrasftcunvswgxwydzk"
        :secret ["Mnbrvcnp!"
                 "Wiup! Rdd is fm, wiup! F tru'm epdfpjp fm'v rdd wiup!"
                 "F wim prmpu eh r ynrwiu sin mqrm snprzfu' mnbrvcnp!"
                 "F wim jiafmpy ck! Eh r yfvwcvlfuw rtfy-enprmqfuw ynrwiu! Ruy uix fm'v WIUP!"
                 "Uim MQP diim. AH diim. Afup!"
                 "Xrfm, xqh yi F vicuy scuuh? Qpddi? Qpddi?"
                 "Xqrm? Ui! Yiu'm hic yrnp!"
                 "Vuprz rmmrtz eiim mi mqp srtp!"
                 "Yrnu vmnrfwqm."]}
   248 {:key "a?bgcpdmesfxgehbilj?kul?mondoap?qcrisntruyvhwfx?ytzw"
        :secret ["Ry dgose du ecggqv rt oii wkssu-emksnrsb."
                 "Qmdg obors?"
                 "Qos emdghmnu cigoeg bgy zggcu ytoseegfkoi mww dg?"]}
   249 {:key "ahb?crdue?fbg?hyioj?kil?mgn?o?pyqermsrtau?v?w?x?y?zt"
        :secret ["Rthfq pid'sq ckmaz..."]}
   251 {:key "atb?c?dseufeg?h?i?jikcl?m?n?oyplq?r?shtau?v?wnxgyrzo"
        :secret ["Oze ktpp ja asf—"
                 "Afw xzpd dtod dsf kyjfd"
                 "Ofts"]}
   260 {:key "aybfc?d?erfog?h?i?j?kpl?men?o?p?q?rbs?tnucv?waxmy?z?"
        :secret ["Ufxm ft, rwra, fkmt bfe xfxxa..."]}
   262 {:key "a?becidnetfaglhwikjykqlmm?npo?poqgrssct?urvzwdxhyvzu"
        :secret ["Jpz rezncw gpsi! Pnbd! Hxj hpd'e jpz pnbd??"
                 "Qpcdq ep icgg jpz..."
                 "C xfeb Flfvcdq-kzfgcej gpsir!"
                 "Qbb, exfdir, dp nubrrzub exbub."
                 "Wfld ce! PNBD!!"
                 "C fl epefggj cd gpyb hcex jpz."]}
   264 {:key "awb?c?dte?fbgch?i?jkk?l?min?ohp?qarls?tsu?v?wexny?z?"
        :secret ["Aqmd. Rmtdwx!"
                 "Fqgj mx dow gwrrt! Fqgj! FQGJ!!"]}
   268 {:key "arbgc?dre?f?g?h?i?j?k?l?m?n?o?p?qbr?s?t?u?v?wsxoy?z?"
        :secret ["Qddd, baxww."]}
   269 {:key "afb?ctd?e?f?g?h?i?j?k?l?m?ncoip?q?ras?t?u?v?wnxsy?z?"
        :secret ["Arwcrxcon!"]}
   275 {:key "a?b?c?d?e?f?g?h?i?j?k?l?m?n?o?p?q?r?s?t?u?vhw?xuy?z?"
        :secret ["Vxv?"]}
   277 {:key "aqbwc?d?e?frg?h?i?j?k?lkmcnbo?p?qer?s?t?u?v?w?x?y?za"
        :secret ["Zfq bq nzml??"]}
   284 {:key "avbicgdyecfng?hbifjpkhlum?nmokprqdr?sstauevtwlxoynzw"
        :secret ["Zktv?"
                 "Fx, ztbv, vktv aupqbev qxusf'v ntou sufsu. Qtzs qxf'v zxpo vktv ztd."
                 "B'au suuf ufxlck xi nd Vkbuaus' Clbwq ipbufqs sufv vx jpbsxf vx ofxz vktv vkbs skxlwq ktau huuf t exfabevbxf."
                 "Hlv... hlv bv qxusf'v ntou tfd sufsu!"
                 "Fx! B'n fxt XO, vkbs zkxwu vpbtw ntqu FX SUFSU!"
                 "Zu ZUPU clbwvd! Bv skxxlwq ktau huuf t clbwvd aupqbev! B etf lfqupsvtfq bi vkud ztfvuq vx puqleu vku sufvufeu qlu vx vku ebpelnsvtfeus, hlv bv skxlwq ktau svbww huuf t clbwvd aupqbev."
                 "Xk, fuaup nbfq, B qxf'v ofxz zkd B'n hxvkupbfc."
                 "Ztbv—Uwtf, zktv qbq dxl rlsv std?"
                 "Xk nd cxqs. Uwtf! Vktv's bv! Uwtf, dxl'pu t cufbls!"]}
   285 {:key "acb?cfddetfhgehli?j?kmlwmdn?orpkqnrysotuuiv?wsxjygza"
        :secret ["Rst azq'e tqmgowezqm kg, ws hge kg xtwe wzr efze rst'og z couyum lueaf zqd rsto efuyfw hssp cze uq efze zokso."]}
   291 {:key "aobacsdte?figgh?irj?k?lem?n?oypwqurds?t?u?v?w?xhy?z?"
        :secret ["F dxaqgxd oaq cbfr oaqi rbr pbc rlbr?"]}
   296 {:key "anb?c?dve?fbgehui?jmkglsm?n?otplq?r?sotruhv?waxcyizf"
        :secret ["Fgxwhlg gdga Y'j lyxh sz otwwlpwoyak ouglg ouyakl."]}
   309 {:key "abbsctdoedfrguhai?jvkhlcmwnio?pgqxrls?tkumvywnx?yezp"
        :secret ["Yrhw, N'u nw rdjy mnck vdg."
                 "Yrhw, N'u nw rdjy mnck vdg. Lduzrycyrv nw rdjy."
                 "Rdjy, rdjy, rdjy, rdjy!"
                 "Uv ehe nb aynwp kyre fhwbdu av hw yjnr enlchcdf."
                 "N'u wdc fyhrrv nw cky Cknyjtb' Pgnre hwvudfy."
                 "N lkyhc hc bdrnchnfy."
                 "N khjy h chccdd vdg'jy wyjyf byyw."
                 "N tnbbye h pnfr dwly."
                 "DT, DT, udfy ckhw dwly!"
                 "Yrhw, nc cgfwb dgc N uhv wdc ay yqhlcrv mkhc vdg mdgre lhrr—"]}
   330 {:key "apbac?dheqfgg?hsicjuk?l?mnn?orpyqtris?t?udvmwexoy?zk"
        :secret ["R fjwhh qdwow'h mx axrmq rm vw bhzrmf b ejwhqrxm."
                 "Pxj ibm jmuwohqbmu vw??"
                 "Djd?"]}
   331 {:key "a?bwc?d?etfhg?h?iyjekslpmin?o?pnqards?tou?v?wmx?y?zc"
        :secret ["Bfqe zqp M rt et fjketfj wi kljjzf?"]}
   340 {:key "afboc?d?enfwgdhai?jmkilsmlncogp?qrrhsktuupvtwjxey?z?"
        :secret ["Vrqxx rtegqxg abqvo lvqkul hte fx'qx LVKMM jhskeo nrxhu qtmx qxyklkbe wbsxl... Obgl, fx ltns."]}
   351 {:key ""
        :secret ["Gpmp, Vzrra vzrra vzrra."]}
   361 {:key "afbhckdaegf?gshiiojdkrl?mnntolpcqwrysetpu?v?wuxmy?z?"
        :secret ["Bior pkdt! Hn'g Mdos!"
                 "Gnit ohgnsmhme ni riwkgsoa gtsdc aik imps dmj OIIC qbsks H'x GBIINHME!"]}
   384 {:key "asbhcudoedfmgfhkinjvk?l?m?n?o?pyqwrcsjtrubvewpxaytzi"
        :secret ["Za ybxy acwwdave yd uv adfv adty dg sdhv?"
                 "Uvrxcav Z rxi'y awvxh? Zg ad, zy qxai'y jvtp gciip."]}
   393 {:key "ambicfdyeofaglh?ibjpkwlemnncokpiqvrgsut?udvswtxryhz?"
        :secret ["MFGL?? P opvvlu Mfgl? Sry!!"
                 "Keeeeee! Opno ypv fvv, Euugd Lcclnwpql Lgfm!"
                 "Ysy?"
                 "Des'ql rew we il obuupmr al. Lgfm, des'xl mew isdpmr wypv, fxl des???"
                 "Ufam vwxfpryw, yl'v gdpmr!"
                 "Yl iesryw al wypv uxlvv! Neal em, kesgu FMD keafm NYEEVL we klfx wypv wypmr?"
                 "Me! P'a mew! P'a vwsjpu! P'a ve vwsjpu!!"
                 "Ec nesxvl mew! Wypv pv fivsxu! P'a Nyfewpn Reeu! Pvy!"
                 "Kyd fxl des gpvwlmpnr we ypa?? Yl'v wyl QPGGFPM!"
                 "Me, P yfqlm'w!"
                 "Des yfql we ilgplql ql! Yl'v gdpmr!!"
                 "Kyfw?? Me! ME!!"
                 "P nfm'w..."
                 "...wypmo..."
                 "P nfm'w gevl ypa..."
                 "P-P wyesryw..."
                 "P wyesryw—"]}})

(defn extend-dict
  [cmap]
  (merge cmap
         (into {} (for [[k v] cmap :when v]
                    [(Character/toUpperCase (.charValue k)) (Character/toUpperCase (.charValue v))]))))

(defn print-strip
  [s d]
  (let [d (extend-dict d)]
    (->> s
         (map (fn [line]
                (str line "\n" (string/escape line d))))
         (string/join "\n\n")
         println)))

(defn print-dict
  [d]
  (println (->> d sort (map (partial apply str)) (apply str)))
  (println (->> d vals (remove #{\?}) sort (apply str))))

(defn p->n
  [fixed? p]
  (loop [p p d {} n []]
    (cond
      (empty? p) n
      (fixed? (first p)) (recur (rest p) d (conj n (first p)))
      (d (first p)) (recur (rest p) d (conj n (d (first p))))
      :else (recur (rest p) (assoc d (first p) (count d)) (conj n (count d))))))

(defn find-words
  [^String pat forb]
  (let [p (.toLowerCase pat)
        fixed? (->> pat (filter #(Character/isUpperCase (.charValue %))) (map #(Character/toLowerCase (.charValue %))) set)
        p (p->n fixed? p)
        c (count pat)]
    (->> words
         (filter #(== c (.length ^String %)))
         (filter (fn [w]
                   (and (= p (p->n fixed? w))
                        (->> w (remove fixed?) (filter forb) empty?)))))))

(def empty-dic (into {} (map #(-> [% \?]) characters)))

(defn decrypt
  [i]
  (printf "Suggested key: %s\n" (get-in strips [i :key]))
  (loop [dict empty-dic ws () i i]
    (print "\n\n")
    (print-dict dict)
    (println)
    (print-strip (get-in strips [i :secret]) dict)
    (print "?> ") (flush)
    (let [^String r (read-line)]
      (cond
        (= r ".")
        (do (prn (take 5 ws))
            (recur dict (drop 5 ws) i))
        (= r ":k")
        (recur (->> (get-in strips [i :key])
                    (partition 2)
                    (map vec)
                    (reduce conj {})) ws i)
        (= r ":q")
        nil
        (.startsWith r ":f ")
        (let [ws (shuffle (find-words (subs r 3 (count r)) (->> dict vals set)))]
          (prn (take 5 ws))
          (recur dict (drop 5 ws) i))
        (.startsWith r ":g ")
        (recur dict ws (Long/parseLong (subs r 3 (count r))))
        (= r ":r") (recur empty-dic () i)
        (when-let [[_ a b] (re-matches #"^([a-z]+) ([a-z]+)$" r)] (= (count a) (count b)))
        (let [[_ a b] (re-matches #"^([a-z]+) ([a-z]+)$" r)]
          (recur (->> (map vector a b) (reduce conj dict))
                 ws
                 i))
        (not (zero? (mod (count r) 2)))
        (recur dict ws i)
        :else
        (recur (->> r
                    (partition 2)
                    (map vec)
                    (reduce conj dict))
               ws
               i)))))
