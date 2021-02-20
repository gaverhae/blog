(ns t.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defmacro make-todo [i j]
  `(let [arr# (int-array 2)]
     (aset arr# 0 (int ~i))
     (aset arr# 1 (int ~j))
     arr#))

;; This is done as a macro to avoid boxing to Integer or converting to long.
(defmacro add [todo imod imat]
  `(if (.get ~'seen (unchecked-add-int ~imod (unchecked-multiply-int ~imat ~'nmod)))
     ~todo
     (conj ~todo (make-todo ~imod ~imat))))

(defn abbr [^String to-modify ^String to-match]
  (let [nmod (int (count to-modify))
        nmat (int (count to-match))
        up (fn [^Character c] (Character/toUpperCase c))
        is-up? (fn [^Character c] (Character/isUpperCase c))
        seen (java.util.BitSet. (unchecked-multiply-int nmod nmat))]
    (loop [todo [(make-todo 0 0)]]
      (if (empty? todo) false
        (let [cur ^ints (peek todo)
              imod (int (aget cur 0))
              imat (int (aget cur 1))]
          (.flip seen (unchecked-add-int imod (unchecked-multiply-int imat nmod)))
          (cond
            (and (== imod nmod) (== imat nmat)) true
            (== imat nmat) (if (= (subs to-modify imod)
                                  (.toLowerCase (subs to-modify imod)))
                             true
                             (recur (pop todo)))
            (== imod nmod) (recur (pop todo))
            :else
            (let [cmod (.charAt to-modify imod)
                  cmat (.charAt to-match imat)]
              (cond
                (= cmod cmat) (recur (add (pop todo) (unchecked-inc-int imod) (unchecked-inc-int imat)))
                (is-up? cmod) (recur (pop todo))
                (not= (up cmod) cmat) (recur (add (pop todo) (unchecked-inc-int imod) imat))
                :else (recur (add (add (pop todo)
                                       (unchecked-inc-int imod) (unchecked-inc-int imat))
                                  (unchecked-inc-int imod) imat))))))))))

(defn -main
  [& _args]
  (let [out-file (get (System/getenv) "OUTPUT_PATH")
        num-of-items (Integer/parseInt (clojure.string/trim (read-line)))]
    (with-open [out (io/writer out-file)]
      (dotimes [i num-of-items]
        (let [to-modify (read-line)
              to-match (read-line)]
          (.write out
                  (if (abbr to-modify to-match)
                    "YES\n"
                    "NO\n")))))))

; uncomment when pasting to Hackerrank
; (-main)

(comment
  (def in
    (with-open [f (io/reader "../test_data/input/13")]
      (->> f
           line-seq
           (drop 1)
           (partition 2 2)
           vec)))
  (def exp
    (with-open [f (io/reader "../test_data/output/13")]
      (->> f
           line-seq
           (map {"YES" true "NO" false})
           vec)))
  (defmacro timed [expr]
    `(let [start# (System/nanoTime)
           _# ~expr]
       (format "%9.6f" (/ (- (System/nanoTime) start#) 1000000.0))))


  (mapv (fn [_] (timed (doseq [[mod mat] in] (abbr mod mat))))
        (range 100))
["859.559049" "805.115721" "693.200448" "824.420480" "710.022214" "858.807406" "757.194399" "702.251565" "675.896818" "652.620444" "682.654010" "675.934967" "681.390973" "662.156458" "659.232768" "671.884114" "679.655591" "671.691157" "683.484534" "672.409882" "684.587291" "672.365704" "672.301961" "670.426442" "672.343169" "683.237410" "680.863402" "656.268823" "712.247830" "684.823526" "672.623836" "684.941528" "670.280300" "671.897126" "680.577217" "689.347896" "666.935214" "680.818871" "667.818404" "676.096327" "676.530944" "681.321779" "677.597600" "703.552185" "693.910075" "716.852827" "700.253582" "713.949718" "675.970157" "692.410960" "684.782811" "767.788522" "690.608519" "677.268815" "674.548389" "688.845014" "713.058926" "711.557973" "736.021980" "677.897556" "676.536948" "698.789778" "675.968131" "677.990347" "668.509567" "693.045300" "737.782803" "675.392815" "678.656412" "678.792668" "701.016625" "678.051347" "679.275707" "701.578698" "688.153383" "705.376557" "684.527645" "665.441870" "678.040414" "677.786000" "699.304591" "693.349714" "697.336995" "674.852535" "671.722564" "682.543641" "705.936094" "699.890569" "692.531820" "670.801738" "681.614390" "675.695975" "670.051843" "676.337212" "673.458143" "702.749127" "712.568487" "677.351684" "672.166795" "678.954619"]
)
