(require 'clojure.set)

(def getvar (comp first second))
(defn generic [bound free exp]
  (if (seq? exp)
    (if (empty? exp)
      #{}
      (if (= (first exp) 'lambda)
        (generic (conj bound (getvar exp)) free ((comp rest rest) exp))
        (clojure.set/union
          (generic bound free (first exp))
          (if (empty? (rest exp))
            #{}
            (generic bound free (second exp))))))
    (if (not (= free (contains? bound exp)))
        #{exp}
        #{})))

(def occurs_free (partial generic #{} true))
(def occurs_bound (partial generic #{} false))

