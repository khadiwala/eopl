
(require 'clojure.set)

(defn recred [params exp]
    (cond
      (not (seq? exp)) true
      (empty? exp) true
      (= (first exp) 'lambda)
      (let [[_ ps s] exp]
        (recred (clojure.set/union (set ps) params) s))
      (= (first exp) 'if)
      (let [[_ c b1 b2] exp]
        (and
          (recred params c)
          (recred params b1)
          (recred params b2)))
      :else
      (let [[b1 b2] exp]
        (and
          (recred params b1)
          (recred params b2)))
      )
    )

(defn redeclared? [exp]
    (recred #{} exp))
