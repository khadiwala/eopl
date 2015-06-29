(defn ofree [exp bound]
  (if (empty? exp)
    #{}
    (if (= (head exp) 'lambda)
      (ofree ((compose rest rest) exp) (conj bound (getvar exp)))


(defn getvar [exp] (head ((compose head rest) exp)))