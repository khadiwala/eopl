(require 'clojure.set)


(defn add-args [bindings args]
  (reduce conj bindings
    (map-indexed
      (fn [x y] {y [0 x]}) args)))

(defn inc-pos [bindings]
  (into {} (map (fn [p] (let [x (first p) y (second p)] {x [(-> first inc) (second y)]})) bindings)))

(defn lexical-address [bound exp]
  (if (seq? exp)
    (cond
      (empty? exp) '()
      (= (first exp) 'lambda)
        (list (first exp)
          (second exp)
          (lexical-address
            (add-args
              (inc-pos bound)
              (second exp))
            ((comp rest rest) exp)))
      (= (first exp) 'if)
        (list
          (first exp)
          (lexical-address bound (nth exp 1))
          (lexical-address bound (nth exp 2))
          (lexical-address bound (nth exp 3)))
      :else
        (let [firs (lexical-address bound (first exp))]
          (if (empty? (rest exp))
            firs
            (list firs (lexical-address bound (second exp))))))

    (if (contains? bound exp)
      (let [x (bound exp)]
        (list exp '| (first x) (second x)))
      (list 'free exp))))


