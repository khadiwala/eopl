(require 'clojure.set)

(defn add-args [bindings args]
  (reduce conj bindings
          (map-indexed
            (fn [x y] {[0 x] y}) args)))

(defn inc-pos [bindings]
  (into
    {}
    (map
      (fn [p]
        (let [ x (first p) y (second p)]
          {[(-> x first inc) (second x)] y}))
      bindings)))

(defn unlexical-address [bound exp]
  (cond
   (empty? exp) '()
   (= (first exp) '|) (let [[_ d p] exp] (bound [d p]))
   (= (second exp) 'free) (first exp)
   (= (first exp) 'lambda)
     (list
       (first exp)
       (second exp)
       (unlexical-address
         (add-args (inc-pos bound) (second exp))
         (-> exp rest rest)))
   (= (first exp) 'if)
   (list
    (first exp)
    (unlexical-address bound (nth exp 1))
    (unlexical-address bound (nth exp 2))
    (unlexical-address bound (nth exp 3)))
   :else
   (let [firs (unlexical-address bound (first exp))]
    (if (empty? (rest exp))
     firs
     (list firs (unlexical-address bound (second exp)))))))
