(ns 118)
(defn compose [f g]
  (fn [x]
    (f (g x))))

(defn wfind [s lis err]
  (if (empty? lis)
    err
    (if (= (first lis) s)
      'first
      (if (seq? (first lis))
        (let [firstrec (wfind s (first lis) err)]
          (if (= firstrec err)
            (let [restrec (wfind s (rest lis) err)]
              (if (= restrec err)
                err
                (list 'compose restrec 'rest)))
            (list 'compose firstrec 'first)))
            ;;firstrec))
        (let [restrec (wfind s (rest lis) err)]
          (if (= restrec err)
            err
            (list 'compose restrec 'rest)))
      ))))

(defn wfind2 [s lis err]
  (if (empty? lis)
    err
    (if (= (first lis) s)
      'first
      (if (seq? (first lis))
        (let [firstrec (wfind2 s (first lis) err)]
          (if (= firstrec err)
            (let [restrec (wfind2 s (rest lis) err)]
              (if (= restrec err)
                err
                (list 'fn '[x] (list restrec '(rest x)))))
            (list 'fn '[x] (list firstrec '(first x)))))
        (let [restrec (wfind2 s (rest lis) err)]
          (if (= restrec err)
            err
            (list 'fn '[x] (list restrec '(rest x)))))
        ))))

(def a 20)

