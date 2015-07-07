

(defn bintree-to-list [tree]
  (cond
    (= (first tree) 'interior-node)
      (let [[f v l r] tree] (concat (list f) (list v) (bintree-to-list l) (bintree-to-list r)))
    (= (first tree) 'leaf)
      (list (first tree) (second tree))))