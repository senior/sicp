(ns sicp.chapter1_1)

(defn recursive-1-11 [n]
  (if (< n 3) 
    n
    (+ (recursive-1-11 (- n 1))
       (recursive-1-11 (- n 2))
       (recursive-1-11 (- n 3)))))
(def memo-1-11 (memoize recursive-1-11))

(defn iterative-1-11 [n]
  (letfn (iter-1-11-inner [[n3 n2 n1]]
			 [n2 n1 (+ n1 n2 n3)])
	(nth (map first (iterate iter-1-11-inner [ 1 2 3 ] (- n 1))))))


