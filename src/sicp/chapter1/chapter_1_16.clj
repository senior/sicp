(ns sicp.chapter-1-16)

(defn square [x]
  (* x x))

(defn fast-expt-iter [b n]
  (letfn [(inner-fast-expt [a n result]
			  (cond (zero? n) (* a result)
				(even? n) (recur a (/ n 2) (square result))
				(odd? n) (recur (* a b) (dec n) result)))]
    (inner-fast-expt 1 n b)))

(defn double-it [x]
  (* x 2))

(defn half-it [x]
  (/ x 2))

(comment
(defn fast-multiply [x y]
  (letfn [(inner-fast-multiply [a result innery]
			       (println a result innery)
			       (cond (< innery 1) (+ a result)
				(even? innery) (recur a (double-it result) (half-it innery))
				(odd? innery) (recur (+ a result) result (dec innery))))]
    (if (< x y)
      (inner-fast-multiply 0 x y)
      (inner-fast-multiply 0 y x)))))

(defn- fast-multiply-iter [x y a result b]
  (cond (<= b 1) (+ a result)
	(odd? b) (recur x y (+ a result) result (dec b))
	(even? b) (recur x y a (double-it result) (half-it b))))

(defn fast-multiply [x y]
  (fast-multiply-iter x y 0 x y))