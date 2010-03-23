(ns sicp.chapter1.ch1-3
  (:require (clojure.contrib.generic.math-functions))
  (:use (sicp.chapter1 ch1-2 ch1-1)))

(defn sum [term a next b]
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(defn simpson [f a b n]
  (let [h (/ (- b a) n)]
    (defn compute-y [k]
      (+ a (* k h)))
    (defn inner-simpson [k]
      (cond (or (= k 0) (= k n)) (f (compute-y k))
	    (odd? k) (* 4 (f (compute-y k)))
	    :else (* 2 (f (compute-y k)))))
    (* (/ h 3) (sum inner-simpson a inc n))))
	    

(comment 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (inc a) (+ (term a) result))
  (iter a 0)))))

;;Exercise 1.31
(defn product [term a next b]
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(defn product-iter [term a next b]
  (defn product-iter-inner [a result]
    (if (> a b)
      result
      (recur (next a) (* (term a) result))))
  (product-iter-inner a 1))

(defn factorial [x]
  (product identity 1 inc x))

(defn factorial-iter [x]
  (product-iter identity 1 inc x))

(defn pi-approx [x]
  (defn pi-term [i]
    (float (if (odd? i)
      (/ (inc i) (+ i 2))
      (/ (+ i 2) (inc i)))))
  (* 4 (product-iter pi-term 1 inc x )))


;;Exercise 1.32
(defn accumulate [combiner nullValue term a next b]
  (if (> a b)
      nullValue
      (combiner (term a)
         (accumulate combiner nullValue term (next a) next b))))

(defn accumulate-iter [combiner nullValue term a next b]
  (defn accumulate-iter-inner [a results]
    (if (> a b)
      results
      (recur (next a) (combiner (term a) results))))
  (accumulate-iter-inner a nullValue))

(def accumulate-sum (partial accumulate + 0))
(def accumulate-product (partial accumulate * 1))
(def accumulate-iter-sum (partial accumulate + 0))
(def accumulate-iter-product (partial accumulate * 1))

(defn acc-sum-cubes [a b]
  (accumulate-sum cube a inc b))

(defn acc-product-factorial [x]
  (accumulate-product identity 1 inc x))

(defn acc-iter-sum-cubes [a b]
  (accumulate-sum cube a inc b))

(defn acc-iter-product-factorial [x]
  (accumulate-product identity 1 inc x))

;;Exercise 1.33
(defn filtered-accumulate [pred combiner nullValue term a next b]
  (defn apply-pred [x]
    (if (pred x)
      (term x)
      nullValue))
  (accumulate combiner nullValue apply-pred a next b))

;;(filtered-accumulate odd? + 0 cube 1 inc 10)

(defn sum-of-square-primes [a b]
  (filtered-accumulate prime? + 0 square a inc b))
	
(def tolerance 0.00001)
(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
          next
          (try-it next))))
  (try-it first-guess))

(defn fixed-point-sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))

;;Exercise 1.35
(defn fixed-point-golden-ratio []
  (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0))

;; (fixed-point-golden-ratio)
;; 1.6180327868852458

;;Exercise 1.36
(defn debugging-fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (let [next (f guess)]
      (println "Guess: " guess)
      (if (close-enough? guess next)
          next
          (try-it next))))
  (try-it first-guess))

(defn exp-estimate [x]
  (debugging-fixed-point (fn [x] (/ (clojure.contrib.generic.math-functions/log 1000) (clojure.contrib.generic.math-functions/log x))) 1.1))

;;Exercise 1.37
(defn cont-frac [n d k]
  (letfn [(cont-frac-inner [i]
			   (if (< i k)
			     (/ (n i) (+ (d i) (cont-frac-inner (inc i))))
			     (d k)))]
    (cont-frac-inner 1)))

;; Using a value of k greater than 10 yields .6180
;; (cont-frac (fn [x] 1.0) (fn [x] 1.0) 11)

(defn cont-frac-iter [n d k]
  (defn cont-frac-iter-inner [i result]
    (if (< i k)
      (recur (inc i) (/ result (+ (* (d i) (n (inc i))) (n i))))
      (/ result (d k))))
  (cont-frac-inner 1))

;;Exercise 1.38

;;(euler-e-approx 1000) -> 0.7182818284590453
(defn euler-e-approx [k]
  (defn d-sequence [i]
    (let [x (mod i 3)]
      (cond (= i 2) 2
	    (< i 1) 0
	    (or (= x 0) (= x 1)) 1
	    :else (+ 2 (d-sequence (- i 3))))))
  (cont-frac (fn [x] 1.0) d-sequence k ))

;; Exercise 1.39
;; (tan-cf 1.36 20) -> 4.6734414
;; (tan-cf 3 20) -> -0.14254655
(defn tan-cf [x k]
  (defn n-fun [i] (if (= i 1) x (- (* x x))))
  (defn d-fun [i] (inc (* 2 (dec i))))
  (float (cont-frac n-fun d-fun k)))


(def dx 0.00001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
       dx)))

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))


;; Exercise 1.41
;; (((double (double double)) inc) 5) = 21
(defn double-call [f]
  (fn [x] (f (f x))))

;; Exercise 1.42
;; ((my-compose square inc) 6) -> 49
(defn my-compose [f g]
  (fn [x] (f (g x))))

;; Exercise 1.43
;; ((my-repeated square 2) 5) -> 625
(defn my-repeated [f n]
  (letfn [(inner-repeated [fprime n]
			  (if (< n 1)
			    (fn [x] (fprime x))
			    (recur (my-compose fprime f) (dec n))))]
    (inner-repeated f (dec n))))

;; Exercise 1.44
(defn smooth [f]
  (fn [x]
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

