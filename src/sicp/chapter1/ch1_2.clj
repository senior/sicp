(ns sicp.chapter1.ch1-2
  (:require sicp.chapter1.ch1-1)
  (:use sicp.chapter1.ch1-1))

(import '(java.util Random))

(comment 1.9

(+ 4 5)
(inc (+ 3 5))	 
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

Above is not tail recursive.

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

The above is tail recursive.

)

(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(comment 1.10

(A 1 10) - 1024
(A 2 4) - 65536
(A 3 3) - 65336
	 
(define (f n) (A 0 n)) - 2 n

(define (g n) (A 1 n)) - 2^n
(A 0 (A 0 (A 0 (A 1 (- n 3)))))

(define (h n) (A 2 n)) - 2^(2^n)

(define (k n) (* 5 n n)) - 5 n^2

)

(defn recursive-1-11 [n]
  (if (< n 3) 
    n
    (+ (recursive-1-11 (- n 1))
       (recursive-1-11 (- n 2))
       (recursive-1-11 (- n 3)))))

(def memo-recursive-1-11 (memoize recursive-1-11))

(defn another-iterative-1-11 [n]
  (loop [a 1 b 2 c 3 i 3]
    (if (= i n)
      c
      (recur b c (+ a b c) (inc i)))))

;; Exercise 1.12
;;(pascal-triangle 6 3) 10
;;(pascal-triangle 4 2) 3
;;(pascal-triangle 5 4) 4

(defn pascal-triangle [row element]
  (if (or (= 1 element) (= row element)) 
    1
    (+ (pascal-triangle (dec row) (dec element))
       (pascal-triangle (dec row) element))))
			
  

(comment 1.13

;To prove this, the base case for the induction hypothesis is Fib(0)=0,
;which is true since phi^0=1 - psi^0 = 1, which then follows 1-1/sqrt 5 = 0.  
;Fib(1) is also a base case, which is (1 + sqrt 5)/2 + (1 - sqrt 5)/2
;which is 2/2 = 1, so Fib(1)=1.

;Inductive Step:
;
;I use Fib(n) = phi^n - psi^n/sqrt 5, which, by induction, 
;I assume Fib(n-1) = (phi^(n-1) + psi^(n-1))/sqrt 5 and 
;Fib(n-2) = (phi^(n-2) + psi^(n-2))/sqrt 5.  Using this, I can then
;say Fib(n) =  (phi^(n-1) + psi^(n-1))/sqrt 5 +  (phi^(n-2) + psi^(n-2))/sqrt 5.

;Some math:
;Fib(n) = (phi^(n-1) - psi^(n-1))/sqrt 5 +  (phi^(n-2) - psi^(n-2))/sqrt 5
;= (phi * phi^(n-2) - psi * psi^(n-2))/sqrt 5 +  (phi^(n-2) - psi^(n-2))/sqrt 5
;= 1/sqrt 5 * ((phi * phi^(n-2) - psi * psi^(n-2)) + (phi^(n-2) - psi^(n-2)))
;= 1/sqrt 5 * ((phi * phi^(n-2) + phi^(n-2)) - (psi * psi^(n-2) + psi^(n-2)))
;= 1/sqrt 5 * ((phi^(n-2) * (phi + 1)) - (psi^(n-2) * (psi + 1)))
;= 1/sqrt 5 * ((phi^(n-2) * (phi^2)) - (psi^(n-2) * (psi^2)))
;= 1/sqrt 5 * (phi^n - psi^n)
;= (phi^n - psi^n)/sqrt 5

)

(comment 1.14

;Each step in the execution of this procedure results in 2 additional 
;subproblems without divide the solution space, thus the problem doubles
;while the solutions space decreases by 1.  Assuming the addition operations
;etc can be done in O(1) time, it is O(2^n)

)

(comment 1.15

;p is applied 5 times, 12.5/3, the 5th time yields .05, less than .1 and so 
;.15 is the last time p is executed

;Number of steps is log a, since a is divided by 3 each time
)

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

	
(defn divides? [a b]
  (= (mod b a) 0))

(defn next-divisor [x]
  (if (= x 2) 3 (+ 2 x)))
      

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (next-divisor test-divisor))))
(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn search-for-primes [primeAlg x y]
  (let [oddNumbers (filter odd? (iterate inc x))]
    (take y (filter primeAlg oddNumbers))))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp) (mod (square (expmod base (/ exp 2) m))m)
	(and (odd? exp)	(= (mod 1 m) (sqrt base))) 0 ;;Adding this avoids the Carmichael number flaw
        :else (mod (* base (expmod base (- exp 1) m))m)))

(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

(defn random [x]
  (. (Random. (System/currentTimeMillis)) nextInt x))

;;Was try-it in the SICP text
(defn fermat-test-specific-number [n a]
  (= (expmod a n n) a))

(defn fermat-test [n]
  (fermat-test-specific-number (+ 1 (random (- n 1)))))

(defn fast-prime? [times n]
   (cond (= times 0) true
        (fermat-test n) (fast-prime? (- times 1) n)
        :else false))

(def search-for-primes-slow
     (partial search-for-primes prime?))

(def search-for-primes-fast
     (partial search-for-primes (partial fast-prime? 10)))

(defn lots-o-primes [searchFunction primeFunction]
  (let [primeNumbers  (concat (searchFunction 1000 3) (searchFunction 10000 3)
			      (searchFunction 100000 3) (searchFunction 1000000 3))]
    (println primeNumbers)
    (time (println (map primeFunction primeNumbers)))))

(defn fermat-test-all [n]
  (loop [num (dec n)]
    (cond (< num 0) (println "all numbers passed fermat test")
	  (true? (fermat-test-specific-number n num)) (recur (dec num))
	  :else (println num " did not pass fermat test"))))

(comment 1.22
No noticeable difference or growth
)
     
(comment 1.23
No noticeable with the next function
)

(comment 1.25

It would work the same, however it is much slower.  Each recursive
iteration of the expmod applies mod m to the number.  This could 
make the number substantially smaller (when the mod operator is 
smaller).  fast-expt wouldn't apply mod until the end (after the
exponential operation would have taken place) making the exponential
operation slower.

)

(comment
  1.26

In the code from problem, Louis is executing (expmod base (/ exp 2) m)
twice for each iteration of the code, this makes the recurrence something more like 
;T(n) = 2 T(n/2) + O(1) leading to an O(n) runtime since the problem is not getting
smaller.

)

(comment 1.28
  Adding avoided the Carmichael number flaw, see expmod above
)
