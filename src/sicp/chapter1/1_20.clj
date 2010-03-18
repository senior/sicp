(ns sicp.ch1.1_20)

(import '(java.util Random))

(defn abs [x]
  (if (< x 0)(- x) x))
(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(defn square [x] (* x x))
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
	 Adding avoided the Carmicheal number flaw, see expmod above
)

