(ns sicp.chapter1.ch1-1)

(comment 1.1

10 - 10
(+ 5 3 4) - 12
(- 9 1) - 8
(/ 6 2) - 3
(+ (* 2 4) (- 4 6)) - 6
(def a 3) - a
(def b (+ a 1)) - b
(+ a b (* a b)) - 19
(= a b) - false
(if (and (> b a) (< b (* a b)))
    b
    a) - 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) - 16
(+ 2 (if (> b a) b a)) - 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) - 16

)

(comment 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

)

(defn square [x]
  (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

;; 1.3
(defn square-top-2 [x y z]
  (cond (or (> x y z) (> y x z)) (sum-of-squares x y)
	(or (> x z y) (> z x y)) (sum-of-squares x z)
	(or (> y z x) (> z y x)) (sum-of-squares y z)))

(comment 1.4

The if statement returns either the + or - function,
with will then have a and b passed to it, either
resulting in (- a b) or (+ a b)

)

(comment 1.5

Stack overflow results in clojure (or infinite looping
in an applicative order language).  What happens is (p) is
evaluated before calling the function in the applicative style.  
In normal order eval, (p) would never be evaluated as long as x 
equaled 0

)

(comment 1.6

Infinite loop.  It will always evaluate the then and else
clauses, regardless of what the predicate says

)	 

(comment 1.7

It has difficulty because of the margin of error (.001) is small
when computing the square roots of larger numbers, but too large
for computing the square roots of smaller numbers.  Using the criteria 
of how much change per iteration is much better, because if there is
no change (or very little change), the number must be right (or close)

)


(defn abs [x]
  (if (< x 0) (- x) x))

(defn good-enough? [prevGuess guess x]
  (< (abs (- guess prevGuess)) 0.001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn- sqrt-iter [prevGuess guess x]
  (if (good-enough? prevGuess guess x)
    guess
    (sqrt-iter guess (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter x 1.0 x))

(defn cube [x]
  (* x x x))

;;Excercise 1.8
(defn newton-improve [guess x]
  (/ (+ (/ x (cube guess))  (* 2 guess)) 3))

(defn- cubert-iter [prevGuess guess x]
  (if (good-enough? prevGuess guess x)
    guess
    (cubert-iter guess (newton-improve guess x) x)))

(defn cubert [x]
  (cubert-iter x 1.0 x))




