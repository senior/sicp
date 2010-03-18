(ns sicp.chapter1)

(defn square [x]
  (* x x))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn good-enough? [prevGuess guess x]
  (< (abs (- guess prevGuess)) 0.00001))

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

(defn newton-improve [guess x]
  (/ (+ (/ x (cube guess))  (* 2 guess)) 3))

(defn- cubert-iter [prevGuess guess x]
  (if (good-enough? prevGuess guess x)
    guess
    (cubert-iter guess (newton-improve guess x) x)))

(defn cubert [x]
  (cubert-iter x 1.0 x))

