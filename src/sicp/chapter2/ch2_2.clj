(ns sicp.chapter2.ch2-2
  (:require [clojure.contrib.generic.math-functions :as math])
  (:use (sicp.chapter1 ch1-1 ch1-2)))

;; Ex 2.17
(defn last-pair [x]
  (if (seq (rest x))
    (recur (rest x))
    (first x)))

;; Ex 2.18
(defn my-reverse [x]
  (if (seq x)
    (concat (my-reverse (rest x))  (list (first x)))
    ()))

;; Ex 2.19

(def us-coins (list 50 25 10 5 1))
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [coins] (empty? coins))
(defn first-denomination [coins] (first coins))
(defn except-first-denomination [coins] (rest coins))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
	(+ (cc amount
	       (except-first-denomination coin-values))
	   (cc (- amount
		  (first-denomination coin-values))
	       coin-values))))


;; Ex 2.20

(defn- same-parity-inner [parity-fn items]
  (cond (empty? items) ()
	(parity-fn (first items))
	(cons (first items) (same-parity-inner parity-fn (rest items)))
	:else (same-parity-inner parity-fn (rest items))))

(defn same-parity [item1 & more]
  (if (odd? item1)
    (same-parity-inner odd? (cons item1 more))
    (same-parity-inner even? (cons item1 more))))

;; Ex 2.21
(defn square-list1 [items]
  (if (empty? items)
    ()
    (cons (square (first items)) (square-list1 (rest items)))))

(defn square-list2 [items]
  (map square items))

;; Ex 2.23
(defn for-each [f coll]
  (if (empty? coll)
    true
    (do
      (f (first coll))
      (recur f (rest coll)))))

;; Ex 2.25

;; (first (rest (first (rest (rest '(1 3 (5 7) 9))))))
;;(first (first '((7))))
;; (first (rest(first(rest (first (rest (first (rest (first (rest (first (rest'(1 (2 (3 (4 (5 (6 7))))))))))))))))))
;;more clojureish (fnext(fnext(fnext(fnext(fnext (fnext '(1 (2 (3 (4 (5 (6 7))))))))))))

;; Ex 2.27

(defn deep-reverse [y]
  (cond (empty? y) nil
	(seq? (first y))
	(concat (deep-reverse (rest y))
		(list (deep-reverse (first y))))
	:else  (concat (deep-reverse (rest y)) (list (first y)))))

;; Ex 2.28

(defn fringe [x]
  (cond (empty? x) nil
	(seq? (first x)) (concat (fringe (first x)) (fringe (rest x)))
	:else (cons (first x) (fringe (rest x)))))

;; Ex 2.29

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (second mobile))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (second branch))

(defn branch-weight [branch]
  (let [s (branch-structure branch)]
    (if (seq? s)
      (+ (-> s
	     left-branch
	     branch-weight)
	 (-> s
	     right-branch
	     branch-weight))
      s)))

(defn total-weight [mobile]
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(defn balance-calc [branch]
  (* (branch-length branch)
     (branch-weight branch)))

(defn balanced? [mobile]
  (= (-> mobile
	 left-branch
	 balance-calc)
     (-> mobile
	 right-branch
	 balance-calc)))

;; Ex 2.30 and 2.31

(defn square-tree-direct [t]
  (cond (empty? t) nil
	(seq? (first t)) (cons (square-tree-direct (first t))
			      (square-tree-direct (rest t)))
	:else (cons (square (first t)) (square-tree-direct (rest t)))))

(defn tree-rec [f t]
  (cond (empty? t) nil
	(seq? (first t)) (cons (tree-rec f (first t))
			      (tree-rec f (rest t)))
	:else (cons (f (first t)) (tree-rec f (rest t)))))

(defn square-tree-rec [t]
  (tree-rec square t))

(defn map-tree [f t]
  (map #(if (seq? %)
	 (map-tree f %)
	 (f %))
       t))

(defn square-tree-map [t]
  (map-tree square t))


;; Ex 2.32
  
(defn subsets [s] 
  (if (empty? s) 
    (list '())
    (let [r (subsets (rest s))] 
      (concat r (map #(cons (first s) %) r))))) 
