(ns sicp.chapter2.ch2-1
  (:use (sicp.chapter1 ch1-1 ch1-2)))

(defn make-rat [n d] (list n d))

(defn numer [x] (first x))

(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* numer y) (denom x)))

;; Exercise 2.1
(defn make-normalized-rat [n d]
  (if (or (neg? n) (neg? d))
    (make-rat (- (abs n)) (abs d))
    (make-rat n d)))

;; Exercise 2.2
(defn make-segment [start-segment end-segment]
  (list start-segment end-segment))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn make-point [x-point y-point]
  (list x-point y-point))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn print-point [p]
  (println "\n(" (x-point p) "," (y-point p) ")"))



;; Exercise 2.3
(defn make-rectangle [bottom-left top-right]
  (let [segment (make-segment bottom-left top-right)
	x-length (+ (abs (x-point (start-segment segment)))
		   (abs (x-point (end-segment segment))))
	y-length (+ (abs (y-point (start-segment segment)))
		   (abs (y-point (end-segment segment))))]
    (list x-length y-length)))

(defn x-length [rectangle]
  (first rectangle))

(defn y-length [rectangle]
  (second rectangle))

(defn rectangle-perimeter [rectangle]
  (+ (* 2 (x-length rectangle)) (* 2 (y-length rectangle))))

(defn rectangle-area [rectangle]
  (* (x-length rectangle) (y-length rectangle)))

(defn make-rectangle-alt [bottom-left top-right]
  (make-segment bottom-left top-right))

(defn x-length-alt [rectangle]
  (+ (abs (x-point (start-segment rectangle)))
		   (abs (x-point (end-segment rectangle)))))

(defn y-length-alt [rectangle]
  (+ (abs (y-point (start-segment rectangle)))
		   (abs (y-point (end-segment rectangle)))))
	
	
  
     