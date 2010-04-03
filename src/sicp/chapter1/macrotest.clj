(ns test-macro
  (:import (java.io PrintWriter)))
(comment
(defmacro deftriplestorefn [fun-name args & body]
  (list 'defn fun-name (vec (cons 'db args))
	(list 'with-open (vec (list 'pw (list 'PrintWriter. (list 'System/out))))
	      (concat 'do (list '.println 'pw "before execution") body)
		   (list '.println 'pw "after execution" ))))

(defmacro deftriplestorefn2 [fun-name args & body]
  (letfn [(actual-call [&rest] body)]
    (list 'defn fun-name (vec (cons 'db args))
	  (list 'with-open (vec (list 'pw (list 'PrintWriter. (list 'System/err))))
		(list 'do
		      (list '.println 'pw "before")
		      (list 'apply (concat (list 'fn (vec (list '&rest))) body) args)
		      (list '.println 'pw "after")
		      (list '.flush 'pw))))))
	
	)   
