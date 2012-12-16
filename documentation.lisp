#.(prog1 "" (ql:quickload '(split-sequence closer-mop interface)))
(defpackage :drewc.org/documentation (:use :cl)) 
(in-package :drewc.org/documentation)

(defun package-external-symbols (package) 
  (sort (with-package-iterator (n (list package) :external) 
	  (loop for i = (nth-value 1 (n))
	     :if i 
	     :collect i into ms :else :do (return ms)))
	#'string<))

(defun symbol-documentation (s)
  `(,s
    ,@(when (macro-function s)
	`((macro
	    ,(swank::arglist s)
	    ,(documentation s 'function))))
    ,@(when (and (fboundp s) (not (macro-function s)))
	`((,(type-of (fdefinition s)) 
	    ,(swank::arglist s)
	    ,(documentation s 'function))))
    ,@(let ((class (ignore-errors (find-class s :errorp nil))))
       (when class
	 `((,(type-of class)
	     ,(documentation class t)
	     ,(c2mop:class-direct-superclasses class)
	     ,(when (typep class 'interface:interface-class)
		    (loop :for k :being :the 
		       :hash-keys :of (interface::interface-generics class)
		       :using (hash-value v) 
		       :collect (list k v)))))))))

(defun interface-documentation/symbol-documentation (sdoc)
  (labels  ((idoc ()
	      (assoc 'INTERFACE:INTERFACE-CLASS (cdr sdoc))))
    (let ((super-interfaces  (third (idoc))))
      (when (idoc)
	(concatenate
	 'string  
     "/Interface Class/ " (symbol-name (first sdoc))
     "

*Super Interface List:*

"
     (format nil "~{*~A* ~}" 
	     (mapcar #'symbol-name (mapcar #'class-name (third (idoc)))))
"

*Description:*
#+begin_example
" (or (second (idoc))
      "/no description/")
"
#+end_example
*Generic Functions:*

" (format nil "~{- *~A*  ~{/~A/  ~} ~%~}"
	  (let ((gfs (fourth (idoc))))
	    (loop for gf in gfs nconc (list (string-downcase  
					     (symbol-name (first gf)))
					    (mapcar (lambda (s)
						      (string-downcase
						       (format nil "~A" s))) 
						    (getf (second gf) :lambda-list))))))
"
*Methods:*

" (format nil "~{- ~{~A:~A ~A :: ~A~%~%   #+begin_example~%   ~A~%   #+end_example~%~}~}"
	   
	  (loop :for (sym . gdoc) in *symbol-documentation*
	     :nconc (when (assoc 'STANDARD-GENERIC-FUNCTION gdoc)
		      (loop for m in (c2mop:generic-function-methods (fdefinition sym))
		       :if (find (find-class (first sdoc)) 
				 (c2mop:method-specializers m))
		       :collect (list (package-name (symbol-package sym))
				      (string-upcase (format nil "~A" sym))
				      (mapcar (lambda (s)
						(string-upcase 
						 (format nil "~A" s)))
					      (mapcar #'class-name 
						      (c2mop:method-specializers m)))
				      (mapcar (lambda (s)
						(string-upcase 
						 (format nil "~A" s)))
					      (swank::arglist sym))
				      
				     (documentation m t)))))))))))

(defun generic-function-documentation/symbol-documentation (sdoc)
  (let ((gdoc (assoc 'STANDARD-GENERIC-FUNCTION (cdr sdoc))))
    (when gdoc
      (concatenate
       'string
       "/Standard Generic Function/ "(symbol-name (first sdoc))
       "

*Syntax:*

"
       (format nil "*~A* ~{/~A/  ~} ~%" (string-downcase (first sdoc)) 
	       (mapcar #'string-downcase (second gdoc)))
      "
*Description:*
#+begin_example

" (or (third gdoc)
      "/no description/")
"
#+end_example"

"
*Methods:*

" (format nil "~{- ~{ ~A ~A ::~%~%   #+begin_example~%   ~A~%   #+end_example~%~}~}"
	   (loop for m in (c2mop:generic-function-methods (fdefinition (first sdoc)))
		       :if T #+nil (find (find-class (first sdoc)) 
					 (c2mop:method-specializers m))
		       :collect (list 
				      (first sdoc) 
				      (mapcar #'string-upcase (mapcar #'class-name (c2mop:method-specializers m))) 
				      
				      
				     (or (documentation m t) "no description"))))))))

(defun macro-documentation/symbol-documentation (sdoc)
  (let ((gdoc (assoc 'macro (cdr sdoc))))
    (when gdoc
      (concatenate
       'string
       "/Macro/ "(symbol-name (first sdoc))
       "

*Syntax:*

"
       (format nil "*~A* ~{/~A/  ~} ~%" (string-downcase (first sdoc)) 
	       (mapcar (lambda (s)
			 (string-downcase
			  (format nil "~A" s))) 
		       (second gdoc)))
      "
*Description:*
#+begin_example
" (or (third gdoc)
      "no description
")
"#+end_example"))))

(defparameter *symbol-documentation* NIL)

(defun generic-function-documentation ()
  (remove-if-not (lambda (d) (assoc 'STANDARD-GENERIC-FUNCTION (cdr d))) 
		 *symbol-documentation*))

(defun %package-documentation (package)
  (loop for s in (package-external-symbols package)
     :collect 
       (let ((sdoc (symbol-documentation s)))
	 (pushnew sdoc *symbol-documentation* :key #'car)
	 (remove 
	  nil 
	  (list (interface-documentation/symbol-documentation sdoc) 
		(generic-function-documentation/symbol-documentation sdoc)
		(macro-documentation/symbol-documentation sdoc))))))

(defun package-documentation (package)
  (with-output-to-string (s) 
    (format s "** /Package/ ~A~%" (string-upcase package))
    (let ((doc (documentation (find-package package) T)))
      (when doc (format s "#+begin_example~%~A~%#+end_example~%" doc)))
    (dolist (d (%package-documentation  package))
      (format s "~{*** ~A~%~}"  d))))

(defun write-package-documentation (package path)
   (alexandria:with-output-to-file (s path :if-exists :supersede 
				      :if-does-not-exist :create) 
     (let ((*print-case* :downcase)) 
	 (princ (package-documentation package) s)
	 (terpri s))))

(defun write-monad-interface-documentation ()
  (let ((packages '(:interface/monad
		    :interface/zero-plus
		    :interface/monad/identity
		    :interface/monad/maybe
		    :interface/monad/list
		    :interface/monad/state
		    :interface/monad/continuation
		    :interface/monad/transformer
		    :interface/monad/transformer/maybe
		    :interface/monad/transformer/list
		    :interface/monad/transformer/state
		    :interface/monad/monads))
	(path-as-string "/home/drewc/drewc.org/src/lisp-interface-library/interface/doc/") 
	(docs (list)))
    (loop :repeat 2 :do
       (setf docs nil)
       (dolist (p packages)
	 (let* ((s (split-sequence:split-sequence #\/ (string-downcase (symbol-name p))))
		(path (format nil "~A~{~A/~}~A.org"
			      path-as-string 
			      (butlast (rest s))
			      (first (last s)))))
	   (push path docs)
	   (write-package-documentation p path))))
       (nreverse docs)))

(defun print-org-includes (paths)
  (princ (format nil "~{#+INCLUDE: ~A~%~}" paths)))

(defun package-dependency-asdf-systems (depends-on &rest packages)
  (loop :for (p . ps) 
     :in (quick-build::order-file-dependency-graph
	  (quick-build::build-file-dependency-graph 
	   packages))
     :collect 
     (let ((n (split-sequence:split-sequence 
	       #\/ (symbol-name p))))
       `(asdf:defsystem ,p 
	  :depends-on (,@(or ps (list depends-on)))
	  :components 
	  (,(labels 
	     ((com (n)
		   (if (second n)
		       `(:module ,(string-downcase (first n))
				 :components (,(com (rest n))))
		       `(:file ,(string-downcase (first n))))))
	     (com n)))))))

(defun write-interface-monad-system (&optional (path "/home/drewc/drewc.org/src/lisp-interface-library/interface-monad.asd"))  
  (alexandria:with-output-to-file (s path :if-exists :supersede) 
    (let ((*print-case* :downcase)) 
      (print '(asdf:defsystem :interface-monads
	       :description "The <monad> done interface passing style"
	       :depends-on (:interface/monad))
	     s)
      (terpri s)
      (dolist (sys (package-dependency-asdf-systems :interface :interface/monad/monads))
	(print sys s)
	(terpri s)))))
	
	
	
