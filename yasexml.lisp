#+(and (not drewc.org/yasexml) quicklisp) 
(ql:quickload '("closure-html" "cxml"))

(defpackage drewc.org/yasexml
  (:documentation 
   "YASEXML: Yet Another Symbolic Expression eXtensible Markup Language")
  (:use :cl)
  (:import-from :closure-html))
(in-package drewc.org/yasexml)

(pushnew :drewc.org/yasexml *features*)

(defun add-attribute (name value)
  "=> [implementation-dependent]

Arguments and Values: 

 name --- an [xml attribute] name
 value --- an [xml attribute] value

"
  (closure-html:attribute name value))

(defun add-text (text)
  "=> text
Arguments and Values: 
 text --- the text to write and return"
  (closure-html:text text))

(defun call-with-element (element function &rest arguments)
  "=> result

Arguments and Values:

 element --- an [atom] or a [list] 
 function --- any [function]
 arguments --- a [list] of values
 result --- the [values] that /function/ returns

Description: 

 Wrapped in an /element/, calls the /function/ with /arguments/.

 /Element/ can be an [atom] or a [list]. 

 For an [atom], it is the _name_ of the element, and should be a
 [symbol] or a [string].

 Otherwise it is a [list]. The first item should be the _name_, and
 the rest should be a [plist] of the attribute names and values.
"
  (destructuring-bind (name &rest attributes)
      (if (listp element) element (list element))
    (closure-html:with-element name 
      (loop :for (name value) 
	 :on attributes :by #'cddr
	 :do (add-attribute name value))
      (apply function arguments))))

(defun call-with-sink-output (sink function &rest arguments)
  "=> result

Examples:
"
  (closure-html:with-html-output (sink :name "yasexml")
    (apply function arguments)))

(defgeneric wrap-in-tag (function arguments 
			 tag &rest tag-attributes)
  (:method (f a tag &rest tag-attributes)
    (apply #'call-with-element (cons tag tag-attributes) 
	   f a))    
  (:method (f a (tag (eql :text)) &rest text)
    (map nil #'add-text text)
    (apply f a))
  (:method (f a (tag (eql :sink)) &rest sink)
    (apply #'call-with-sink-output (first sink) f a)))
  
(defgeneric call-with-tag (tag function &rest arguments)
  (:method ((tag symbol) f &rest a)
    (wrap-in-tag f a tag))
  (:method ((tag string) f &rest a)
    (wrap-in-tag f a :text tag))
  (:method ((tag list) f &rest a)
    (apply #'wrap-in-tag f a tag)))

(defmacro <> (tag &body body)
  `(call-with-tag 
    , (typecase tag 
	(symbol `',tag)
	(list
	 (if (let* ((*package* (find-package :cl))
		    (tag-prefix (aref (princ-to-string tag) 0)))
	       (or (eql #\` tag-prefix)
		   (eql #\' tag-prefix)))
	     ;; This must be a quote or backquote, so pass it along.
	     tag
	     `(list ',(first tag)
		    ,@(rest tag))))
	(t tag))
      (lambda () ,@body)))

(setf (documentation '<> 'function)
 #.(symbol-name '#:|
Example : (<> (:sink (cxml:make-string-sink
		     :indentation 1))
	   (<> (test-tag :test-attribute "test-attribute-value")
	     (<> "Test Text as Tag" (<> (br)))
	     (<> (:text "Test :TEXT as Tag") 
	       (<> ("foo bar=baz" :bat "?"))
	       (<> (:text "Test " ":TEXT" "many strings"))
	       (<> `(:text ,(concatenate 
			     'string "Test " ":TEXT "
			      "many strings with backquote " 
			      "and concatenate" ))
		 (<> `(,(funcall (constantly 'backquoted-tag-name))
			,@(list 'list "attribute")))))))
	     
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<test-tag test-attribute=\"test-attribute-value\">
 Test Text as Tag
 <br/>
 Test :TEXT as Tag
 <foo bar=baz bat=\"?\"/>
 Test
 :TEXT
 many strings
 Test :TEXT many strings with backquote and concatenate
 <backquoted-tag-name list=\"attribute\"/>
</test-tag>"|))

;; Copyright (c) 2013 Drew Crampsie <drewc@drewc.org>
;; All rights reserved. 
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; ")AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
