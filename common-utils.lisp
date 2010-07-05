(defpackage :common-utils
  (:use :common-lisp)
  (:export :a.while
	   :a.when
	   :a.if
	   :each-file-line
	   :octet
	   :octets
	   :simple-octets
	   :defmain
	   :lm
	   :while
	   :with-gensyms
	   :nlet
	   :nlet-acc
	   :flatten
	   :find-min
	   :find-max
	   :read-file
	   :read-binary-file
	   :install-readmacro :uninstall-readmacro
	   :defconst-once-only
	   :set-package-nickname
	   :delete-package-nickname
	   :s
	   :split-by-chars
	   :maphash-to-list))
(in-package :common-utils)

(deftype octet () '(unsigned-byte 8))
(deftype octets (&optional size) 
  (if size
      `(vector octets ,size)
    '(vector octet)))
(deftype simple-octets (&optional size) 
  (if size
      `(simple-array octet (,size))
    '(simple-array octet)))

(define-symbol-macro it (intern "IT"))
(define-symbol-macro $  (intern "$"))
(define-symbol-macro accumulate  (intern "ACCUMULATE"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun s (&rest args)
    "ARGSを連接した文字列に変換する"
    (with-output-to-string (s)
      (dolist (a args)
	(typecase a
	  (string    (write-string a s))
	  (character (write-char a s))
	  (otherwise (princ a s))))))

  
  (defun formalize-letargs (args)
    (mapcar (lambda (a) (if (atom a) (list a) a)) args))

  (defun symb (&rest args)
    (intern (apply #'s args)))

  (defun pop-symbol (sym &optional (n 1))
    (intern (subseq (symbol-name sym) n))))
  
;;; XXX: delimsの扱い修正(定数やsequenceも使えるように。余裕があるなら変数も->compiler macro化?)
;;; XXX: この関数は外すかも. or package移動
(defmacro split-by-chars (delims str &optional count (remove-delim t))
  (assert (typep delims 'string) (delims) "DELIMS must be STRING (input is ~A)" (type-of delims))
  `(let (tokens (len (length ,str)) ,@(when count (list (list 'cnt count))))
     ,(when count '(declare (fixnum cnt)))
     (nlet self ((pos 0) (beg 0))
	   (declare (fixnum pos beg))
	   (if (= pos len)
	       (nreverse (if (= beg pos) tokens (cons (subseq ,str beg pos) tokens)))
	     (case (schar ,str pos)
		   (,(coerce delims 'list)
		    (push (subseq ,str beg pos) tokens)
		    (while (and (/= pos len) 
				(case (schar ,str pos) 
				      (,(coerce delims 'list) 
				       ,(unless remove-delim
					  `(push (subseq ,str pos (1+ pos)) tokens))
				       (incf pos)))))
		    ,(when count 
		       `(when (zerop (decf cnt))
			  (return-from self (nreverse 
					     (if (>= pos len) tokens (cons (subseq ,str pos) tokens))))))
		    (self pos pos))
		   (otherwise
		    (self (1+ pos) beg)))))))

(defun read-file (path)
  #+SBCL (sb-ext:octets-to-string  
	  (with-open-file (in path :element-type '(unsigned-byte 8))
	    (let ((as (make-array (file-length in) :element-type '(unsigned-byte 8))))
	      (read-sequence as in)
	      as)))
  #-SBCL (with-output-to-string (out)
           (with-open-file (in path)
             (let ((io (make-echo-stream in out)))
	       (unwind-protect
		   (loop while (read-line io nil nil))
		 (close io))))))

(declaim (ftype (function (t) simple-octets) read-binary-file))
(defun read-binary-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((as (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence as in)
      as)))

;;;;;;
;;; map系
(defun gen-anaph-params (n)
  (if (= n 1)
      (list $)
    (loop for i from 1 to n collect (symb "$" i))))

(defmacro while (expr &body body)
  `(loop
    (unless ,expr (return))
    ,@body))

(defmacro a.while (expr &body body)
  `(let (,it)
     (while (setf ,it ,expr)
       ,@body)))

(defmacro a.when (expr &body body)
  `(let ((,it ,expr))
     (when ,it
       ,@body)))

(defmacro a.if (expr consequent &optional alternative)
  `(let ((,it ,expr))
     (if ,it
	 ,consequent
       ,alternative)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s) 
		   `(,s (gensym)))
		 syms)
     ,@body))

(defmacro nlet (fn-name letargs &body body)
  (setf letargs (formalize-letargs letargs))
  `(labels ((,fn-name ,(mapcar #'car letargs)
              ,@body))
     (,fn-name ,@(mapcar #'cadr letargs))))

(defmacro nlet-acc (fn-name letargs  &body body)
  (with-gensyms (acc)
    `(let ((,acc '()))
       (flet ((,accumulate (x) (push x ,acc)))
         (nlet ,fn-name ,letargs
           ,@body))
       (nreverse ,acc))))

(defun flatten (lst)
  (nlet-acc self ((x lst))
    (if (consp x)
	(progn (self (car x)) (self (cdr x)))
      (when x
	(accumulate x)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; "/dir/file.ext" -> "file.ext"
  (defun basename (pathstring)
    (let ((path (parse-namestring pathstring)))
      (format nil "~A~@[.~A~]" (pathname-name path) (pathname-type path))))

  ;; '(a b c &optional c &key (d e)) -> '(a b c d)
  (defun collect-varsym (args)
    (mapcar (lambda (a)
	      (if (consp a) (car a) a))
	    (remove-if (lambda (a)
			 (and (symbolp a) (string= "&" a :end2 1)))
		       args))))

#+SBCL
(defmacro defmain (fn-name args &body body)
  (let ((usage nil))
    ;; If first expression of body is string type, it treated as command documentation
    (when (stringp (car body))
      (setf usage (car body)
	    body  (cdr body)))
    
    `(defun ,fn-name ()
       ;; Need to override *invoke-debugger-hook*
       (let ((sb-ext:*invoke-debugger-hook*
	      (lambda (condition hook)
		(declare (ignore hook))
		(format *error-output* "Error: ~A~%" condition)
		(sb-ext:quit :unix-status 1))))
         
	 ;; When failed arguments destructuring, show documentation and exit
	 ,(when usage
	    `(handler-case 
	      (destructuring-bind ,args (cdr sb-ext:*posix-argv*) 
	        (declare (ignore ,@(collect-varsym args))))
	      (error ()
	        (format *error-output* "~&~?~%~%" 
			,usage
			(list (basename (car sb-ext:*posix-argv*))))
		(sb-ext:quit :unix-status 1))))

         (destructuring-bind ,args (cdr sb-ext:*posix-argv*)
           ,@body
	   (sb-ext:quit :unix-status 0))))))

;; (defun install-readmacro)
;; (defun uninstall-readmacro)

(defmacro lm (&body body)
  (let* ((num? (integerp (car body)))
	 (arg-num (if num? (car body) 1))
	 (body    (if num? (cdr body) body)))
    `(lambda ,(gen-anaph-params arg-num)
       ,@body)))

(defun init-readtable ()
  (let ((rt (copy-readtable)))
    (set-dispatch-macro-character #\# #\$ 
      (lambda (stream subchar arg)
	(declare (ignore subchar))
	`(lm ,@(if arg (list arg) '()) 
	     ,(read stream)))
      rt)
    rt))

(let ((original nil))
  (defun install-readmacro ()
    (assert (null original))
    (shiftf original common-lisp:*readtable* (init-readtable)))
  
  (defun uninstall-readmacro ()
    (assert (not (null original)))
    (shiftf common-lisp:*readtable* original nil)))

(defmacro each-file-line ((line filepath &rest keys) &body body)
  `(with-open-file (#1=#:in ,filepath ,@keys)
     (let (,line)
       (while (setf ,line (read-line #1# nil nil nil))
	 ,@body))))

(macrolet ((find-xxx-impl (empty? first loop compare key-fn seq)
             `(if (,empty? ,seq)
		  nil
		(let* ((top-elem  (,first ,seq))
		       (top-value (funcall ,key-fn top-elem)))
		  (,@loop
		   (let ((value (funcall ,key-fn elem)))
		     (when (,compare value top-value)
		       (setf top-elem elem
			     top-value value))))
		  (values top-elem top-value))))
	   (find-xxx (compare key-fn seq)
	     (let ((list-loop '(dolist (elem (cdr seq))))
		   (vector-loop '(loop for i from 1 below (length seq) 
				       for elem = (aref seq i) do))
		   (#1=vector-empty? '(lambda (v) (zerop (length v))))
		   (#2=vector-first  '(lambda (v) (aref v 0))))
	       `(etypecase ,seq
	          (list   (find-xxx-impl null car  ,list-loop   ,compare ,key-fn ,seq))
		  (vector (find-xxx-impl ,#1# ,#2# ,vector-loop ,compare ,key-fn ,seq))))))

  (defun find-min (key-fn seq) (find-xxx < key-fn seq))
  (defun find-max (key-fn seq) (find-xxx > key-fn seq)))

(defmacro defconst-once-only (name value &optional documentation)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when documentation (list documentation))))

(defmacro set-package-nickname (package nickname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package '(,nickname))))

(defmacro delete-package-nickname (package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rename-package ,package ,package)))

(declaim (inline maphash-to-list))
(defun maphash-to-list (function-designator hash-table &aux acc)
  (declare (optimize (safety 1) (debug 1) (speed 3))
           (hash-table hash-table)
           ((or symbol function) function-designator))
  (locally
   (declare (optimize (debug 0) (safety 0)))
   (let ((fn (if (typep function-designator 'function)
                 function-designator
               (symbol-function function-designator))))
     (maphash
      (lambda (k v)
        (push (funcall fn k v) acc))
      hash-table))
   acc))
