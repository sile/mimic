(load "common-utils")
(defpackage :mimic
  (:use :common-lisp :common-utils)
  (:export train train-file
	   split
	   dump-model))
(in-package :mimic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 最適化宣言用スペシャル変数
(defvar *fastest* '(optimize (speed 3) (safety 0) (debug 1)))

;;;;;;;;;;;;;
;;; 素性構造体
(defstruct (feature (:conc-name f-))
  (surface "" :type simple-string)
  (type     0 :type fixnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; feature用のハッシュ関数定義
(defun feature= (f1 f2) 
  (declare #.*fastest*)
  (and (string= (f-surface f1)  (f-surface f2))
       (= (f-type f1) (f-type f2))))

(defun sxhash-feature (f)
  (declare #.*fastest*)
  (+ (sxhash (f-surface f)) (f-type f)))

(sb-ext:define-hash-table-test feature= sxhash-feature)

;;;;;;;;;
;;; 文字種
(defun char-type (char)
  (declare #.*fastest*
	   (character char))
  (cond ((char<= #\0 char #\9)                        0)   ; 数字
	((char<= #\ぁ char #\HIRAGANA_DIGRAPH_YORI)    1)   ; ひらがな
	((char<= #\ァ char #\KATAKANA_LETTER_SMALL_RO) 2)   ; かたかな
	((find char ",.!?")                            3)   ; シンボル
	(t                                             4))) ; その他

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; feature-vector作成関数群
(defun generate-unigram-fv (text center)
  (declare #.*fastest*
	   (simple-string text)
	   (fixnum center))
  (loop FOR i fixnum FROM  (- center 2)
	             BELOW (+ center 2)
        FOR j fixnum FROM 0
    WHEN (and (< -1 i (length text)))
    COLLECT (make-feature :surface (subseq text i (1+ i)) :type j)))

(defun generate-bigram-fv (text center)
  (declare #.*fastest*
	   (simple-string text)
	   (fixnum center))
  (when (and (< center (length text))
	     (plusp center))
    (list
     (make-feature :surface (subseq text (1- center) (1+ center))
		   :type 4))))

(defun generate-trigram-fv (text center)
  (declare #.*fastest*
	   (simple-string text)
	   (fixnum center))
  (when (< 1 center (length text))
    (list 
     (make-feature :surface (subseq text (- center 2) (1+ center))
		   :type 5))))

(defun generate-type-bigram-fv (text center)
  (declare #.*fastest*
	   (simple-string text)
	   (fixnum center))
  (when (and (< center (length text))
	     (plusp center))
    (list 
     (make-feature :surface ""
		   :type (+ (char-type (char text (1- center)))
			    (* (char-type (char text center)) 5))))))

(defun generate-fv (text center)
  (declare #.*fastest*)
  (nconc (generate-unigram-fv text center)
	 (generate-bigram-fv text center)
	 (generate-trigram-fv text center)
	 (generate-type-bigram-fv text center)))

;;;;;;;
;;; SVM
(defstruct svm
  (words       (make-hash-table :test #'feature=))
  (last-update (make-hash-table :test #'feature=))
  (example-n      0 :type fixnum)
  (eta          2.0 :type float)
  (lambda 0.0000001 :type float))

(declaim (inline get-word-score get-last-update))
(defun get-word-score (feature svm)
  (gethash feature (svm-words svm) 0.0))
(defsetf get-word-score (feature svm) (new-val)
  `(progn
     (unless (nth-value 1 #1=(gethash ,feature (svm-words ,svm)))
       (setf #1# 0.0))
     (setf #1# ,new-val)))

(defun get-last-update (feature svm)
  (gethash feature (svm-last-update svm) 0))
(defsetf get-last-update (feature svm) (new-val)
  `(progn
     (unless (nth-value 1 #1=(gethash ,feature (svm-last-update ,svm)))
       (setf #1# 0))
     (setf #1# ,new-val)))

(defun svm.dotproduct (svm fv) 
  (declare #.*fastest*)
  (loop FOR f IN fv
	SUM (get-word-score f svm)))

(declaim (inline svm.margin))
(defun svm.margin (svm fv cut?)
  (* (svm.dotproduct svm fv) (if cut? 1 -1)))

(defun svm.muladd (svm fv cut? scale)
  (declare #.*fastest*
	   (float scale))
  (loop WITH sign = (if cut? 1 -1)
	FOR f IN fv 
    DO
    (incf (get-word-score f svm)
	  (* sign scale))))

(declaim (inline clip-by-zero))
(defun clip-by-zero (a b) 
  (if (> a 0.0)
      (if (> a b) (- a b) 0.0)
    (if (< a (- b)) (+ a b) 0.0)))

(defun svm.l1-regularize (svm fv) 
  (declare #.*fastest*)
  (with-slots (example-n lambda) svm
    (loop FOR f IN fv
	  FOR c = (- example-n (get-last-update f svm))
      DO
      (setf (get-last-update f svm) example-n
	    (get-word-score  f svm) (clip-by-zero (get-word-score f svm) (* lambda c))))))

(defun svm.train-example (svm fv cut?)
  (declare #.*fastest*)
  (when (< (svm.margin svm fv cut?) 1.0)
    (svm.muladd svm fv cut? (svm-eta svm))
    (svm.l1-regularize svm fv))
  
  (with-slots (example-n words lambda) svm
    (when (zerop (mod example-n 500000))
      (maphash
       (lambda (f score)
	 (when (nth-value 1 (get-last-update f svm))
	   (let* ((c (- example-n (get-last-update f svm)))
		  (newv (clip-by-zero score (* lambda c))))
	     (setf (get-last-update f svm) example-n)
	     (if (< (abs newv) lambda)
		 (remhash f (svm-words svm))
	       (setf (get-word-score f svm) newv)))))
       (svm-words svm)))
    (incf example-n)))

;;;;;;;;;;;;;;;;;;;;
;;; 学習データ読み込み　
(defmacro each-sentence ((sentence file) &body body)
  (let ((line     (gensym)))
    `(let ((,sentence '()))
       (each-file-line (,line ,file)
         (if (zerop (length ,line))  ; 空行は文末の印
	     (progn (setf ,sentence (nreverse ,sentence))
		    ,@body
		    (setf ,sentence '()))
	   (push ,line ,sentence)))  ; 一行 = 一単語
       nil)))

;;;;;;;;
;;; 学習
(defun train (svm words)
  (let ((text (apply #'concatenate 'string words))
	(cuts (loop FOR w IN words 
		    SUM (length w) INTO pos
		    COLLECT pos)))
    (loop FOR i FROM 0 BELOW (length text) 
	  FOR fv = (generate-fv text i)
	  FOR cut? = (eql i (car cuts))
      DO
      (when cut?
	(pop cuts))
      (svm.train-example svm fv cut?))))

(defun train-file (filepath)
  (let ((svm (make-svm))
	(cnt 0))
    (handler-bind (#+SBCL (sb-int:stream-decoding-error
                            (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'sb-impl::input-replacement #\?))))
      (each-sentence (sentence filepath)
        (when (zerop (mod (incf cnt) 10000))
	  (format *error-output* "; ~A~%" cnt))
        (train svm sentence)))
    svm))

;;;;;;;;
;;; 分割
(defun split(svm line)
  (loop WITH prev = 0
	FOR i FROM 1 BELOW (length line)
	FOR fv = (generate-fv line i)
    WHEN (>= (svm.dotproduct svm fv) 0.0)
    COLLECT (prog1 (subseq line prev i)
	      (setf prev i)) INTO words
    FINALLY (return (append words (list (subseq line prev))))))

;;;;;;;;;;;;;;;;
;;; C++用のダンプ 
(defun dump-model (svm filepath)
  (with-open-file (out filepath :if-exists :supersede :direction :output)
    (maphash
     (lambda (f score)
       (format out "~2,'0D~A~C~D~%" 
	       (f-type f)
	       (f-surface f) #\Tab
	       (round (* score 100000))))
     (svm-words svm))
    'done))
