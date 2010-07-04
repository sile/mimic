;;;;
(defmacro each-sentence ((sentence file) &body body)
  (let ((line     (gensym)))
    `(let ((,sentence '()))
       (each-file-line (,line ,file)
         (if (zerop (length ,line))
	     (progn (setf ,sentence (nreverse ,sentence))
		    ,@body
		    (setf ,sentence '()))
	   (push ,line ,sentence)))
       nil)))

(defun char-type (char)
  (cond ((char<= #\0 char #\9)  0)
	((char<= #\ぁ char #\ん) 1)
	((char<= #\ァ char #\ン) 2)
	((find char ",.!?")     3) 
	(t                      4)))

(defstruct (feature (:conc-name f-))
  surface 
  type
  score)

(defun f-key (feature)
  (s (f-surface feature) #\Tab (f-type feature)))

(defun init-feature (surface type &optional (score 1.0))
  (make-feature :surface surface :type type :score score))

;;;;
(defun generate-unigram-fv (text center)
  (loop FOR i FROM  (- center 2)
	      BELOW (+ center 2)
        FOR j FROM 0
    WHEN (and (< -1 i (length text)))
    COLLECT (init-feature (subseq text i (1+ i)) j)))

(defun generate-bigram-fv (text center)
  (when (and (< center (length text))
	     (plusp center))
    (list
     (init-feature (subseq text (1- center) (1+ center))
		   4))))

(defun generate-trigram-fv (text center)
  (when (and (< center (length text))
	     (plusp center))
    (list 
     (init-feature (subseq text (max 0 (- center 2)) (1+ center))
		   5))))

(defun generate-type-bigram-fv (text center)
  (when (and (< center (length text))
	     (plusp center))
    (list 
     (init-feature 
      ""
      (+ (char-type (char text (1- center)))
	 (* (char-type (char text center)) 5))))))

(defun generate-fv (text center)
  (append (generate-unigram-fv text center)
	  (generate-bigram-fv text center)
	  (generate-trigram-fv text center)
	  (generate-type-bigram-fv text center)))

(defun train-sentence (svm words) 
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


;;;;;;;;;;;;;;;;  
(defstruct svm
  (words       (make-hash-table :test #'equal)) ;; XXX: ft => float
  (last-update (make-hash-table :test #'equal)) ;; XXX: ft => int
  (example-n 0)
  (eta 2.0)
  (lambda 0.0000001))

(defun get-word (ft svm &optional create-if-not-exists)
  (multiple-value-bind (val exists?)
		       #1=(gethash (f-key ft) (svm-words svm))
    (when (and (not exists?)
	       create-if-not-exists)
      (setf val (setf #1# (init-feature (f-surface ft) (f-type ft)))))
    val))

(defun get-last-update (ft svm &optional create-if-not-exists)
  (multiple-value-bind (val exists?)
		       #1=(gethash (f-key ft) (svm-last-update svm))
    (when (and (not exists?)
	       create-if-not-exists)
      (setf val (setf #1# (init-feature (f-surface ft) (f-type ft) 0))))
    val))

(defun svm.dotproduct (svm fv) 
  (loop FOR f IN fv
	FOR w = (get-word f svm)
    WHEN w
    SUM (* (f-score f) (f-score w))))  ; XXX: (f-score f)は常に1.0にならないか?

(defun svm.margin (svm fv cut?)
  (* (svm.dotproduct svm fv) (if cut? 1 -1)))

(defun svm.muladd (svm fv cut? scale)
  (loop WITH sign = (if cut? 1 -1)
	FOR f IN fv 
    DO
    (incf (f-score (get-word f svm t)) 
	  (* sign (f-score f) scale)))) ; XXX: (f-score f)は常に1.0にならないか?

(defun clip-by-zero (a b) 
  (if (> a 0.0)
      (if (> a b) (- a b) 0.0)
    (if (< a (- b)) (+ a b) 0.0)))

(defun svm.l1-regularize (svm fv) 
  (with-slots (example-n lambda) svm
    (loop FOR f IN fv
	  FOR u = (get-last-update f svm t)
	  FOR w = (get-word f svm t)
      DO
      (let ((c (- example-n (f-score u))))
	(setf (f-score u) example-n
	      (f-score w) (clip-by-zero (f-score w) (* lambda c)))))))

(defun svm.train-example (svm fv cut?)
  (when (< (svm.margin svm fv cut?) 1.0)
    (svm.muladd svm fv cut? (svm-eta svm))
    (svm.l1-regularize svm fv))
  
  (with-slots (example-n words lambda) svm
    (when (zerop (mod example-n 500000))
      (loop FOR f BEING EACH HASH-VALUE OF words DO
        (a.when (get-last-update f svm)
          (let* ((c (- example-n (f-score it)))
		 (newv (clip-by-zero (f-score f) (* lambda c))))
	    (setf (f-score it) example-n)
	    (if (< (abs newv) lambda)
		(remhash (f-key f) (svm-words svm))
	      (setf (f-score f) newv))))))
    (incf example-n)))

;;;;;;;;;;;
(let ((svm (make-svm)))
  (each-sentence (sentence "kokoro.corpus")
    (train-sentence svm sentence))
  (defparameter *svm* svm))


;;;;;
(defun split(svm line)
  (loop WITH prev = 0
	FOR i FROM 1 BELOW (length line)
	FOR fv = (generate-fv line i)
    DO
    (print (s "=="i"=="))
    (print (mapcar (lm (get-word $ svm)) fv))
    (print (svm.dotproduct svm fv))
    
    WHEN (>= (svm.dotproduct svm fv) 0.0)
    COLLECT (prog1 (subseq line prev i)
	      (setf prev i)) INTO words
    FINALLY (return (append words (list (subseq line prev))))))

;;;;
(defun save (svm filepath)
  (with-open-file (out filepath :if-exists :supersede :direction :output)
    (loop FOR f BEING EACH HASH-VALUE OF (svm-words svm) DO
      (format out "~2,'0D~A~C~D~%" 
	      (f-type f)
	      (f-surface f) #\Tab
	      (round (* (f-score f) 100000)))))
  'done)

#|
namespace std {
  namespace tr1 {
    
    template<class T>
    struct hash {
    };
    
    template <>
    struct hash<feature> : public std::unary_function<feature, std::size_t>
    {
      size_t
      operator()(feature val) const
      {
	size_t __length = val.len_;
	const char *__first = val.str_;
	
	size_t __result = static_cast<size_t>(2166136261UL);
	__result ^= static_cast<size_t>(val.ftype_);
	//    __result += static_cast<size_t>(val.ftype_ << 8);
	__result *= static_cast<size_t>(16777619UL);
	
	for (; __length > 0; --__length)
	  {
	    __result ^= static_cast<size_t>(*__first++);
	    __result *= static_cast<size_t>(16777619UL);
	  }
	return __result;
      }
    };
  }
}
|#
