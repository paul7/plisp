(in-package :plisp)

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (list 'synced-form (read stream t nil t))))

(set-dispatch-macro-character #\# #\?
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (list 'unsynced-form (read stream t nil t))))

(defclass proxy () 
  ((evaluator :initform (constantly nil) 
	     :initarg :evaluator
	     :accessor proxy-evaluator)))

(defgeneric synced-value (object))

(defmethod synced-value ((object t))
  (format t "syncing ~a~%" object)
  object)

(defmethod synced-value ((object proxy))
  (format t "evaluating")
  (funcall (proxy-evaluator object)))

(defmacro unsynced-form (form)
  form)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-atoms*
    '(nil t))
  (defparameter *quotes* 
    '(defstruct defclass quote defgeneric defmacro
      declare declaim proclaim))
  (defparameter *quoting-specials*
    '(((if setq tagbody progn) . 1)
      ((block eval-when lambda) . 2)
      ((#+sbcl sb-int:named-lambda) . 3)))
  (defparameter *let-like-specials*
    '(let flet labels symbol-macrolet let*)))

(defun make-synced-form (form &optional (except 0))
  `(,@(subseq form 0 except)
      ,@(mapcar #'(lambda (subform)
		    `(synced-form ,subform))
		(subseq form except))))

(defmacro synced-form (form &environment env)
  (cond
    ((and (symbolp form)
	  (not (member form *special-atoms*)))
     `(synced-value ,form))
    ((or (atom form)
	 (member (car form) *quotes*))
     form)
    ((eq (car form) 'unsynced-form)
     (if (atom (cadr form))
	 (cadr form)
	 (make-synced-form (cadr form) 1)))
    (t
     (let ((exp-form (macroexpand form env)))
       (let ((form (if (eq (car exp-form) 'function)
		       (if (consp (cadr exp-form))
			   (cadr exp-form)
			   (return-from synced-form exp-form))
		       exp-form)))
	 (acond
	   ((member (car form) *let-like-specials*)
	    `(,(car form)
	       ,(mapcar #'(lambda (binding)
			    `(,(car binding)
			       (synced-form ,(cadr binding))
			       ,@(cddr binding)))
			(cadr form))
	       ,@(make-synced-form (cddr form))))
	   ((dolist (spec *quoting-specials*)
	      (if (member (car form) (car spec))
		  (return (cdr spec))))
	    (make-synced-form form it))
	   ((not (special-operator-p (car form)))
	    `(synced-value ,(make-synced-form form 1)))
	   (t 
	    (warn "unsupported special")
	    form)))))))

(defmacro with-screened-symbols (screen-fun (&rest symbols) &body body)
  (let* ((sym-num (length symbols))
	 (gsyms (loop for i from 1 to sym-num collect (gensym))))
    `(let ,(mapcar #'(lambda (gsym sym)
		       `(,gsym ,sym))
		   gsyms symbols)
       (symbol-macrolet ,(mapcar #'(lambda (sym gsym)
				     `(,sym (,screen-fun ,gsym)))
				 symbols gsyms)
	 ,@body))))

(defmacro with-synced ((&rest symbols) &body body)
  `(with-screened-symbols synced-form ,symbols ,@body))

;; bad, bad macro :(
(defmacro with-unsynced ((&rest symbols) &body body)
  `(with-screened-symbols unsynced-form ,symbols ,@body))

(defun print-specials ()
  (do-symbols (s) 
    (if (special-operator-p s)
	(print s))))


(defclass lazy-list ()
  ((storage :initform nil
	    :initarg :storage
	    :accessor lazy-list-storage)))

(defmethod synced-value ((object lazy-list))
  (lazy-list-storage object))

(defun test-lazy-list ()
  (let ((l1 '(1))
	(l2 '(1 2))
	(l3 '(1 2 3)))
    (let ((ll1 (make-instance 'lazy-list :storage l1))
	  (ll2 (make-instance 'lazy-list :storage l2))
	  (ll3 (make-instance 'lazy-list :storage l3)))
      (let ((lazy (make-instance 'lazy-list 
				 :storage (list ll1 ll2 ll3))))
	#!(mapcar #'(lambda (list) (length list)) lazy)))))
