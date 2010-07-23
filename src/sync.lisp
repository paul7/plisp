(in-package :plisp)

(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      (list 'synced-form (read stream t nil t))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-atoms*
    '(nil t))
  (defparameter *quotes* 
    '(defstruct defclass quote defgeneric declare declaim proclaim))
  (defparameter *lambdas*
    '(lambda sb-int:named-lambda))
  (defparameter *quoting-specials*
    '(((if setq tagbody progn) . 1)
      ((block eval-when lambda) . 2)
      ((defun defmacro defmethod 
	 #+sbcl sb-int:named-lambda) . 3)))
  (defparameter *let-like-specials*
    '(let flet labels macrolet let*)))

(defmacro synced-form (form &environment env)
  (cond
    ((and (symbolp form)
	  (not (member form *special-atoms*)))
     `(synced-value ,form))
    ((or (atom form)
	 (member (car form) *quotes*))
     form)
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
	       ,@(mapcar #'(lambda (subform)
			     `(synced-form ,subform))
			 (cddr form))))
	   ((dolist (spec *quoting-specials*)
	      (if (member (car form) (car spec))
		  (return (cdr spec))))
	    `(,@(subseq form 0 it)
		,@(mapcar #'(lambda (subform)
			      `(synced-form ,subform))
			  (subseq form it))))
	   ((or (not (special-operator-p (car form)))
		(member (car form) *function-like-specials*))
	    `(synced-value (,(car form) ,@(mapcar #'(lambda (subform)
						      `(synced-form ,subform))
						  (cdr form)))))
	   (t 
	    (warn "unsupported special")
	    form)))))))
  
(defun print-specials ()
  (do-symbols (s) 
    (if (special-operator-p s)
	(print s))))

