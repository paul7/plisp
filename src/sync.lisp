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
  (defparameter *quotes* 
    '(defstruct defclass quote defgeneric declare))
  (defparameter *function-like-specials* 
    '(if setq block tagbody progn function))
  (defparameter *let-like-specials*
    '(let flet labels macrolet let*))
  (defparameter *defun-like-specials*
    '(defun defmacro defmethod))
  (defparameter *lambda-like-specials*
    '(lambda)))

(defmacro synced-form (macro-form &environment env)
  (let ((form (macroexpand macro-form env)))
    (cond
      ((symbolp form)
       `(synced-value ,form))
      ((or (atom form)
	   (member (car form) *quotes*))
       form)
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
      ((member (car form) *defun-like-specials*)
       `(,(car form) ,(cadr form) ,(caddr form)
	  ,@(mapcar #'(lambda (subform)
			`(synced-form ,subform))
		    (cdddr form))))
      ((member (car form) *lambda-like-specials*)
       `(,(car form) ,(cadr form)
	  ,@(mapcar #'(lambda (subform)
			`(synced-form ,subform))
		    (cddr form))))
      ((or (not (special-operator-p (car form)))
	   (member (car form) *function-like-specials*))
       `(synced-value (,(car form) ,@(mapcar #'(lambda (subform)
						 `(synced-form ,subform))
					     (cdr form)))))
      (t 
       (warn "unsupported special")
       form))))
  
(defun print-specials ()
  (do-symbols (s) 
    (if (special-operator-p s)
	(print s))))

