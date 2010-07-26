(in-package :plisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\!
    #'(lambda (stream char1 char2)
        (declare (ignore char1 char2))
        (list 'synced-form (read stream t nil t))))

  (set-dispatch-macro-character #\# #\?
    #'(lambda (stream char1 char2)
        (declare (ignore char1 char2))
        (list 'unsynced-form (read stream t nil t)))))

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

(defmacro synced-values (form) 
  `(apply #'values (mapcar #'synced-value 
			   (multiple-value-list ,form))))

(defmacro unsynced-form (form)
  form)

; THROW                 +
; PROGV                 +
; IF                    +
; UNWIND-PROTECT        +
; SYMBOL-MACROLET       + 
; LOAD-TIME-VALUE       +
; SETQ                  + 
; LOCALLY               +
; EVAL-WHEN             +
; THE                   +
; MULTIPLE-VALUE-PROG1  +
; MACROLET              +
; RETURN-FROM           +
; LET                   +
; TAGBODY               +
; FLET                  +
; BLOCK                 +
; MULTIPLE-VALUE-CALL   +
; GO                    +
; CATCH                 +
; FUNCTION              +
; PROGN                 +
; QUOTE                 +
; LET*                  +
; LABELS                +
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-atoms*
    '(nil t))
  (defparameter *quotes* 
    '(defstruct defclass quote defgeneric defmacro
      declare declaim proclaim))
  (defparameter *quoting-specials*
    '(((if setq tagbody progn throw unwind-protect
	   locally multiple-value-prog1 multiple-value-call
	   catch progn progv) . 1)
      ((block eval-when lambda return-from the go) . 2)
      ((#+sbcl sb-int:named-lambda) . 3)))
  (defparameter *let-like-specials*
    '(let symbol-macrolet let*))
  (defparameter *flet-like-specials*
    '(flet labels macrolet))
  (defparameter *setq-like-specials*
    '(setq #+allegro psetq))

  (defun make-synced-form (form except env &key tagbody) 
    `(,@(subseq form 0 except)
	,@(mapcar #'(lambda (subform)
		      (synced-form/stage0 subform env :tagbody tagbody))
		  (subseq form except))))

  (defun make-synced-form/setq (form env)
    (labels ((setq-body (form env)
	       (if form
		   `(,(car form) ,(synced-form/stage0 (cadr form) env)
		      ,@(setq-body (cddr form) env)))))
      (if (oddp (length form))
	  `(,(car form) ,@(setq-body (cdr form) env))
	  form))) ; let the compiler complain

  (defun synced-form-not-macro (form env &key tagbody)
    (acond
      ((member (car form) *let-like-specials*)
       `(,(car form)
	  ,(mapcar #'(lambda (binding)
		       (make-synced-form binding 1 env))
		   (cadr form))
	  ,@(make-synced-form (cddr form) 0 env)))
      ((member (car form) *flet-like-specials*)
       `(,(car form)
	  ,(mapcar #'(lambda (binding)
		       (make-synced-form binding 2 env))
		   (cadr form))
	  ,@(make-synced-form (cddr form) 0 env)))
      ((member (car form) *setq-like-specials*)
       (make-synced-form/setq form env))
      ((eq (car form) 'load-time-value)
       `(,(car form) ,(synced-form/stage0 (cadr form) env) ,@(cddr form)))
      ((eq (car form) 'tagbody)
       (make-synced-form form 1 env :tagbody t))
      ((dolist (spec *quoting-specials*)
	 (if (member (car form) (car spec))
	     (return (cdr spec))))
       (make-synced-form form it env))
      ((not (special-operator-p (car form)))
       (if tagbody
	   (make-synced-form form 1 env)
	   `(synced-values ,(make-synced-form form 1 env))))
      (t 
       (warn "unsupported special")
       form)))

  (defun synced-form/stage0 (form env &key tagbody)
    (cond
      ((and (symbolp form)
	    (not (member form *special-atoms*)))
       (if tagbody
	   form
	   `(synced-value ,form)))
      ((or (atom form)
	   (member (car form) *quotes*))
       form)
      ((eq (car form) 'unsynced-form)
       (if (atom (cadr form))
	   (cadr form)
	   (make-synced-form (cadr form) 1 env)))
      ((eq (car form) 'function)
       (if (consp (cadr form))
	   (synced-form-not-macro (cadr form) env)
	   form))
      (t
       (multiple-value-bind (exp-form macro-p) (macroexpand-1 form env)
	 (if macro-p
	     `(synced-form ,exp-form)
	     (synced-form-not-macro form env :tagbody tagbody)))))))
  
(defmacro synced-form (form &environment env)
  (synced-form/stage0 form env))

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

(defvar *proxy* (make-instance 'proxy :evaluator (constantly 42)))

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
