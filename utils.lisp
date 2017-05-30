(in-package :hunch)


(defun make-int-list (l)
  (mapcar #'(lambda (x)
	      (cond
		((numberp x) x)
		((stringp x) (parse-integer (concatenate 'string x "z") :junk-allowed t))
		(t 0))) l))

(defun distinct-p (l)
  "Return T if in l all elements are different"
  (let ((l1 NIL))
    (mapcar #'(lambda (x) (when (and
				 (not (member  x l1))
				 (not (null x)))
			    (push x l1)))
	    l)
    (eq (length l) (length l1))))

(defun in-limits-p (l p1 p2)
  "Return T if in l all elements are different"
  (let ((l1 NIL))
    (mapcar #'(lambda (x) (when (not (member  x l1))
			    (push x l1)))
	    l)
    (eq (length l) (length l1))))

(defun check-list (l)
  (if (consp l)
      (let ((l1 (make-int-list l)))
	(and (distinct-p l1)
	     (= (loop for x in l1 sum x)
		(/ (* (length l1) (+ 1 (length l1))) 2))
	     (= (loop for x in l1 maximize x) (length l1))
	     (= (loop for x in l1 minimize x) 1)))
      NIL)) 
