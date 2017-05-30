
(ql:quickload '(cl-ppcre))
(ql:quickload 'cl-mysql)
(load "/home/fff/work/lisp/mysql/fl-lib.lisp")
(defpackage :ank-mysql
  (:use :cl :cl-mysql :fl-lib :cl-ppcre)
  (:export check-user
	   add-user
	   initialize
	   get-question
	   get-tests
	   get-user-answer
	   set-user-answer
	   max-question-no
	   answered-questions
	   get-list-question-nums
	   remember-answer-list))
(in-package :ank-mysql)

(defparameter *user* "fff")
(defparameter *password* "1qaz2wsx")
(defparameter *db* "ank")
(defun flatten (lst &aux (result '()))
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (if (listp el)
                 (rflatten el)
                 (push el result)))))
    (nreverse (rflatten lst))))

(defmacro create-table (name-str struct-str)
  (let ((g (gensym)))
    `(let ((,g ,name-str))
       (when (not (member ,g (flatten (caar (query "SHOW TABLES"))) :test 'string-equal))
	 (query (concatenate 'string "CREATE TABLE " ,g ,struct-str))))))

(defmacro create-index (iname tname struct-str &key (unique NIL))
  (let ((i (gensym))
	(tbl (gensym)))
    `(let ((,i ,iname)
	   (,tbl ,tname))
       (when (not (member ,i (get-index-list ,tbl) :test 'string-equal))
	 (if :unique
	     (query (concatenate 'string "CREATE UNIQUE INDEX " ,i " on " ,tbl " " ,struct-str))
	     (query (concatenate 'string "CREATE INDEX " ,i " on " ,tbl " " ,struct-str)))))))

(defun initialize (&key (user "fff") (passw "1qaz2wsx"))
  (when (connect :user user :password passw)
    (when (not (member "ank" (flatten (caar (query "SHOW DATABASES"))) :test 'string-equal))
      (query "CREATE DATABASE ank"))
    (query "USE ank")
    (create-table "user" "(name VARCHAR(50), passwd VARCHAR(50), e_mail VARCHAR(100))")
    (create-index "name" "user" "(name)" :unique T)
    ;; (query "create unique index name on user (name)")
    (create-table "test" "(code VARCHAR(10), name TINYTEXT)")
    (create-table "question" "(test_code VARCHAR(10), no INT, description TEXT)")
    (create-table "variant" "(test_code VARCHAR (10), question_no INT, no INT, description TEXT)")
    (create-table "answer" " (user_name varchar(50), test_code VARCHAR(10), question_no INT, variant_no INT, answer_no INT)")
    (create-index "answer" "answer" "(user_name, test_code, question_no, variant_no)" :unique T)
    (add-user "admin" "admin")
  ))

(defun add-user (name passw &optional (e-mail ""))
  (when (not (check-user name)) 
      (eval `(query (concatenate 'string
				 "INSERT into user (name, passwd, e_mail) VALUES ('"
				 ,name
				 "', '"
				 ,passw
				 "', '"
				 ,e-mail
				 "');")))))

(defun check-user (name &key (password NIL))
  (when (connect :user *user* :password *password* :database *db*)
    (if (not (NULL password))
	(caar (eval
	       `(query (concatenate 'string
				    "SELECT * from user WHERE name = '"
				    ,name
				    "' and passwd = '"
				    ,password
				    "';"))))
	(caar (eval
	       `(query (concatenate 'string
				    "SELECT * from user WHERE name = '"
				    ,name
				    "';")))))))

(defun change-passw (name)
  T)

(defun split-firstnum-rest (str1)
  (setf fstr (scan-to-strings "([0-9.]* )" str1))
  (setf reststr (subseq str1 (length fstr)))
  (values (parse-integer (remove #\. fstr))
	  reststr))

(defun get-index-list (tbl-name)
  (setf l1 (caar (query (concatenate 'string "show indexes in " tbl-name))))
  (loop
     for item in l1
       collect (nth 2 item)))

(let ((qnumber 0)
      (vnumber 0))
  (defun reset-vnumber ()
      (setf vnumber 0))
  (defun insert-quest (test_code statement)
    (if (digit-char-p (coerce (subseq statement 0 1) 'character))
	(progn
	  (multiple-value-bind (qnumb insert-str)
	      (split-firstnum-rest statement)
	    (reset-vnumber)
	    (setf qnumber qnumb)
	    (format T "all-str: ~A~%"
		    (list "test_code" test_code
			  "no" (write-to-string qnumber)
			  "description" insert-str))
	    (insert-into "question"
			 (list "test_code" test_code
			       "no" (write-to-string qnumber)
			       "description" insert-str))
	    (format T "insert-str: ~A~%" insert-str)))
	(progn
	  (incf vnumber)
	  (insert-into "variant"
		       (list  "test_code" test_code
			      "question_no" (write-to-string qnumber)
			      "no" (write-to-string vnumber)
			      "description" statement))))))

(defun import-test-questions (testname filename)
  (let ((seq NIL)
	(item-list NIL))
    (setf seq (read-file-as-seq filename 'charset:cp866))
    (setf item-list (cl-ppcre:split #\Newline seq :limit (length seq)))
    (mapc #'(lambda (x) (when (> (length x) 0) (insert-quest testname x))) item-list)))

;; (defun insert-test (name)
;;   (query "INSERT INTO TEST VALUES ("))

(defmacro insert-into (tablename values)
  "Values is list (field-name-str value-str)"
  (let ((table (gensym "t"))
	(val (gensym "val")))
    `(let ((,table ,tablename)
	   (,val (pairs ,values)))
       (query
	(concatenate 'string
		     ;; "INSERT into user (name, passwd, e_mail) VALUES ('"
		     "INSERT into "
		     ,table
		     " ("
		     (make-string-with-mask (mapcar 'car ,val) ", ")
		     ") VALUES ('"
		     (make-string-with-mask (mapcar 'cadr ,val) "', '")
		     "');")))))

(defmacro get-list-question-nums (test-name)
  "return list of numbers of question"
  `(mapcar #'cadr (caar (query (concatenate 'string
					    "select * from question where test_code = '"
					    ,test-name
					    "'")))))
(defmacro update-in (tablename values &key (where-values NIL))
  "Values is list (field-name-str value-str)"
  (let ((table (gensym "t"))
	(val (gensym "val"))
	(where-val (gensym "where")))
    `(let ((,table ,tablename)
	   (,val (pairs ,values))
	   (,where-val (pairs ,where-values)))
       (query
	(concatenate 'string
		     ;; "INSERT into user (name, passwd, e_mail) VALUES ('"
		     "UPDATE "
		     ,table
		     " SET "
		     (make-string-with-mask
		      (mapcar #'(lambda (x) (concatenate 'string
							 (car x)
							 " = '"
							 (cadr x)
							 "'")) ,val)
		      ", ")
		     (if ,where-val
			 (concatenate 'string " WHERE "
				      (make-string-with-mask
				       (mapcar #'(lambda (x) (concatenate 'string
									  (car x)
									  " = '"
									  (cadr x)
									  "'")) ,where-val)
				       " and "))
			 "")
		     ";"))))) 

;; (defun name-str (values) 
;;   (setf list1 (cl-ppcre:split #\Space (car l1)))
;;   ())

(defun pairs (l)
  (loop for (first second) on l by #'cddr
     collect (list first second)))
  
(defun make-string-with-mask (list-str mask)
  (let ((l1 (car  list-str)))
    (mapcar (lambda (x) (setf l1 (concatenate 'string l1 mask x))) (cdr list-str))
    l1))
(defun get-question (test-str no-str &key username)
  "Get list (quest-string (var1-str ... var5-str))"
  (let ((result NIL))
    (when no-str
      (setf result (caaar (query (concatenate 'string
					      "select * from question where test_code = '"
					      test-str
					      "' and no = "
					      no-str))))
      (setf result (cons result
			 (list (caar (query (concatenate 'string
							 "select * from variant where test_code = '"
							 test-str
							 "' and question_no = "
							 no-str))))))
      (when username
	(setf result (append result (get-user-answer :test test-str :quest-no no-str :username username )))))
    result))

(defun max-question-no (test)
  (let ((result NIL))
    (setf result (caaaar (query (concatenate 'string
					   "select max(question_no) from variant where test_code = '"
					   test
					   "'"))))
    result))

(defun answered-questions (test user)
  (let ((result NIL))
    (setf result (caaaar (query (concatenate 'string
					     "SELECT COUNT(*) FROM answer where user_name = '"
					     user
					     "' and test_code = '"
					     test
					     "';"))))
    result))

(defun get-user-answer (&key test quest-no username (variant nil))
  (let ((result NIL))
    (setf result  (query (concatenate 'string
				      "select answer_no from answer where user_name = '"
				      username
				      "' and test_code = '"
				      test
				      "' and question_no = '"
				      quest-no
				      (when variant
					"' and variant_no = '")
				      (when  variant variant)
				      "';")))
    (caaaar result)))

(defun set-user-answer (test question uname variant answer)
  (setf var1 (get-user-answer
	      :test test :quest-no question
	      :username uname :variant variant))
  (when (or (and var1 (not (eq answer var1)))
	    (null var1))
    (if var1
	
	(update-in "answer" (list "answer_no" answer) :where-values (list "user_name" uname
								    "question_no" question
								    "test_code" test
								    "variant_no" variant))
	(insert-into "answer" (list "user_name" uname
				"question_no" question
				"test_code" test
				"variant_no" variant
				"answer_no" answer))
)))
(defun remember-answer-list (&key test username answer-list)
  (loop for x in answer-list
       for q from 1
     do (loop for y in x
	   for i from 1
	   do (set-user-answer
	       test
	       (write-to-string q)
	       username
	       (write-to-string i) y))))

	  ;; (setf answer-list (loop for q from 1 to (session-value 'max-question-no)
	  ;; 		       for qv = (length (cadr (get-question (session-value 'test) (session-value 'question-no) :username (session-value 'username))))
	  ;; 		       collect (loop for v from 1 to qv
	  ;; 				  collect (post-parameter
	  ;; 					   (concatenate 'string "pr" (write-to-string q)
	  ;; 							"n" (write-to-string v)) ))))







(defun get-tests ()
  (let ((result NIL))
    (setf result (caar (query (concatenate 'string
					   "select * from test"))))
    result))
