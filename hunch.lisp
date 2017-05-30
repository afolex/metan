(ql:quickload :hunchentoot)
(ql:quickload :cl-who)
(ql:quickload :parenscript)
(load "/home/fff/work/lisp/mysql/ank.lisp")

(in-package :cl-user)
(defpackage hunch
  (:use :cl
        :cl-who
        :hunchentoot
	:cl-mysql
	:ank-mysql
	 :parenscript
;;	:nav-indentation
	)
  (:shadowing-import-from :cl-mysql :escape-string))

(load "/home/fff/work/lisp/mysql/utils.lisp")
(load "/home/fff/work/lisp/mysql/template.lisp")
(load "/home/fff/work/lisp/mysql/js.lisp")

(in-package :hunch)

;; (setf hunchentoot:*hunchentoot-default-external-format* 
;;       hunchentoot::+utf-8+)
(setf *hunchentoot-default-external-format* 
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"
        *catch-errors-p* nil)

(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(parenscript:import-macros-from-lisp '$$)

(defvar *h* (make-instance 'easy-acceptor :port 3004
			   :document-root #p"~/work/lisp/mysql/"))
(defvar *tmp-test-files* NIL)
(defvar *tmp-test-directory*
    #+(or :win32 :mswindows) #p"c:\\hunchentoot-temp\\test\\"
    #-(or :win32 :mswindows) #p"/usr/local/fff/tmp/hunchentoot/test/")
(setf *tmp-directory*
    #+(or :win32 :mswindows) #p"c:\\hunchentoot-temp\\"
    #-(or :win32 :mswindows) #p"/usr/local/fff/tmp/hunchentoot/")
(defvar *qnumber* 1)

(defmacro with-lisp-output ((var) &body body)
  `(with-output-to-string (,var #+:lispworks nil
			       #+:lispworks :element-type
			       #+:lispworks 'lw:simple-char)
    ,@body))

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))


;; define a handler with the arbitrary name my-greetings:
(defun start-page ()
  (setf (hunchentoot:content-type*) "text/html; charset=UTF-8")
  (with-html-output (*standard-output*)

    (:head (:title "Стартовая страница")
	   (:link :rel "stylesheet" :type "text/css"
                  :href "/css/style.css"))
   (:body 
    (:p (:div :id "h1" "Добро пожаловать"))
    (:p (:div :id "leftp" 
	      (:a :href "/enter" "Войти")
	      (:br)
	      (:a :href "/registration" "Регистрация"))))))

(defun test-finished (&key test-no question-no  (method :post) (charset :utf-8))
  (setf (content-type*)
	(format nil "text/html; charset=~A" charset))
  (with-html
      (:html
       (:head (:title (fmt "Тест завершен")))
       (:body
	(:p (fmt "Тест ~A завершен." (session-value 'test)))
	(:form
	 :method "post"
	 :action "/exit"
	 (:p (:input :type "submit" 
		     :name "exit"
		     :value "Выход" 
		     :class "btn"))))       
       ))
  )

(defmacro with-http-authentication (&rest body)
  `(progn
     (when (null (session-value 'username))
       (setf (session-value 'username) "username")
       (require-authorization "Tests"))
     (multiple-value-bind (user1 password) (authorization)
       (cond ((ank-mysql:check-user user1 :password password)
	      (progn
		(setf (session-value 'username) user1)
		,@body))
	     (t (require-authorization "Tests"))))))

(defun auth-page ()
  (with-http-authentication
      (show-tests)))
	     

(defun clear-authorization ()
  (setf (session-value 'username) NIL))

(defun show-tests  (&key (method :post) (charset :utf-8))
  (setf (content-type*)
        (format nil "text/html; charset=~A" charset))
  (with-html
    (:html
     (:head (:title (fmt "Тесты" )))
     (:body
      (:h2 "Список тестов:")
      (loop for test in (get-tests)
	 do (htm (:p (:a :href (format nil "tests/~A" (car test))
			 (esc (fmt "~A" (nth 1 test)))))))
      (:form
       :method "post"
       :action "/exit"
       (:p (:input :type "submit" 
		   :name "exit"
		   :value "Выход" 
		   :class "btn")))))))

(defmacro span-row-table (prefix answers &rest forms)
  (let ((=value= (gensym))
        (=first= (gensym))
	(item-qty (length forms)))
    `(progn
       ;; (setf item-qty (length ',forms))
       (with-html-output (*standard-output*)
	      (:p (:table :class "ftable" :border 1 :cellpadding 2 :cellspacing 0 :width "70%"
			  (:tr (:td :id ,prefix :rowspan ,(write-to-string (+ 1 item-qty))
				    :style (concatenate 'string
							       "width:20px; background-color:"
							       (if (check-list ',answers)
								   "green"
								   "red")) 
				    "   "))
			  ,@(loop for form in forms
			       for i from 1
			       collect `(:tr
					 (:td :style "width:20px"
					  (:input :type :text
						  :style "width:10px"

						  :onblur (ps  ((@ make-color)
								       ,(make-symbol (concatenate 'string "\"" (string-upcase prefix) "\""))
								       ,item-qty
								       ,i))
						  :id (concatenate 'string ,prefix
									  "n" (write-to-string ,i))
						  :name (concatenate 'string ,prefix
									    "n" (write-to-string ,i))
						  :value (or  (nth (- ,i 1) ',answers) "")
						  ))
					 (:td :valign "top"
					      (:pre :style "padding: 0px" 
						    (esc (with-lisp-output (s)
							    (format t "~A" ,form)))))))))))))

(defun f-span-row-table (x a &rest forms)
  (eval `(span-row-table ,x ,a ,@forms)))


(defun show-test-table  (&key (method :post) (charset :utf-8))
  (setf (content-type*)
        (format nil "text/html; charset=~A" charset))
  (let ((problem-text NIL)
	(answer-list NIL)
	(err-list NIL))
    (let ((finish-test (post-parameter "end-test")))
      (if finish-test
	(progn
	  (setf answer-list (loop for q from 1 to (session-value 'max-question-no)
			       for qv = (length (cadr (get-question (session-value 'test) (write-to-string q) :username (session-value 'username))))
			       collect (loop for v from 1 to qv
					  collect (post-parameter
						   (concatenate 'string "pr" (write-to-string q)
								"n" (write-to-string v))))))
	  (setf err-list (remove T (loop for x in answer-list
				      for i from 1 
				      collect (or (check-list x)
						  i))))
	  (when (not err-list)
	    (remember-answer-list
	     :test (session-value 'test)
	     ;; :question-no (session-value 'question-no)
	     :username (session-value 'username)
	     :answer-list answer-list)
	    (redirect "/finish")))
	(progn
	  (setf answer-list (loop for q from 1 to (session-value 'max-question-no)
			       for qv = (length (cadr (get-question (session-value 'test) (session-value 'question-no) :username (session-value 'username))))
			       collect (loop for v from 1 to qv
					    ;; get-user-answer (test question uname variant)
					  collect (get-user-answer
						   :test (session-value 'test)
						   :quest-no (write-to-string q)
						   :username (session-value 'username)
						   :variant (write-to-string v))))))))
    
    (with-html
	(:html
	 (:head
	  (:title (fmt "Таблица" ))    
	  (:link :rel "stylesheet" :type "text/css"
		 :href "/css/style.css")
	  (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")
	  
	  (script-make-color "red" "green"))
	 (:body
	  (make-instruction *instr-list* 40)
	  (when err-list (htm (:p (esc (with-lisp-output (s) (format s "Список некорректно отвеченных вопросов: ~A" err-list ))))))
	  (:form
	   :method "post"
       
	   (loop for y in (mapcar #'(lambda (x) (write-to-string x)) (get-list-question-nums (session-value 'test)))
	      for ql = (get-question (session-value 'test) y)
	      for pref =  (concatenate 'string  "pr" y )
		
	      do (progn
		   (htm (:p (:a (format t "~A. ~A" (cadar ql) (caddar ql)))))
		   (apply #' f-span-row-table (append (list  pref (nth (- (parse-integer y) 1) answer-list))
						      (mapcar #'cadddr (cadr ql))))))
	   
	   
	   (:p (:input :type "submit"
		       :name "end-test"
		       :value "Закончить"
		       :class "btn"))))))))
(defun registration (&key (charset :utf-8))
  (setf (content-type*)
        (format nil "text/html; charset=~A" charset))
  (let ((username (post-parameter "username")) 
	(password (post-parameter "password"))
	(password1 (post-parameter "password1"))
	(email (post-parameter "email"))
	(user-exist NIL))
    (when username
      (setf user-exist (check-user username)))
    (if (and (not (NULL username))
	       (string= password password1)
	       (not (NULL email))
	       (not user-exist))
      
	(progn (add-user username password email)
	       (reg-success username))
	
	(with-html
	    (:html
	     (:head (:title (fmt "Регистрация" )))
	     (:body
	      (when (or username
			password
			password1
			email)
		(when (NULL username )
		  (htm  (:p (fmt "Не задано имя пользователя."))))
		(when (and password
			   (not (string= password password1)))
		  (htm  (:p (fmt "Пароли не совпадают." password password1))))
		(when (NULL email)
		  (htm  (:p (fmt "Не задан адрес электронной почты.")))))
	      (:h2 "Введите данные для регистрации:")
	      (:form
	       ;; :action "/allok"
	       :method "post"
	       (:p "Имя пользователя:" (:br)
		   (:input :type :text
			   :name "username"))
	       (:p "Пароль:" (:br)
		   (:input :type :password
			   :name "password"))
	       (:p "Пароль еще раз:" (:br)
		   (:input :type :password
			   :name "password1"))
	       (:p "Адрес электронной почты:" (:br)
		   (:input :type :text
			   :name "email"))
	       
	       (:p (:input :type "submit" 
			   :name "next"
			   :value "Ок" 
			   :class "btn")))))))))



(defmacro script-make-color (clr1 clr2)
"Add script with function to change color"
  `(with-html-output (*standard-output*)
    (:script
     (str (ps
	   (defun make-color (id qty cntname)
	     (let ((sum1 0)
		   (check-list (array))
		   (val1 "")
		   (val2 0)
		   (val3 "")
		   (ptr ""))
	       (setf ptr (concatenate 'string id "n"  cntname ))
	       (setf val1 (chain document (get-element-by-id ptr) value))
	       (if (eql  val1  "")
		   (return false))
	       (setf val2 (parse-int val1))
	       (setf val3 (chain val2 (to-string 10)))
	       (when (or (not (eql val1 val3))
			 (< val1 0)
			 (> val1 qty))
		 (alert "Ошибка")
		 (setf  (chain document (get-element-by-id ptr) value) "")
		 (setf (chain document (get-element-by-id id) style background) ,clr1)
		 (return false))
	       (when (= val1 val3)
		 (dotimes (ctr qty t)
		   (progn
		     (setf ctr1 (+ ctr 1))
		     (setf ptr (concatenate 'string id "n"  ctr1 ))
		     (setf val1 (chain document (get-element-by-id ptr) value))
		     (when (not (= "" val1))
		       (setf sum1 (+ sum1 (parse-int val1)))
		       (if (= (chain check-list (index-of val1)) -1) 
			   (chain check-list (push val1))
			   (setf (chain document (get-element-by-id id) style background) ,clr1))))))
	       (if (eql sum1 (/ (* (+ 1 qty) qty) 2))
		   (setf (chain document (get-element-by-id id) style background) ,clr2)
		   (setf (chain document (get-element-by-id id) style background) ,clr1)))
	     (return true)))))))

(defun exit-session ()
  (clear-authorization)
  (no-cache)
  (redirect "/start-page"))


(defun start-test (test)
  (setf (session-value 'test) test)
  (no-cache)
  (setf (session-value 'question-no) "1")
  (setf (session-value 'max-question-no) (max-question-no test))
  (redirect "/show-test"))

(defun reg-success (username &key (charset :utf-8))
  (setf (content-type*)
        (format nil "text/html; charset=~A" charset))
  (with-html
      (:html
       (:head (:title (fmt "Регистрация успешна" )))
       (:body
	(:p (fmt "Пользователь ~A успешно зарегистрирован." username))

	(:form
	 :method "post"
	 :action "/enter"
	 (:p (:input :type "submit" 
		     :name "next"
		     :value "Ок" 
		     :class "btn")))))))

(defun oops ()
  (with-html
    (log-message* :error "Oops \(error log level).")
    (log-message* :warning "Oops \(warning log level).")
    (log-message* :info "Oops \(info log level).")
;;;;    (error "Errors were triggered on purpose.  Check your error log.")
    (:html
     (:body "You should never see this sentence..."))))


(defparameter *link-list*
  '(("/start" start-page)
    ("/registration" registration)
    ("/oops" oops)
    ("/enter" auth-page)
    ("/exit" exit-session)
    ("/js" js-test)
    ("/pie" js-pie)
    ("/finish" test-finished)
    ;; ("/clickme" click-me)
    ;; ("/cm" click-me2)
    ;; ("/ts" table-span-rows)
    ("/show-test" show-test-table)
    ;; ("/finish-test" finish-test)
    ))

(defun make-test-functions (test-list)
  "defun functions for tests and return list to append to *link-list*"
  (loop for f in test-list
     do (progn
	  (setf newn (intern (format NIL "START-~A" (string-upcase f))))
	  (eval `(defun ,newn () (start-test ,(string-downcase f)))))
     collect (eval `(list (format nil "/tests/~A" ,(string-downcase f))
		       newn))))
(ank-mysql:initialize)

(setf *link-list* (nconc *link-list* (make-test-functions (mapcar #'car (get-tests))) ))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers)      
       (mapcar (lambda (args)
		 (apply 'create-prefix-dispatcher args))
	       *link-list*)))



;; (defun make-test-functions (test-list)
;;   "defun functions for tests and return list to append to *link-list*"
;;   (loop for f in test-list
;;      do (progn
;; 	  (setf newn (intern (format NIL "START-~A" (string-upcase f))))
;; 	  (eval `(defun ,newn () (start-test ,(string-downcase f))))
;; 	  (eval `(setf *link-list*
;; 		       (nconc *link-list*
;; 			       (list (list (format nil "/tests/~A" ,(string-downcase f))
;; 					   newn))))))))


(defun canvas ()
  ;; <html>
  ;;   <head>
  ;;       <title>Canvas</title>
  ;;       <meta charset='utf-8' />
  ;;   </head>
  ;;   <body>
  ;;       <canvas id='test' height='320' width='480'>Текст показывается, если элемент не поддерживается</canvas>
  ;;       <script type="text/javascript">
  ;;           var canvas  = document.getElementById("test");
  ;;           var ctx     = canvas.getContext('2d');
  ;;           /* Рисует контур прямоугольника на всю ширину и высоту canvas */
  ;;           ctx.strokeRect(0, 0, canvas.width, canvas.height);
  ;;       </script>
  ;;   </body>
  ;;   </html>
  (with-html
    (:html 
     (:head
      (:title "Canvas")
      ;; (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")
      ;; (:script 
      ;;  (progn
      ;; 	 (str (ps ((@ ($ document) ready)
      ;; 			(lambda ()
      ;; 			  (
      ;; 			   ;; (@ ($ "error") fade-out)
      ;; 			   (@ ($ "button") click)
      ;; 			   (lambda ()
      ;; 			     (let ((text (chain document (get-element-by-id "email") value)))
      ;; 			       (setf text (chain text (to-string) (trim)))
      ;; 			       (cond ((equal text "")
      ;; 				      ((@ ($ "#error") fade-in))
      ;; 				      (return false))))))))))))
      )
     (:body
      (:canvas :id "canvas" :height "320" :width "480"
	       "Текст показывается, если элемент не поддерживается")
      (:script :type "text/javascript"
	       ()
       )
	       (:p "Адрес электронной почты:" (:br)
		   (:input :type :text
			   :name "email"
			   :id "email"))
	       
      (:p "Это параграф")
      (:p :id "error" :style "display:none" "Ошибка! Ничего не введено")
      (:button :type "button"
	       ;;:onclick (ps-inline (on-click))
	       "Click me!")))))

;; (setf (alexandria:last-elt hunchentoot:*dispatch-table*)
;;       (hunchentoot:create-folder-dispatcher-and-handler "/" (resource-path "www")))
;; (push (hunchentoot:create-folder-dispatcher-and-handler "/css/" "~/work/lisp/mysql/css/") hunchentoot:*dispatch-table*)
;; (push (create-static-file-dispatcher-and-handler
;;        "/style.css" "~/work/lisp/mysql/css/style.css")
;;       *dispatch-table*)

;(hunchentoot:start *h*)





