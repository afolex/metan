(in-package :hunch)
(defun make-pie-data (par-list)
  "Param list is collection of (label, value, color)"
  (lambda () `(str (ps (defvar pie-data (create labels (array ,@(loop for item in par-list
								    collect `(,@(nth 0 item))))
							  datasets (array (create data (array ,@(loop for item in par-list
												   collect `(,@ (nth 1 item))))
										  background-color (array ,@(loop for item in par-list
													       collect `(,@ (nth 2 item))))))))))))

(defun js-test ()
       (with-html
	(:html
	 (:head
	  (:title (fmt "Таблица" ))    
	  (:link :rel "stylesheet" :type "text/css"
		 :href "/css/style.css")
  	  (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")
	  (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js")

	  (:script :src "script/Chart.js" :type "text/javascript"))
	 (:body
	  (:canvas :id "myCanvas" :width "200" :heights "100" :style "border:1px solid #000000;")
	  ;; <canvas id="myCanvas" width="200" height="100" style="border:1px solid #000000;">
	  (:p "test js")
	  (:script
	   (str (ps
		  ;; (let)
		   (defvar chart-data
			  (create labels (array "January" "February" "March" "April" "May" "June" "July")
				  datasets (array
					    (create label "First"
						    data (array 65 59 80 81 56 55 40))
					    (create label "Second"
						    data (array 15 99 20 121 16 25 70)))))
		  (defvar ctx (chain document (get-element-by-id "myCanvas") (get-context "2d")))
		  (defvar my-chart (new (-chart ctx
				       (create type 'line'
					       data chart-data)))))))))))

(defmacro js-pie-test ()
       `(with-html
	(:html
	 (:head
	  (:title (fmt "Пирог" ))    
	  (:link :rel "stylesheet" :type "text/css"
		 :href "/css/style.css")
  	  (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js")
	  (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js")

	  (:script :src "script/Chart.js" :type "text/javascript"))
	 (:body

	  (:div :id "container" :style "width:30%; height:30%;" 
		(:canvas :id "myCanvas"  :width "50" :heights "50" :style "border:1px solid #000000;"))
	  ;; <canvas id="myCanvas" width="200" height="100" style="border:1px solid #000000;">
	  (:p "test js")
	  (:script
	   ,(funcall (make-pie-data '(("Red" 1 "red") ("Yellow" 2 "yellow")))))
	  (:script  (str
		     (ps
		       (defvar ctx (chain document (get-element-by-id "myCanvas") (get-context "2d")))
		       (defvar my-chart (new (-chart ctx
	  						 (create type 'pie'
	  							 data pie-data
	  							 options (create legend (create position "right"))))))
		       (defvar canvas (chain document (get-element-by-id "myCanvas") ))
		       )))))))

(defun js-pie ()
  (js-pie-test))

(defmacro make-pie-data2 (par-list)
  "Param list is collection of (label, value, color)"
  `(create labels (array ,@(loop for item in par-list
			       collect `(nth 0 ,item)))
     datasets (array (create data (array ,@(loop for item in par-list
					      collect `(nth 1 ,item)))
		      background-color (array ,@(loop for item in par-list
					      collect `(nth 2 ,item)))))
     ))

;; var data = {
;;     labels: [
;;         "Red",
;;         "Blue",
;;         "Yellow"
;;     ],
;;     datasets: [
;;         {
;;             data: [300, 50, 100],
;;             backgroundColor: [
;;                 "#FF6384",
;;                 "#36A2EB",
;;                 "#FFCE56"
;;             ],
;;             hoverBackgroundColor: [
;;                 "#FF6384",
;;                 "#36A2EB",
;;                 "#FFCE56"
;;             ]
;;         }]
;; };

		    ;; (create labels
		    ;; 	    (array "Red" "Blue" "Yellow" )
		    ;; 	    datasets
		    ;; 	    (array (create data (array 300 50 100)
		    ;; 			   background-color
		    ;; 			   (array "red" "blue" "yellow")
		    ;; 			   ;; hoverBackgroundColor
		    ;; 			   ;; (array
		    ;; 			   ;;  "red"
		    ;; 			   ;;  "blue" "yellow")
		    ;; 			   )))


;; (defmacro script-make-color1 (clr1 clr2)
;; "Add script with function to change color"
;;   `(with-html-output (*standard-output*)
;;     (:script
;;      (str (ps
;; 	   (defun make-color (id qty cntname)
;; 	     (let ((sum1 0)
;; 		   (check-list (array))
;; 		   (val1 "")
;; 		   (val2 0)
;; 		   (val3 "")
;; 		   (ptr ""))
;; 	       (setf ptr (concatenate 'string id "n"  cntname ))
;; 	       (setf val1 (chain document (get-element-by-id ptr) value))
;; 	       (if (eql  val1  "")
;; 		   (return false))
;; 	       (setf val2 (parse-int val1))
;; 	       (setf val3 (chain val2 (to-string 10)))
;; 	       (when (or (not (eql val1 val3))
;; 			 (< val1 0)
;; 			 (> val1 qty))
;; 		 (alert "Ошибка")
;; 		 (setf  (chain document (get-element-by-id ptr) value) "")
;; 		 (setf (chain document (get-element-by-id id) style background) ,clr1)
;; 		 (return false))
;; 	       (when (= val1 val3)
;; 		 (dotimes (ctr qty t)
;; 		   (progn
;; 		     (setf ctr1 (+ ctr 1))
;; 		     (setf ptr (concatenate 'string id "n"  ctr1 ))
;; 		     (setf val1 (chain document (get-element-by-id ptr) value))
;; 		     (when (not (= "" val1))
;; 		       (setf sum1 (+ sum1 (parse-int val1)))
;; 		       (if (= (chain check-list (index-of val1)) -1) 
;; 			   (chain check-list (push val1))
;; 			   (setf (chain document (get-element-by-id id) style background) ,clr1))))))
;; 	       (if (eql sum1 (/ (* (+ 1 qty) qty) 2))
;; 		   (setf (chain document (get-element-by-id id) style background) ,clr2)
;; 		   (setf (chain document (get-element-by-id id) style background) ,clr1)))
;; 	     (return true)))))))
