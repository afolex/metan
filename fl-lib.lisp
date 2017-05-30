(defpackage :fl-lib
  (:use :common-lisp)
  (:export :slurp-stream   
           :read-file-as-seq
           :write-to-file))

(in-package :fl-lib)
;;;; Работа с файлами - прочитать - записать
;;читает послед-ть из stream
(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

;; открывает файл и передает stream для чтения в послед-ть
(defun read-file-as-seq (filename charset)
  (with-open-file (file-stream filename :external-format
			       #+sbcl `(,charset)
			       #+clisp (ext:make-encoding
                                                           :charset charset
                                                           :line-terminator :dos))
    (slurp-stream file-stream)))

;; записывает в файл с именем name строку  content
(defun write-to-file (name content charset) ; content and name are both strings
  (with-open-file
    ( stream  name ;  creating a stream object named name
              :direction :output
              :if-exists :append
              :if-does-not-exist :create
              :external-format
      	      #+sbcl `(,charset) 
	      #+clisp (ext:make-encoding
				 :charset charset  ;'charset:cp866
				 :line-terminator :dos) ;  creates a file named name if it does not already exist,
)
    (do ((i 0 (+ i 100000))
	 (j 100000 (+ j 100000))
	 (l (length content)))
	((< l i))
      (if (> j l)
					;          (format stream (subseq content i l))
					;        (format stream (subseq content i j)))))
	  (write-sequence (subseq content i l) stream)
	  (write-sequence (subseq content i j) stream))))


  ;	  (format stream content)) ; content is the input that is written to the file via the format function

  name) ; This function returns the name of the file for further processing !

;;;;****************************************
