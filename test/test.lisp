(load "~/quicklisp/setup.lisp")
(load "src/main.lisp")
(ql:quickload :lisp-unit)

(defun compare-files (fst-name snd-name)
  (with-open-file (i-res fst-name
                         :direction :input
                         :if-does-not-exist :error)
		  (with-open-file (i-data snd-name
					  :direction :input
					  :if-does-not-exist :error)
				  (let ((result t))
				    (loop :for res = (read-line i-res nil :eof)
					  :for data = (read-line i-data nil :eof)
					  :do (if (string= res data) nil (setf result nil))
					  :until (and (eq res :eof) (eq data :eof)))
					; (format t "RES=~a~%" result)
				    (return-from compare-files result)))))

(defun parse-string-to-float-test (res-name answer-name str)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		(format output "(~{~a ~})~%" (main:parse-string-to-float str)))
  (compare-files res-name answer-name))

(defun add-values-test (res-name answer-name win nums size)
	(with-open-file (output res-name
							:direction :output
							:if-exists :supersede)
			(format output "(~{~a ~})~%" (main:add-values win nums size)))
	(compare-files res-name answer-name))

(defun push-line-test (res-name answer-name win line size)
	(with-open-file (output res-name
							:direction :output
							:if-exists :supersede)
		(handler-case
			(format output "(~{~a ~})~%" (main:push-line win line size))
			(main:push-line-fault (pe) (format output "~a" (main:push-line-fault-text pe)))))
	(compare-files res-name answer-name))

(defun print-array (output arr)
	(format output "(")
  	(loop for i below (car (array-dimensions arr)) do
    	(let ((cell (aref arr i)))
            (format output "~a " cell)))
    (format output ")~%"))

(defun create-arrays-test (res-name answer-name win)
	(with-open-file (output res-name
							:direction :output
							:if-exists :supersede)
			(multiple-value-bind (x-list y-list point) (main:make-arrays-from-win win)
					(print-array output x-list)
					(print-array output y-list)
					(format output "~a~%" point)))
	(compare-files res-name answer-name))

(lisp-unit:define-test test-1
		       (lisp-unit:assert-true (parse-string-to-float-test "test/files/result1" "test/files/answers-1" "1 2 3")))

(lisp-unit:define-test test-2
		       (lisp-unit:assert-true (parse-string-to-float-test "test/files/result2" "test/files/answers-2" "1.2")))

(lisp-unit:define-test test-3
		       (lisp-unit:assert-true (parse-string-to-float-test "test/files/result3" "test/files/answers-3" "-1	2.3")))

(lisp-unit:define-test test-4
		       (lisp-unit:assert-true (add-values-test "test/files/result4" "test/files/answers-4" '((1 2 3) (4 5 6)) '(7 8) 3)))

(lisp-unit:define-test test-5
		       (lisp-unit:assert-true (add-values-test "test/files/result5" "test/files/answers-5" '((1) (2)) '(3 4) 2)))

(lisp-unit:define-test test-6
		       (lisp-unit:assert-true (add-values-test "test/files/result6" "test/files/answers-6" nil '(7 8) 3)))

(lisp-unit:define-test test-7
		       (lisp-unit:assert-true (push-line-test "test/files/result7" "test/files/answers-7"  nil "7 8" 3)))

(lisp-unit:define-test test-8
		       (lisp-unit:assert-true (push-line-test "test/files/result8" "test/files/answers-8" '((1 2 3) (4 5 6)) nil 3)))

(lisp-unit:define-test test-9
		       (lisp-unit:assert-true (push-line-test "test/files/result9" "test/files/answers-9" '((1 2 3) (4 5 6)) "kljsfd" 3)))

(lisp-unit:define-test test-10
		       (lisp-unit:assert-true (push-line-test "test/files/result10" "test/files/answers-10" '((1 2 3) (4 5 6)) "7 8 9" 3)))

(lisp-unit:define-test test-11
		       (lisp-unit:assert-true (create-arrays-test "test/files/result11" "test/files/answers-11" '((1 2) (4 5)))))

(lisp-unit:define-test test-12
		       (lisp-unit:assert-true (create-arrays-test "test/files/result12" "test/files/answers-12" '((1 2 3 4) (5 6 7 8)))))

(lisp-unit:run-tests)

; """
; ;тестовый сценарий, чтобы проверить, что сигналы работают
; (defun error-caller ()
;     (error 'push-line-fault :text "line has to contain numbers"))

; (defun runner ()
;     (loop for i from 0 to 3
;         do 
;         ; (print i )))
;         (handler-case
;             (progn (error-caller) (print i))
;             (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe))))))
; """
