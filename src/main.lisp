(defpackage :main
  (:use :cl)
  (:documentation "Модуль для интрополяции")
  (:export :main
           :parse-string-to-float
           :add-values
           :push-line-fault
           :push-line-fault-text
           :push-line
           :make-arrays-from-win))

(in-package :main)
(load "src/input.lisp")
(load "src/line.lisp")
(load "src/lagrange.lisp")
(ql:quickload :generators)
					; (use-package :generators)

(defun parse-string-to-float (line)
  (with-input-from-string (s line)
			  (loop
			   :for num := (read s nil nil)
			   :while num
			   :collect num)))

(define-condition push-line-fault (error)
  ((text :initarg :text :reader push-line-fault-text)))

(defun add-values (win nums win-size)
					;добавляем новый элемент
  (if (null win)
      (progn (push (list (first nums)) win)
             (push (list (second nums)) win))
    (progn (push (first nums) (first win))
           (push (second nums) (second win))))
					; (if (>= (length (first win)) win-size) (format t "t") (format t "nil"))
					;удаляем последний лишний элемент
  (if (>= (length (first win)) (+ win-size 1))
      (list (butlast (first win) 1) ;butlast создаёт копию, где последнего элемента нет.
            (butlast (second win) 1))
    win))

					; (add-values '((1 2 3) (4 5 6)) '(7 8) 3)
					; (add-values '((1) (2)) '(3 4) 2)
					; (add-values nil '(7 8) 3)

(defun push-line (win line win-size)
  """Добавление новой пару значений"""
  (if (null line) (error 'push-line-fault :text "line can't be empty")
    (let ((nums (parse-string-to-float line)))
      (cond ; считанная строка не должна быть пустой
       ((null nums) (error 'push-line-fault :text "line has to contain numbers, but is null"))
					; в строке должно быть ровно два числа
       ((/= (length nums) 2) (error 'push-line-fault :text (format nil "line has contain exectly two numbers but has (~{~a ~})" nums)))
					; новый х должен быть больше последнего старого
       ((and (not (null win))
             (<= (first nums) (first (first win))))
        (error 'push-line-fault :text
               (format t "x(~a) value in numbers pair has to be greater than last x(~a) value in window"
                       (first nums) (first (first win)))))
					; всё в порядке, добавляем новые значения
       (t (add-values win nums win-size))))))

(defun make-arrays-from-win (win)
  """Подготовка окна к апппроксимации"""
  (let ((n (length (first win))))
    (let  ;переворачиваем list-ы чтобы они были отсортированы, от ранних к поздним
        ((x-list (make-array (list n) :initial-contents (reverse (first win))))
         (y-list (make-array (list n) :initial-contents (reverse (second win)))))
      (values x-list
              y-list
              (/ (+ (aref x-list (- (/ n 2) 1))
                    (aref x-list (/ n 2)))
                 2)))))


(defun main (win-size filename &key (line nil) (lagrange nil))
  """Интрополяция потока точек"""
  (format t "line=~a~%" line)
  (format t "lagrange=~a~%" lagrange)
  (format t "win size=~a~%" win-size)
  (if (/= (mod win-size 2) 0) (format t "Can't find middle of odd window")
    (progn (input:open-file filename)
           (let ((win nil))
             (loop with l = (input:get-line)
                   :until (eq (generators:next l) :eof)
                   :do (handler-case
                           (progn (setf win (push-line win (generators:next l) win-size))
                                  (print win)
                                  (terpri)
                                  (when (= (length (first win)) win-size)
                                    (multiple-value-bind (x-list y-list point) (make-arrays-from-win win)
							 (when line (format t "line-appr: (~a ~a)~%" point (line:appr-line x-list y-list point)))
							 (when lagrange (format t "lagrange-appr: (~a ~a)~%" point (lagrange:appr-lagrange x-list y-list point))))))
                         (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe))))))
           (input:close-file))))

					; (progn
					; (input:open-file "input")
					; (loop with l = (input:get-line)
					;   ; :repeat 10
					;   :until (eq (generators:next l) :eof)
					;   :do (print (generators:next l)))
					;   (input:close-file))

					; (defun dice (input)
					; (generators:make-generator ()
					;     (with-open-file (file input
					;                   :direction :input
					;                   :if-does-not-exist :error)
					;       (loop :for l = (read-line file nil :eof)
					;             :until (eq l :eof)
					;             :do (progn
					;             ; (print l)
					;                   (generators:yield l))))))

					; (defun get-line ()
					;   (generators:make-generator ()
					;     (loop :for l = (read-new-line)
					;           ; :until (eq l :eof)
					;           :do (generators:yield l))))

					; (loop with l = (dice "input" )
					;   :repeat 10
					;   :do (print (generators:next l)))
					;   :until (eq (generators:next l) :eof)
