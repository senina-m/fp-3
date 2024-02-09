(defpackage :main
  (:use :cl)
  (:export main))

(in-package :main)
(load "src/input.lisp")
(load "lagrange.lisp")

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
    (push (first nums) (first win))
    (push (second nums) (second win))
    (if (>= (length (first win)) win-size) (format t "t") (format t "nil"))
    ;удаляем последний лишний элемент
    (when (>= (length (first win)) win-size)
          (list (butlast (first win) 1) ;butlast создаёт копию, где последнего элемента нет.
                 (butlast (second win) 1))))

; (add-values '((1 2 3) (4 5 6)) '(7 8) 3)

(defun push-line (win line win-size)
    """Добавление новой пару значений"""
    (if (null line) (error 'push-line-fault :text "line can't be empty")
        (let ((nums (parse-string-to-float line)))
            (cond ; считанная строка не должна быть пустой
                  ((null nums) (error 'push-line-fault :text "line has to contain numbers, but is null"))
                  ; в строке должно быть ровно два числа
                  ((/= (length nums) 2) (error 'push-line-fault :text (format nil "line has contain exectly two numbers but has ~{~a~}" nums)))
                  ; новый х должен быть больше последнего старого
                  ((< (first nums) (last (first win))) 
                   (error 'push-line-fault :text "x value in numbers pair has to be greater than last x value in window"))
                  ; всё в порядке, добавляем новые значения
                  (t (add-values win nums win-size))))))

(defun make-arrays-from-win (win)
    """Подготовка окна к апппроксимации"""
    (let ((n (length (first win))))
        (let  ;переворачиваем list-ы чтобы они были отсортированы, от ранних к поздним
          ((x-list (make-array (list n) :initial-contents (reverse (first win))))
           (y-list (make-array (list n) :initial-contents (reverse (second win)))))
        ;   (format t "~a~%" (aref x-list (- (/ n 2) 1)))
        ;   (format t "~a~%" (aref x-list (/ n 2)))
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
    (input:open-file filename)
    (let ((win nil))
        (loop :for line = (input:get-line)
            :until (eq line :eof)
            :do (handler-case
                    (progn (setf win (push-line win line win-size))
                           (print win)
                        ; (multiple-value-bind (x-list y-list point) (make-arrays-from-win win)
                                ; (cons (line (format t "line: (~a~%, ~a~%)\n" point (line:appr-line x-list y-list point)))
                                    ;   (lagrange (format t "line: (~a~%, ~a~%)\n" point (lagrange:appr-lagrange x-list y-list point))))))
                    (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe))))))
    (input:close-file))
)

(defun main (win-size filename &key (line nil) (lagrange nil))
    """Интрополяция потока точек"""
    (format t "line=~a~%" line)
    (format t "lagrange=~a~%" lagrange)
    (format t "win size=~a~%" win-size)
    (open-file filename)
    (let ((win nil))
        (loop :for line = (get-line)
            :until (eq line :eof)
            :do (handler-case
                    (progn (setf win (push-line win line win-size))
                           (print win)
                        ; (multiple-value-bind (x-list y-list point) (make-arrays-from-win win)
                                ; (cons (line (format t "line: (~a~%, ~a~%)\n" point (line:appr-line x-list y-list point)))
                                    ;   (lagrange (format t "line: (~a~%, ~a~%)\n" point (lagrange:appr-lagrange x-list y-list point))))))
                    (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe))))))
    (close-file))
)


"""
;тестовый сценарий, чтобы проверить, что сигналы работают
(defun error-caller ()
    (error 'push-line-fault :text "line has to contain numbers"))

(defun runner ()
    (loop for i from 0 to 3
        do 
        ; (print i )))
        (handler-case
            (progn (error-caller) (print i))
            (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe))))))
"""

(defun runner ()
    (handler-case
        (progn (print (push-line '((1 2 3) (1 2 3)) "3 5" 3)))
        (push-line-fault (pe) (format t "~a~%" (push-line-fault-text pe)))))