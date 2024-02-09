(defpackage :newton
  (:use :cl)
  (:export appr-newton))

(in-package :newton)

(defun check-h-const (x-list accuracy)
    """Проверка на равность промежутков между значениями х"""
    (let ((h (abs (- (aref x-list 1) (aref x-list 0)))))
        (loop for i from 1 to (- n 1)
            ;if abs(abs(x[i+1] - x[i]) - h) > e
            (if (> accuracy h)
            ; (> (abs (- (abs (- (aref x-list i) 
            ;                           (aref x-list (- i 1))))
            ;                   h)) 
            ;         accuracy)
                (return-from check-h-const nil))
                nil))
    t)

(defun appr-newton (window)
    (let ((x ()) (y ()) (point ()))
        (if (check-h-const x 0.001)
            (newton-const x y point)
            (newton-not-constant x y point))))


(defun finit-diff-const (x-list y-list)
    """Вычисление конечных разностей для значений с равными промежутками"""
    ; (finit-diff-const (make-array '(3) :initial-contents (list 1 2 3)) (make-array '(3) :initial-contents (list 4 5 6)))
    (let ((n (length x-list)) 
          (diffs (make-array (list (length x-list) (length x-list))
                             :initial-element 0))
          (k 1))
        (loop 
            for i from 0 to (- n 1)
                do (setf (aref diffs i 0) (aref y-list i)))
        (loop while (<= k n)
              do (progn 
                    (loop for i from 0 to (- (- n k) 1)
                        ;diffs[i][k] = (diffs[i + 1][k - 1] - diffs[i][k - 1]) / (x[i + k] - x[i])
                        do (progn (format t "i=~a~%" i) (format t "k=~a~%" k)
                                (setf (aref diffs i k) 
                                 (/ (- (aref diffs (+ 1 i) (- k 1))
                                       (aref diffs i (- k 1)))
                                    (- (aref x-list (+ i k))
                                       (aref x-list i))))))
                    (setf k (+ 1 k))))
        ;print
        (dotimes (i n)
            (dotimes (j n)
                (print (aref diffs i j))))
    diffs))

; (defun test (y-list n diffs)
;         (loop for y in y-list
;             for i from 0 to n
;             do (setf (aref diffs i 0) y)))
 
(defun newton-const (x-list y-list x-point)
    (let ((n (length x-list)) 
          (diffs (finit-diff-const x-list y-list))
          (h (abs (- (aref x-list 1) (aref x-list 0)))))))