(defpackage :lagrange
  (:use :cl)
  (:export appr-lagrange))

(in-package :lagrange)

(defun appr-lagrange (x-list y-list point)
  """Интрополяция методом Лагранжа"""
  (let ((res 0)
        (n (length x-list)))
    (loop :for i from 0 to (- n 1)
          :do (let ((c1 1)
                    (c2 1))
                (loop for j from 0 to (- n 1)
                      :do (when (/= i j)
                            (progn (setf c1 (* c1 (- point (aref x-list j))))
                                   (setf c2 (* c2 (- (aref x-list i) (aref x-list j)))))))
                (setf res (+ res (* (aref y-list i) (/ c1 c2))))))
    res))

					; (main 2 "input" :lagrange t)
