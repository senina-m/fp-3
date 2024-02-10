(defpackage :line
  (:use :cl)
  (:export appr-line))

(in-package :line)

(defun appr-line (x-list y-list point)
  """Линейная интрополяция"""
  (let ((n (length x-list)))
    (let ((x1 (aref x-list (- (/ n 2) 1)))
          (x2 (aref x-list (/ n 2)))
          (y1 (aref y-list (- (/ n 2) 1)))
          (y2 (aref y-list (/ n 2))))
      (+ y1 (* (- point x1) (/ (- y2 y1) (- x2 x1)))))))

					; (line:appr-line (make-array (list 2) :initial-contents '(3 2)) (make-array (list 2) :initial-contents '(4 1)) 3/2)
