(defpackage :input
  (:use :cl)
  (:documentation "Пакет потокового чтения данных из файла")
  (:export get-line
           open-file
           close-file))

(in-package :input)
(ql:quickload :generators)

(defparameter *stream* nil)
(defparameter *line-number* 0)

(defun open-file (filename)
  """Открытие файла на чтение"""
  (setf *line-number* 0)
  (setf *stream* (open filename
                       :direction :input
                       :if-does-not-exist :error)))

(defun close-file ()
  """Закрытие файла"""
  (close *stream*))

(defun print-line-number ()
  (format t "~%~%line number:~a~%" *line-number*)
  (setf *line-number* (+ 1 *line-number*)))

(defun read-new-line (filename)
  """Считать строку из файла"""
  (print-line-number)
					; (format t "file=~a~%" filename)
  (if filename (read-line *stream* nil :eof)
    (read-line)))

(defun get-line (filename)
  (generators:make-generator ()
			     (loop :for l = (read-new-line filename)
					; :until (eq l :eof)
				   :do (generators:yield l))))
