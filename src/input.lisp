(defpackage :input
  (:use :cl)
  (:export get-line
           open-file
           close-file))

(in-package :input)

(defparameter *stream* nil)
(defparameter *line-number* 0)

(defun open-file (filename)
    """Открытие файла на чтение"""
    (let ((*stream* (open filename
                        :direction :input
                        :if-does-not-exist :error)))))

(defun close-file ()
    """Закрытие файла"""
    (close *stream*))

(defun print-line-number ()
    (format t "line number ~a~%:\n" *line-number*))

(defun get-line ()
    """Считать строку из файла"""
    (print-line-number)
    (read-line *stream* nil :eof))