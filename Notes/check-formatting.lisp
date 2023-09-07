;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A first pass at a parser/format checker.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(import '(alexandria:define-constant))




(define-constant +TeX-directory+ #P"/home/rkrug/Documents/DR/TeX/"
  :documentation "Where the project lives.")




;;; We define a reader for TeX files.
;;; Note that we can have several of these readers active at any one
;;; time.

;;; TODO: Check whether two TeX-readers are reading from the same
;;; file. This would make line-numbers meaningless, and could confuse
;;; other things.

(defparameter *TeX-readtable* (copy-readtable)
  "A readtable in which #\\ is #\\, not a single escape.")

(set-macro-character #\\ (constantly #\\) nil *TeX-readtable*)

(defstruct (TeX-reader (:constructor %make-TeX-reader))
  "A TeX reader."
  (filename     ""  :type string :read-only 'T)
  (current-line -1  :type integer)
  (contents     #() :type array  :read-only 'T))

(defun make-TeX-reader (filename)
  (let* ((*readtable* *TeX-readtable*)
         (contents-list (uiop:read-file-lines filename))
         (contents (make-array (length contents-list)
                               :initial-contents contents-list)))
    (%make-TeX-reader :filename     filename
                      :current-line -1
                      :contents     contents)))

(defun read-TeX-line (r)
  (aref (TeX-reader-contents r) (incf (TeX-reader-current-line r))))

(warn
