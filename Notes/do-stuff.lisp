;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; A first pass at a parser/format checker.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; With Slime, C-c C-c, first loads the form and then compiles
;; it. SBCL is overly picky in such situations, so I use this:
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

;; Where the project lives:
(define-constant +TeX-directory+ #P"/home/rkrug/Documents/DR/TeX/")

(setq *default-pathname-defaults* +TeX-directory+)




;; We want to be able to read TeX files, so we have to fiddle with
;; the readtable to handle the use of #\\. That is, we want #\\ to be
;; #\\, rather than a single escape.
(defparameter *TeX-readtable* (copy-readtable))

;; Make #\\ read as the character #\\
(set-macro-character #\\ (constantly #\\) nil *TeX-readtable*)

(defun TeX-read-line (s)
  (let ((*readtable* *TeX-readtable*))
    (read-line s nil "EOF")))




(defun check-header-beginning (s)
  (let ((65% (make-sequence 'string 65 :initial-element #\%)))
    (and (string= (TeX-read-line s) 65%)
         (string= (TeX-read-line s) "%%%%")
         (string= (TeX-read-line s) "%%%% The (original) Douay Rheims Bible ")
         (string= (TeX-read-line s) "%%%%"))))

(defun check-header-ending (s)
  (let ((65% (make-sequence 'string 65 :initial-element #\%)))
    (and (string= (TeX-read-line s) "%%%%")
         (string= (TeX-read-line s) 65%)
         (string= (TeX-read-line s) "")
         (string= (TeX-read-line s) "")
         (string= (TeX-read-line s) "")
         (string= (TeX-read-line s) ""))))




(defun check-book-header (s)
  (and (check-header-beginning s)
       (let ((volume-line (TeX-read-line s))
             (book-line   (TeX-read-line s))
             ;; Volume and book as appears in the header text
             (volume (substitute #\  #\-
                                 (car (nthcdr (- (length (pathname-directory s))
                                                 2)
                                              (pathname-directory s)))))
             (book (substitute #\  #\-
                               (car (last (pathname-directory s))))))
         (and (string= (subseq volume-line 0 5) "%%%% ")
              (string= (subseq volume-line 5 'NIL)     volume)
              (string= (subseq book-line 0 5)   "%%%% ")
              (string= (subseq book-line 5 'NIL)     book)))
       (check-header-ending s)))

(defun check-start-book (s)
  (let ((line (TeX-read-line s)))
    (and (string= (subseq line 0 16)   "\\startcomponent ")
         (string= (subseq line 16 NIL) (pathname-name s))
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "\\project douay-rheims")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    ""))))

(defun check-stop-book (s)
  (and (string= (TeX-read-line s) "\\stopcomponent")
       (equal   (TeX-read-line s) "EOF")))

(defun check-book (pathname)
  (with-open-file (s pathname :direction :input)
    (format t "~&Book: ~A~%" s)
    (and (string-equal (pathname-name s)
                       (car (last (pathname-directory s))))
         (check-book-header s)
         (check-start-book  s)
         ;;(check-chapters    s)
         ;;(check-stop-book   s)
         )))




(defun check-volume-header (s)
  (and (check-header-beginning s)
       (let ((line (TeX-read-line s))
             ;; Volume as appears in the header text
             (volume (substitute #\  #\-
                                 (car (last (pathname-directory s))))))
         (and (string= (subseq line 0 5) "%%%% ")
              (string= (subseq line 5 'NIL) volume)))
       (check-header-ending s)))

(defun check-start-volume (s)
  (let ((line (TeX-read-line s)))
    (and (string= (subseq line 0 14)   "\\startproduct ")
         (string= (subseq line 14 NIL) (pathname-name s))
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "\\project douay-rheims")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    ""))))

(defun check-books (s)
  (let ((line (TeX-read-line s)))
    (if (string= line "")
        ;; second blank line terminates list of books.
        'T
        (and (string= (subseq line 0 11) "\\component ")
             (let* ((directory (car (last (pathname-directory s))))
                    (pathname (concatenate 'string
                                           directory
                                           "/"
                                           (subseq line 11 'NIL)
                                           ".tex"))
                    (book (merge-pathnames pathname
                                           +TeX-directory+)))
               (and (check-book book)
                    (string= (TeX-read-line s) "")
                    (check-books s)))))))

(defun check-stop-volume (s)
  (and (string= (TeX-read-line s) "\\stopproduct")
       (equal   (TeX-read-line s) "EOF")))

(defun check-volume (pathname)
  (with-open-file (s pathname :direction :input)
    (format t "~&Volume: ~A~%" s)
    (and (string-equal (pathname-name s)
                       (car (last (pathname-directory s))))
         (check-volume-header s)
         (check-start-volume  s)
         (check-books         s)
         (check-stop-volume   s))))




(defun check-project-header (s)
  (and (check-header-beginning s)
       (let ((line (TeX-read-line s)))
         (and (string= (subseq line 0 5) "%%%% ")
              (string= (subseq line 5 'NIL) "Project definition")))
       (check-header-ending s)))

(defun check-start-project (s)
  (let ((line (TeX-read-line s)))
    (and (string= (subseq line 0 14)   "\\startproject ")
         (string= (subseq line 14 NIL) (pathname-name s))
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    "\\environment environment")
         (string= (TeX-read-line s)    "")
         (string= (TeX-read-line s)    ""))))

(defun check-volumes (s)
  (let ((line (TeX-read-line s)))
    (if (string= line "")
        ;; second blank line terminates list of volumes.
        'T
        (and (string= (subseq line 0 9) "\\product ")
             (let* ((pathname (concatenate 'string
                                           (subseq line 9 'NIL)
                                           ".tex"))
                    (volume (merge-pathnames pathname
                                             +TeX-directory+)))
               (and (check-volume volume) 
                    (string= (TeX-read-line s) "")
                    (check-volumes s)))))))

(defun check-stop-project (s)
  (and (string= (TeX-read-line s) "\\stopproject")
       (equal   (TeX-read-line s) "EOF")))

(defun check-project ()
  (let ((project (merge-pathnames "douay-rheims.tex"
                                  +TeX-directory+)))
  (with-open-file (s project :direction :input)
    (and (check-project-header s)
         (check-start-project  s)
         (check-volumes        s)
         (check-stop-project   s)))))

