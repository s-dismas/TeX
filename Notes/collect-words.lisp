
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Notes to myself
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; What to do with \Fix{}{}{}, \Sc
;;; Also Cite, XRef

;;; Hebrew diacritics should stick with the modified character
;;; Greek diacritics should already be part of the character?
;;; Oxia vs. Tonos, pick one.

;;; (cl-unicode:combining-class #\֝)
;;; (cl-unicode:code-block #\֝)
;;; (cl-unicode:has-property #\ô "LC")
;;; (cl-unicode:lowercase-mapping #\Ô)
;;; (defvar *x* "abc")
;;; (setf (elt *x* 1) #\GREEK_SMALL_LETTER_UPSILON_WITH_VARIA)


;;; "abcãẽĩõũ"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Background
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-unicode"))



;; SBCL is overly picky about constant redefinitions:
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(define-constant +TeX-directory+ #P"/home/rkrug/Documents/DR/TeX/")

(define-constant +word-separators+ '(#\  #\. #\, #\; #\: #\! #\? #\~
                                #\{ #\} #\( #\) #\[ #\] #\' #\`))

(define-constant +exclude-line+ '("\\startproject"
                                  "\\stopproject"
                                  "\\project"
                                  "\\startproduct"
                                  "\\stopproduct"
                                  "\\product"
                                  "\\startcomponent"
                                  "\\stopcomponent"
                                  "\\component"
                                  "\\environment"
                                  ;; Check that these are not too aggressive.
                                  "\\CNote"
                                  "\\XRef"
                                  "\\Cite"
                                  ;; This is certainly too aggressive.
                                  "\\Fix"))

(define-constant +max-files-to-list+ 3)



(defparameter *all-words* (make-hash-table :test 'equal :size 40000))

;; This will be further processed in phase-2.
(defparameter *english-words-1*   (make-hash-table :test 'equal :size 30000))
(defparameter *tilde-words-1* (make-hash-table :test 'equal :size 1000))

(defparameter *english-words*   (make-hash-table :test 'equal :size 30000))
(defparameter *tilde-words*     (make-hash-table :test 'equal :size 1000))

(defparameter *TeX-words*       (make-hash-table :test 'equal :size 100))
(defparameter *greek-words*     (make-hash-table :test 'equal :size 500))
(defparameter *hebrew-words*    (make-hash-table :test 'equal :size 20))
(defparameter *other-words*     (make-hash-table :test 'equal :size 10))



;; We want to be able to read TeX files, so we have to fiddle with
;; the readtable to handle the use of #\\. That is, we want #\\ to be
;; #\\, rather than a single escape.
(defparameter *TeX-readtable* (copy-readtable))

(set-macro-character #\\ (constantly #\\) nil *TeX-readtable*)

(defun TeX-read-line (s)
  (let ((*readtable* *TeX-readtable*))
    (read-line s nil 'eof)))


(defun has-tilde (x) (member x '(#\ã #\ẽ #\ĩ #\õ #\ũ)))

(define-constant +tilde-to-base+ '((#\ã . #\a)
                                   (#\ẽ . #\e)
                                   (#\ĩ . #\i)
                                   (#\õ . #\o)
                                   (#\ũ . #\u)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; We paw through the .tex files (other than environment.tex) under
;;;; +TeX-directory+, storing all the words found in the hash table
;;;; *all-words*. If the word has been seen in no more than
;;;; +max-files-to-list+ files, we store the count, and the files
;;;; where it was seen as the words value.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun process-words (words file)
  (dolist (word words)
    (unless (or (string= word "")
                (parse-integer word :junk-allowed 'T))
      (let* ((value (gethash word *all-words*))
             (new-value (cond ((null value)
                               `(1 (,file)))
                              ((< (car value) +max-files-to-list+)
                               (destructuring-bind (count list)
                                   value
                                 `(,(+ 1 count) ,(cons file list))))
                              (T
                               `(,(+ 1 (car value)) ())))))
        (setf (gethash word *all-words*) new-value)))))

(defun exclude-line (line)
  (or (equal 0 (position #\% line))
      (let* ((trimmed-line (string-left-trim "#\ " line))
             (pos (or (position #\{ trimmed-line)
                      (position #\  trimmed-line))))
        (when pos
          (member (subseq trimmed-line 0 pos)
                  +exclude-line+
                  :test 'string=)))))

(defun process-file (file)
  (with-open-file (s file :direction :input :if-does-not-exist nil)
    (when s
      (do ((line (TeX-read-line s) (TeX-read-line s)))
          ((eq line 'eof) "Reached end of file.")
        (unless (exclude-line line)
          (let* ((comment-start (position #\% line :test #'char=))
                 (words (uiop:split-string (subseq line 0 comment-start)
                                           :separator +word-separators+)))
            (process-words words file)))))))

(defun words-collector (dir)
  (let ((files (uiop:directory-files dir)))
    (dolist (file files)
      (let* ((pathname (uiop:unix-namestring file))
             (length (length pathname)))
        (when (and (string= (subseq pathname (+ -3 length))
                            "tex")
                   (not (string= (subseq pathname (+ -15 length))
                                 "environment.tex")))
          (process-file file))))))

(defun collect-words ()
  (clrhash *all-words*)
  (uiop:collect-sub*directories +TeX-directory+ 
                                (constantly 'T) 
                                (constantly 'T)
                                #'words-collector))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process Words Phase One
;;;;
;;;; We separate *all-words* into:
;;;; 1. *english-words-1* --- includes latin also --- will be
;;;; further processed in phase 2. See also collate-english.
;;;; 2. *tilde-words-1* --- words with a vowel that has a tilde. This
;;;; is a scribal abbreviation indicating that an 'n' or 'm' has been
;;;; elided. We collect these here so that we can try to guess the
;;;; correct expansion in phase 2. These will be a subset of words in
;;;; *english-words-1*.
;;;; 3. *greek-words*
;;;; 4. *TeX-words*
;;;; 5. *hebrew-words*
;;;; 6. *other-words* --- things that do not belong anywhere else.
;;;;
;;;; We do this separation based on the unicode code-block of the
;;;; first character.
;;;; The values stored in these hashtables is just the word itself
;;;; except for *english-words-1* --- where an entry may come from
;;;; multiple entries --- in which case we store all the original
;;;; words.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun normalize-word (word)
  (let ((n (length word))
        (new-word (make-array 0
                              :element-type 'character
                              :fill-pointer 0
                              :adjustable t)))
    (do ((i 0))
        ((= i n) new-word)
      (cond ((and (char= (char word i) #\v)
                  (< (+ i 1) n)
                  (char= (char word (+ i 1)) #\v))
             (vector-push-extend #\w new-word)
             (setf i (+ i 2)))
            ((and (char= (char word i) #\V)
                  (< (+ i 1) n)
                  (char= (char word (+ i 1)) #\V))
             (vector-push-extend #\W new-word)
             (setf i (+ i 2)))
            ((char= (char word i) #\ſ)
             (vector-push-extend #\s new-word)
             (setf i (+ i 1)))
            (T
             (vector-push-extend (char word i) new-word)
             (setf i (+ i 1)))))))

(defun collate-english (word)

  (let ((num-tildes (count-if #'has-tilde word)))
    (when (> num-tildes 2)
      (format T "~&Just found a word with more than two tildes: ~A"
              word)))

  (let ((normalized-word (normalize-word word)))
    (when (find-if #'has-tilde word)
      (let ((old-value (gethash normalized-word *tilde-words-1*)))
        (if old-value
            (setf (gethash normalized-word *tilde-words-1*)
                  (union old-value `(,word) :test #'string=))
            (setf (gethash normalized-word *tilde-words-1*)
                  `(,word)))))
    (let ((old-value (gethash normalized-word *english-words-1*)))
      (if old-value
          (setf (gethash normalized-word *english-words-1*)
                (union old-value `(,word) :test #'string=))
          (setf (gethash normalized-word *english-words-1*)
                `(,word))))))

(defun process-collected-words-phase-1 ()
  (clrhash *TeX-words*)
  (clrhash *greek-words*)
  (clrhash *hebrew-words*)
  (clrhash *other-words*)
  (clrhash *tilde-words-1*)
  (clrhash *english-words-1*)

  (with-hash-table-iterator (my-iterator *all-words*)
    (loop
      (multiple-value-bind (entry-p word value)
          (my-iterator)
        (declare (ignore value))
        (if entry-p
            (let* ((first-char (char word 0))
                   (code-block (cl-unicode:code-block first-char)))
              (cond ((char= first-char #\\)
                     (setf (gethash word *TeX-words*) `(,word)))
                    ((search "Greek" code-block :test #'string-equal)
                     (setf (gethash word *greek-words*) `(,word)))
                    ((search "Hebrew" code-block :test #'string-equal)
                     (setf (gethash word *hebrew-words*) `(,word)))
                    ((search "Latin" code-block :test #'string-equal)
                     (collate-english word))
                    (T
                     (setf (gethash word *other-words*) `(,word)))))
            (return))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Process Words Phase Two
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun capitalize-word (word)
  (let ((new-word (copy-seq word)))
    (setf (char new-word 0)
          (cl-unicode:uppercase-mapping (char word 0)))
    new-word))

(defun uncapitalize-word (word)
  (let ((new-word (copy-seq word)))
    (setf (char new-word 0)
          (cl-unicode:lowercase-mapping (char word 0)))
    new-word))

(defun add-1-to-2 (letter guesses)
  (vector-push-extend letter (first guesses))
  (vector-push-extend letter (second guesses)))

(defun add-2-to-2 (letter guesses)
  (vector-push-extend letter (first guesses))
  (vector-push-extend letter (second guesses))
  (vector-push-extend #\n (first guesses))
  (vector-push-extend #\m (second guesses)))

(defun add-1-to-4 (letter guesses)
  (vector-push-extend letter (first guesses))
  (vector-push-extend letter (second guesses))
  (vector-push-extend letter (third guesses))
  (vector-push-extend letter (fourth guesses)))

(defun add-2-to-4 (letter current-pos first-tilde-pos guesses)
  (vector-push-extend letter (first guesses))
  (vector-push-extend letter (second guesses))
  (vector-push-extend letter (third guesses))
  (vector-push-extend letter (fourth guesses))
  (vector-push-extend #\n (first guesses))
  (if (= current-pos first-tilde-pos)
      (vector-push-extend #\n (second guesses))
      (vector-push-extend #\n (third guesses)))
  (if (= current-pos first-tilde-pos)
      (vector-push-extend #\m (third guesses))
      (vector-push-extend #\m (second guesses)))
  (vector-push-extend #\m (fourth guesses)))

(defun add-to-all (letter expand-abbreviation-p num-guesses
                   current-pos first-tilde-pos
                   guesses)
  (if (= num-guesses 2)
      (if expand-abbreviation-p
          (add-2-to-2 letter guesses)
          (add-1-to-2 letter guesses))
      (if expand-abbreviation-p
          (add-2-to-4 letter current-pos first-tilde-pos guesses)
          (add-1-to-4 letter guesses))))

(defun empty-word ()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable t))

(defun try-to-find-expansion (word)
  ;; This could almost certainly be done better.
  (let* ((first-tilde-pos (position-if #'has-tilde word))
         (num-tildes (count-if #'has-tilde word))
         (num-guesses (* 2 num-tildes))
         ;;(guesses (make-list num-guesses :initial-element (empty-word)))
         (guesses (if (= num-guesses 2)
                      `(,(empty-word) ,(empty-word))
                      `(,(empty-word) ,(empty-word) ,(empty-word) ,(empty-word)))))
    (dotimes (current-pos (length word))
      (let* ((char (char word current-pos))
             (base-char (cdr (assoc char +tilde-to-base+))))
        (if base-char
            (add-to-all base-char 'T num-guesses
                        current-pos first-tilde-pos
                        guesses)
            (add-to-all char 'NIL num-guesses
                        current-pos first-tilde-pos
                        guesses))))
    (let ((hits '()))
      (mapc #'(lambda (x) (when (gethash x *english-words*)
                            (push x hits)))
            guesses)
      (unless hits
        ;; Try again
        (mapc #'(lambda (x) (when (gethash (uncapitalize-word x)
                                           *english-words*)
                              (push (uncapitalize-word x) hits)))
              guesses))
      hits)))

(defun process-tilde-word-phase-2 (word value)
  ;; Note: The tilde word being processed may have come from more than
  ;; one original, before normalization. The value stored in
  ;; *tilde-words* are the guesses as to possible expansions of the
  ;; normalized tilde word. To see the originals see *tilde-words-1*
  (declare (ignore value))
  (setf (gethash word *tilde-words*)
        (try-to-find-expansion word)))


(defun process-english-word-phase-2 (word value)
  (let ((first-char (char word 0)))
    (cond ((not (cl-unicode:has-property first-char "LC"))
           (format T "~&Just found a word without an upper/lower case ~
                      mapping: ~A"
                   word))
          
          ((cl-unicode:has-property first-char "Ll")
           (let* ((cap-word (capitalize-word word))
                  (cap-word-value (gethash cap-word *english-words-1*)))
             (if cap-word-value
                 (setf (gethash word *english-words*)
                       (union value cap-word-value :test #'string=))
                 (setf (gethash word *english-words*)
                       value))))
          ((cl-unicode:has-property first-char "Lu")
           (unless (gethash (uncapitalize-word word) *english-words-1*)
             (setf (gethash word *english-words*) value)))
          
          (T
           (format T "~&Just found a word with an upper/lower case ~
                      mapping that is neither lower nor upper case: ~A"
                   word)))))

(defun process-collected-words-phase-2 ()

  (clrhash *tilde-words*)
  (clrhash *english-words*)

  (with-hash-table-iterator (my-iterator *english-words-1*)
    (loop
      (multiple-value-bind (entry-p word value)
          (my-iterator)
        (if entry-p
            (process-english-word-phase-2 word value)
            (return)))))

  (with-hash-table-iterator (my-iterator *tilde-words-1*)
    (loop
      (multiple-value-bind (entry-p word value)
          (my-iterator)
        (if entry-p
            (process-tilde-word-phase-2 word value)
            (return))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Print Words
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Hebrew characters may have following combining diacritics. Make it
;; all one character? Nope, Hebrew does not have combined
;; charaters. Only 'raw' characters, followed by combining diacritics.
;; This would also have to be accounted for when
;; printing them (the characters) below.
;; (dotimes (i (length word))
;;   (let ((char (elt word i))
;;         (count (car value)))
;;     (if (assoc char chars :test #'char=)
;;         (incf (cadr (assoc char chars :test #'char=))
;;               count)
;;         (push (list char count) chars)))))
;; ~@C to print a character in LISP literal character format

(defun print-words (words)
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (declare (ignore value))
          (if entry-p
              (push word unsorted-words)
              (return)))))
    (let ((words (sort unsorted-words #'string<)))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun print-words-and-values (words)
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (if entry-p
              (push `(,word ,value) unsorted-words)
              (return)))))
    (let ((words (sort unsorted-words #'string< :key #'car)))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun print-english-words-and-values (words)
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (if entry-p
              (let ((count 0))
                (dolist (word value)
                  (incf count (car (gethash word *all-words*))))
                (push `(,word ,count ,value) unsorted-words))
              (return)))))
    (let ((words (sort unsorted-words #'string< :key #'car)))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun print-unique-english-words-and-values (words)
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (if entry-p
              (let ((count 0))
                (dolist (word value)
                  (incf count (car (gethash word *all-words*))))
                (when (= count 1)
                  (push `(,word ,value ,(cadr (gethash (car value) *all-words*)))
                        unsorted-words)))
              (return)))))
    (let ((words (sort unsorted-words #'string< :key #'car)))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun print-unique-words (words)
  ;; *all-words*
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (if entry-p
              (destructuring-bind (count list)
                  value
                (when (= count 1)
                  (push `(,word ,list) unsorted-words)))
              (return)))))
    (let ((words (sort unsorted-words #'string< :key #'car)))
      (format T "~&~S~&" (length words))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun print-two-guess-words (words)
  ;; *tilde-words*
  (let ((unsorted-words '()))
    (with-hash-table-iterator (my-iterator words)
      (loop
        (multiple-value-bind (entry-p word value)
            (my-iterator)
          (if entry-p
                (when (= (length value) 2)
                  (push `(,word ,value) unsorted-words))
              (return)))))
    (let ((words (sort unsorted-words #'string< :key #'car)))
      (format T "~&~S~&" (length words))
      (dolist (word words)
        (format T "~&~A" word)))))

(defun display-words ()  
  (collect-words)

  (process-collected-words-phase-1)
  (process-collected-words-phase-2)

  (format T "~3&==========")
  (format T "~3&English/Latin: ~A~2&" (hash-table-count *english-words*))
  (print-words-and-values *english-words*)
  (format T "~3&==========")
  (format T "~3&Greek: ~A~2&" (hash-table-count *greek-words*))
  (print-words *greek-words*)
  (format T "~3&==========")
  (format T "~3&Hebrew: ~A~2&" (hash-table-count *hebrew-words*))
  (print-words *hebrew-words*)
  (format T "~3&==========")
  (format T "~3&TeX: ~A~2&" (hash-table-count *TeX-words*))
  (print-words *TeX-words*)
  (format T "~3&==========")
  (format T "~3&Other: ~A~2&" (hash-table-count *other-words*))
  (print-words *other-words*)
  (format T "~3&==========")
  (format T "~3&Tilde: ~A~2&" (hash-table-count *tilde-words*))
  (print-words-and-values *tilde-words*))

#|
(defun spel ()
  (interactive)
  (next-line)
  (move-beginning-of-line 1)
  (forward-char 1)
  (let ((beg (point)))
    (forward-sexp)
    (kill-ring-save beg (point)))
  (move-end-of-line 1)
  (insert " :  ")
  (yank)
  (backward-sexp)
  (let ((val (ispell-word)))
    (hyph)
    (when (not val)
      (spel))))

(defun hyph ()
  (interactive)
  (move-end-of-line 1)
  (let ((end (point)))
    (backward-sexp)
    (kill-region (point) end))
  (with-current-buffer "*shell*"
    (end-of-buffer)
    (insert "\\showhyphens{")
    (yank)
    (insert "}")
    (comint-send-input)
    (backward-sexp 2)
    (let ((beg (point)))
      (forward-sexp)
      (kill-ring-save beg (point))))
  (yank))
|#
