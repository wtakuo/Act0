;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Reader Macros
;;; Copyright (c) 1990 Takuo WATANABE

(defun bracket? (exp)
  (and (consp exp) (eql (car exp) ':bkt)))
(defmacro bkt-first (bkt) `(cadr ,bkt))
(defmacro bkt-second (bkt) `(third ,bkt))
(defmacro bkt-cdr (bkt) `(cons ':bkt (cddr ,bkt)))
(defmacro bkt->list (bkt) `(cdr ,bkt))
(defmacro list->bkt (list) `(cons ':bkt ,list))

(defun brace? (exp)
  (and (consp exp) (eql (car exp) ':brc)))
(defmacro brc-first (brc) `(cadr ,brc))
(defmacro brc-second (brc) `(third ,brc))
(defmacro brc-cdr (brc) `(cons ':brc (cddr ,brc)))
(defmacro brc->list (brc) `(cdr ,brc))
(defmacro list->brc (list) `(cons ':brc ,list))

(defun |[-reader| (stream macro-char)
  (declare (ignore macro-char))
  (cons ':bkt (read-delimited-tree #\] stream t nil t)))

(defun |{-reader| (stream macro-char)
  (declare (ignore macro-char))
  (cons ':brc (read-delimited-tree #\} stream t nil t)))

(defun read-delimited-tree (limitchar &optional
                                      (input-stream *standard-input*)
                                      (eof-error-p t)
                                      (eof-value nil)
                                      (recursive-p nil)
                                      &aux
                                      (reslist nil))
  (do ((c (npeek-char input-stream eof-error-p eof-value recursive-p)
          (npeek-char input-stream eof-error-p eof-value recursive-p)))
      ((char= c limitchar)
       (read-char input-stream eof-error-p eof-value recursive-p)
       (nreverse reslist))
    (if (char= c #\.)
      (progn
        (if (null reslist)
          (error "Syntax"))
        (read-char input-stream eof-error-p eof-value recursive-p)
        (if (char= (npeek-char input-stream eof-error-p eof-value recursive-p)
                   limitchar)
          (error "Unexpected EOF")
          (let ((result (nconc (nreverse reslist)
                               (read input-stream eof-error-p eof-value recursive-p))))
            (if (char= (npeek-char input-stream eof-error-p eof-value recursive-p)
                       limitchar)
              (progn
                (read-char input-stream eof-error-p eof-value recursive-p)
                (return-from read-delimited-tree result))
              (error "Syntax")))))
      (push (read input-stream eof-error-p eof-value recursive-p)
            reslist))))

(defun npeek-char (&optional (input-stream *standard-input*)
                             (eof-error-p t)
                             (eof-value nil)
                             (recursive-p nil))
  #-(or :Coral Symbolics)
  (peek-char t input-stream eof-error-p eof-value recursive-p)
  #+Symbolics
  (do ((c (peek-char t input-stream eof-error-p eof-value recursive-p)
	  (peek-char t input-stream eof-error-p eof-value recursive-p)))
      ((and (characterp c) (not (char= c #\;))) c)
    ;; skip comments
    (if (and (characterp c) (char= c #\;))
	(read-line input-stream eof-error-p eof-value recursive-p)))
  #+:Coral
  ;(if (eql input-stream ccl:*top-listener*)
    (do ((c (peek-char t input-stream nil nil recursive-p)
            (peek-char t input-stream nil nil recursive-p)))
        ((and (characterp c) (not (char= c #\;))) c)
      ;; skip comments
      (if (and (characterp c) (char= c #\;))
        (read-line input-stream nil nil recursive-p)))
    ;(peek-char t input-stream eof-error-p eof-value recursive-p))
    )

#+Symbolics
(defun unmatch-error (stream macro-char)
  (declare (ignore stream))
  (error "Unmatched parenthesis: ~C" macro-char))

;;; reader macros 

(defun single-cmac-reader (stream macro-char)
  (cond ((char= macro-char #\!)
         (list :excl (read stream t nil t)))
        (t (error "unknown macro character: ~C" macro-char))))

(eval-when (load eval)
  (set-macro-character #\[ #'|[-reader| t)
  #-Symbolics
  (set-macro-character #\] (get-macro-character #\)))
  #+Symbolics
  (set-macro-character #\] #'unmatch-error)
  (set-macro-character #\{ #'|{-reader| t)
  #-Symbolics
  (set-macro-character #\} (get-macro-character #\)))
  #+Symbolics
  (set-macro-character #\} #'unmatch-error)
  ;;
  (set-macro-character #\! #'single-cmac-reader t)
  )

#| test
(setq test1 '[1 2 3
      ;; comment
      4 5 ; more comment
      ;; one more comment
      ])

(setq test2 '[1 2 3
      ;; comment
      . ; more comment
      ;; one more
      last
      ])
|#