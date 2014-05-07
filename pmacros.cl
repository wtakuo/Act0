;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Pattern Maching Macros
;;; Copyright (c) 1990 Takuo WATANABE

; (provide 'pmacros)

(in-package 'pmacros :use '(lisp))

(export '(pcase pecase pccase match match-loop plet))
(export '(comp-pattern comp-where-code add-where-code pattern-var? void-var?))


;;;
;;; PCASE, PECASE, PCCASE
;;; keyform (pattern form...)...

(defmacro pcase (expr &body clauses)
  (expand-selector-macro expr clauses #'expand-pcase-clause nil))

(defmacro pecase (expr &body clauses)
  (expand-selector-macro expr clauses #'expand-pcase-clause 'error))

(defmacro pccase (expr &body clauses)
  (expand-selector-macro expr clauses #'expand-pcase-clause 'cerror))

;;; ABCL/1 like pattern matching case macros
;;; MATCH, MATCH-LOOP
;;; keyform ([is] pattern [where condition] form...)...

(defmacro match (expr &body clauses)
  (expand-selector-macro expr clauses #'expand-match-clause nil))

(defmacro match-loop (expr &body clauses)
  (expand-selector-loop-macro expr clauses #'expand-match-clause))


;;; PLET
;;; (pattern expr) form...

(defmacro plet ((pattern expr) &body body)
  (let ((var (gensym "TMP")))
    (multiple-value-bind (pcode bindings)
                         (comp-pattern pattern nil var)
      (declare (ignore pcode))
      `(let* ((,var ,expr) ,@bindings) ,@body))))


;;; Expander functions

(defun expand-selector-macro (expr clauses expander errorfn)
  (if (symbolp expr)
    `(cond ,@(mapcar #'(lambda (clause)
                         (funcall expander expr clause))
                     clauses)
           ,@(if errorfn
               `((t (,errorfn "Key ~A does not match anything..."
			      ,expr)))))
    (let ((var (gensym "TEMP")))
      `(let* ((,var ,expr))
         (cond ,@(mapcar #'(lambda (clause)
                             (funcall expander var clause))
                         clauses)
               ,@(if errorfn
                   `((t (,errorfn "Key ~A does not match anyting..."
				  ,var)))))))))

(defun expand-selector-loop-macro (expr clauses expander)
  (if (symbolp expr)
    `(loop
       (cond ,@(mapcar #'(lambda (clause)
                           (funcall expander expr clause))
                       clauses)
             (t (return nil))))
    (let ((var (gensym "TEMP")))
      `(loop
         (let ((,var ,expr))
           (cond ,@(mapcar #'(lambda (clause)
                               (funcall expander var clause))
                           clauses)
                 (t (return nil))))))))


;;; Macro Expansion

(defun expand-pcase-clause (var clause)
  (multiple-value-bind (pcode bindings)
                       (comp-pattern (car clause) nil var)
    (cons pcode
          (if (null bindings)
            (cdr clause)
            `((let ,bindings ,@(cdr clause)))))))

(defun expand-match-clause (var clause)
  (cond ((and (symbolp (car clause))
              (STRING= (symbol-name (car clause)) "IS"))
         (setq clause (cdr clause)))
        ((member (car clause) '(t otherwise))
         (setq clause (cons '_ (cdr clause)))))
  (let ((where) (body))
    (if (and (symbolp (second clause))
             (string= (symbol-name (second clause)) "WHERE"))
      (setq where (third clause)
            body (nthcdr 3 clause))
      (setq where nil
            body (cdr clause)))
    (multiple-value-bind (pcode bindings)
                         (comp-pattern (car clause) where var)
      (cons pcode
            (if (null bindings)
              body
              `((let* ,bindings ,@body)))))))


;;; Pattern Compilation

;;; COMP-PATTERN
;;; pattern where-code position --> pattern-code bindings

(defun comp-pattern (pattern where position)
  (multiple-value-bind (pcodes bindings)
                       (comp-pattern1 pattern position nil nil nil)
    (if (and (null pcodes) (null where))
      (values t (nreverse bindings))
      (values (build-pcode (if where
                             (cons (comp-where-code where bindings) pcodes)
                             pcodes)
                           nil)
              (nreverse bindings)))))

(defun comp-pattern1 (pattern position pcodes bindings uvars)
  (cond ((atom pattern)
         (comp-atom-pattern pattern position pcodes bindings uvars))
        ((consp pattern)
         (if (complex-pattern? pattern position)
           (let ((position1 (gensym "POS")))
             (multiple-value-bind
		 (pcodes1 bindings1)
	       (comp-list-pattern pattern
				  position1
				  nil
				  nil
				  (append bindings uvars))
               (values (nconc pcodes1
                              `((:bind ,position1 ,position))
                              pcodes)
                       (nconc bindings1
                              (list (list position1 position))
                              bindings))))
           (comp-list-pattern pattern position pcodes bindings uvars)))
        (t
         (error "pattern"))))

(defun comp-atom-pattern (pattern position pcodes bindings uvars)
  (cond ((numberp pattern)
         (values (cons `(= ,pattern ,position) pcodes)
                 bindings))
        ((stringp pattern)
         (values (cons `(string= ,pattern ,position) pcodes)
                 bindings))
        ((characterp pattern)
         (values (cons `(char= ,pattern ,position) pcodes)
                 bindings))
        ((null pattern)
         (values (cons `(null ,position) pcodes)
                 bindings))
        ;; a pattern variable
        ((pattern-var? pattern)
         (if (void-var? pattern)
           (values pcodes bindings)
           (let ((vpos (or (cadr (assoc pattern bindings))
                           (cadr (assoc pattern uvars)))))
             (if vpos
               (values (cons `(eql ,vpos ,position) pcodes)
                       bindings)
               (values pcodes
                       (cons (list pattern position) bindings))))))
        ;; a constant (non-variable) symbol
        ((symbolp pattern) 
         (values (cons `(eql ',pattern ,position) pcodes)
                 bindings))
        (t
         (error "Patterns cannot contain ~A as a constant" pattern))))

(defun comp-list-pattern (pattern position pcodes bindings uvars)
  (comp-list-pattern1 pattern
                      position
                      (if (fixed-length? pattern)
                        (let ((lpat (length pattern)))
                          (list* (case lpat
                                   (1 `(null (cdr ,position)))
                                   (t `(= (length ,position) ,lpat)))
                                 `(consp ,position)
                                 pcodes))
                        (let ((mpat (minlength pattern)))
                          (if (> mpat 1)
                            (list* (case mpat
                                     (2 `(cdr ,position))
                                     (3 `(cddr ,position))
                                     (t `(>= (length ,position) ,mpat)))
                                   `(consp ,position)
                                   pcodes)
                            (list* (case mpat
                                     (0 `(listp ,position))
                                     (1 `(consp ,position)))
                                   pcodes))))
                      bindings
                      uvars))

(defun comp-list-pattern1 (pattern position pcodes bindings uvars)
  (do* ((i 0 (1+ i))
        (p pattern (cdr p)))
       ((or (atom p)
            ; (and (eql (car p) '&) (cadr p) (atom (cadr p)))
            (large-tree? p))
        (cond ((null p)
               (values pcodes bindings))
              ((atom p)
               (comp-atom-pattern p (make-nthcdr-position i position)
                                  pcodes bindings uvars))
              ; ((and (eql (car p) '&) (cadr p) (atom (cadr p)))
              ;  (comp-atom-pattern (cadr p) (make-nthcdr-position i position)
              ;                     pcodes bindings uvars))
              ((large-tree? p)
               (values (list* `(equal ,(make-nthcdr-position i position) ',p)
                              pcodes)
                       bindings))))
    (multiple-value-setq (pcodes bindings)
                         (comp-pattern1
                          (car p)
                          (make-nth-position i position)
                          pcodes
                          bindings
                          uvars))))

(defun comp-where-code (code bindings)
  (cond ((pattern-var? code)
         (or (cadr (assoc code bindings)) code))
        ((atom code) code)
        (t
         (mapcar #'(lambda (c) (comp-where-code c bindings)) code))))

(defun build-pcode (pcodes result)
  (if (null pcodes)
    (case (length result)
      (0 t)
      (1 (car result))
      (t (cons 'and result)))
    (build-pcode
     (cdr pcodes)
     (if (and (listp (car pcodes)) (eql :bind (caar pcodes)))
       `((let ((,(cadar pcodes) ,(caddar pcodes))) ,@result))
       (cons (car pcodes) result)))))

;(defun add-where-code (pcode wcode)
;  (cond ((eql pcode 't) wcode)
;        ((and (consp pcode) (eql (car pcode) 'and))
;         (nconc pcode (list wcode)))
;        (t `(and ,pcode ,wcode))))

(defun complex-pattern? (pattern position)
  (and (>= (n-of-pattern-vars pattern) 2)
       (listp position)
       (or (listp (cadr position))
           (not (member (car position) '(car cdr))))))

(defun fixed-length? (pattern)
  (and (null (cdr (last pattern)))
       ; (not (member '& pattern))
       ))

(defun large-tree? (pattern)
  (and (zerop (n-of-pattern-vars pattern))
       (cdr pattern)))

(defun make-nth-position (n position)
  (case n
    (0 `(car ,position))
    (1 `(cadr ,position))
    (2 `(caddr ,position))
    (t `(nth ,n ,position))))

(defun make-nthcdr-position (n position)
  (case n
    (0 position)
    (1 `(cdr ,position))
    (2 `(cddr ,position))
    (t `(nthcdr ,n ,position))))

(defun pattern-var? (thing)
  (and (symbolp thing) (char= #\_ (elt (symbol-name thing) 0))))

(defun void-var? (thing)
  (and (symbolp thing)
       (string= (symbol-name thing) "_")))

(defun n-of-pattern-vars (pattern)
  (cond ((pattern-var? pattern) 1)
        ((consp pattern)
         (+ (n-of-pattern-vars (car pattern))
            (n-of-pattern-vars (cdr pattern))))
        (t 0)))

(defun minlength (l)
  (minlength1 l 0))

(defun minlength1 (l i)
  (cond ((atom l) i)
        ; ((eql (car l) '&) i)
        (t (minlength1 (cdr l) (1+ i)))))


#+:Coral
(eval-when (load eval)
  (pushnew '(pcase . 1) ccl:*fred-special-indent-alist* :test #'equal)
  (pushnew '(pecase . 1) ccl:*fred-special-indent-alist* :test #'equal)
  (pushnew '(pccase . 1) ccl:*fred-special-indent-alist* :test #'equal)
  (pushnew '(match . 1) ccl:*fred-special-indent-alist* :test #'equal)
  (pushnew '(match-loop . 1) ccl:*fred-special-indent-alist* :test #'equal)
  (pushnew '(plet . 1) ccl:*fred-special-indent-alist* :test #'equal)
  )
