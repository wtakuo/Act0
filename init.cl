;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Examples
;;; Copyright (c) 1990 Takuo WATANABE

;;; Loading System Initialization File for MACL
; #+:CCL
; (require 'initialization "CCL;init")

;;; Free DEFSYSTEM package by Mark Kantrowitz (CMU).
; #-Symbolics
; (require :defsystem)

;;; Pattern Matching Macros by T. Watanabe.
(unless (find-package 'pmacros)
  (make-package 'pmacros :use '(lisp))
  (use-package 'pmacros)
  )


;;; System Definition

#-Symbolics
(defsystem actor
  :source-pathname "/home/fs003/takuo/work/act0/"
  :components
  ((:module syntax
            :source-pathname ""
            :components ((:file "syntax"))
            )
   (:module macros
            :source-pathname ""
            :components ((:file "pmacros"))
            )
   (:module compiler
            :source-pathname ""
            :components ((:file "comp"))
            :depends-on (syntax macros)
            )
   (:module runtime
            :source-pathname ""
            :components ((:file "runtime"))
            :depends-on (syntax macros)
            )
   (:module toplevel
            :source-pathname ""
            :components ((:file "toplevel"))
            :depends-on (syntax macros)
            )
   (:module :builtin
            :source-pathname ""
            :components ((:file "builtin"))
            :depends-on (compiler))
   ))

#+Symbolics
(defsystem ACT-R
    (:pretty-name "ACT/R Kernel Language"
     :default-pathname "USER:takuo;ACT-R;"
    )
  (:serial "syntax" "pmacros"
	   (:parallel "comp" "runtime")
	   (:parallel "toplevel" "builtin")
	   )
  )

(defun compile-actor (&key verbose)
  #-Symbolics
  (operate-on-system 'actor 'compile :verbose verbose)
  #+Symbolics
  (compile-system 'ACT-R :silent (not verbose))
  )

(defun load-actor (&key verbose)
  #-Symbolics
  (operate-on-system 'actor 'load :verbose verbose)
  #+Symbolics
  (load-system 'ACT-R :silent (not verbose))
  )
