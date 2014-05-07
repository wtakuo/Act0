;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Toplevel
;;; Copyright (c) 1990 Takuo WATANABE

(defun actor (&optional (stream *terminal-io*))
  (reset-tasks)
  (match-loop (prompt-and-read stream)
    (is bye
        (return-from actor))
    (is _Var where (var? _Var)
        (format stream "~% = ~A" (eval _Var)))
    (is _Command
        (eval (comp-tpl-cmd _Command))
        (format t "~&~D tasks has been processed" (time (run-tasks))))))

(defun prompt-and-read (stream)
  (format stream "~%Actor> ")
  (read *terminal-io*))

#|
(defun actor-toplevel (&aux v idle)
  (setq v
        (catch-abort
          (loop
            (format t "~%ACT/R> ")
            (catch-cancel
              (format t "~%~A"
                      (eval (prog2 (setq idle *lisp-idle* *lisp-idle* t)
                                   (read)
                                   (setq *lisp-idle* idle)))
              )
            ))))
  (format t "~%Abort : ~A" v)
  (if (y-or-n-dialog "Quit ACT/R?"
                     :yes-text "No"
                     :no-text "Yes"
                     :cancel-text nil)
    (actor-toplevel)))
|#
