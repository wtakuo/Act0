;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Runtime System
;;; Copyright (c) 1990 Takuo WATANABE

(defvar *debug* nil)

;;; tasks

(defvar *tasks* nil)

(defun no-tasks? ()
  (null *tasks*))
(defun reset-tasks ()
  (setq *tasks* nil))
(defun add-task (task)
  (setq *tasks* (nconc *tasks* (list task))))
(defun next-task ()
  (pop *tasks*))


;;; Actor

(defstruct (actor (:print-function actor-printer))
  id       ; name of the actor
  storage  ; local storage (acquintances)
  behavior ; behavior
  )

(defun actor-printer (actor stream level)
  (declare (ignore level))
  (format stream "#<Actor ~A (~A)>"
          (actor-id actor)
          (behavior-name (actor-behavior actor))))


;;; Task = (<target> <message> <customer>)

(defun run-tasks (&aux (count 0))
  (match-loop (next-task)
    (is (_Target _Message _Customer)
        (funcall (behavior-script (actor-behavior _Target)) ; compiled script
                 _Target                      ; Self
                 (actor-storage _Target)      ; aquintances vector
                 _Message
                 _Customer)
        (incf count)))
  (values count))
        

;;; Runtime Libraries

(defun actor-xmit1 (target message)
  (typecase target
    (null
     (when *debug*
       (warn "XMIT: message ~A was sent to a null target." message)))
    (function
     (funcall target message))
    (actor
     (add-task (list target message nil)))
    ))

(defun actor-xmit2 (target message customer)
  (typecase target
    (null
     (when *debug*
       (warn "XMIT: message ~A was sent to a null target." message)))
    (function
     (actor-xmit1 customer (funcall target message)))
    (actor
     (add-task (list target message customer)))
    ))

(defun actor-new (BD &rest args)
  (let* ((beh (find-behavior BD))
         (id (new-actor-id beh))
         (sz (length (behavior-avars beh))))
    (if (= sz (length args))
      (make-actor
       :id id
       :storage (make-array (length (behavior-avars beh))
                            :initial-contents args)
       :behavior beh)
      (error "NEW: number of arguments: ~A" (behavior-name beh)))))

(defun actor-become (actor BD &rest args)
  (let* ((beh (find-behavior BD))
         (sz (length (behavior-avars beh))))
    (if (= sz (length args))
      (setf (actor-behavior actor)
            beh
            (actor-storage actor)
            (make-array sz :initial-contents args))
      (error "NEW: number of arguments: ~A" (behavior-name beh)))))

(defun actor-error (format-string &rest args)
  (apply 'format *error-output* format-string args))


;;; Debugging

(defun describe-actor (actor &optional (stream *standard-output*))
  (let* ((beh (actor-behavior actor))
         (avars (behavior-avars beh))
         (avec (actor-storage actor)))
    (format stream "~&~A is an instance of ~A" actor (behavior-name beh))
    (dotimes (i (length avars))
      (format stream "~&    ~A = ~A" (elt avars i) (elt avec i)))))


;;; Misc. Functions

(defun new-actor-id (beh)
  (gensym (symbol-name (behavior-name beh))))
