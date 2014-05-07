;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Bult-In Actors
;;; Copyright (c) 1990 Takuo WATANABE

(defBehavior VOID ()
  (=> _Any
      (become-ready)
      (warn "A void actor received a message: ~A" _Any)))

(defBehavior Printer (stream)
  (=> _Any
      (become-ready)
      (disp "~&~A" _Any)))

;;; setting up

(actor-commands nil

(define disp (new Printer *standard-output*))

)
