;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Examples
;;; Copyright (c) 1990 Takuo WATANABE

(defBehavior Fact ()
  (=> 0 !1)
  (=> _N where (numberp _N)
      [self <= (1- _N) @
        [customer R !(* R _N)]]))

(defBehavior Fact1 ()
  (=> 0 !1)
  (=> _N
      [self <= (1- _N) @
        [customer R !(* R _N)]]))

(defBehavior Fact2 ()
  (=> _N
      (if (zerop _N)
        !1
        [self <= (1- _N) @
          (customer R !(* R _N))])))


;;; Tree Product

(defBehavior Tree-Product ()
  (=> _Leaf where (numberp _Leaf)
      !_Leaf)
  (=> (_Tree) @ C
      [self <= _Tree @ C])
  (=> (_Ltree . _Rtree) @ C
      (let ((prod (new Product C)))
        [self <= _Ltree @ prod]
        [self <= _Rtree @ prod]))
  )

(defBehavior Product (customer)
  (=> _Number
      (become Product2 customer _Number)))
(defBehavior Product2 (customer number)
  (=> _Number
      (become-void)
      [customer <= (* number _Number)]))

(defBehavior TP2 ()
  (=> _Leaf where (numberp _Leaf)
      !_Leaf)
  (=> (_Tree) @ C
      [self <= _Tree @ C])
  (=> (_Ltree . _RestTree) @ C
      [self <= _Ltree @ C]
      [self <= _RestTree @ C])
  )

(defBehavior NProd (num)
  (=> :init
      (become NProd 1))
  (=> _N
      (become NProd (* num _N))))

(actor-commands t
  (define tp2 (new TP2))
  (define nprod (new NProd 1))
  [nprod <= :init]
  )

;;; Fibonacci

(defBehavior Fib ()
  (=> 0 !1)
  (=> 1 !1)
  (=> _N @ C where (numberp _N)
      (let ((add (new Adder C)))
        [self <= (1- _N) @ add]
        [self <= (- _N 2) @ add]))
  )

(defBehavior Adder (customer)
  (=> _N
      (become Adder2 customer _N)))
(defBehavior Adder2 (customer n1)
  (=> _N2
      (become-void)
      [customer <= (+ n1 _N2)]))
