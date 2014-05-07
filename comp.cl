;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER; Base: 10; -*-
;;; Act0 (Kernel Actor Language): Translator
;;; Copyright (c) 1990 Takuo WATANABE

#|

<Def>	::= (defBehavior <Name> (<Var>...) . <script>)
<BD>	::= <Name>
	  | (behevior (<Var>...) . <script>)
<script> ::= (<Method>...)
<Method> ::= (=> <Pttn> {@ <Var>} {where <Pexp>} <Cmd>...)
<Cmd>	::= [<Exp> <= <Exp> {@ <Exp>}]
          | !<Exp>
          | (wait-for [<Exp> <== <Exp>] <Method>...)
	  | (become <BD> <Exp>...)
	  | (become-ready)
          | (become-void)
	  | (para <Cmd>...)
	  | (if <Exp> <Cmd> <Cmd>)
	  | (case <Exp> <Selection>...)
	  | (match <Exp> <Selection>...)
          | (let ((<var> <Exp>)...) <Cmd>...)
          | (let* ((<var> <Exp>)...) <Cmd>...)
          | (warn <string> <Exp>...)
          | (error <string> <Exp>...)
          | (disp <string> <Exp>...)
          | (lisp! <Lisp-Exp>...)
<Exp>	::= <Pexp>
	  | (new <BD> <Exp>...)
	  | [customer <Var> <Cmd>...]
	  | (customer <Var> <Cmd>...)
	  | [<Exp>...]
	  | (if <Exp> <Exp> <Exp>)
	  | (<Pexp> <Exp> <Exp>)
<Pexp>	::= <Const>
	  | <Var>
	  | (<Pexp> <Pexp>...)
<Selection> ::= (is <Pttn> {where <Pexp>} <Cmd>...)
	      | (otherwise <Cmd>...)
<Var>	::= <Name>
	  | <PttnVar>
<Pttn>	::= <Const>
	  | <Name>
	  | _
	  | <PttnVar>
	  | (<Pttn> . <Pttn>)
	  | [<Pttn>...]
<PttnVar> ::= _<Name>

|#


(defvar *preserve-behavior-text* t)

;;; Behavior

(defstruct (behavior (:print-function behavior-printer))
  name      ; name of the behavior
  avars     ; list of acquintance variables
  script    ; compiled script (lisp function)
  text      ; script text
  )

(defun behavior-printer (beh stream level)
  (declare (ignore level))
  (format stream "#<Behavior: ~A ~A>"
          (behavior-name beh)
          (behavior-avars beh)))


(defmacro defBehavior (name acq-vars &body script-body)
  `(progn
     (setf (get ',name :behavior)
           (make-behavior
            :name ',name
            :avars ',acq-vars
            :script ,(comp-script acq-vars script-body)
            :text ',(if *preserve-behavior-text* script-body)))
     ',name))

(defmacro find-behavior (symbol)
  `(get ,symbol :behavior))

(defun show-behavior (symbol)
  (let ((beh (find-behavior symbol)))
    (pprint `(defbehavior ,(behavior-name beh)
                          ,(behavior-avars beh)
               ,@(behavior-text beh)))))


;;; Script Compiler

(defun comp-script (avars script)
  (let ((v_avec (gensym "ACQ"))    ;; variable for acquintance vector
        (v_mesg (gensym "MSG"))    ;; variable for incoming message
        (v_cust (gensym "CST")))   ;; variable for customer
    `(function
      (lambda (self ,v_avec ,v_mesg ,v_cust)
        (match ,v_mesg
          ,@(mapcar #'(lambda (method)
                        (comp-method avars v_avec v_cust method))
                    script))))))

;;; compiles one method
(defun comp-method (avars v_avec v_cust method)
  (match method
    (is (=> _Pattern . _Body)
        (match _Body
          (is (@ _CVar . _Body)
              (match _Body
                (is (where _Where . _Body)
                    (comp-method1 avars v_avec v_cust _Pattern _CVar _Where _Body))
                (is _Body
                    (comp-method1 avars v_avec v_cust _Pattern _CVar nil _Body))))
          (is (where _Where . _Body)
              (comp-method1 avars v_avec v_cust _Pattern nil _Where _Body))
          (is _Body
              (comp-method1 avars v_avec v_cust _Pattern nil nil _Body))))
    (otherwise
     (error "Method Syntax: ~A" method))))

(defun comp-method1 (avars v_avec v_cust pattern cvar wh_exp body)
  `(is ,pattern
       ,@(if wh_exp
           (list 'where (comp-pexp avars v_avec cvar v_cust wh_exp)))
       ,@(comp-para-cmds avars v_avec cvar v_cust body)))

(defun comp-cmd (avars v_avec cvar v_cust cmd)
  (match cmd
    ;; XMIT with a customer
    (is [_Exp1 <= _Exp2 @ _Exp3]
        `(actor-xmit2 ,(comp-exp avars v_avec cvar v_cust _Exp1)
                      ,(comp-exp avars v_avec cvar v_cust _Exp2)
                      ,(comp-exp avars v_avec cvar v_cust _Exp3)))

    ;; XMIT without a customer
    (is [_Exp1 <= _Exp2]
        `(actor-xmit1 ,(comp-exp avars v_avec cvar v_cust _Exp1)
                      ,(comp-exp avars v_avec cvar v_cust _Exp2)))

    ;; XMIT to current customer
    (is !_Exp
        `(actor-xmit1 ,v_cust ,(comp-exp avars v_avec cvar v_cust _Exp)))

    ;; WAIT-FOR
    (is (wait-for [_Exp1 <== _Exp2] . _Methods)
        (error "WAIT-FOR: not available")
        )

    ;; BECOME to VOID actor
    (is (become-void)
        `(actor-become self 'VOID)
        )

    ;; BECOME to itself
    (is (become-ready)
        )

    ;; BECOME
    (is (become _BD . _Exps) where (symbolp _BD)
        `(actor-become self
                       ',_BD
                       ,@(comp-exps avars v_avec cvar v_cust _Exps)))

    ;; PARA: parallel execution
    (is (para . _Cmds)
        `(progn ,@(comp-para-cmds avars v_avec cvar v_cust _Cmds)))

    ;; IF command
    (is (if _Exp _Then . _Else) where (null (cdr _Else))
        `(if ,(comp-exp avars v_avec cvar v_cust _Exp)
           ,(comp-cmd avars v_avec cvar v_cust _Then)
           ,@(if _Else
               (list (comp-cmd avars v_avec cvar v_cust (car _Else))))))

    ;; CASE
    (is (case _Exp . _Clauses)
        (comp-match avars v_avec cvar v_cust _Exp _Clauses))

    ;; MATCH
    (is (match _Exp . _Clauses)
        (comp-match avars v_avec cvar v_cust _Exp _Clauses))

    ;; LET
    (is (let _Bindings . _Body)
        (comp-let avars v_avec cvar v_cust _Bindings _Body 'let))

    ;; LET*
    (is (let* _Bindings . _Body)
        (comp-let avars v_avec cvar v_cust _Bindings _Body 'let*))

    ;; WARN format-string exp...
    (is (warn _String . _Exps)
        `(warn ,(comp-exp avars v_avec cvar v_cust _String)
               ,@(comp-exps avars v_avec cvar v_cust _Exps)))

    ;; ERROR format-string exp...
    (is (error _String . _Exps)
        `(actor-error ,(comp-exp avars v_avec cvar v_cust _String)
                      ,@(comp-exps avars v_avec cvar v_cust _Exps)))

    ;; DISP format-string exp...
    (is (disp _String . _Exps)
        `(format t ,(comp-exp avars v_avec cvar v_cust _String)
                 ,@(comp-exps avars v_avec cvar v_cust _Exps)))

    ;; LISP! lisp-exp...
    (is (lisp! . _Exps)
        `(progn ,@(comp-exps avars v_avec cvar v_cust _Exps)))

    (otherwise
     (error "Unknown Command: ~A" cmd)))
    )

(defun comp-para-cmds (avars v_avec cvar v_cust cmds)
  (mapcar #'(lambda (cmd)
              (comp-cmd avars v_avec cvar v_cust cmd))
          cmds))

(defun comp-match (avars v_avec cvar v_cust exp clauses)
  `(match ,(comp-exp avars v_avec cvar v_cust exp)
     ,@(mapcar #'(lambda (clause)
                   (match clause
                     (is (is _Pattern where _WhExp . _Body)
                         `(is ,_Pattern
                              where ,(comp-pexp avars v_avec cvar v_cust _WhExp)
                              ,@(comp-para-cmds avars v_avec cvar v_cust _Body)))
                     (is (is _Pattern . _Body)
                         `(is ,_Pattern
                              ,@(comp-para-cmds avars v_avec cvar v_cust _Body)))
                     (is (otherwise . _Body)
                         `(otherwise
                           ,@(comp-para-cmds avars v_avec cvar v_cust _Body)))
                     (otherwise
                      (error "MATCH Syntax: ~A" clause))))
               clauses)))

(defun comp-let (avars v_avec cvar v_cust bindings body let)
  `(,let ,(mapcar #'(lambda (pair)
                      `(,(car pair)
                        ,(comp-exp avars v_avec cvar v_cust (cadr pair))))
                  bindings)
         ,@(comp-para-cmds avars v_avec cvar v_cust body)))

(defun comp-exp (avars v_avec cvar v_cust exp)
  (match exp
    ;; NEW 
    (is (new _BD . _Exps) where (symbolp _BD)
        `(actor-new ',_BD ,@(comp-exps avars v_avec cvar v_cust _Exps))
        )

    ;; CUSTOMER
    (is [customer _Pattern . _Cmds]
        (comp-customer avars v_avec cvar v_cust _Pattern _Cmds))

    ;; CUSTOMER
    (is (customer _Pattern . _Cmds)
        (comp-customer avars v_avec cvar v_cust _Pattern _Cmds))

    ;; BRACKET
    (is []
        `(list->bkt nil))
    (is [_Exp . _Exps]
        `(list->bkt
          (list ,(comp-exp avars v_avec cvar v_cust _Exp)
                ,@(comp-exps avars v_avec cvar v_cust _Exps))))

    ;; IF
    (is (if _Exp _Then . _Else) where (null (cdr _Else))
        `(if ,(comp-exp avars v_avec cvar v_cust _Exp)
           ,(comp-exp avars v_avec cvar v_cust _Then)
           ,@(if _Else
               (list (comp-exp avars v_avec cvar v_cust (car _Else)))))
        )

    ;; FUNCTION
    (is (_LispFun . _Exps) where (and (symbolp _LispFun)
                                      (functionp _LispFun))
        `(,_LispFun ,@(comp-exps avars v_avec cvar v_cust _Exps))
        )

    ;; primitive expression
    (is _A where (pexp? _A)
        (comp-pexp avars v_avec cvar v_cust _A))
    (otherwise
     (error "Exp. Syntax: ~A" exp))))

(defun comp-exps (avars v_avec cvar v_cust exps)
  (mapcar #'(lambda (exp)
              (comp-exp avars v_avec cvar v_cust exp))
          exps))

(defun comp-customer (avars v_avec cvar v_cust pattern cmds)
  (match pattern
    (is _Variable where (var? _Variable)
        `(function
          (lambda (,_Variable)
            ,@(comp-para-cmds avars v_avec cvar v_cust cmds))))
    (is _Pattern
        (let ((var (gensym "CV")))
          `(function
            (lambda (,var)
              (match ,var
                (is ,_Pattern
                    ,@(comp-para-cmds avars v_avec cvar v_cust cmds)))))))
    ))


(defun pexp? (thing)
  (match thing
    (is (new _ & _) nil)
    (is [customer _ & _] nil)
    (is (customer _ & _) nil)
    (is [ & _] nil)
    (is (if _ _ & _) nil)
    (otherwise t)))

(defun comp-pexp (avars v_avec cvar v_cust pexp)
  (match pexp
    ;; constant
    (is _Const where (constant? _Const)
        _Const)

    ;; SELF
    (is _SelfVar where (eql _SelfVar 'self)
        (declare (ignore _SelfVar))
        'self)

    ;; customer
    (is _CVar where (eql _CVar cvar)
        (declare (ignore _CVar))
        v_cust)

    ;; pattern variable
    (is _Pvar where (pattern-var? _Pvar)
        _Pvar)

    ;; variable
    (is _Var where (var? _Var)
        (if (member _Var avars)
          `(aref ,v_avec ,(position _Var avars))
          _Var))

    ;; Lisp function application
    (is _List where (listp _List)
     (cons (car _List)
           (mapcar #'(lambda (arg)
                       (comp-exp avars v_avec cvar v_cust arg))
                   (cdr _List))))
    (otherwise
     (error "Pexp. Syntax: pexp? ~A" pexp))))

(defun constant? (thing)
  (or (null thing)
      (numberp thing)
      (keywordp thing)
      (vectorp thing)
      (eql thing t)))

(defun var? (thing)
  (and (symbolp thing) (not (constant? thing))))


;;; Toplevel Command Compiler

(defun comp-tpl-cmd (cmd)
  (match cmd
    (is (define _Var _Exp) where (var? _Var)
        `(progn (defvar ,_Var)
                (setq ,_Var ,(comp-exp nil 'void '#:ignore 'disp _Exp))))
    (is (defBehavior . _)
        cmd)
    (is _
        (comp-cmd nil 'void '#:ignore 'disp cmd))))

(defmacro actor-commands (run-p &body tpl-cmds)
  `(progn ,@(mapcar #'comp-tpl-cmd tpl-cmds)
          ,(if run-p '(run-tasks))))


#+:CCL
(eval-when (load eval)
  (pushnew '(customer . 1) ccl:*fred-special-indent-alist* :test #'equal)
  )
