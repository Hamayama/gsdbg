;; -*- coding: utf-8 -*-
;;
;; gsdbg.scm
;; 2019-11-1 v1.09
;;
;; ＜内容＞
;;   Gauche で、スクリプトのデバッグを行うためのモジュールです。
;;   現状、制限事項がいろいろと存在します。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/gsdbg
;;
(define-module gsdbg
  (use gauche.interactive)
  (use gauche.interactive.toplevel)
  (use util.match)
  (export
    gsdbg-on
    gsdbg-off
    gsdbg
    getlv))
(select-module gsdbg)

(define *gsdbg-disabled*   #f)
(define *gsdbg-ret-val*    #f)
(define *local-vars-table* #f)


;; == API ==

;; debugger on/off
(define (gsdbg-on)  (set! *gsdbg-disabled* #f))
(define (gsdbg-off) (set! *gsdbg-disabled* #t))

;; invoke debugger
;;   prompt-add - additional string to prompt
;;   local-vars - specify local variables such as '((name1 value1) ...) .
;;                these variables can be displayed by ,locvar command.
;;                NB: for now, debugger can't recognize local environment,
;;                so that user should specify this argument explicitly.
;;   ret-val    - return value of debugger
;;   e.g. (gsdbg "proc1" `((x ,x) (y ,y)) #f)
(define (gsdbg :optional (prompt-add #f) (local-vars #f) (ret-val #f))
  (set! *gsdbg-ret-val* ret-val)
  (unless *gsdbg-disabled*
    (%make-local-vars-table local-vars)
    (read-eval-print-loop #f #f #f (%make-prompter prompt-add)))
  *gsdbg-ret-val*)

;; get local variable's value (limited)
;;   name - local variable name
;;          it must be specified by local-vars argument of gsdbg procedure.
;;   e.g. (getlv x)
(define-syntax getlv
  (syntax-rules ()
    [(_ sym)
     (begin
       (unless (symbol? 'sym)
         (errorf "symbol required, but got ~s" 'sym))
       (if-let1 val (hash-table-get *local-vars-table* 'sym #f)
         (car val)
         (errorf "local variable ~s is not found." 'sym)))]))


;; == private ==

;; make prompter
(define (%make-prompter prompt-add)
  (^[]
    (let ([prompt      "debugger"]
          [prompt-add1 (if prompt-add #"(~(x->string prompt-add))" "")]
          [mod         ((with-module gauche.internal vm-current-module))]
          [delim       ">"])
      (if (eq? mod (find-module 'user))
        (format #t "~a~a~a " prompt prompt-add1 delim)
        (format #t "~a~a[~a]~a " prompt prompt-add1 (module-name mod) delim))
      (flush))))

;; make local variables table
(define (%make-local-vars-table local-vars)
  (set! *local-vars-table* (make-hash-table 'eq?))
  (when (list? local-vars)
    (dolist [var local-vars]
      (when (and (list? var) (= (length var) 2))
        (let ([sym (car  var)]
              [val (cadr var)])
          (when (symbol? sym)
            (hash-table-put! *local-vars-table* sym (list val))))))))

;; display local variables
(define (%disp-local-vars syms)
  (when *local-vars-table*
    (if (null? syms)
      (for-each (^[var] (format #t "~s = ~s\n" (car var) (cadr var)))
                (sort (hash-table->alist *local-vars-table*)
                      string<? (^[var] (x->string (car var)))))
      (dolist [sym syms]
        (if-let1 val (hash-table-get *local-vars-table* sym #f)
          (format #t "~s = ~s\n" sym (car val))
          (format #t "local variable ~s is not found.\n" sym))))))


;; == gauche.interactive.toplevel ==

(select-module gauche.interactive.toplevel)

;; add toplevel commands
;; ,continue
(define-toplevel-command (continue c) :read
  "\
 \n<debugger> Continue execution."
  (^[args]
    (match args
      [() (eof-object)]
      [_ (usage)])))

;; ,go
(define-toplevel-command (go) :read
  "\
 \n<debugger> Go without break."
  (^[args]
    (match args
      [()
       (with-module gsdbg (set! *gsdbg-disabled* #t))
       (eof-object)]
      [_ (usage)])))

;; ,quit
(define-toplevel-command (quit) :read
  " [code]\
 \n<debugger> Quit program."
  (^[args]
    (match args
      [()     `(,(rename 'exit))]
      [(code) `(,(rename 'exit) ,code)]
      [_ (usage)])))

;; ,backtrace
(define-toplevel-command (backtrace bt) :read
  "\
 \n<debugger> Display backtrace."
  (^[args]
    (match args
      [()
       (%vm-show-stack-trace (vm-get-stack-trace-lite))
       `(,(rename 'values))]
      [_ (usage)])))

;; ,curmod
(define-toplevel-command (curmod cm) :read
  "\
 \n<debugger> Display current module name."
  (^[args]
    (match args
      [() `(,(rename 'module-name) (,(rename 'current-module)))]
      [_ (usage)])))

;; ,selmod [module]
(define-toplevel-command (selmod sm) :read
  " [module]\
 \n<debugger> Select current module."
  (^[args]
    (match args
      [()    `(,(rename 'select-module) user)]
      [(mod) `(,(rename 'select-module) ,mod)]
      [_ (usage)])))

;; ,locvar [name1 name2 ...]
(define-toplevel-command (locvar lv) :read
  " [name1 name2 ...]\
 \n<debugger> Display local variables (limited)."
  (^[args]
    (match args
      [((? symbol? syms) ...)
       (with-module gsdbg (%disp-local-vars syms))
       `(,(rename 'values))]
      [_ (usage)])))

;; ,ret value
(define-toplevel-command (ret) :read
  " [value]\
 \n<debugger> Set return value of debugger."
  (^[args]
    (match args
      [()
       (format #t "~s\n" (with-module gsdbg *gsdbg-ret-val*))
       `(,(rename 'values))]
      [(val)
       ($ with-module gsdbg
          (set! *gsdbg-ret-val*
                (eval val ((with-module gauche.internal vm-current-module)))))
       `(,(rename 'values))]
      [_ (usage)])))

;; ,help [command]
;;   overwrite original ,help command.
;;   it displays debugger commands after standard commands.
(define-toplevel-command (help h) :read
  " [command]\
 \nShow the help message of the command.\
 \nWithout arguments, show the list of all toplevel commands."
  (^[args]
    (define (get-cmd&help help-string)
      (let* ((ls   (call-with-input-string help-string port->string-lseq))
             (cmd  (or (rxmatch->string #/^\S*/ (list-ref ls 0 "")) ""))
             (help (list-ref ls 1 "")))
        (cons cmd help)))
    (match args
      [()
       (print "You're in REPL (read-eval-print-loop) of Gauche shell.")
       (print "Type a Scheme expression to evaluate.")
       (print "Evaluate (exit) to exit REPL.")
       (print "A word preceded with comma has special meaning.  Type ,help <cmd> ")
       (print "to see the detailed help for <cmd>.")
       (print "Commands can be abbreviated as far as it is not ambiguous.")
       (print)
       (dolist [cmd&help
                ;(sort (map (^p (get-cmd&help (cdr p)))
                ;           (toplevel-command-keys))
                ;      string<? car)]
                (let1 cmd&help-list (sort (map (^p (get-cmd&help (cdr p)))
                                               (toplevel-command-keys))
                                          string<? car)
                  (append (remove (^[c&h] (#/<debugger>/ (cdr c&h))) cmd&help-list)
                          '(hline)
                          (filter (^[c&h] (#/<debugger>/ (cdr c&h))) cmd&help-list)))]
         ;(format #t (if (> (string-length (car cmd&help)) 10)
         ;             " ,~10a\n             ~a\n"
         ;             " ,~10a ~a\n")
         ;        (car cmd&help)
         ;        (cdr cmd&help)))
         (if (eq? cmd&help 'hline)
           (format #t "  ----------\n")
           (format #t (if (> (string-length (car cmd&help)) 10)
                        " ,~10a\n             ~a\n"
                        " ,~10a ~a\n")
                   (car cmd&help)
                   (cdr cmd&help))))
       *no-value*]
      [(('unquote cmd)) ((toplevel-command-helper cmd)) *no-value*]
      [(cmd) ((toplevel-command-helper cmd)) *no-value*]
      [_ (usage)])))

(select-module gsdbg)

