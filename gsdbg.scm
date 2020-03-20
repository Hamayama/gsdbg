;; -*- coding: utf-8 -*-
;;
;; gsdbg.scm
;; 2020-3-20 v2.04
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
    getlv
    setlv
    retval))
(select-module gsdbg)

(define *gsdbg-disabled*   #f)
(define *gsdbg-running*    #f)
(define *gsdbg-ret-val*    #f)
(define *local-vars-table* (make-hash-table 'eq?))


;; == API ==

;; debugger on/off
(define (gsdbg-on)  (set! *gsdbg-disabled* #f))
(define (gsdbg-off) (set! *gsdbg-disabled* #t))

;; invoke debugger
;;  keyword arguments:
;;   :pa prompt-add - additional string to prompt
;;   :lv local-vars - specify local variables such as :lv (name1 name2 ...) .
;;                    these variables can be displayed by ,locvar command
;;                    and can be accessed by getlv/setlv procedure.
;;                    NOTE: for now, debugger can't recognize local
;;                    environment, so that you must specify this argument
;;                    if you want to access local variables.
;;   :rv ret-val    - return value of debugger
;;  example:
;;   (gsdbg :pa "proc1" :lv (x y z) :rv 0)
(define-syntax gsdbg
  (syntax-rules ()
    [(_ args ...)
     (gsdbg-aux args ... (%gsdbg))]))

(define-syntax gsdbg-aux
  (syntax-rules (%gsdbg :pa :lv :rv)
    [(_ (%gsdbg args ...))
     (%gsdbg args ...)]
    [(_ :pa prompt-add  rest ... (%gsdbg args ...))
     (gsdbg-aux rest ... (%gsdbg args ... :pa prompt-add))]
    [(_ :lv #f          rest ... (%gsdbg args ...))
     (gsdbg-aux rest ... (%gsdbg args ... :lv-in #f :lv-out #f))]
    [(_ :lv (sym ...)   rest ... (%gsdbg args ...))
     (gsdbg-aux rest ... (%gsdbg args ...
                                 :lv-in  `((sym ,sym) ...)
                                 :lv-out (^[] (set! sym (getlv sym)) ...)))]
    [(_ :rv ret-val     rest ... (%gsdbg args ...))
     (gsdbg-aux rest ... (%gsdbg args ... :rv ret-val))]
    [(_ rest ...)
     (syntax-error "malformed gsdbg-aux:" (gsdbg-aux rest ...))]))

(define (%gsdbg :key
                ((:pa     prompt-add)        #f)
                ((:lv-in  local-vars)        #f)
                ((:lv-out update-local-vars) #f)
                ((:rv     ret-val)           #f))
  (if *gsdbg-disabled*
    ret-val
    (begin
      (set! *gsdbg-running* #t)
      (set! *gsdbg-ret-val* ret-val)
      (%make-local-vars-table local-vars)
      (read-eval-print-loop #f #f #f (%make-prompter prompt-add))
      (when (applicable? update-local-vars) (update-local-vars))
      (set! *gsdbg-running* #f)
      *gsdbg-ret-val*)))

;; get local variable's value (limited)
;;  argument:
;;   name  - local variable name
;;           it must be specified in :lv keyword argument of gsdbg.
;;  example:
;;   (getlv x)
(define-syntax getlv
  (syntax-rules ()
    [(_ sym)
     (begin
       (unless (symbol? 'sym)
         (errorf "symbol required, but got ~s" 'sym))
       (if-let1 val (hash-table-get *local-vars-table* 'sym #f)
         (car val)
         (errorf "local variable ~s is not found." 'sym)))]))

;; set local variable's value (limited)
;;  arguments:
;;   name  - local variable name
;;           it must be specified in :lv keyword argument of gsdbg.
;;   value - new local variable's value
;;  example:
;;   (setlv x 100)
(define-syntax setlv
  (syntax-rules ()
    [(_ sym val)
     (begin
       (unless (symbol? 'sym)
         (errorf "symbol required, but got ~s" 'sym))
       (if (hash-table-get *local-vars-table* 'sym #f)
         (let ((v val))
           (hash-table-put! *local-vars-table* 'sym (list v))
           v)
         (errorf "local variable ~s is not found." 'sym)))]))

;; set return value of debugger
(define retval
  (case-lambda
    [()    *gsdbg-ret-val*]
    [(val)
     (set! *gsdbg-ret-val* val)
     val]))


;; == private ==

;; make prompter
(define (%make-prompter prompt-add)
  (^[]
    (let ([prompt      "debugger"]
          [prompt-add1 (if prompt-add (format "(~a)" (x->string prompt-add)) "")]
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
      [() (if (with-module gsdbg *gsdbg-running*)
            (eof-object)
            (begin
              (print "you aren't in debugger.")
              `(,(rename 'values))))]
      [_ (usage)])))

;; ,go
(define-toplevel-command (go) :read
  "\
 \n<debugger> Go without break."
  (^[args]
    (match args
      [() (if (with-module gsdbg *gsdbg-running*)
            (begin
              (with-module gsdbg (set! *gsdbg-disabled* #t))
              (eof-object))
            (begin
              (print "you aren't in debugger.")
              `(,(rename 'values))))]
      [_ (usage)])))

;; ,quit [code]
(define-toplevel-command (quit) :read
  " [code]\
 \n<debugger> Quit program."
  (^[args]
    (define (confirm)
      (format #t "Really want to quit program? [y/n]: ")
      (flush)
      (match (read-line)
        [(or "y" "Y") #t]
        [(or "n" "N") #f]
        [_ (confirm)]))
    (match args
      [()     (if (confirm)
                `(,(rename 'exit))
                `(,(rename 'values)))]
      [(code) (if (confirm)
                `(,(rename 'exit) ,code)
                `(,(rename 'values)))]
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

;; ,retval [value]
(define-toplevel-command (retval rv) :read
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
       ;(print "You're in REPL (read-eval-print-loop) of Gauche shell.")
       ;(print "Type a Scheme expression to evaluate.")
       ;(print "Evaluate (exit) to exit REPL.")
       ;(print "A word preceded with comma has special meaning.  Type ,help <cmd> ")
       ;(print "to see the detailed help for <cmd>.")
       ;(print "Commands can be abbreviated as far as it is not ambiguous.")
       ;(print)
       (print "Command List ( \",h <cmd>\" for details ):")
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

