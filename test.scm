;;;
;;; Test gsdbg
;;;

(add-load-path "." :relative)
;(import (scheme base)
;        (gauche base)
;        (gauche interactive)
;        (gsdbg))
(use gauche.interactive)
(use gsdbg)

(define *x*   1000)
(define *y*   '(1 2 3 4 5))
(define *ret* #f)

(define (proc1)
  (define x 2000)
  (define y '(6 7 8 9 10))
  (define z "12345")
  (print "===== proc1 (global) start =====")
  (print "global variables")
  (format #t "  *x*=~s\n"   *x*)
  (format #t "  *y*=~s\n"   *y*)
  (print "local variables")
  (format #t "  x=~s\n"     x)
  (format #t "  y=~s\n"     y)
  (format #t "  z=~s\n"     z)
  (print "Debugger is invoked.")
  (print "Please type \",c\" to exit debugger ( \",h\" for command help ).")
  (print "You can access global variables normally.")
  (print " (e.g. *x*  (set! *x* 1010) )")
  (print "If you want to access local variables, use getlv/setlv.")
  (print " (e.g. (getlv x)  (setlv x 3000) )")
  ;(read-eval-print-loop)
  (set! *ret* (gsdbg :pa "proc1" :lv (x y z)))
  (print "global variables")
  (format #t "  *x*=~s\n"   *x*)
  (format #t "  *y*=~s\n"   *y*)
  (print "local variables")
  (format #t "  x=~s\n"     x)
  (format #t "  y=~s\n"     y)
  (format #t "  z=~s\n"     z)
  (format #t "return value = ~s\n" *ret*)
  (print "===== proc1 (global) end ====="))

(define-module test-mod
  (use gsdbg)
  (export proc2)
  (define *u*   10000)
  (define *v*   '(10 20 30 40 50))
  (define *ret* #f)
  (define (proc2)
    (define u 20000)
    (define v '(60 70 80 90 100))
    (define w "ABCDE")
    (print "===== proc2 (in test-mod) start =====")
    (print "global variables (in test-mod)")
    (format #t "  *u*=~s\n"   *u*)
    (format #t "  *v*=~s\n"   *v*)
    (print "local variables")
    (format #t "  u=~s\n"     u)
    (format #t "  v=~s\n"     v)
    (format #t "  w=~s\n"     w)
    (print "Debugger is invoked.")
    (print "Please type \",c\" to exit debugger ( \",h\" for command help ).")
    (print "If you want to access global variables, type \",sm test-mod\".")
    (print "Then, you can access global variables normally.")
    (print " (e.g. *u*  (set! *u* 10100) )")
    (print "If you want to access local variables, use getlv/setlv.")
    (print " (e.g. (getlv u)  (setlv u 30000) )")
    ;(read-eval-print-loop)
    (set! *ret* (gsdbg :pa "proc2" :lv (u v w)))
    (print "global variables (in test-mod)")
    (format #t "  *u*=~s\n"   *u*)
    (format #t "  *v*=~s\n"   *v*)
    (print "local variables")
    (format #t "  u=~s\n"     u)
    (format #t "  v=~s\n"     v)
    (format #t "  w=~s\n"     w)
    (format #t "return value = ~s\n" *ret*)
    (print "===== proc2 (in test-mod) end ====="))
  )

;(import (test-mod))
(import test-mod)

(proc1)

(newline)

(proc2)

