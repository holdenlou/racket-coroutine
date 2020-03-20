#lang racket

(require (for-syntax syntax/parse racket/syntax racket/format racket/base))
(define (current-continuation)
  (call/cc (lambda (cc) cc )))
(define-syntax (coro stx)
  (syntax-parse stx
    [(coro (arg) body ... )
     (with-syntax ((yield (format-id #'coro "~a" "yield")))
       #'(letrec (
                  (func (λ (arg) body ...))
                  (internal-state 'init)
                  (return-addr #f)
                  (yield (λ (v) (call/cc (λ (k)
                                           (set! internal-state k)
                                           (return-addr v))))))
           (λ ([arg #f])
             (let ((ret (current-continuation)))
               (if (continuation? ret)
                   (begin
                     (set! return-addr ret) ;save return address for yield
                     (cond ((eq? internal-state 'init)
                            (let ((val (func arg))) (set! internal-state 'finish) val ))
                           ((eq? internal-state 'finish) 'finished)
                           (else (internal-state arg))));resume from internal state
                   ret ;yield will jump here
                   ))
             )
           ))]))

(define cor1
  (coro (x)
   (let loop ((i 1))
     (yield i)
     (loop (+ i 1)))))
(define (test)
(displayln (cor1))
(displayln (cor1 #f));optinal argument
(displayln (cor1))
)
