(define (argfc) (call/cc (lambda (k) (k (lambda (x) (k (lambda (y) x)))))))
(define M argfc)
(define (S x) (lambda (p) (p x x)))
(define (D x y) (lambda (p) (p x y)))
(define (P x y) (((x) 1) ((y) 2)))

;((S M) P); supposed to give 1
((D M M) P); supposed to give 2