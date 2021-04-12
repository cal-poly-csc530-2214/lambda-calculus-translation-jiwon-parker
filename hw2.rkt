 
#lang racket
 
(require rackunit)
 
#|LC	 	=	 	num
                |	 	id
                |	 (	(/ id => LC)
                |	 	(LC LC)
                |	 	(+ LC LC)
                |	 	(* LC LC)
                |	 	(ifleq0 LC LC LC)
                |	 	(println LC)|#
 
(define (my-read str)
  (call-with-input-string str read))


;;(check-equal? (is-lam? "(/ x => (+ x 14))") #true)
;(check-equal? (is-lam? "(+ x 14)") #false)
 
;; translate prefix plus into infix plus
(define (translate-arithmetic str)
  (~a (translate-arithmetic/stx (my-read str))))
 
;; translate prefix plus as a list into infix plus
(define (translate-arithmetic/stx stx)
  (cond [(number? stx) stx]
        [(symbol? stx) stx]
        [(and (list? stx)
              (= 3 (length stx))
              (equal? (first stx) '+))
         (list (translate-arithmetic/stx (second stx))
               '+
               (translate-arithmetic/stx (third stx)))]
        [(and (list? stx)
              (= 3 (length stx))
              (equal? (first stx) '*))
         (list (translate-arithmetic/stx (second stx))
               '*
               (translate-arithmetic/stx (third stx)))]
        [else (error 'ta "bad input: ~v\n"
                     stx)]))


;; determines whether a string represents a print
(define (is-print? str)
  (is-print?/stx (my-read str)))
 
(define (is-print?/stx stx)
  (cond [(and (list? stx)
              (= 2 (length stx))
              (~a "print(" (first (list (translate-arithmetic/stx (second stx)))) ")"))]
        [else (error 'ta "bad input: ~v\n"
                     stx)]
))


;; determines whether a string represents a number
(define (is-num? str)
  (is-num?/stx (my-read str)))
 
(define (is-num?/stx stx)
  (cond [list (number? stx) (first stx)]
        [else (error 'ta "bad input: ~v\n"
                     stx)]
))


;; determines whether a string represents a lambda
(define (is-lam? str)
  (is-lam?/stx (my-read str)))
 
(define (is-lam?/stx stx)
  (cond [(and (list? stx)
              (= 4 (length stx))
              (equal? (first stx) '/)
              (symbol? (second stx))
              (equal? (third stx) '=>))
         (~a "lambda " (second stx) " : (" (translate-arithmetic/stx (fourth stx)) ")")]
        [else (error 'ta "bad input: ~v\n"
                     stx)])
)


;; determines whether a string represents a ifleq0
(define (is-leq? str)
  (is-leq?/stx (my-read str)))
 
(define (is-leq?/stx stx)
  (cond [(and (list? stx)
              (= 4 (length stx))
              (equal? (first stx) 'ifleq0))
         (~a (translate-arithmetic/stx (third stx)) " if " (translate-arithmetic/stx (second stx)) " <= 0 else " (translate-arithmetic/stx (fourth stx)))]
        [else (error 'ta "bad input: ~v\n"
                     stx)])
)


(is-lam? "(/ x => (+ x 5))")
(is-print? "(println (* 4 5))")
(is-leq? "(ifleq0 2 3 4)")
(is-num? "(4)")


(define l-str
  "(+ (+ 4 5)
   (+ (+ 5 6)
      8))")
(define l
  (my-read l-str))

(translate-arithmetic l-str)

(define test-str
  "(* 4 2)")
(define test
  (my-read test-str))

(define t2-str
  "(+ (+ 4 5)
   (* (+ 5 6)
      8))")
(define t2
  (my-read t2-str))

(translate-arithmetic test-str)
(translate-arithmetic t2-str)

