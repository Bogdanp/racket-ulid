#lang racket/base

(require racket/string)

(provide
 number->base32-string
 base32-string->number)

(define ALPHABET
  (apply vector (string->list "0123456789ABCDEFGHJKMNPQRSTVWXYZ")))

(define ALPHABET_REVERSE
  (for/hasheqv ([(c i) (in-indexed ALPHABET)])
    (values c i)))

(define (number->base32-string n)
  (define cs
    (let loop ([n  n]
               [cs null])
      (cond
        [(< n 32)
         (cons (vector-ref ALPHABET n) cs)]

        [else
         (loop (quotient n 32)
               (cons (vector-ref ALPHABET (remainder n 32)) cs))])))

  (string->immutable-string
   (apply string cs)))

(define (base32-string->number s)
  (for/fold ([n 0])
            ([c (in-string s)])
    (define d
      (hash-ref ALPHABET_REVERSE c (lambda ()
                                     (raise-argument-error 'base32-string->number "a base32 character" c))))

    (+ (* n 32) d)))

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (check-equal? n ((compose1 base32-string->number number->base32-string) n)))))
