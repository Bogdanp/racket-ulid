#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/random
         "base32.rkt")

(provide
 make-ulid-factory
 ulid/c
 ulid
 ulid-time
 ulid-randomness)

(define MAX_TIME_COMPONENT
  (sub1 (expt 2 48)))

(define MAX_RANDOM_COMPONENT
  (sub1 (expt 2 80)))

(define (randomness)
  (for/fold ([n 0])
            ([b (in-bytes (crypto-random-bytes 10))])
    (+ (arithmetic-shift n 8) b)))

(define (pad s w)
  (~a s #:width w #:align 'right #:left-pad-string "0"))

(define (make-ulid-factory)
  (define s (box (cons -1 #f)))
  (lambda ()
    (define-values (t n)
      (let loop ([t (current-milliseconds)])
        (define os (unbox s))
        (match-define (cons ot on) os)

        (define n
          (if (= ot t)
              (add1 on)
              (randomness)))

        (if (box-cas! s os (cons t n))
            (values t n)
            (loop))))

    (when (> n MAX_RANDOM_COMPONENT)
      (error 'ulid "exhausted"))

    (string-append-immutable
     (pad (number->base32-string t) 10)
     (pad (number->base32-string n) 16))))

(define ulid
  (make-ulid-factory))

(define ulid/c
  (make-flat-contract
   #:name 'ulid/c
   #:first-order (lambda (s)
                   (and string? (= (string-length s) 26)))))

(define/contract (ulid-time s)
  (-> ulid/c exact-nonnegative-integer?)
  (define t (base32-string->number (substring s 0 10)))
  (begin0 t
    (when (> t (sub1 (expt 2 48)))
      (raise-argument-error 'ulid-time "a ULID with a valid time component" s))))

(define/contract (ulid-randomness s)
  (-> ulid/c exact-nonnegative-integer?)
  (base32-string->number (substring s 10)))

(module+ test
  (require rackunit)

  (check-equal?
   (ulid-time (~a "7" (make-string 25 #\Z)))
   MAX_TIME_COMPONENT)

  (check-exn
   exn:fail:contract?
   (lambda ()
     (ulid-time (make-string 26 #\Z))))

  (check-exn
   exn:fail:contract?
   (lambda ()
     (ulid-randomness (~a "7" (make-string 30 #\Z))))))
