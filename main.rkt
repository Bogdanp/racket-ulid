#lang racket/base

(require racket/contract
         racket/format
         racket/match
         racket/random
         "base32.rkt")


;; core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-ulid-factory
 ulid/c
 ulid
 (contract-out
  [ulid-time (-> ulid/c exact-nonnegative-integer?)]
  [ulid-randomness (-> ulid/c exact-nonnegative-integer?)]))

(define MAX_TIME_COMPONENT
  (sub1 (expt 2 48)))

(define MAX_RANDOM_COMPONENT
  (sub1 (expt 2 80)))

(define (randomness)
  (unpack (crypto-random-bytes 10)))

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

    (make-ulid-string t n)))

(define (make-ulid-string t r)
  (string-append-immutable
   (pad (number->base32-string t) 10)
   (pad (number->base32-string r) 16)))

(define ulid
  (make-ulid-factory))

(define ulid/c
  (make-flat-contract
   #:name 'ulid/c
   #:first-order (lambda (s)
                   (and (string? s)
                        (= (string-length s) 26)))))

(define (ulid-time s)
  (define t (base32-string->number (substring s 0 10)))
  (begin0 t
    (when (> t (sub1 (expt 2 48)))
      (raise-argument-error 'ulid-time "a ULID with a valid time component" s))))

(define (ulid-randomness s)
  (base32-string->number (substring s 10)))

(module+ test
  (require rackcheck
           rackunit)

  (check-equal?
   (ulid-time (~a "7" (make-string 25 #\Z)))
   MAX_TIME_COMPONENT)

  (check-exn
   exn:fail:contract?
   (lambda ()
     (ulid-time (make-string 26 #\Z))))

  (check-property
   (make-config #:tests 10000)
   (property ([n gen:natural])
     (check-eqv? n (unpack (pack n 8))))))


;; binary representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 ulid-bytes/c
 (contract-out
  [ulid->bytes (-> ulid/c ulid-bytes/c)]
  [bytes->ulid (-> ulid-bytes/c ulid/c)]))

(define ulid-bytes/c
  (make-flat-contract
   #:name 'ulid-bytes/c
   #:first-order (lambda (bs)
                   (and (bytes? bs)
                        (= (bytes-length bs) 16)))))

(define (ulid->bytes s)
  (define t (ulid-time s))
  (define r (ulid-randomness s))
  (bytes-append
   (pack (arithmetic-shift t -16) 4)
   (pack t 2)
   (pack (arithmetic-shift r -64) 2)
   (pack r 8)))

(define (bytes->ulid bs)
  (make-ulid-string
   (unpack (subbytes bs 0 6))
   (unpack (subbytes bs 6))))

(define (pack v n)
  (let loop ([n  n]
             [v  v]
             [bs null])
    (if (zero? n)
        (apply bytes bs)
        (loop (sub1 n)
              (arithmetic-shift v -8)
              (cons (bitwise-and v #xFF) bs)))))

(define (unpack bs)
  (for/fold ([n 0])
            ([b (in-bytes bs)])
    (+ (arithmetic-shift n 8) b)))

(module+ test

  (define gen:ulid
    (gen:let ([t1 (gen:integer-in 0 #xFFFFFF)]
              [t2 (gen:integer-in 0 #xFFFFFF)]
              [r1 (gen:integer-in 0 #xFFFF)]
              [r2 (gen:integer-in 0 (sub1 #xFFFFFF))]
              [r3 (gen:integer-in 0 (sub1 #xFFFFFF))]
              [r4 (gen:integer-in 0 (sub1 #xFFFF))])
      (make-ulid-string (+ (arithmetic-shift t1 24)
                           t2)
                        (+ (arithmetic-shift r1 64)
                           (arithmetic-shift r2 40)
                           (arithmetic-shift r3 16)
                           r4))))

  (check-property
   (make-config #:tests 10000)
   (property ([id gen:ulid])
     (check-equal? id (bytes->ulid (ulid->bytes id))))))
