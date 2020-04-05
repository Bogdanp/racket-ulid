#lang racket/base

(require racket/contract
         racket/format
         "private/base32.rkt"
         "private/random.rkt")


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

;; We zero out the most significant bit of the randomness component to
;; guarantee that monotonically-generated ids during the same
;; millisecond can effectively never exhaust the randomness space.
;; This diverges from the spec, but is still compatible with it.
(define RANDOMNESS_MASK
  #x01111111111111111111111111111111111111111111111111111111111111111111111111111111)

(define (pad s w)
  (~a s #:width w #:align 'right #:left-pad-string "0"))

(define (make-ulid-factory)
  (define sema (make-semaphore 1))
  (define rs (make-randomness-source (add1 (* 1 1000 1000))))
  (define (randomness)
    (bitwise-and
     (unpack (randomness-take! rs 10))
     RANDOMNESS_MASK))

  (define ot -1)
  (define on 0)
  (lambda ()
    (define-values (t n)
      (call-with-semaphore sema
        (lambda ()
          (define t (current-milliseconds))
          (define n
            (if (= ot t)
                (add1 on)
                (randomness)))

          (begin0 (values t n)
            (set! ot t)
            (set! on n)))))

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
    (when (> t MAX_TIME_COMPONENT)
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
  (bytes-append
   (pack (ulid-time s) 6)
   (pack (ulid-randomness s) 10)))

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
