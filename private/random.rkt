#lang racket/base

;; On my machine, grabbing any amount of bytes from /dev/urandom has a
;; baseline cost of about 130 microseconds, which is kind of high when
;; you consider that that means you can only generate at most 10 ULIDs
;; per millisecond if you blindly call `crypto-random-bytes' every
;; time you generate one.  Instead, this module provides a randomness
;; source that grabs large chunks of bytes from urandom and caches
;; them for latter use, greatly amortizing the cost.

;; random.rkt> (define s (make-randomness-source (* 10 1024 1024)))
;; random.rkt> (time (for ([_ (in-range 100000)]) (randomness-take! s 10)))
;; cpu time: 7 real time: 6 gc time: 0
;; random.rkt> (time (for ([_ (in-range 100000)]) (crypto-random-bytes 10)))
;; cpu time: 3049 real time: 3054 gc time: 7

(require racket/random)

(provide
 make-randomness-source
 randomness-take!)

(struct source ([buf #:mutable] k [p #:mutable])
  #:transparent)

(define (make-randomness-source k)
  (source (crypto-random-bytes k) k 0))

(define (randomness-take! s n)
  (define k (source-k s))
  (define p (source-p s))
  (define p* (+ p n))
  (cond
    [(< p* k)
     (set-source-p! s p*)
     (subbytes (source-buf s) p p*)]

    [else
     (set-source-buf! s (crypto-random-bytes k))
     (set-source-p! s n)
     (subbytes (source-buf s) 0 n)]))
