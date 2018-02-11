#lang scheme
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

;3.5.1
(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
;; in racket, use <define-syntax-rule> instead of <define>
(define-syntax-rule (cons-stream x1 x2)
  (cons x1 (delay x2)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
    (let ((already-run? false) (result false))
      (lambda ()
        (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? true)
                   result)
            result))))
(define-syntax-rule (delay exp)
  (memo-proc (lambda () exp)))

(define (force delayed-object)
  (delayed-object))

;Ex 3.50, extended stream-map with multi-args
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s1
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))


;;test
(define (show x)
  (display-line x)
  x)
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define ones (cons-stream 1 ones))