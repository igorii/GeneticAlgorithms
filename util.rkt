#lang racket

(provide chance zip insert-at fill-nulls get-coords-from-file scan)

(define (zip l1 l2) (map cons l1 l2))
(define (chance percent-true) (>= percent-true (random)))
(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))
(define (fill-nulls with l acc)
  (cond [(null? l) acc]
        [(null? (car l)) (fill-nulls (cdr with) (cdr l) (append acc (list (car with))))]
        [else (fill-nulls with (cdr l) (append acc (list (car l))))]))
(define (get-coords-from-file filename sep [start-ind 0])
  (let* ([lines      (file->lines filename)]
         [tokens     (map    (lambda (x) (string-split x sep))        lines)]
         [ntokens    (filter (lambda (x) (not (null? x)))             tokens)]
         [str-coords (map    (lambda (x) (take (drop x start-ind) 2)) ntokens)]
         [num-coords (map    (lambda (x) (map string->number x))      str-coords)])
    num-coords))
(define (scan f val list)
  (if (null? list) '()
    (cons (f val (car list)) (scan f (f val (car list)) (cdr list)))))
