#lang racket

(+ 1 2)

; Définition simple
(define PI 3.14)

(+ PI)

; Définition condition
(equal? 'abc 'abc)

; Définition liste
(define m (cons 1 ( cons 2 '())))
(first m)
(rest m)

;Définition pair
(define p (cons 1 2))
p
(car p)
(cdr p)

; Création fonction
(define square
  (lambda (x) (* x x)))

(define cube
  (lambda (x) (* x (square x))))

(define (ffy x . queue) x)

'map
(map square '(1 2 3 4 5))

(define (mymap f l)
  (if(empty? l)
     '()
     (cons (f (car l)) (mymap f(cdr l)))))

(define (test)
  (let ((x 2))
    (* x x)))

;Quasi quote
'( 1 (+ 2 3) 17)
`( 1 ,(+ 2 3) 17)
;Unquote
'( 1 (list 2 3) 17)
`( 1 ,@(list 2 3) 17)

;Fonction partielle