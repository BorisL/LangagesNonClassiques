#lang racket
; TD vendredi 17 mai 2013
;  inverse l’ordre des caractères d’un symbole
(define (revsymb symb)
  
  )

;  renvoie la liste des n premiers entiers naturels (de 0 à n-1)
(define (iota n)
  (if(= n 0) 
    '()
  (buildRange 0 (- n 1))
  ))

; collecte les résultats des appels à f pour chaque entier de min à max (bornes comprises)
(define (buildRange min max)
        (if (> min max)
            '()
            (cons min (buildRange(+ min 1) max))
        ))

(define  (map-interval f min max) 
  (letrec ((mapr (buildRange min max)))                      
  (map f mapr)
  ))

; déterminant si n est un nombre premier.

(define (multiple? i n) (if(= (modulo n i) 0) #t #f))

(define (prime? n)
  (when (<= n 0)
      #f
      )
  
  ;(for-each (multiple? 2 n) '(range n))
  )

; renvoie le plus petit nombre impair strictement supérieur à n.
(define (next-odd n)
  (if(= (modulo n 2) 0)
            (+ n 1)
            (+ n 2)))

; fonction de test unitaire
(define (test a  b)
      (unless (equal? a b ) (error "Bad test result"))
  )

(test (next-odd 1) 3)
(test (next-odd 2) 3)
(test (prime? -5) #f)
(test (prime? 0) #f)
;(test (prime? 1) #f)
;(test (prime? 2) #t)
;(test (prime? 3) #t)
;(test (prime? 4) #f)
;(test (prime? 19) #t)
;(test (prime? 21) #f)
;(test (prime? 25) #f)
(test (map-interval (lambda (x) (+ 2 x)) 10 13) '(12 13 14 15))
(test (iota 5) '(0 1 2 3 4))
(test (iota 0) '())
(test (iota -1) '())