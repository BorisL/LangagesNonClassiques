#lang racket
; TD vendredi 17 mai 2013
(require (lib "defmacro.ss"))

; **** Structures ****

; **** Programmation par contrat ****

; !!! En cours de travail !!!
(define (pre exp)
  (unless exp (error "Précondition fausse")))

(define-macro (contract . args)
  `(begin ,args ... )
  )
; !!! En cours de travail !!!


; **** Trace ****

(define-syntax define-trace
  (syntax-rules ()
  ((define-trace (f . args) body ...)
   (define (f . args) (begin (display f) (newline)  body ... )))))

(define-trace (test1 x y)
  (* x y)
  )


; **** Transformation de code ****

(define-syntax while
  (syntax-rules ()
    ((while cond body ...)
       (let loop()
         (unless(equal? cond #f)
         body ...
         (loop))))))

(define-syntax or
  (syntax-rules ()
    ((or)
     #f)
    ((or a b ...)
     (let ((resultat a))
       (if resultat
           resultat
           (or b ...))))))

(define-syntax and
  (syntax-rules ()
  ((and)
  #t)
  ((and a)
   a)
  ((and a b ...)
   (if (equal? a #f)
       #f
       (and b ...)))))

; **** Manipulation de données ****

; applique l’opérateur binaire operator sur les deux premiers éléments de l, 
; puis sur ce résultat et le troisième élément, puis sur ce nouveau résultat et le quatrième élément, etc
(define (slash operator l) 
  (match-let (
              ((list n1 n2 t ...) l) 
              )
    (if (empty? t)
    (operator n1 n2)
    (slash operator (cons (operator n1 n2) t)))))
  
; renvoie une liste avec les éléments de la liste l pour lesquels test est vrai
(define (filter test list)
  (if(empty? list ) 
     '()
     (let ((filtered (car list)))
       (if (equal? (test filtered) #t)
           (cons (car list) (filter test (cdr list)))
           (filter test (cdr list))))))
  

; prend une liste de chaînes en paramètre 
; et les affiche les unes après les autres en passant à la ligne entre les différentes chaînes
(define (display-all list)
  (for-each (lambda(x) (display x) (newline)) list)
  )
; prend une liste de symboles en paramètre et renvoie la même liste dans laquelle les caractères sont inversés
(define (trans list)
  (map revsymb list)
  )

;  inverse l’ordre des caractères d’un symbole
(define (revsymb symb)
  (string->symbol (list->string(reverse (string->list (symbol->string symb)))))
  )





; **** Échauffement ****

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
  
  (if(<= n 2)
      (if (= n 2)
          #t
          #f
          )
      (if (= (modulo n 2) 0)
      #f
      (let loop ((t (round(sqrt n)))
                 (n n))
         (if (>= t n)
             #t
             (if(= (modulo n t) 0)
             #f
             (loop (next-odd t) n)
           ))))))
  
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
(test (prime? 1) #f)
(test (prime? 2) #t)
(test (prime? 3) #t)
(test (prime? 4) #f)
(test (prime? 19) #t)
(test (prime? 21) #f)
(test (prime? 25) #f)
(test (map-interval (lambda (x) (+ 2 x)) 10 13) '(12 13 14 15))
(test (iota 5) '(0 1 2 3 4))
(test (iota 0) '())
(test (iota -1) '())

(test (revsymb 'foobar) 'raboof)
(test (trans '(foo bar)) '(oof rab))
(test (filter (lambda (x) (> x 3)) '(1 10 2 20)) '(10 20))
(test (slash * '(10 20 30)) 6000)
(test (slash string-append '("foo" "bar")) "foobar")
(test (slash + '(1 2 3)) 6)
(test (slash - '(10 2 3)) 5)
(test (slash expt '(2 3 4)) 4096)
(test (slash * (filter prime? (iota 100))) 2305567963945518424753102147331756070)

(test (let ((i 0) (c 0)) (while (< i 5) (set! c (+ c i)) (set! i (+ i 1))) c) 10)