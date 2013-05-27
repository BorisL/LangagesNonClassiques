#lang scheme

; Implémentation de l’opérateur angélique
; Implémentation de l'opérateur angélique (ou ambigu) "amb".

(define fail #f)

(define (reset)
  (set! fail (lambda () (error "No more alternatives"))))

(reset)

(define (amb1 . l)
  (if (empty? l)
      (fail)
      (let/cc k
        (let ((old-fail fail))
          (set! fail (lambda ()
                       (set! fail old-fail)
                       (k (apply amb1 (cdr l))))))
        (car l))))

(define (produce n)
  (printf "producing ~s~n" n)
  n)

(define-syntax amb
  (syntax-rules ()
    ((amb) (fail))
    ((amb a) a)
    ((amb a b)
     ((let/cc k 
       (let ((old-fail fail))
         (set! fail (lambda ()
                      (set! fail old-fail)
                      (k  (lambda () b)))))
       (lambda () a))))
  ((amb a others ...)
   (amb a (amb others ...)))))


(define (bag-of f)
  (let ((ret '()))
    (if (amb #t #f)
        (begin
          (set! ret (cons (f) ret))
          (fail))
        (reverse ret))))

;(amb (produce 1) (produce 2) (produce 3))
;(amb)(amb)

(define (mult)
  (let ((x (amb 1 2 3 4 5 6 7 8 9 10))
       (y (amb 1 2 3 4 5 6 7 8 9 10)))
    (if (= (* x y) 30) (list x y) (amb))))
;(mult)
;(bag-of mult)




; Problème des huits reines

(define (check_one place1 place2)
  (let ((c1 (car place1))
        (c2 (car place2))
        (r1 (cdr place1))
        (r2 (cdr place2)))
  (cond ((equal? c1 c2) #f)
        ((equal? r1 r2) #f)
        ((equal? (abs (/ (- c1 c2) (- r1 r2))) 1) #f)
        (#t)
    )))

(define (check column row placed) 
  (unless (empty? placed)
    (if (check_one (cons column row) (car placed))
        (check column row (cdr placed))
        (check column (amb) placed)
        )))

(define (queens)
  (let loop ((good '())
             (line (amb 1 2 3 4 5 6 7 8 ))
            (next_column 1))
    good
    (check next_column 
           line good)
    (if(> next_column 7)
       (append good (list (cons next_column line)))
       (loop (append good (list (cons next_column line))) (amb 1 2 3 4 5 6 7 8 ) (+ next_column 1)))
    ))

(queens)
(length (bag-of queens))

; Coroutine
; λ : ctrl + \
(define thread_list '())

