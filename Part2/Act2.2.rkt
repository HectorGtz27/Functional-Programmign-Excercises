#lang racket

; Ejercicio 1

(define (insert n lst)
  (cond
    [(null? lst) (list n)]
    [(<= n (car lst)) (cons n lst)]
    [else (cons (car lst) (insert n (cdr lst)))]))

; Casos de prueba

(insert 5 '(1 3 6 7 9 16))
(insert 10 '(1 5 6))

; Ejercicio 2

(define (insertion-sort lst)
  (if (null? lst)
      '()
      (insert (car lst) (insertion-sort (cdr lst)))))


; Casos de prueba

(insertion-sort '(4 3 6 8 3 0 9 1 7))
(insertion-sort '(5 5 5 1 5 5 5))

; Ejercicio 3

(define (encode-modified lst)
  (define (pack lst)
    (if (null? lst) '()
        (let loop ((sub (list (car lst))) (rest (cdr lst)))
          (if (null? rest)
              (list sub)
              (if (equal? (car sub) (car rest))
                  (loop (append sub (list (car rest))) (cdr rest))
                  (cons sub (pack rest)))))))
  (define (encode-helper packed-list)
    (map (lambda (item)
           (if (= (length item) 1)
               (car item)
               (list (length item) (car item))))
         packed-list))
  (encode-helper (pack lst)))



; Casos de prueba

(encode-modified '(a a a a b c c a a d e e e e))
(encode-modified '(9 9 9 9 9 9 9 9 9))

; Ejercicio 4

(define (prime-factors n)
  (define (factor n divisor)
    (if (> (* divisor divisor) n)
        (if (= n 1) '() (list n))
        (if (zero? (remainder n divisor))
            (cons divisor (factor (/ n divisor) divisor))
            (factor n (+ divisor 1)))))
  (factor n 2))




; Casos de prueba

(prime-factors 6)
(prime-factors 666)

; Ejercicio 5

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))



; Casos de prueba

(gcd 13 7919)
(gcd 6307 1995)



; Ejercicio 6

(define (deep-reverse lst)
  (if (null? lst) '() 
      (reverse
       (map (lambda (x)
              (if (list? x)
                  (deep-reverse x)
                  x)) 
            lst))))


; Casos de prueba

(printf "Casos de prueba:\n")

(printf "() -> ")
(display (deep-reverse '())) (newline)
(printf "a (b c d) 3) -> ")
(display (deep-reverse '(a (b c d) 3))) (newline)
(printf "((1 2) 3 (4 (5 6))) -> ")
(display (deep-reverse '((1 2) 3 (4 (5 6))))) (newline)
(printf "a (b (c (d (e (f (g (h i j)))))))) -> ")
(display (deep-reverse '(a (b (c (d (e (f (g (h i j)))))))))) (newline)


; Ejercicio 7

(define (insert-everywhere x lst)
  (if (null? lst)
      (list (list x)) 
      (let loop ((left '()) (right lst) (acc '()))
        (if (null? right)
            (append acc (list (append left (list x)))) 
            (loop (append left (list (car right))) 
                  (cdr right) 
                  (append acc (list (append left (list x) right))))))))


; Casos de prueba

(printf "Casos de prueba:\n")

(display (insert-everywhere 1 '())) (newline)
(display (insert-everywhere 1 '(a))) (newline)
(display (insert-everywhere 1 '(a b c))) (newline)
(display (insert-everywhere 1 '(a b c d e))) (newline)
(display (insert-everywhere 'x '(1 2 3 4 5))) (newline)

; Ejercicio 8

(define (pack lst)
  (if (null? lst) '() 
      (let loop ((current (car lst)) 
                 (rest (cdr lst)) 
                 (packed '()) 
                 (temp '())) 
        (cond
          ((null? rest) 
           (append packed (list (append temp (list current)))))
          ((equal? current (car rest)) 
           (loop current (cdr rest) packed (append temp (list current)))) 
          (else 
           (loop (car rest) 
                 (cdr rest)
                 (append packed (list (append temp (list current)))) 
                 '()))))) 
)


; Casos de prueba

(printf "Casos de prueba:\n")

(printf "() -> ")
(display (pack '())) (newline)
(printf "(a a a a b c c a a d e e e e) -> ")
(display (pack '(a a a a b c c a a d e e e e))) (newline)
(printf "(1 2 3 4 5) -> ")
(display (pack '(1 2 3 4 5))) (newline)
(printf "(9 9 9 9 9 9 9 9 9) -> ")
(display (pack '(9 9 9 9 9 9 9 9 9))) (newline)


; Ejercicio 9

(define (compress lst)
  (if (null? lst) '()
      (let loop ((current (car lst))
                 (rest (cdr lst))
                 (result '())) 
        (if (null? rest)
            (reverse (cons current result)) 
            (if (equal? current (car rest))
                (loop current (cdr rest) result) 
                (loop (car rest) (cdr rest) (cons current result)))))))

; Casos de prueba

(printf "Casos de prueba:\n")

(printf "() -> ")
(display (compress '())) (newline)
(printf "(a b c d) -> ")
(display (compress '(a b c d))) (newline)
(printf "(a a a b c c a a d e e e e) -> ")
(display (compress '(a a a b c c a a d e e e e))) (newline)
(printf "(a a a a a a a a a) -> ")
(display (compress '(a a a a a a a a a))) (newline)
