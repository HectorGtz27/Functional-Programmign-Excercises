#lang racket

(define farenheit-to-celsius
  (lambda (f)
    (* (- f 32) (/ 5 9))))


(define sign
  (lambda (x)
    (cond
      ((< x 0) -1)
      ((= x 0) 0)
      (else 1))))


(define roots
  (lambda (a b c)
    (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
       (* 2 a))))


(define BMI
  (lambda (w h)
    (let ([bmi (/ w (* h h))])
      (cond
        ((< bmi 20) (printf "underweight\n"))
        ((< bmi 25) (printf "normal\n"))
        ((< bmi 30) (printf "obese1\n"))
        ((< bmi 40) (printf "obese2\n"))
        (else (printf "obese3\n"))))))


(define (factorial x)
  (cond
    [(= x 0) 1]
    [else (* x (factorial (- x 1)))]))


(define (duplicate lst)
  (if (empty? lst) '()
      (append (list (car lst) (car lst))
              (duplicate (cdr lst)))))


(define pow
  (lambda (x y)
    (if (= y 0) 1
        (* x (pow x (- y 1))))))

(define fib
  (lambda (x)
    (cond
      ((= x 0) 0)
      ((= x 1) 1)
      (else (+ (fib (- x 1)) (fib (- x 2)))))))

(define (enlist lst)
  (map list lst))
