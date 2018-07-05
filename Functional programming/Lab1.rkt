;LABORATORIO 1

;Ex1
;a
(/ ( - 5 1) (* 20 (- 6 5)))

;b
(/ (+ 5 6 7) (- 7 8 9))

;Ex2
(define (id x) x)
(define (cube x) (expt x 3))
(define (f1 a b) (- (cube a) (cube b)))

;Ex3
(define (f2 x)
  (cond ((< x 0) "negative")
        ((> x 0) "positive")
        ( else "zero")))

;Factorial
(define (fac n)
  (if (= n 1)
      1
      (* n (fac (- n 1)))))
;Factorial2
(define (fac2 n)
  (define (iter product count)
    (if (> count n)
        product
        (iter (* product count) (+ count 1))))
  (iter 1.0 1.0))

;Newton's iter
(define (sqrt-Ex x)
  (define (good? guess)
    (< (abs (- (* guess guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (iter guess)
    (if (good? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;Ex4
(define (cube-root x)
  (define (good-cube? guess)
    (< (abs (- x (* guess guess guess))) 0.001))
  (define (improve-cube guess)
    (/ (+ (* 2 guess) (/ x (* guess guess))) 3))
  (define (cube-iter guess)
    (if (good-cube? guess)
        guess
        (cube-iter (improve-cube guess))))
  (cube-iter 1.0))

;Ex5
;a
(define (f3a x n)
  (if (= n 1)
      x
      (+ (expt x n) (f3a x (- n 1)))))

(define (f3b x n)
  (if (= n 0)
      0
      (+ (expt x n) (f3b x (- n 1)))))
          
;b
(define (f3 x n)
  (define (iter product n)
    (if (= n 0)
        product
        (iter (+ product (expt x n)) (- n 1))))
  (iter 0.0 n))

;Fibonacci
(define (fibo n)
  (define (iter  a b n)
    (if (= n 0)
        b
        (iter (+ a b) a (- n 1))))
  (iter 1 0 n))

;Expo
;1
(define (expo x n)
  (if (= n 0)
      1
      (* x (expo x (- n 1)))))

(define (expo-2 x n)
  (define (iter p c)
    (if (= c 0)
        p
        (iter (* x p) (- c 1))))
  (iter 1 n))