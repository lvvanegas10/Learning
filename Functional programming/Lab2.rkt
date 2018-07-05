;LABORATORIO 2
;Sum
;a
(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a) (sum (next a) b term next))))
;b
(define (sum2 a b term next)
  (define (sum-iter a acumm)
    (if (> a b)
        acumm
        (sum-iter (next a) (+ acumm (term a)))))
  (sum-iter a 0))

;Ex2
;a
;a.1
(define (product-int a b)
  (if (> a b)
      1
      (* a (product-int (+ a 1) b))))

(define (product-int2 a b)
  (define (p-iter a result)
    (if (> a b)
        result
        (p-iter (+ a 1) (* a result))))
  (p-iter a 1))

;a.2
(define (product-cube a b)
  (if (> a b)
      1
      (* (expt a 3) (product-cube (+ a 1) b))))

(define (product-cube2 a b)
  (define (p-iter a result)
    (if (> a b)
        result
        (p-iter (+ a 1) (* (expt a 3) result))))
  (p-iter a 1))

;b
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

(define (product-int3 a b)
  (define (id x) x)
  (define (next x) (+ x 1))
  (product id a next b))

(define (product-cube3 a b)
  (define (cube x) (* x x x))
  (define (next x) (+ x 1))
  (product cube a next b))

;d
(define (product2 factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (factor a)))))
  (iter a 1))

(define (product-int4 a b)
  (define (id x) x)
  (define (next x) (+ x 1))
  (product2 id a next b))

(define (product-cube4 a b)
  (define (cube x) (* x x x))
  (define (next x) (+ x 1))
  (product2 cube a next b))

;c
(define (pi n)
  (define (factor x)
    (/ (* x (+ x 2.0)) (expt (+ x 1) 2)))
  (define (next x)
    (+ x 2))
  (* 4 (product2 factor 2 next n)))

;Ex4
(define (acummulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-acumm term a next b)
  (acummulate + 0 term a next b))

(define (inc n) (+ n 1))
(define (identity x) x)
(define (sum-integers-acum a b)
  (sum-acumm identity a inc b))
  
