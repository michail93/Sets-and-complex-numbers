(define (square x) (* x x ))

; представление комплексных чисел
(define (add-complex z1 z2)
  (make-from-real-imag  (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Неизвестный тип -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Неизвестный тип -- REAL-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Неизвестный тип -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Неизвестный тип -- ANGLE" z))))

; пораждает комплексные числа в декатровом виде
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

; пораждает комплекные числа в полярном виде
(define (make-from-mag-ang r a)
  (make-from-real-imag-polar r a))

; селекторы и конструкторы комплексных чисел,
; декартова форма представления комплексных чисел
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z) (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)))))

(define (angle-rectangular z) (atan (imag-part-rectangular z)  (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y) (cons x y))

(define (make-from-mag-ang-rectangular r a) (cons (* r (cos a)) (* r (sin a))))

; селекторы и конструкторы комплексных чисел,
; полярная форма предствления комплексных чисел
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag `polar (cons (sqrt (+ (square x) (square y))) (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag `polar (cons r a)))

; создание метки типа
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum) (car datum) (error "Некоректные данные -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum) (error "Некоректные данные -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) `rectangular))

(define (polar? z) (eq? (type-tag z) `polar))
