

(eq? (apply (lambda (x y z w w2 w4) w2 w4) '(1 2 3 4 5 6)) 6)

; (eq? (apply (lambda () #f) '()) #f)
; (bin=? (apply (lambda (x y) (bin+ x y)) '(1 2)) 3)
; (eq? (apply (lambda (x y) y) '(1 2)) 2)
;  (eq? (apply (lambda (x y z w) w) '(1 2 3 4)) 4)