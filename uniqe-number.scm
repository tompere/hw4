(define current-unique-tag 3342060)
		
(define get-unique-tag
	(lambda()
		(set! current-uniqe-tag (+ current-uniqe-tag 1))
		(number->string current-uniqe-tag)))