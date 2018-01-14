#lang racket

(define fileName (vector-ref (current-command-line-arguments) 0))
(define myline (file->lines fileName))
(define mylist (map string->number (string-split (car myline))))
(define rest (cdr mylist))
(define N (car mylist))
(define D (cadr mylist))
(define k (caddr mylist))
(define e (cadddr mylist))
(define MinPts (cadddr rest))
;Extracting the parameters from the first line of mylist
(define const 1)
(define (my-sum lst)                                                               ;Function to calculate the sum of the elements of a list
 (foldr + 0 lst))

(define comprise member)                                                           ;Function to check if a given list contains a value

(define (diff list1 list2) (if (null? list2)                                       ; Function to calculate the difference between two lists
      null
      (if (comprise (car list2) list1)
          (diff list1 (cdr list2) )
          (cons (car list2) (diff list1 (cdr list2))))))

(define (sum-squares lst)                                                          ;Function to calculate the sum of the squares of the elements of a list
  (apply + (map (lambda (x) (expt x 2)) lst)))
(define (sub-list lst1 lst2) (map (lambda (x y) (- x y)) lst1 lst2))               ;Function to calculate the difference between the corresponding elements of two lists 
(define pointlines (cdr myline))
(define (euc-dist list1 list2) (sqrt(sum-squares (sub-list list1 list2))))         ;Function to calculate the Euclidean distance between two data points

(define mypoints (build-list N (lambda (x)
 (map string->number (string-split (list-ref pointlines x))) )))

(define step1 (build-list N (lambda (x)                                            
  (list (+ x 1) (list-ref mypoints x)))))
;(display step1)
;(newline)
; Part 1 over


;Part 2 starts
(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define mstep2 (build-list N (lambda (x)
  (build-list N (lambda (y) (if (= x y) (list (+ y 1) +inf.0) (list (+ y 1) (euc-dist (list-ref mypoints x) (list-ref mypoints y)) )))))))
(define step2 (modify_precision mstep2))
;(display step2)
;(newline)
; Part 2 over

;Part 3 starts

(define sort-mat (build-list N (lambda (x) (sort (list-ref step2 x) #:key cadr <))))                  ;Sorting the step2 list elements in ascending order to use them later in step3
;(display sort-mat)
(define step3 (build-list N (lambda (x) (sort (build-list k (lambda (y) (car (list-ref (drop-right (list-ref sort-mat x) (- N k)) y)))) <))))
;(display step3)
;(newline)
; Part 3 over
;Part 4 starts
(define (intersection a b)                                                                             ;Function to calculate the intersection of two lists
  (if (null? a)
      '()
      (if (comprise (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))


(define (present list1 value)                                                                           ;Function to check if a value is present in a list using recursion
 (cond
  [(empty? list1) false]
  [(= (car list1) value) true]
  [else (present (cdr list1) value)]))



;(newline)
(define step4p (build-list N (lambda (x) (filter (lambda (e) (not (null? e))) (build-list k (lambda (y) (if  (and
                                                           ;(not(equal? (list-ref mypoints x) (list-ref mypoints (- (list-ref (list-ref step3 x) y) 1))))
                                                           (present (list-ref step3 x) (list-ref (list-ref step3 x) y))
                                                           (present (list-ref step3 (- (list-ref (list-ref step3 x) y) 1)) (+ x 1) ))
                                    (list (list-ref (list-ref step3 x) y) (length (intersection (list-ref step3 x) (list-ref step3 (- (list-ref (list-ref step3 x) y) 1))))) (list))))))))
 
                                                                                                        ; The penultimate step for step4 which perfomrs the algorithm 2.a and 2.b
 
(define step4 (build-list N (lambda (x) (sort (list-ref step4p x) #:key cadr >))))                      ; Step4 is finally built after applying sorting using the key.
;(display  step4)
;(newline)

(define cor-mat (build-list N (lambda (x) (filter (lambda (e) (not (null? e))) (build-list k (lambda (y) (if (and
                                                           (> (length (intersection (list-ref step3 x) (list-ref step3 (- (list-ref (list-ref step3 x) y) 1)))) (- e 1))
                                                           (present (list-ref step3 x) (list-ref (list-ref step3 x) y))
                                                           (present (list-ref step3 (- (list-ref (list-ref step3 x) y) 1)) (+ x 1) )) 
                                    (list (list-ref (list-ref step3 x) y) (length (intersection (list-ref step3 x) (list-ref step3 (- (list-ref (list-ref step3 x) y) 1))))) (list))))))))
                                                                                                     
																									    ; Applying algorithm 3.a and passing the obtained list to cor-mat



(define step5 (build-list N (lambda (x) (length (list-ref cor-mat x)))))                                ; Step5 finally stores the length of each list obtained after applying algorithm 3.a
;(display step5)
;(newline)

(define step6 (filter (lambda (e) (not (null? e))) (build-list N (lambda (x)  (if (> (list-ref step5 x) (- MinPts 1)) (+ x 1) (list))))))

                                                                                                         ; Calculate the list of indices of the core-points.
;(display step6)
;(newline)
;Step 6 over

;Step 7 starts
(define (boolwtthrs lst1 val)                                                        ;Function boolean weight threshold to check if an element is present in a list and if it is, it clears the threshold epsilon provided in the input
  (cond
    ((null? lst1) #f)
    ((equal? val (first (first lst1))) (if (>= (cadar lst1) e) #t #f))
    (else (boolwtthrs (cdr lst1) val))
    ))

(define (doubleindexcheck lst1 val1 val2)                                                          ;Function doubleindexcheck to check if val2 exists in the val1 index of the list passed
  (cond
    ((equal? val1 1) (boolwtthrs (car lst1) val2))
    (else
     (doubleindexcheck (cdr lst1) (- val1 1) val2))      
    ))
 
(define (dic-checker lst1 val1 val2)                                                              ;Function dic-dic-checker 
  (cond
    ((doubleindexcheck lst1 val2 val1) (doubleindexcheck lst1 val1 val2))
   (else #f))
 )

(define (clusterpenult lst1 lst2 val1)                                                                   ;Penultimate step for clustering
  (if (null? lst2) lst1 (clusterpenult (cond ((dic-checker step4 val1 (first lst2)) (append lst1 (list (first lst2)))) (else lst1)) (cdr lst2) val1)))

(define (clusterfinal lst1 lst2 lst3 val)                                                          ;Final step for clustering
  (cond
    ((null? lst3) lst1)
    ((null? lst2) lst1)
    (else (clusterfinal (remove-duplicates (append lst1 (clusterpenult (list val) lst3 val) )) (cdr (clusterpenult (list val) lst3 val)) (remove* (clusterpenult (list val) lst3 val) lst3)
	  (cond
        ((null? (cdr (clusterpenult (list val) lst3 val))) (list))
      (else (car(cdr (clusterpenult (list val) lst3 val)))))))))

(define (step7p lst1 lst2 val)
  (if (null? lst2) lst1 
      (step7p (append lst1  (list (append (list val) (list (sort (remove-duplicates (clusterfinal (list (car lst2)) (list (car lst2)) (cdr lst2) (car lst2))) <)))))
			  (remove* (clusterfinal (list (car lst2)) (list (car lst2))  (cdr lst2) (car lst2)) lst2 ) (+ val 1) )))

(define step7 (step7p (list) step6 const))
;(display step7)

;Step 7 over


(define (zero list1)                                                                                             ;Function to calculate the indices of all the zeroes present in a list
  (filter (lambda (e) (not (null? e))) (build-list N (lambda (x) (if (equal? (list-ref list1 x) 0) (+ x 1) (list))))))

(define step8(zero step5))

;(display step8)
;(newline)

sdfwsefqaef




(define step10 (list))
;(display step10)

(provide step1)
(provide step2)
(provide step3)
(provide step4)
(provide step5)
(provide step6)
(provide step7)
(provide step8)
(provide step9)
(provide step10)