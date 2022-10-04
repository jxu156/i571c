#lang racket
(define EMPLOYEES
  '((tom 33 cs 85000.00)
    (joan 23 ece 110000.00)
    (bill 29 cs 69500.00)
    (john 28 me 58200.00)
    (sue 19 cs 22000.00)
    ))

;; #1: 15-points
;;return list of employees having department dept
;;must be implemented recursively
;; Hint: use equal? to check for department equality

(define dept-employees
  (lambda (dept employees)
     (cond[(null? employees) '()]
          [(equal? (list-ref(list-ref employees 0)2) dept)
  (append (list (car employees)) (dept-employees  dept (cdr employees))) ]
          [else  (dept-employees dept (cdr employees))]
  ))) 

(equal? (dept-employees 'ece EMPLOYEES) '((joan 23 ece 110000.00)))

(equal? (dept-employees 'cs EMPLOYEES)
 	      '((tom 33 cs 85000.00)
		(bill 29 cs 69500.00)
		(sue 19 cs 22000.00)
		))

(equal? (dept-employees 'ce EMPLOYEES) '())

;; #2: 5-points
;;return list of names of employees belonging to department dept
;;must be implemented recursively
;;Hint: almost the same as the previous exercise


(define dept-employees-names
  (lambda (dept employees)
     (cond[(null? employees) '()]
          [(equal? (list-ref(list-ref employees 0)2) dept)
  (append (list (list-ref(car employees) 0)) (dept-employees-names  dept (cdr employees))) ]
          [else  (dept-employees-names dept (cdr employees))]
  )))    


(equal? (dept-employees-names 'ece EMPLOYEES) '(joan))
(equal? (dept-employees-names 'cs EMPLOYEES) '(tom bill sue))
(equal? (dept-employees-names 'ce EMPLOYEES) '())



;; #3: 15-points
;;Given list indexes containing 0-based indexes and a list possibly
;;containing lists nested to an abitrary depth, return the element
;;in list indexed successively by indexes. Return 'nil if there is
;;no such element.
;;Hint: use list-ref

(define list-access
  (lambda(indexes list)
       (cond [(null? indexes) list]
             [(<= (length list)   (list-ref indexes 0) ) 'nil]
             [(integer? (list-ref indexes 0))
                        
          (list-access (cdr indexes)  (list-ref list (list-ref indexes 0)))]

)))

(equal? (list-access '(1) '(a b c)) 'b)
(equal? (list-access '(2) '(a b (c))) '(c)) (equal? (list-access '(2 0) '(a b (c))) 'c)
(equal? (list-access '(3) '(a b (c))) 'nil)
(equal? (list-access '(2 1) '(a b (c))) 'nil)

(equal? (list-access '() '((1 2 3) (4 (5 6 (8)))) )  '((1 2 3) (4 (5 6 (8)))))

(equal? (list-access '(1) '((1 2 3) (4 (5 6 (8)))) )'(4 (5 6 (8))))
(equal? (list-access '( 1 1 2) '((1 2 3) (4 (5 6 (8)))) )'(8))
(equal? (list-access '( 1 1 2 0) '((1 2 3) (4 (5 6 (8)))) ) '8)
(equal? (list-access '(0 1) '((1))) 'nil)


;; #4: 15-points
;;return sum of salaries for all employees
;;must be tail-recursive
;;Hint: use a nested auxiliary function with an accumulating parameter
(define employees-salary-sum
   (lambda (employees)
     (define salary 0)
     (cond
         [(null? employees) 0]
      [else (employees-salary-sum-add employees salary )]
       )
           
  ))      

(define employees-salary-sum-add
    (lambda (employees salary)
      (cond
         [(null? employees) salary]
      [else (employees-salary-sum-add (cdr employees) (+ salary (list-ref( car employees )3) ))]
 )))

(equal? (employees-salary-sum EMPLOYEES) 344700.00)
(equal? (employees-salary-sum '()) 0)

;; #5: 15-points
;;return list of pairs giving name and salary of employees belonging to
;;department dept
;;cannot use recursion
;;Hint: use filter and map from the standard Racket library
;;(google "racket filter", "racket map")
(define (dept-employees-names-salaries dept employees)
          (map (lambda(x)  (list (list-ref x 0) (list-ref x 3)))
                 (filter (lambda (x) (equal? (list-ref x 2) dept)) employees)))                        

(equal? (dept-employees-names-salaries 'ece EMPLOYEES) '((joan 110000.00)))
(equal? (dept-employees-names-salaries 'cs EMPLOYEES)
	      '((tom 85000.00)
 		(bill 69500.00)
 		(sue 22000.00)
 		))
(equal? (dept-employees 'ce EMPLOYEES) '())


;; #6: 15-points
;;return average salary of all employees; 0 if employees empty
;;cannot use recursion
;;Hint: use foldl from the standard Racket library
;;(google "racket foldl").
(define (employees-average-salary  employees)
      (cond[(null? employees) 0]   
       [else     (/  (foldl + 0
          (map (lambda(x)  (list-ref x 3))
                  employees)) (length employees))]  )
  )
                      

(equal? (employees-average-salary EMPLOYEES) (/ 344700.00 5))

(equal? (employees-average-salary '()) 0)

;; #7: 20-points
;; given an integer or list of nested lists containing integers,
;; return a string containing its JSON representation without any
;; whitespace
;; Hints: use (number->string n) to convert integer n to a string.
;;        use (string-append str1 str2 ...) to append str1 str2 ...
;;        use (string-join str-list sep) to join strings in str-list using sep
;; also see toJson() methods in java-no-deps Parser.java in prj1-sol


(define (int-list-json int-list)
  (cond [(integer? int-list ) (number->string int-list)]
        [else (int-list-json-T int-list)]
        ))

(define (int-list-json-T int-list)   

 (string-append  "[" (string-join   (map (lambda(x)
         (cond [(integer? x)  (number->string  x)]
               [else (int-list-json-T x)]
           )
         ) int-list) ",") "]")

)


 (equal? (int-list-json '(1 2 3)) "[1,2,3]")
 (equal? (int-list-json '(1 (2 (4 5) 6))) "[1,[2,[4,5],6]]")
 (equal? (int-list-json '()) "[]")
 (equal? (int-list-json 42) "42")