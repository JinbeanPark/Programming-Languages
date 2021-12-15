#lang racket

; Question 1: Write a Scheme procedure (expr-compare x y).


; Combine two symbols and insert "!" between two symbols.
(define (combineTwoSyms sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) "!" (symbol->string sym2)))
)

; Check whether the type of symbol is lambda or not.
(define (chkMemLam s) (member s '(λ lambda)))

; Compare the expressions x and y.
(define (expr-compare x y)
        ; The case that x is equal to y
  (cond [(equal? x y) x]
        ; The case that the data types of x and y are boolean type.
        [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
        ; The case that either x or b is not list.
        [(or (not (list? x)) (not (list? y))) (list 'if '% x y)]
        ; The case that x and y are list, but length of x and y are diiferent.
        [(and (list? x) (list? y) (not (equal? (length x) (length y)))) (list 'if '% x y)]
        ; The case that x and y are list as well as the length of x is equal to the length of y.
        [(and (list? x) (list? y) (equal? (length x) (length y))) (compareListXY x y)]))

; Compare the elements of list x and list y when the length of list x is equal to the length of list y.
(define (compareListXY x y)
        ; The case that the first element of list x or list y is "quote".
  (cond [(or (equal? 'quote (car x)) (equal? 'quote (car y))) (list 'if '% x y)]
        ; The case that the first element of only one list is "if".
        [(and (or (equal? 'if (car x)) (equal? 'if (car y))) (not (equal? (car x) (car y)))) (list 'if '% x y)]
        ; The case that the first element of both list x and list y is "lambda" or "λ".
        [(or (and (equal? 'lambda (car x)) (equal? 'lambda (car y))) (and (equal? 'λ (car x)) (equal? 'λ (car y))))
             ; The case that the length of arguments of list x is different to the length of arguments of list y.
             (cond [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]
                   ; The case that the first element of list x and list y is "lambda"
                   [(equal? 'lambda (car x)) (compareLambdaList 'lambda (cdr x) (cdr y) '() '())]
                   ; The case that the first element of list x and list y is "λ"
                   [(equal? 'λ (car x)) (compareLambdaList 'λ (cdr x) (cdr y) '() '())])]
        ; The case that the first element of lambda symbol is different in list x and list y.
        [(and (chkMemLam (car x)) (chkMemLam (car y)))
         (cond [(equal? (length (car (cdr x))) (length (car (cdr y)))) (compareLambdaList 'λ (cdr x) (cdr y) '() '())]
               [else (list 'if '% x y)])]
        ; The case that the first element is "lambda" or "λ" in the only one list, not both.
        [(or (chkMemLam (car x)) (chkMemLam (car y))) (list 'if '% x y)]
        ; The other cases.
        [else (compareListXY_otherCases x y)]))

; Compare the elements of list x and list y when two lists have the same length and not belong to the above cases.
(define (compareListXY_otherCases x y)
        ; The case that list x and y are empty.
  (cond [(and (empty? x) (empty? y)) '()]
        ; The case that the first element of both list x and list y is same.
        [(equal? (car x) (car y)) (cons (car x) (compareListXY_otherCases (cdr x) (cdr y)))]
        ; The case that the first element of both list x and list y is boolean type.
        [(and (boolean? (car x)) (boolean? (car y)))
         (cons (if (equal? (car x) '#t) '% '(not %)) (compareListXY_otherCases (cdr x) (cdr y)))]
        ; The case that the first element of both list x and list y is list type as well as has the same length.
        [(and (list? (car x)) (list? (car y)) (equal? (length (car x)) (length (car y))))
         (cons (compareListXY (car x) (car y)) (compareListXY_otherCases (cdr x) (cdr y)))]
        ; The other cases.
        [else (cons (list 'if '% (car x) (car y)) (compareListXY_otherCases (cdr x) (cdr y)))]))

; Compare the parameters of lists which has the first element as a lambda expression and has the same length.
(define (compareLambdaList lambda parameterX parameterY hashX hashY)
  (list lambda (compareLambdaArgs (car parameterX) (car parameterY))
     (compareLambdaFunc (car (cdr parameterX)) (car (cdr parameterY))
                        ; #t is for the list x & #f is for the list y
                        (cons (findHash (car parameterX) (car parameterY) #t) hashX)
                        (cons (findHash (car parameterX) (car parameterY) #f) hashY))))

; Compare the arguments of lists which has the first element as a lambda expression and has the same length.
(define (compareLambdaArgs argX argY)
        ; The case that arguments are empty.
  (cond [(and (empty? argX) (empty? argY)) '()]
        ; The case that two arguments are same.
        [(equal? (car argX) (car argY)) (cons (car argX) (compareLambdaArgs (cdr argX) (cdr argY)))]
        ; THe case that two arguments are different.
        [else (cons (combineTwoSyms (car argX) (car argY)) (compareLambdaArgs (cdr argX) (cdr argY)))]))

; Compare the function of listst which has the first element as a lambda expression and has the same length.
(define (compareLambdaFunc funX funY hashX hashY)
  ; Check whethere there is renamed variable for the first element in function part.
  (let ([renamedFunX (if (equal? (findRenamedVar funX hashX) "Not found") funX (findRenamedVar funX hashX))]
         [renamedFunY (if (equal? (findRenamedVar funY hashY) "Not found") funY (findRenamedVar funY hashY))])
         ; The case that the element in the function part is boolean type. 
   (cond [(and (boolean? funX) (boolean? funY)) (if funX '% '(not %))]
         ; The case that first elements in the function part at list X and list Y are list type and have the same length.
         [(and (list? funX) (list? funY) (equal? (length funX) (length funY))) (compareFunListElmt funX funY hashX hashY)]
         ; The case that first elements in the function part at list X and list Y are list type but has the different length. 
         [(and (list? funX) (list? funY) (not (equal? (length funX) (length funY))))
         (list 'if '% (replaceWithRenamedVarH funX hashX) (replaceWithRenamedVarH funY hashY))]
         ; The case that renamedFunX is equal to renamedFunY.
         [(equal? renamedFunX renamedFunY) renamedFunX]
         ; The case that nethier funX or funY is not list.
         [(or (not (list? funX)) (not (list? funY)))
         (list 'if '% (if (list? funX) (replaceWithRenamedVarH funX hashX) renamedFunX) (if (list? funY) (replaceWithRenamedVarH funY hashY) renamedFunY))])))

; Compare the first elements which are the list type and have the same length in function part.
(define (compareFunListElmt funX funY hashX hashY)
        ; The case that at least one of the first element of funX or funY is "quote".
  (cond [(or (equal? 'quote (car funX)) (equal? 'quote (car funY)))
         (if (equal? funX funY) funX (list 'if '% (replaceWithRenamedVarH funX hashX) (replaceWithRenamedVarH funY hashY)))]
        ; The case that the first elements of funX and funY is "if".
        [(and (equal? 'if (car funX)) (equal? 'if (car funY))) (cons 'if (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY))]
        ; The case that the first element of either funX or funY is "if".
        [(or (equal? 'if (car funX)) (equal? 'if (car funY))) (list 'if '% (replaceWithRenamedVarH funX hashX) (replaceWithRenamedVarH funY hashY))]
        ; The case that the first elements of funX and funY is "lambda" or "λ".
        [(and (chkMemLam (car funX)) (chkMemLam (car funY)))
         (cond [(equal? (length (car (cdr funX))) (length (car (cdr funY))))
                (if (and (equal? 'lambda (car funX)) (equal? 'lambda (car funY)))
                    (compareLambdaList 'lambda (cdr funX) (cdr funY) hashX hashY)
                    (compareLambdaList 'λ (cdr funX) (cdr funY) hashX hashY))]
               [else (list 'if '% (replaceWithRenamedVarH funX hashX) (replaceWithRenamedVarH funY hashY))])]
        ; The case that the only one first element of funX or funY is "lambda".
        [(or (chkMemLam (car funX)) (chkMemLam (car funY))) (list 'if '% (replaceWithRenamedVarH funX hashX) (replaceWithRenamedVarH funY hashY))]
        ; The other cases
        [else (compareFunListElmtOther funX funY hashX hashY)]))

; Compare the first elements which are the list type and have the same length in function part for other cases.
(define (compareFunListElmtOther funX funY hashX hashY)
  (cond [(and (empty? funX) (empty? funY)) '()]
        [else (let ([renamedFunX (if (equal? "Not found" (findRenamedVar (car funX) hashX)) (car funX) (findRenamedVar (car funX) hashX))]
                    [renamedFunY (if (equal? "Not found" (findRenamedVar (car funY) hashY)) (car funY) (findRenamedVar (car funY) hashY))])
                (cond [(and (list? renamedFunX) (list? renamedFunY))
                       (if (equal? (length (car funX)) (length (car funY)))
                          (cons (compareFunListElmt (car funX) (car funY) hashX hashY) (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY))
                          (cons (list 'if '% (replaceWithRenamedVarH (car funX) hashX) (replaceWithRenamedVarH (car funY) hashY)) (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY)))]
                      [(or (list? renamedFunX) (list? renamedFunY))
                       (list 'if '% (if (list? funX) (replaceWithRenamedVarH funX hashX) renamedFunX) (if (list funY) (replaceWithRenamedVarH funY hashY) renamedFunY))]
                      [(equal? renamedFunX renamedFunY) (cons renamedFunX (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY))]
                      [(and (boolean? (car funX)) (boolean? (car funY)))
                       (cons (if (car funX) '% '(not %)) (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY))]
                      [else (cons (list 'if '% renamedFunX renamedFunY) (compareFunListElmtOther (cdr funX) (cdr funY) hashX hashY))]))]))

; Replace the elements with renamed variables for the head element.
(define (replaceWithRenamedVarH funElmt hashmap)
  (cond [(empty? funElmt) '()]
        [(boolean? (car funElmt)) (cons (car funElmt) (replaceWithRenamedVarT (cdr funElmt) hashmap))]
        [(equal? (car funElmt) 'quote) funElmt]
        [(list? (car funElmt)) (cons (replaceWithRenamedVarH (car funElmt) hashmap) (replaceWithRenamedVarT (cdr funElmt) hashmap))]
        [(equal? (car funElmt) 'if) (cons (car funElmt) (replaceWithRenamedVarT (cdr funElmt) hashmap))]
        [(chkMemLam (car funElmt)) (cons (car funElmt) (cons (car (cdr funElmt)) (replaceWithRenamedVarT (cdr (cdr funElmt)) (cons (findHash (car (cdr funElmt)) (car (cdr funElmt)) #t) hashmap))))]
        [else (cons (if (equal? "Not found" (findRenamedVar (car funElmt) hashmap))
                        (car funElmt) (findRenamedVar (car funElmt) hashmap))
                    (replaceWithRenamedVarT (cdr funElmt) hashmap))]))

; Replace the elements with renamed variables for the tail elements.
(define (replaceWithRenamedVarT funElmt hashmap)
  (cond [(empty? funElmt) '()]
        [(boolean? (car funElmt)) (cons (car funElmt) (replaceWithRenamedVarT (cdr funElmt) hashmap))]
        [(equal? (car funElmt) 'quote) funElmt]
        [(list? (car funElmt)) (cons (replaceWithRenamedVarH (car funElmt) hashmap) (replaceWithRenamedVarT (cdr funElmt) hashmap))]
        [else (cons (if (equal? "Not found" (findRenamedVar (car funElmt) hashmap))
                        (car funElmt) (findRenamedVar (car funElmt) hashmap))
                    (replaceWithRenamedVarT (cdr funElmt) hashmap))]))

; Find the hashmap for the list x and list y.
(define (findHash argX argY tXfY)
  (cond [(and (empty? argX) (empty? argY)) (hash)]
        [(equal? (car argX) (car argY)) (hash-set (findHash (cdr argX) (cdr argY) tXfY) (car argX) (car argY))]
        [else
         (if tXfY
         (hash-set (findHash (cdr argX) (cdr argY) tXfY) (car argX) (combineTwoSyms (car argX) (car argY)))
         (hash-set (findHash (cdr argX) (cdr argY) tXfY) (car argY) (combineTwoSyms (car argX) (car argY))))]))

; Find the renamed variables for the first element of function part in list x and list y
(define (findRenamedVar funElmt hashmap)
  (cond
    [(empty? hashmap) "Not found"]
    [(equal? (hash-ref (car hashmap) funElmt "Not found") "Not found") (findRenamedVar funElmt (cdr hashmap))]
    [else (hash-ref (car hashmap) funElmt "Not found")]))




; Question 2: Write a Scheme procedure (test-expr-compare x y).
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))))




; Question 3: Define two scheme variables test-expr-x and test-expr-y.
(define test-expr-x '((lambda (a b) (lambda (c) (+ d))) (λ (a b) (λ (c d) (+ e))) (lambda (a b) (quote (if d)))))
(define test-expr-y '((lambda (a if) (lambda (b c) (if b c))) (lambda (a b) (λ (c d e) (+ if g))) (lambda (a b) (c '(d e)))))




