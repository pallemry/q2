(module mytests (lib "eopl.ss" "eopl")
    (require "utils.scm")
    (require "drscheme-init.scm")
    (require "data-structures.scm")  ; for expval constructors
    (require "lang.scm")             ; for scan&parse
    (require "interp.scm")           ; for value-of-program
    (require "top.scm")            ; for run-all

    (define run-list-test-int
      (lambda (test-name test-code test-list expfn)
        (let ((res (expfn (run test-code))))
            (if (equal? res test-list)
                #t
                (begin
                  (display "Test failed: ")
                  (display test-name)
                  (newline)
                  (display "Expected: ")
                  (display test-list)
                  (newline)
                  (display "Actual: ")
                  (display res)
                  (newline)
                  #f)))))

    (define run-list-test
      (lambda (test-name test-code test-list)
        (let ((res (run-list-test-int test-name test-code test-list expval->list)))
            (if res
                (begin
                  (display "Test passed: ")
                  (display test-name)
                  (newline))
                #f))))

    (define run-list-test-num
      (lambda (test-name test-code test-list)
        (let ((res (run-list-test-int test-name test-code test-list expval->num)))
            (if res
                (begin
                  (display "Test passed: ")
                  (display test-name)
                  (newline))
                #f))))

    (define run-all-list-tests
      (lambda ()
        (run-list-test 'test-cons-1 "cons(1,cons(2,emptylist))" '(1 2))
        (run-list-test 'test-cons-2 "cons(1,emptylist)" '(1))
        (run-list-test 'test-cons-3 "cons(-5,cons(1,emptylist))" '(-5 1))
        (run-list-test 'test-cdr-2 "cdr(cons(1,emptylist))" '())
        (run-list-test 'test-cdr-3 "cdr(cons(-5,cons(1,emptylist)))" '(1))
        (run-list-test 'test-emptylist "emptylist" '())
        (run-list-test 'test-list-1 "list(1, 2)" '(1 2))
        (run-list-test 'test-list-2 "list()" '())
        (run-list-test 'test-list-3 "list(-(1,1), if null?(list()) then 1 else 2)" '(0 1))
    ))
    
    (run-all)
    (run-all-list-tests)
)


