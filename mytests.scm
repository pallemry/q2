(module mytests (lib "eopl.ss" "eopl")
    (require "utils.scm")
    (require "drscheme-init.scm")
    (require "data-structures.scm")  ; for expval constructors
    (require "lang.scm")             ; for scan&parse
    (require "interp.scm")           ; for value-of-program
    (require "top.scm")            ; for run-all

    (define run-list-test
      (lambda (test-name test-code test-list)
        (let ((res (expval->list (run test-code))))
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
          
    (display (run "cons(-5,cons(1,emptylist))"))
    (display (run-list-test 'test1 "cons(-5,cons(1,emptylist))" '(-5 1)))
)


