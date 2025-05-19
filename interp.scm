(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))
        
        ;\commentbox{\ma{\theemptylistspec}}
        (empty-list-exp ()
          (empty-list-val))

        ;\commentbox{\ma{\theconsspec}}
        (cons-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (list-val val1 val2)))
        
        ;; Added a car operation to the grammar
        ;\commentbox{\ma{\thecarspec}}
        (car-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (empty-list-val () 
                (eopl:error 'car-exp "car of empty list"))
              (list-val (head tail) head)
              (else 
                (eopl:error 'car-exp "car of non-list")))))
        
        ;; Added a cdr operation to the grammar
        ;\commentbox{\ma{\thecdrspec}}
        (cdr-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (empty-list-val () 
                (eopl:error 'cdr-exp "cdr of empty list"))
              (list-val (head tail) tail)
              (else 
                (eopl:error 'cdr-exp "cdr of non-list")))))
        
        ;; Added a null? operation to the grammar
        ;\commentbox{\ma{\thenullspecexp}}
        (null?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (cases expval val1
              (empty-list-val () 
                (bool-val #t))
              (list-val (head tail) 
                (bool-val #f))
              (else 
                (eopl:error 'null?-exp "null? of non-list")))))
        
        ;; Added a list? operation to the grammar that can take any number of arguments and
        ;; generate a list with them
        ;\commentbox{\ma{\thelistspec}}
        (list-exp (exps)
          (if (null? exps) (empty-list-val)
            (let ((val (value-of (car exps) env)))
              (list-val val (value-of (list-exp (cdr exps)) env)))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))

        )))


  )

