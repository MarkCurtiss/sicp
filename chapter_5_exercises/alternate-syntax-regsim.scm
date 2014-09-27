(load "book_code/ch5-regsim.scm")

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'jump-if-true)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (register-exp? exp) (tagged-list? exp 'register))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'operator)))

'(ALTERNATE SYNTAX REGISTER SIMULATOR LOADED)
