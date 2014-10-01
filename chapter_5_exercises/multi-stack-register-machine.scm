(load "book_code/ch5-regsim.scm")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine register-names)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-stack)
  (let ((s '()))
    (define (push x register)
      (let ((stack (cdr (assoc register s))))
	(set-cdr! (assoc register s) (cons x stack))
	))
    (define (pop register)
      (let ((stack (cdr (assoc register s))))
	(if (null? stack)
	    (error "Empty stack -- POP")
	    (let ((top (car stack)))
	      (set! stack (cdr stack))
	      top))))

    (define (initialize register-names)
      (for-each (lambda (register-name)
		  (set! s (cons (cons register-name '()) s)))
		  register-names)
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) initialize)
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack register)
  ((stack 'pop) register))

(define (push stack value register)
  ((stack 'push) value register))

(define (make-new-machine register-names)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () ((stack 'initialize) register-names)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
	(push stack (get-contents reg) (stack-inst-reg-name inst))
	(advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
      (lambda ()
	(set-contents! reg (pop stack (stack-inst-reg-name inst)))
	(advance-pc pc))))

'(MULTI STACK REGISTER SIMULATOR LOADED)
