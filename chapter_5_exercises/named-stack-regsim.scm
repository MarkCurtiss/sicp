(load "book_code/ch5-regsim.scm")

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons reg (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((popped-value (pop stack)))
	(let ((register-name (car popped-value))
	      (register-contents (cdr popped-value)))
	  (if (not (eq? register-name (stack-inst-reg-name inst)))
	      (error "Attempted to restore the wrong stack value to a register:" (stack-inst-reg-name inst))
	      (else
	       (set-contents! reg (pop stack))
	       (advance-pc pc))))))))

'(NAMED STACK REGISTER SIMULATOR LOADED)
