(load "book_code/ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
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
      (define (get-instruction-set)
	(define (iter instructions results)
	  (if (null? instructions)
	      results
	      (let ((next-instruction (car (car instructions))))
		(iter (cdr instructions) (cons next-instruction results)))))

	(sort
	 (unique
	  (iter (map car the-instruction-sequence) '())
	 )
	 symbol<?)
	)
      (define (get-goto-register-set)
	(define (iter gotos results)
	  (if (null? gotos)
	      results
	      (let ((next-goto (car gotos)))
		(let ((destination (goto-dest next-goto)))
		  (if (not (register-exp? destination))
		      (iter (cdr gotos) results)
		      (let ((register (register-exp-reg destination)))
			(iter (cdr gotos) (cons register results))
		      ))))))
	(sort
	 (unique
	  (iter (filter goto-exp? (map car the-instruction-sequence)) '())
	  )
	 symbol<?)
	)
      (define (get-value-register-set)
	(define (iter stack-operations results)
	  (if (null? stack-operations)
	      results
	      (let ((next-register-operation (car stack-operations)))
		(let ((register (stack-inst-reg-name next-register-operation)))
		  (iter (cdr stack-operations) (cons register results)))))
	  )

	(sort
	 (unique
	  (iter (filter stack-operation? (map car the-instruction-sequence)) '())
	  )
	 symbol<?)
	)
      (define (get-register-assignment-sources register)
	(define (iter assignment-operations results)
	  (if (null? assignment-operations)
	      results
	      (let ((next-assignment-operation (car assignment-operations)))
		(if (eq? (assign-reg-name next-assignment-operation) register)
		    (iter (cdr assignment-operations) (cons (assign-value-exp next-assignment-operation) results))
		    (iter (cdr assignment-operations) results))))
	  )

	(unique
	 (iter (filter assignment-operation? (map car the-instruction-sequence)) '())
	 )
	)
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
	      ((eq? message 'instruction-set) (get-instruction-set))
	      ((eq? message 'goto-register-set) (get-goto-register-set))
	      ((eq? message 'value-register-set) (get-value-register-set))
	      ((eq? message 'register-assignment-sources) get-register-assignment-sources)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (instruction-set machine)
  (machine 'instruction-set))

(define (stack-operation? exp)
  (or (eq? (car exp) 'save)
      (eq? (car exp) 'restore)))

(define (assignment-operation? exp)
  (eq? (car exp) 'assign))

(define (goto-exp? exp)
  (eq? (car exp) 'goto))

(define (goto-register-set machine)
  (machine 'goto-register-set))

(define (value-register-set machine)
  (machine 'value-register-set))

(define (register-assignment-sources machine register)
  ((machine 'register-assignment-sources) register))

(define (unique list)
  (define (iter input results)
    (if (null? input)
	results
	(let ((element (car input)))
	  (if (memv element results)
	      (iter (cdr input) results)
	      (iter (cdr input) (cons element results))))))

  (iter list '()))
