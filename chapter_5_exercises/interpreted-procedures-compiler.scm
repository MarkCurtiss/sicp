(load "book_code/ch5-compiler.scm")

;; generate interpreted procedure calls
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
	(interpreted-branch (make-label 'interpreted-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
	  (test (op compound-procedure?) (reg proc))
	  (branch (label, interpreted-branch))))
       (parallel-instruction-sequences
	 (append-instruction-sequences
	  compiled-branch
	  (compile-proc-appl target compiled-linkage))
	(parallel-instruction-sequences
	 (append-instruction-sequences
	  interpreted-branch
	  (compile-interp-proc-appl target compiled-linkage))
	 (append-instruction-sequences
	  primitive-branch
	  (end-with-linkage linkage
	    (make-instruction-sequence '(proc argl)
	    (list target)
	    `((assign ,target
		      (op apply-primitive-procedure)
		      (reg proc)
		      (reg argl)))))))
	)
       after-call))))

;; applying interpreted procedures
(define (compile-interp-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
	     (save continue)
	     (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
	      (save continue)
	      (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
	  '((save continue)
	    (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))
