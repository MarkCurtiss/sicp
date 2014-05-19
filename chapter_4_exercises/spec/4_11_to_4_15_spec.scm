(load "4_11_to_4_15.scm")

(describe "environmental Frames"
  (it "contains a list of bindings"
    (lambda ()
      (define frame (make-frame '(var1 var2) '(value1 (value2 value3))))

      (assert
       (equal?
	(frame-variables frame)
	'(var1 var2)))

      (assert
       (equal?
	(frame-values frame)
	'(value1 (value2 value3))))
      ))

  (it "lets you add new bindings"
    (lambda ()
      (define frame (make-frame '(var1) '(value1)))

      (add-binding-to-frame! 'var2 '(value2 value3) frame)

      (assert
       (equal?
	(frame-variables frame)
	'(var1 var2)))

      (assert
       (equal?
	(frame-values frame)
	'(value1 (value2 value3))))
    ))
  )

(describe "manipulating variables in the environment"
  (it "can find variables that have been defined in the environment"
    (lambda ()
      (define env (extend-environment '(oceansize) '("everyone into position") the-empty-environment))

      (assert
       (equal?
	(lookup-variable-value 'oceansize env)
	"everyone into position"))
      ))

  (it "can set a variable in an environment"
    (lambda ()
      (define env (extend-environment '(oceansize) '("superimposer") the-empty-environment))

      (set-variable-value! 'oceansize "everyone into position" env)

      (assert
       (equal?
	(lookup-variable-value 'oceansize env)
	"everyone into position"))
       ))

  (it "can define a new binding for an existing variable in an environment"
    (lambda ()
      (define env (extend-environment '(oceansize) '("superimposer") the-empty-environment))

      (define-variable! 'oceansize "everyone into position" env)

      (assert
       (equal?
	(lookup-variable-value 'oceansize env)
	"everyone into position"))
       ))

  (it "can define a new binding for a variable in an environment"
    (lambda ()
      (define env (extend-environment '(oceansize) '("superimposer") the-empty-environment))

      (define-variable! 'owls "the lion" env)

      (assert
       (equal?
	(lookup-variable-value 'owls env)
	"the lion"))
      ))

  (it "can remove variable bindings from the specified environment"
    (lambda ()
      (define env (extend-environment '(oceansize) '("superimposer") the-empty-environment))
      (define child-env (extend-environment '(oceansize owls) '("long forgotten", "the lion") env))

      (make-unbound! 'oceansize child-env)

      (assert
       (null? (find-variable-in-environment 'oceansize child-env)))
      ))
)