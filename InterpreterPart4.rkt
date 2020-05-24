#lang racket
;;;; Group Member: Jack Lu zxl629 Chen Nie cxn146
;;;; EECS 345
;;;; Interpreter Part 4


(require "classParser.rkt")

;; Input takes a file name, returns the value of the code in the file.
(define interpret
  (lambda (file class)
    (call/cc
     (lambda (return)
       (let ((classState (newGlobal (parser file) init return initbreak initcontinue inithrow)))
         (runMain (lookup (string->symbol class) classState) classState return initbreak initcontinue inithrow))))))

;; Runs the main function in class closure
(define runMain
  (lambda (classClosure state return break continue throw)
    (call/cc
     (lambda (newReturn)
       (calFunc (lookuplayer 'main (caddr classClosure)) '() state newReturn break continue throw)))))
     



;; Evaluates the parse tree, returns the state
(define evaluate
  (lambda (tree state return break continue throw type)
    (cond
      [(null? tree) state] ;;???
      [else (evaluate (cdr tree) (Mstate (car tree) state return break continue throw type) return break continue throw type)])))
      
;; A new global level that reads a list of class definitions and stores their names with their closures in the state.
(define newGlobal
  (lambda (tree classState return break continue throw)
    (cond
      [(null? tree) classState]
      [else (newGlobal (cdr tree) (newGlobalMstate (car tree) classState) return break continue throw)])))

;; Abstration
(define className cadr)
(define fillClosure cddr)
;; Handles the definition of each class, stores them in the closure.
(define newGlobalMstate
  (lambda (bnf state)
    (create (className bnf) (classClosure (fillClosure bnf) state (className bnf)) state)))

;; classClosure creates a class closure
;; 1. Parent/super class
;; 2. list of instance field names
;; 3. List of methods/function names and closures
(define classClosure
  (lambda (classdef state name)
    (cond
      ;; Parent class closure is an empty list if there is no parent
      [(null? (car classdef)) (cons '() (parseClass (cadr classdef) 'NULL name))]
      ;; Passes the parent class closure to build the closure for this class
      [else (cons (lookup (cadar classdef) state) (parseClass (cadr classdef) (lookup (cadar classdef) state) name))])))

;; Do the M_state functions for variable declaration and function definitions in a class, stores them in a state.
(define parseClass
  (lambda (bnf parent name)
    (cond
      [(eq? 'NULL parent) (cons (getFields bnf layer) (list (getMethods bnf layer name)))]  ;; Combines the list of instance fields names and method closures                                                                      
      [else (cons (combineLayers (cadr parent) (getFields bnf layer)) (list (combineLayers (caddr parent) (getMethods bnf layer name))))])))
      ; If there is a parent, combines the inherited methods to the method closure.

;; Returns a list of instance fields names
(define getFields
  (lambda (bnf state)
    (cond
      [(null? bnf) state]
      [(and (eq? 'var (caar bnf)) (null? (cddar bnf))) (getFields (cdr bnf) (createhelper (cadar bnf) 'und state))]
      [(eq? 'var (caar bnf)) (getFields (cdr bnf) (createhelper (cadar bnf) (caddar bnf) state))]
      [else (getFields (cdr bnf) state)])))

;; Returns a list of methods
(define getMethods
  (lambda (bnf state name)
    (cond
      [(null? bnf) state]
      [(eq? (caar bnf) 'static-function) (getMethods (cdr bnf) (createhelper 'main (createFuncClosure '() (cadddr (car bnf)) 'main name) state) name)]
      [(eq? 'function (caar bnf)) (getMethods (cdr bnf) (createhelper (cadar bnf) (createFuncClosure (caddar bnf) (cadddr (car bnf)) (cadar bnf) name) state) name)]
      [else (getMethods (cdr bnf) state name)])))

;; Combines two layers. Layer2 will overwrite the variables with the same name in layer 1.
(define combineLayers
  (lambda (layer1 layer2)
    (cond
      ((null? (car layer1)) layer2)
      ((null? (car layer2)) layer1)
      ((ifexistlayer (caar layer1) layer2) (combineLayers (remove (caar layer1) layer1) layer2))
      (else (combineLayers (remove (caar layer1) layer1) (createhelper (caar layer1) (car (cadr layer1)) layer2))))))

    
    

;; Creates an instance closure that contains:
;; 1. instance's class (true type)
;; 2. A list of instance field values
(define instanceClosure
  (lambda (trueType state)
    (cons (lookup trueType state) (cons (getFieldValues trueType state) '()))))

;; Returns the instance field values.
(define getFieldValues
  (lambda (type state)
    (cadr (lookup type state))))

;; Returns the instance closure of a variable.
(define getInstanceClosure
  (lambda (var state)
     (lookup-box var state)))


;; Find the right function closure from an instance closure
(define getFuncIns
  (lambda (var closure)
    (lookuplayer var (caddr (car closure)))))
  

(define getThis
  (lambda (type state)
    (cadr (lookup type state))))
    


;; M_state takes a BNF and a state and updates the state.
(define Mstate
  (lambda (bnf state return break continue throw type)
    (cond
      [(not (list? bnf)) state]

      ;; Calls declarehandle when it reads a statement that declares a variable
      [(eq? 'var (car bnf)) (declarehandle bnf state return break continue throw type)]

      [(and (eq? '= (car bnf)) (eq? 'this (caadr bnf))) (assignhandle2 bnf state return break continue throw type)]
      
      ;; Calls assignhandle when it reads a statement that assigns a value to a variable
      [(eq? '= (car bnf)) (assignhandle bnf state return break continue throw type)]
      
      ;; Calls whilehandle when it encounters a while loop
      [(eq? 'while (car bnf)) (call/cc (lambda (realbreak) (whilehandle bnf state return realbreak continue throw type)))]
      
      ;; Calls ifhandle when it encounters an if statement
      [(eq? 'if (car bnf)) (ifhandle bnf state return break continue throw type)]
      
      ;; Calls returnhandle when it encounters a return statement
      [(eq? 'return (car bnf)) (returnhandle bnf state return break continue throw type)]

      ;; Calls blockhandle when it encounters a block
      [(eq? 'begin (car bnf)) (blockhandle (cdr bnf) state return break continue throw type)]

      ;; Calls continuehandle when it encounters a continue statement
      [(eq? 'continue (car bnf)) (continuehandle bnf state return break continue throw type)]

      ;; Calls breakhandle when it encounters a break statement
      [(eq? 'break (car bnf)) (breakhandle bnf state return break continue throw type)]

      ;; Calls tryhandle when it encounters a try statement
      [(eq? 'try (car bnf)) (tryhandle bnf state return break continue throw type)]

      ;; Calls throwhandle when it encounters a throw statement
      [(eq? 'throw (car bnf)) (throw (Mvalue (cadr bnf) state return break continue throw type) (pop state))]

      ;; Evaluates the function and ignores the return value, returns the state after evaluation
      [(eq? 'funcall (car bnf)) (evalFunc (getFuncIns (funcName bnf) (getInstanceClosure (cadadr bnf) state)) (cddr bnf) state return break continue throw type)]

      ;; Binds the function definition to a closure
      [(eq? 'function (car bnf)) (bindclosure bnf state return break continue throw type)]
      
      [else error 'badop "The syntax is wrong."])))

;; M_value (<value1> <value2> +, state) = M_value(<value1>, state) + M_value(<value2>, state)
;; M_value returns the value of an expression, it could be an interger or a boolean value.
(define Mvalue
  (lambda (expression state return break continue throw type)
    (cond
      [(null? expression) (error 'parser "parser should have caught this.")]
      [(number? expression) expression]
      [(eq? 'true expression) '#t]
      [(eq? 'false expression) '#f]
      [(not (list? expression)) (lookup-box expression state)]
      [(eq? 'new (operator expression)) (instanceClosure (cadr expression) state)]
      [(and (eq? 'dot (operator expression)) (eq? 'this (cadr expression))) (lookuplayer (caddr expression) (getThis type state))]
      [(eq? 'dot (operator expression)) (lookuplayer (caddr expression) (cadr (getInstanceClosure (cadr expression) state)))]
      
      ;; Sets a new return continuation to for the value returned by function.
      [(eq? 'funcall (operator expression)) (call/cc (lambda (newReturn) (calFunc (getFuncIns (funcName expression) (getInstanceClosure (cadadr expression) state)) (cddr expression) state newReturn break continue throw)))]
      
      [(eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(and (eq? '- (operator expression)) (pair? (cddr expression))) (- (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(and (eq? '- (operator expression)) (not (pair? (cddr expression)))) (* '-1 (Mvalue (leftoperand expression) state return break continue throw type))]
      [(eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]

     

      [(eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type)))]
      [(eq? '< (operator expression)) (< (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '> (operator expression)) (> (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '&& (operator expression)) (and (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '|| (operator expression)) (or (Mvalue (leftoperand expression) state return break continue throw type) (Mvalue (rightoperand expression) state return break continue throw type))]
      [(eq? '! (operator expression)) (not (Mvalue (leftoperand expression) state return break continue throw type))]
      [else (error 'error "The operator is not known")])))


    
;; The helper methods to define and abstract away the operator, leftoperand, and rightoperand parts of an expression
(define operator
  (lambda (expression)
    [car expression]))

;; Abstraction for left operand of an operation
(define leftoperand cadr)

;; Abstraction for function name
(define funcName
  (lambda (input)
    (caddr (cadr input))))

;; Abstraction for right operand of an opperation
(define rightoperand caddr)


;; Handles the definition of function in global layer. Binds the function name to the funciton closure.
;; Closure is a triple
;; 1. Formal param list
;; 2. Function body
;; 3. A function that takes a state as input and outputs the proper state for this statically scoped function.
(define bindclosure
  (lambda (bnf state return break continue throw type)
    (create (functionname bnf) (createFuncClosure (paramlist bnf) (body bnf) (functionname bnf) type) state)))

;; Creates function closure
(define createFuncClosure
  (lambda (param body name className)
    (list param body (functionstate name) className)))
;;
;(define classState
;  (lambda (name)
;    (lambda (state)
;      (lookup name state))))

;; Abstraction
(define functionname cadr)
(define paramlist caddr)
(define body cadddr)

;; Takes a function name and returns a function that takes a the current state as input and creates the function environment.
(define functionstate
 (lambda (name)
   (lambda (state)
     state)))

;; evalFunc ignores the return in function and runs the function
(define evalFunc
  (lambda (closure params state return break continue throw type)
    (run (cadr closure) (bindparam params (car closure) (addlayer ((caddr closure) state)) state return break continue throw type) return break continue throw type)))


;; Returns the state of the function
(define functionreturn
  (lambda (state)
    (lambda (v)
      state)))

;; Runs the function, ignores the return value and returns a state.
(define run
  (lambda (tree state return break continue throw type)
    (cond
      [(null? tree) state]
      [else (run (cdr tree) (Mstate (car tree) state (functionreturn state) break continue throw type) return break continue throw type)])))
     

    

;; Calculates the function and returns the value
(define calFunc
  (lambda (closure params state return break continue throw)
       (evaluate (cadr closure) (bindparam params (car closure) (addlayer ((caddr closure) state)) state return break continue throw (cadddr closure)) return break continue throw (cadddr closure))))

;; Evaluates the actual parameters in the current environment and bind them to the formal parameters.
;; Returns the current environment
(define bindparam
  (lambda (actualparam formalparam state paramstate return break continue throw type)
    (create-box 'this type (bindparam1 actualparam formalparam state paramstate return break continue throw type))))
    
(define bindparam1
  (lambda (actualparam formalparam state paramstate return break continue throw type)
    (cond
      [(not (sameLength actualparam formalparam)) (error 'error "Arguments unmatched")]
      [(null? actualparam) state]
      [else (bindparam1 (cdr actualparam) (cdr formalparam) (create-box (car formalparam) (Mvalue (car actualparam) paramstate return break continue throw type) state) paramstate return break continue throw type)])))

;; Helper function that compares the arguments list of actual param and formal param
(define sameLength
  (lambda (lis1 lis2)
    (cond
      [(and (null? lis1) (null? lis2)) '#t]
      [(or (null? lis1) (null? lis2)) '#f]
      [else (sameLength (cdr lis1) (cdr lis2))])))
    


;; Handles the return statement, returns the Mvalue of the statement and changes it to 'true/'false if it is a boolean.
(define returnhandle
  (lambda (bnf state return break continue throw type)
    (cond
      [(boolean? (Mvalue (cadr bnf) state return break continue throw type)) (return (properbool (Mvalue (cadr bnf) state return break continue throw type)))]
      [else (return (Mvalue (cadr bnf) state return break continue throw type))])))

;; Helper function that transfroms #t into 'true and #f into 'false
(define properbool
  (lambda (x)
    (if (eq? '#t x)
        'true
        'false)))


;; Handles the variable declare statements
(define declarehandle
  (lambda (bnf state return break continue throw type)
    (cond
      ;;[(eq? '#t (ifexist (declarevar bnf) state)) (error 'error "The variable has already been declared")]
      [(null? (declarevallis bnf))  (create-box (declarevar bnf) 'und state)]
      [else (create-box (declarevar bnf) (Mvalue (declareval bnf) state return break continue throw type) state)])))

;; Abstraction for variable declaration
(define declarevar cadr)
(define declarevallis cddr)
(define declareval caddr)

;; Handles the while loop, use call/cc for continue so that it will skips the current iteration when there is a continue statement.
;; Replaces the initial continue function with the real continue function.
(define whilehandle
  (lambda (bnf state return break continue throw type)
    (cond
      [(and (eq? '#t (Mvalue (whilecond bnf) state return break continue throw type)) (eq? 'begin (caaddr bnf))) (Mstate bnf (call/cc (lambda (realcontinue) (Mstate (whilestmt bnf) state return break realcontinue throw type))) return break continue throw type)]
      [(eq? '#t (Mvalue (whilecond bnf) state return break continue throw type)) (Mstate bnf (call/cc (lambda (realcontinue) (Mstate (whilestmt bnf) (addlayer state) return break realcontinue throw type))) return break continue throw type)]
      [else state])))

;; Abstraction for whilehandle
(define whilecond cadr)
(define whilestmt caddr)

;; Handles the variable assignment
(define assignhandle
  (lambda (bnf state return break continue throw type)
    (update-box (assignvar bnf) (Mvalue (assignval bnf) state return break continue throw type) state)))

;; Handles the variable assignment
(define assignhandle2
  (lambda (bnf state return break continue throw type)
    (updateClassValue type (caddr (cadr bnf)) (Mvalue (assignval bnf) state return break continue throw type) state)))




;; Abstraction for assignhandle
(define assignvar cadr)
(define assignval caddr)

;; Handles the if statement
;; 1. If Else
;; 2. If
(define ifhandle
  (lambda (bnf state return break continue throw type)
    (cond
      [(pair? (ifstmt2lis bnf)) (if (Mvalue (ifcond bnf) state return break continue throw type)
                                    (Mstate (ifstmt1 bnf) state return break continue throw type)
                                    (Mstate (elsestmt bnf) state return break continue throw type))]
      [(if (Mvalue (ifcond bnf) state return break continue throw type)
           (Mstate (ifstmt1 bnf) state return break continue throw type)
           state)])))

;; Abstraction for ifhandle
(define ifstmt2lis cdddr)
(define ifcond cadr)
(define ifstmt1 caddr)
(define elsestmt cadddr)

;; Handles a block of code, adds a layer to the front of the state, pops it when it returns the state after executing the code in the block.
(define blockhandle
  (lambda (bnf state return break continue throw type)
    (runblock bnf (addlayer state) return break continue throw type)))


;; Runs the code in the block, returns the state after running it.
(define runblock
  (lambda (bnf state return break continue throw type)
    (cond
      [(null? bnf) state]
      [else (runblock (cdr bnf) (Mstate (car bnf) state return break continue throw type) return break continue throw type)])))

;; Handles the continue statement that skips to the next iteration of the loop
(define continuehandle
  (lambda (bnf state return break continue throw type)
    (continue (pop state))))

;; Handles the break statement that breaks out of the immediate loop.
(define breakhandle
  (lambda (bnf state return break continue throw type)
    (break (pop state))))

;; Handles try catch
;; 1. try catch cause
;; 2. try finally
;; 3. try-catch-finally cause
(define tryhandle
  (lambda (bnf state return break continue throw type)
    (cond
      [(null? (tryfinally bnf)) (tryhelper (try bnf) (trycatch bnf) '() state return break continue throw type)] ;; no finally
      [(null? (trycatch bnf)) (tryhelper (try bnf) '() (tryfinally bnf) state return break continue throw type)] ;; no catch
      [else (tryhelper (try bnf) (trycatch bnf) (tryfinally bnf) state return break continue throw type)]))) ;; try-catch-finally

;;Abstraction for try catch
(define try cadr)
(define trycatch caddr)
(define tryfinally cadddr)

;; If there is no finally after try, returns the state after executing try-catch.
;; If there is finally, runs the code in finally after running the code in try-catch
;; Use call/cc so that if there is a throw in try, it will skips the rest of code and go to catchhandle, else run through the whole try block.
(define tryhelper
  (lambda (try catch finally state return break continue throw type)
    (cond
      [(null? finally) (call/cc (lambda (realthrow) (blockhandle try state return break continue (lambda (v newstate) (realthrow (catchhandle catch v (merge state newstate) return break continue throw type))) type)))]
      [else (blockhandle (cadr finally) (call/cc (lambda (realthrow) (blockhandle try state return break continue (lambda (v newstate) (realthrow (catchhandle catch v (merge state newstate) return break continue throw))) type))) return break continue throw type)])))
    
;; Merges the state returned by the throw and the current state before running the try block.
(define merge
  (lambda (lis1 lis2)
    (cond
      [(null? (cdr lis1)) lis2]
      [else (cons (car lis1) (merge (cdr lis1) lis2))])))
    
      

;; Running the code in the catch block, which takes a state returned from try and a value that try throws. The function returns the
;; state after running through the block.
(define catchhandle
  (lambda (catch v state return break continue throw type)
    (cond
      [(null? catch) state]
      [else (pop (runblock (caddr catch) (create-box (caadr catch) v (addlayer state)) return break continue throw type))])))


;-----------------
; HELPER FUNCTIONS
;-----------------

;; Pops the first layer from the state
(define pop
  (lambda (s)
    (cdr s)))

;; Adds a layer to the front of the state
(define addlayer
  (lambda (s)
    (cons layer s)))

;; Defines the break continuation that is used inside a loop
(define initbreak
  (lambda (v) (error 'error "Illegal break statement")))

;; Give a error when break is called outside of a loop
(define realbreak
  (lambda (v) v))

;; Give a error when continue is called outside of a loop
(define initcontinue
  (lambda (v) (error 'error "Illegal continue statement")))

;; Defines the continue continuation that is used inside a loop
(define realcontinue
  (lambda (v) v))

;; Defines the throw continuation when it is illegal to use throw
(define inithrow
  (lambda (v state) (error 'error "Illegal throw")))

;; Throws the value and the state
; (define realthrow
; (lambda (v state) (cons v (pop state))))


;; State is a list that contains layers, each layer contains two sublists where the first one stores the variables and the second one stores the values.

;;Initializes the state with a layer 
; (define init '(()()))
(define init '((()())))

;; Defines the structure of a layer in the state
(define layer '(()()))

;; Defines the top layer in the state
(define toplayer car)

;; Defines the rest of the layers in a state
(define restlayer cdr)

;; Creates a new binding pair in the top layer of the state
(define create
  (lambda (var value state)
    (cons (createhelper var value (toplayer state)) (restlayer state))))

;; Creates a new binding pair in a layer (part1)
(define createhelper
  (lambda (var value state)
    (cons (append (car state) (cons var '())) (cons (append (cadr state) (cons value '())) '()))))

;; Creates a new binding pair in the top layer using box
(define create-box
  (lambda (var value state)
    (cons (boxhelper var value (toplayer state)) (restlayer state))))

;; Creates a new binding pair using box in a layer
(define boxhelper
  (lambda (var value state)
    (cons (append (car state) (cons var '())) (cons (append (cadr state) (cons (box value) '())) '()))))
    

;; Updates a binding in a state.
(define update
  (lambda (var value state)
    (cond
      [(null? state) (error 'error "Variable has not need declared.")]
      [(eq? '#t (ifexistlayer var (toplayer state))) (cons (updatelayer var value (toplayer state)) (restlayer state))]
      [else (cons (toplayer state) (update var value (restlayer state)))])))

(define updateClassValue
  (lambda (type var value state)
    (cond
      [(null? state) (error 'error "Variable has not need declared.")]
       [(eq? '#t (ifexistlayer type (toplayer state))) (cons (updatelayer type (list (car (lookuplayer type (toplayer state)))
                          (updatelayer var value (cadr (lookuplayer type (toplayer state)))) (caddr (lookuplayer type (toplayer state))))
                                                           (toplayer state)) (cdr state))]
       [else (cons (toplayer state) (update var value (restlayer state)))])))



;; Updates a binding in a layer
(define updatelayer
  (lambda (var value state)
    (cond
      [(eq? (caar state) var) (cons (car state) (cons (cons value (cdadr state)) '()))]
      [else (cons (cons (caar state) (car (updatelayer var value (nofirst state))))
                  (cons (cons (caadr state) (cadr (updatelayer var value (nofirst state)))) '()))])))

;; Updates a binding in a state using box.
(define update-box
  (lambda (var value state)
    (cond
      [(null? state) (error 'error "Variable is not in scope.")]
      [(eq? '#t (ifexistlayer var (toplayer state))) (cons (updatelayer-box var value (toplayer state)) (restlayer state))]
      [else (cons (toplayer state) (update-box var value (restlayer state)))])))



;; Updates a binding in a layer
(define updatelayer-box
  (lambda (var value state)
    (cond
      [(eq? (caar state) var) (begin (set-box! (caadr state) value)
                                     state)]
       ;(cons (car state) (cons (cons value (cdadr state)) '()))]
      [else (cons (cons (caar state) (car (updatelayer-box var value (nofirst state))))
                  (cons (cons (caadr state) (cadr (updatelayer-box var value (nofirst state)))) '()))])))


;; Looks up a binding in a state and returns the value
(define lookup
  (lambda (var state)
    (cond
      [(null? state)  (error 'error "Variable has not need declared.")]
      [(eq? '#t (ifexistlayer var (toplayer state))) (lookuplayer var (toplayer state))]
      [else (lookup var (restlayer state))])))

;; Looks up a binding to a box in a state and returns the value
(define lookup-box
  (lambda (var state)
    (cond
      [(null? state)  (error var "Variable has not need declared.")]
      [(eq? '#t (ifexistlayer var (toplayer state))) (lookuplayer-box var (toplayer state))]
      [else (lookup-box var (restlayer state))])))

; Looks up a binding to a box in a layer and returns the value
(define lookuplayer-box
  (lambda (var layer)
    (cond
      [(eq? (caar layer) var) (if (eq? (unbox (caadr layer)) 'und)
                                  (error 'error "Variable has not been assigned.")
                                  (unbox (caadr layer)))]
      [else (lookuplayer-box var (nofirst layer))])))

; Looks up a binding in a layer and returns the value
(define lookuplayer
  (lambda (var layer)
    (cond
      [(eq? (caar layer) var) (if (eq? (caadr layer) 'und)
                                  (error 'error "Variable has not been assigned.")
                                  (caadr layer))]
      [else (lookuplayer var (nofirst layer))])))

;; Checks if a variable is in a state
(define ifexist
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(eq? '#f (ifexistlayer var (toplayer state))) (ifexist var (restlayer state))]
      [else '#t])))


;; Checks if a variable is in a layer
(define ifexistlayer
  (lambda (var layer)
    (cond
      [(null? (car layer)) #f]
      [(eq? (caar layer) var) #t]
      [else (ifexistlayer var (nofirst layer))])))

;; Helper function: Input takes a state and returns a state without the first variable and first value
(define nofirst
  (lambda (state)
    (cons (cdar state) (cons (cdadr state) '()))))

;; Helper function: removes a varialbe from a layer
;; (define remove


;; Part4

;; insClosure creates an instance closure that contains:
;; 1. Instancce's class
;; 2. List of instance field values.
(define insclosure
  (lambda (type fields)
    (cons type (list fields))))

;; Access the insClosure
(define trueType car)
(define fieldValues cadr)

;; Searches the parent class closure in the state
(define searchParent
  (lambda (name state)
    (cond
      [(null? state) (error 'error "No such parent class found.")]
      [(eq? (caar state) name) (cadar state)]
      [else (searchParent name (cdr state))])))

;; Helper function: Input takes a variable and a state and remove the variable in the state
(define remove
  (lambda (var layer)
    (cond
      [(null? (car layer)) (error 'error "Variable has not been assigned.")]
      [(eq? (caar layer) var) (nofirst layer)]
      [else (cons (append (caar layer) (car (remove layer))) (append (cadr layer) (cdr (remove layer))))]))) 