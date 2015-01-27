#lang racket
(require srfi/1)

(define (compile-l1 p)
  (define (emit-l1 p)
    (let* ([is (first p)]
           [compiled (map compile is)])
      (fold (lambda (str acc) (format "~a\n ~a" acc str)) "" compiled)))
  ;; a p is a list surrounding a list of i
  (let* ([is (car p)]
         [compiled (map compile-instruction is)])
    (emit-l1 compiled)))

(define num? integer?)

(define (n4? n)
  (and (= (modulo n 4) 0) (num? n)))

(define (label? l)
  (and (symbol? l) 
       (regexp-match #rx"^:[a-zA-Z_][a-zA-Z_0-9]*$" (symbol->string l))))

(define (cx? cx) (member cx '(eax ecx edx ebx)))
(define (x? x) (or (cx? x) (member x '(esi edi ebp esp))))
(define (sx? sx) (equal? sx 'ecx))
(define (aop=? aop) (member aop '(+= -= *= &=)))
(define (sop=? sop) (member sop '(<<= >>=)))
(define (cmp? cmp) (member cmp '(< <= =)))

(define (reg? r) (or (x? r) (sx? r)))
(define (s? s) (or (x? s) (label? s) (num? s)))
(define (u? u) (or (x? u) (label? u)))
(define (t? t) (or (x? t) (num? t)))

(define (mem? cand) 
  (match cand
    [`(mem ,(? x? x) ,(? n4?)) #t]
    [_ #f]))

(define (compile-instruction i)
  (match i
    [(? label? l) (fmt-label i)]
    [(? reg? r) (reg-asm r)]
    [(? number? n) (string-append "$" (number->string n))]
    [`(eax <- (allocate ,(? t? l) ,(? t? r))) (allocate-asm l r)]
    [`(,(? x? x) <- ,(? s? s)) (update-mem-asm x s)]
    [`(,(? x? x) <- ,(? mem? mem)) (read-mem-asm x (cadr mem) (caddr mem))]
    [`(,(? mem? mem) <- ,(? s? s)) (assign-asm s (cadr mem) (caddr mem))]
    [`(,(? x? x) ,(? aop=? aop) ,(? t? t)) (arith-app-asm x aop t)]
    [`(,(? x? x) ,(? sop=? sop) ,(? sx? sx)) (shift x sop sx)]
    [`(,(? x? x) ,(? sop=? sop) ,(? num? num)) (shift sop num)]
    [`(,(? cx? cx) <- ,(? t? l) ,(? cmp? cmp) ,(? t? r)) (save-cmp-asm cx l cmp r)]
    [`(goto ,(? label? label)) (goto-asm label)]
    [`(call ,(? u? u)) (call-asm u)]
    [`(tail-call ,(? u? u)) (tail-call-asm u)]
    ['(return) (return-asm)]
    [`(cjump ,(? t? l) ,(? cmp? cmp) ,(? t? r) ,(? label? t-label) ,(? label? f-label)) (cjump-asm l cmp r t-label f-label)]
    [`(eax <- (print ,(? t? t))) (print-asm t)]
    [`(array-error ,(? t? l) ,(? t? r)) (array-error-asm l r)]))

(define (allocate-asm l r)
  (string-append "pushl " (compile-instruction r) "\n"
                 "pushl " (compile-instruction l) "\n"
                 "call allocate\n"
                 "addl $8, %esp"))

(define (assign-asm src dst off)
  (let* ([new-src (compile-instruction src)]
         [new-dst (compile-instruction dst)]
         [new-off (number->string off)])
    (when (eq? #\_ (string-ref new-src 0))
      (set! new-src (string-append "$" new-src)))
    (string-append "movl " new-src ", " new-off "(" new-dst ")")))

(define (read-mem-asm src dst off)
  (let* ([new-src (compile-instruction src)]
         [new-dst (compile-instruction dst)]
         [new-off (number->string off)])
    (string-append "movl " new-off "(" new-src ")" ", " new-dst)))

(define (update-mem-asm lhs rhs)
  (let* ([new-l (compile-instruction rhs)]
         [new-r (compile-instruction lhs)])
    (when (eq? #\_ (string-ref new-l 0))
      (set! new-l (string-append "$" new-l)))
    (string-append "movl " new-l ", " new-r)))

(define (arith-app-asm lhs op rhs)
  (let* ([new-l (compile-instruction rhs)]
         [new-r (compile-instruction lhs)])
    (match op
      ['+= (string-append "addl " new-l ", " new-r)]
      ['-= (string-append "subl " new-l ", " new-r)]
      ['*= (string-append "imull " new-l ", " new-r)]
      ['&= (string-append "andl " new-l ", " new-r)])))

(define (shift lhs op rhs)
  (let * ([new-l (compile rhs)]
          [new-r (compile lhs)]
          [fmt-str (match op
                   ['<<= "sall "]
                   ['>>= "sarl "])])
    (unless (eq? #\$ (string-ref new-l 0))
      (set! new-l "%cl"))
    (string-append fmt-str new-l ", " new-r)))

(define introduction 
  ".globl _main
   _main:
   pushl   %ebp
   movl    %esp, %ebp\n
   pushl   %ebx
   pushl   %esi
   pushl   %edi
   pushl   %ebp
   movl    %esp, %ebp")

(define conclusion
  "# restore callee-saved registers\n
   popl   %ebp
   popl   %edi
   popl   %esi
   popl   %ebx
   # restore caller's base pointer
   leave
   ret")
                                
(define (target rv)
  (match rv
    [(? num? rliteral) (format "$~a")]
    [(? label? rlabel) (fmt-label rlabel)]
    [(? reg? rreg) (format "*~a" (reg-asm rreg))]))

(define (call-asm u)
  (let ([tmp-label (fmt-label gensym)])
    (format "pushl $~a
             pushl %%ebp
             movl %%esp, %%ebp
             jmp ~a
             ~a:" tmp-label (target u) tmp-label)))

(define (tail-call-asm u)
  (format "movl %%ebp, %%esp\njmp ~a\n" (target u)))

(define (array-error-asm l r)
  (format "pushl ~a
           pushl ~a
           call print_error
           addl $8, %%esp"
           (rval-asm l)
           (rval-asm r)))

(define (print-asm rv)
  (format "pushl ~a
           call print
           addl $4, %%esp"
          (rval-asm rv)))

(define (goto-asm label)
  (format "jmp ~a\n" (fmt-label label)))

(define (fmt-label label)
  (let ([label-string (symbol->string label)])
    (string-set! label-string 0 #\_)
  label-string))

(define (aop-asm aop)
  (match aop
    ['+= "addl"]
    ['-= "subl"]
    ['*= "imul"]
    ['&= "andl"]))

(define (sop-asm sop)
  (match sop
    ['<<= "sal"]
    ['>>= "sar"]))

(define (mem-ref-asm i r)
  (format "~a(~a)" i (reg-asm r)))

(define (reg-asm reg)
  (format "%~a" (symbol->string reg)))

(define (rval-asm rv)
  (cond
    [(num? rv) (format "$~a" rv)]
    [(label? rv) (format "$~a" (fmt-label rv))]
    [(reg? rv) (reg-asm rv)]))

(define (return-asm)
  "movl %ebp,%esp
   popl %ebp
   ret")

(define (cjump-asm l cmp r t-label f-label)
  (define (fmt l r jump t-label f-label)
    (format "cmpl ~a ~a
    ~a ~a
    jmp ~a" r l jump t-label f-label))
  (match cmp
    ['< (fmt r l "jl" (fmt-label t-label) (fmt-label f-label))]
    ['<= (fmt r l "jle" (fmt-label t-label) (fmt-label f-label))]
    ['= (fmt r l "jne" (fmt-label f-label) (fmt-label t-label))]))

(define (save-cmp-asm cx l cmp r)
  (let ([short-reg (reg-asm cx)]
        [cmp-asm (lambda (cmp rev)
                   (match (list cmp rev)
                     ['(< #t) "setge"]
                     ['(< #f) "setl"]
                     ['(<= #t) "setlg"]
                     ['(<= #f) "setle"]
                     [`(= ,_) "sete"]))]
        [cmp-string "cmp ~a, ~a
                     ~a ~a
                     movzx ~a ~a"])
    (cond
      [(and (num? l) (num? r)) (let* ([op (match cmp
                                            ['< <]
                                            ['<= <=]
                                            ['= =])]
                                      [result ((if (op l r) 1 0))])
                                 (assign-asm cx result))]
      [(num? l) (format cmp-string
                        (rval-asm r)
                        (rval-asm l)
                        (cmp-asm cmp #t)
                        short-reg
                        short-reg
                        (reg-asm cx))]
      [#t (format cmp-string
                        (rval-asm r)
                        (rval-asm l)
                        (cmp-asm cmp #f)
                        short-reg
                        short-reg
                        (reg-asm cx))])))

;list containing a list of instructions
(define prog '(((esi <- 89)  ;; size of the array to create
                (eax <- (allocate esi 3))
                (edi <- eax) ;; save this register for the array base pointer 
                (ebx <- edi) ;; - loop index variable is now a pointer into
                (ebx += 12)   ;;  the array; it starts at the 2nd word.
                
                (esi *= 2)    ;; - convert esi so it is the location of 
                (esi += 2)    ;;   the word just past the end of 
                (esi += edi)  ;;   the array
                
                :loop
                (cjump ebx < esi :keep_going :done)
                :keep_going
                
                (edx <- (mem ebx -8))
                (ecx <- (mem ebx -4))
                (edx += ecx)
                (edx -= 1)
                ((mem ebx 0) <- edx)
                
                (ebx += 4)
                
                ;; go back to the loop test
                (goto :loop)
                
                :done
                (eax <- (print edi)))))

(define (run-test)
  (let ([instructions (car prog)])
    (for ([i instructions])
      (displayln i)
      (displayln (compile-instruction i)))))
