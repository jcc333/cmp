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

(define (s? s) (or (x? s) (label? s) (num? s)))
(define (u? u) (or (x? u) (label? u)))
(define (t? t) (or (x? t) (num? t)))

(define (mem? cand) 
  (match cand
    [`(mem ,(? x? x) ,(? n4?)) #t]
    [_ #f]))

(define (compile-instruction i)
  (if (label? i) "label"
      (match i
        [`(eax <- (allocate ,(? t? l) ,(? t? r))) (allocate-asm l r)]
        [`(,(? x? x) <- ,(? s? s)) (assign-asm x s)]
        [`(,(? x? x) <- ,(? mem? mem)) (read-mem-asm x mem)]
        [`(,(? mem? mem) <- ,(? s? s)) (update-mem-asm mem s)]
        [`(,(? x? x) ,(? aop=? aop) ,(? t? t)) (arith-app-asm x aop t)]
        [`(,(? x? x) ,(? sop=? sop) ,(? sx? sx)) (shift-app-sx-asm x sop sx)]
        [`(,(? x? x) ,(? sop=? sop) ,(? num? num)) (shift-app-num-asm sop num)]
        [`(,(? cx? cx) <- ,(? t? l) ,(? cmp? cmp) ,(? t? r)) (save-cmp-asm cx l cmp r)]
        [`(goto ,(? label? label)) (goto-asm label)]
        [`(call ,(? u? u)) (call-asm u)]
        [`(tail-call ,(? u? u)) (tail-call-asm u)]
        ['(return) (return-asm)]
        [`(cjump ,(? t? l) ,(? cmp? cmp) ,(? t? r) ,(? label? t-label) ,(? label? f-label)) (cjump-asm l cmp r t-label f-label)]
        [`(eax <- (print ,(? t? t))) (print-asm t)]
        [`(array-error ,(? t? l) ,(? t? r)) (array-error-asm l r)])))

(define introduction 
  ".globl _main\n
   _main:\n
   pushl   %ebp\n
   movl    %esp, %ebp\n
   pushl   %ebx\n
   pushl   %esi\n
   pushl   %edi\n
   pushl   %ebp\n
   movl    %esp, %ebp\n")

(define conclusion
  "# restore callee-saved registers\n
   popl   %ebp\n
   popl   %edi\n
   popl   %esi\n
   popl   %ebx\n
   # restore caller's base pointer\n
   leave\n
   ret\n")

(define (label-asm label)
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
  (format "~a(~a)" i (register-asm r)))

(define (register-asm reg)
  (format "%~a" (symbol->string reg)))

(define (return-asm)
  "movl %ebp,%esp
   popl %ebp
   ret")

(define (cjump-asm l cmp r t-label f-label)
  (define (formatter l r t-label f-label)
    (format "cmpl ~a ~a
    jl ~a
    jmp ~a" r l t-label f-label))
  (match cmp
    ['< ...]
    ['<= ...]
    ['= ...]))
  #|(cjump eax < ebx :true :false)
  =>
  
  cmpl %ebx, %eax   // note reversal of argument order here.
                        // that's just the way cmpl takes its arguments
      jl _true          // jl for "jump less", rewrite labels to
                        // prefix w/ underscores
      jmp _false|#

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
