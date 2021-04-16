#lang racket

(provide vim%)

(define vim%
  (class object%
         (super-new)
         (init s)
         (define m-str s)
         (define m-pos 0)
         (define m-find-char #f)

         (define (find str c [pos 0])
           (let loop ([pos pos])
             (cond
               [(>= pos (string-length str)) #f]
               [(eq? (string-ref str pos) c) pos]
               [else (loop (add1 pos))])))

         (define/public (f char #:update [update? #t])
                        (set! m-find-char char)
                        (let ([new-pos (find m-str char (add1 m-pos))])
                          (and new-pos
                               (not (eq? new-pos m-pos))
                               update?
                               (begin
                                 (set! m-pos new-pos)
                                 new-pos))))

         (define/public (semicolon)
                        (f m-find-char))

         (define/public (t char #:update [update? #t])
                        (set! m-find-char char)
                        (and (< m-pos (sub1 (string-length m-str)))
                             (let ([new-pos (find m-str char (add1 m-pos))])
                               (and new-pos (>= (sub1 new-pos) 0)
                                    (begin
                                      (when update? (set! m-pos (sub1 new-pos)))
                                      (sub1 new-pos))))))
#;(define/public (i str)
                        (let ([snip-length (string-length str)])
                          (set! m-str
                            (string-append
                              (substring m-str 0 m-pos)
                              str
                              (substring m-str m-pos)))
                          (set! m-pos
                            (+ m-pos (sub1 snip-length)))))

         (define/public (D)
                        (set! m-str (substring m-str 0 m-pos))
                        (set! m-pos (sub1 m-pos)))
;123456
       
      
         (define/public (dt char)
                        (let ([pos (t char #:update #f)])
                          (and pos
                               (not (eq? pos m-pos))
                               (begin
                                 (set! m-str
                                   (string-append
                                     (substring m-str 0 m-pos)
                                     (substring m-str (add1 pos) (string-length m-str))))
                                 #t))))

#;(define/public (ct char str))
#;(define/public (cf char str))

         #;(define/public (df char))

         (define/public (~)
                        (set! m-str
                          (string-append
                            (substring m-str 0 m-pos)
                            (string-upcase (substring m-str m-pos (add1 m-pos)))
                            (substring m-str (add1 m-pos))))
                        (l))

         (define/public (l)
                        (when (< m-pos (sub1 (string-length m-str)))
                          (set! m-pos (add1 m-pos))))

    (define (insert/append str #:insert? insert?)
insert:
0 pos / str / 
append:
0 pos+1 / str / 

append:
afterbefore
    4
aftertestbefore
        8

insert:
afterbefore
     5
aftertestbefore
        8
   

      (set! m-str
        (string-append
          (substring m-str 0 ((cond [insert? identity] [else add1]) m-pos))
          str
          (substring m-str (add1 m-pos) (string-length m-str))))
      (set! m-pos ((if insert? sub1 identity) (+ m-pos (string-length str)))))

    (define/public (a str)
     (insert/append str #:insert? #f))

    (define/public (i str)
     (insert/append str #:insert? #t))

#;(define/public (a str)
                   (set! m-str
                     (string-append
                       (substring m-str 0 (add1 m-pos))
                       str
                       (substring m-str (add1 m-pos) (string-length m-str))))
                   (set! m-pos (+ m-pos (string-length str))))


         (define/public (h)
                        (unless (zero? m-pos)
                          (set! m-pos (sub1 m-pos))))
         (define/public ($)
                        (set! m-pos (sub1 (string-length m-str))))

         (define/public (A str)
                        (set! m-str (string-append m-str str))
                        (set! m-pos (sub1 (string-length m-str))))

         (define/public (C str)
                        (D)
                        (A str))

         #;(define/public (R str)
         )
  ; TODO: e, E, w, W, R, x, F, T, ;

  (define/public (get-str) m-str)
  (define/public (get-pos) m-pos)))

(module+ test
         (require rackunit)
         (define (vimobject init-string) (new vim% [s init-string]))
         (define (check-vimobject obj [str #f] [pos #f])
           (when str (check-equal? (send obj get-str) str))
           (when pos (check-equal? (send obj get-pos) pos)))

         (test-case "vim command: l"
                    (define v (vimobject "123"))
                    (send v l)
                    (check-vimobject v #f 1)
                    (send v l)
                    (check-vimobject v #f 2)
                    (send v l)
                    (check-vimobject v #f 2))

         (test-case "vim command: $"
                    (define v (vimobject "123"))
                    (send v $)
                    (check-equal? (send v get-pos) 2))

         (test-case "vim command: h"
                    (define v (vimobject "123"))
                    (send v l)
                    (check-vimobject v #f 1)
                    (send v h)
                    (check-vimobject v #f 0)
                    (send v h)
                    (check-vimobject v #f 0))

         (test-case "vim command: A"
                    (define v (vimobject "123"))
                    (send v A "456")
                    (check-vimobject v "123456" 5))

         (test-case "vim command: C"
                    (define v (vimobject "123"))
                    (send v l)
                    (send v C "456")
                    (check-vimobject v "1456" 3))

         (test-case "vim command: f"
                    (define v (vimobject "1_345678_0"))
                    (send v f #\5)
                    (check-vimobject v "1_345678_0" 4)
                    (send v f #\5)
                    (check-vimobject v "1_345678_0" 4)
                    (send v f #\_)
                    (check-vimobject v "1_345678_0" 8)
                    (send v f #\5)
                    (check-vimobject v "1_345678_0" 8)
                    (send v $)
                    (send v f #\0)
                    (check-vimobject v "1_345678_0" 9)
                    (check-false (send v f #\_))
                    (check-vimobject v "1_345678_0" 9))

         (test-case "vim command: f (no update)"
                    (define v (vimobject "1_345678_0"))
                    (check-false (send v f #:update #f #\_))
                    (check-equal? (send v get-pos) 0))

         (test-case "vim command: t"
                    (define v (vimobject "1_345678_0"))
                    (check-equal? (send v t #\5) 3)
                    (check-vimobject v "1_345678_0" 3)
                    (check-equal? (send v t #\5) 3)
                    (check-vimobject v "1_345678_0" 3)
                    (send v l)
                    (check-equal? (send v t #\_) 7)
                    (check-vimobject v "1_345678_0" 7)
                    (send v $)
                    (check-false (send v t #\x))
                    (check-vimobject v "1_345678_0" 9))

         (test-case "f, t bug"
                    (define v (vimobject "12345"))
                    (send v f #\5)
                    (check-vimobject v #f 4)
                    (send v t #\5)
                    (check-vimobject v #f 4))

         (test-case "vim command: t (no update)"
                    (define v (vimobject "1_345678_0"))
                    (check-equal? (send v t #:update #f #\5) 3)
                    (check-vimobject v "1_345678_0" 0)
                    (send v $)
                    (check-false (send v t #:update #f #\x))
                    (check-vimobject v "1_345678_0" 9))

         (test-case "vim command: ; repeated twice, shall find twice"
                    (define v (vimobject "1_345678_0"))
                    (send v f #\_)
                    (check-vimobject v #f 1)
                    (send v semicolon)
                    (check-vimobject v #f 8))

         (test-case "vim command: ; repeated twice, shall find once"
                    (define v (vimobject "1_345678x0"))
                    (send v f #\_)
                    (check-vimobject v #f 1)
                    (send v semicolon)
                    (check-vimobject v #f 1))

         (test-case "vim command: dt (1)"
          (define v (vimobject "1234567890"))
          (check-true (send v dt #\5))
          (check-vimobject v "567890" 0))

         (test-case "vim command: dt (2)"
          (define v (vimobject "1234567890"))
          (send v f #\5)
          (check-vimobject v #f 4)
          (check-false (send v dt #\5))
          (check-vimobject v "1234567890" 4))

         (test-case "vim command: a"
          (define v (vimobject "1234567890"))
          (send v f #\5)
          (check-vimobject v #f 4)
          (send v a "hello")
          (check-vimobject v "12345hello67890" 9)
          (send v $)
          (send v a "there")
          (check-vimobject v "12345hello67890there" 19))

         #;(test-case "vim command: t"
         )

(test-case "complex test 1"
           (define v (vimobject "m_parametersDow_in"))
           (send v l)
           (check-vimobject v #f 1)
           (send v l)
           (check-vimobject v #f 2)
           (send v i "data")
           (check-vimobject v "m_dataparametersDow_in" 5)
           (send v l)
           (check-vimobject v #f 6)
           (send v ~)
           (check-vimobject v "m_dataParametersDow_in" 7)
           (send v f #\_)
           (check-vimobject v #f 19)
           (send v D)
           (check-vimobject v "m_dataParametersDow" 18))


#;(define v (new vim% [s "m_parametersDow_in"]))
#;(send* v
(l)
(l)
(i "data")
(l)
(~)
(f #\_)
(D))
#;(check-equal? (send v get-str) "m_dataParametersDow"))
