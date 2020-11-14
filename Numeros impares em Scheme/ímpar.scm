; author: コマンド


;それについて勉強する
(define-syntax while!
  (syntax-rules ()
    ((while! (conditional) body)
      (call/cc (lambda (_break)
                 (let ((_continue 0))
                   (call/cc (lambda (__continue)
                              (set! _continue __continue)))
                              (if conditional
                                (begin (body _continue _break)
                                       (_continue))
                                (_break))))))))


(define-syntax for!
  (syntax-rules ()
    ((for! () body)
      (while (#t) body))
    ((for! (() () ()) body)
      (for! ()))
    ((for! ((binds ...)) body)
      (for! ((binds ...) #t) body))
    ((for! ((binds ...) conditional))
      (let* (binds ...)
        (while! (conditional) body)))
    ((for! ((binds ...) conditional (validates ...)) body)
      (let* (binds ...)
        (letrec ((_validates (lambda () validates ...)))
          (while! (conditional)
            (lambda (continue break)
              (body (lambda () (_validates) (continue))
                    break)
              (_validates))))))))


; 10の間の奇数を印刷してテストします
(for! (((m 0)) (< m 20) ((set! m (+ m 1))))
  (lambda (continue break)
    (if (zero? (modulo m 2))
      (continue))
    (if (>= m 10)
      (break))
    (display m)
    (newline)))
