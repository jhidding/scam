(library (scam quaternions)
  
  (export make-quat quat? quat-v quat-s
          rotation-quat scalar-quat quat-mul
          quat-conj quat-sqr quat-scale quat-inv
          quat-conjugation)

  (import (rnrs (6))

          (scam lib)
          (scam vectors))

  ;;;======================================================
  ;;; quaternion
  ;;;------------------------------------------------------
  (define-record-type quat
    (fields S V))
  
  (define quat-s (lambda (q) (quat-S q)))
  (define quat-v (lambda (q) (quat-V q)))
  
  (define rotation-quat
    (lambda (u theta)
      (make-quat (cos (/ theta 2))
                 (a-scale (sin (/ theta 2)) u))))
  
  (define scalar-quat
    (lambda (s) (make-quat s (make-a-vector 0 0 0))))
  
  ;;;======================================================
  ;;; multiplication
  ;;;------------------------------------------------------
  (define quat-mul2
    (lambda (q2 q1)
      (make-quat 
        (-     (*     (quat-s q1) (quat-s q2))
               (a-dot (quat-v q1) (quat-v q2)))
  
        (a-add (a-scale (quat-s q1) (quat-v q2))
               (a-scale (quat-s q2) (quat-v q1))
               (a-cross (quat-v q1) (quat-v q2))))))
  
  (define quat-mul
    (lambda qs
      (cond
        ((null? qs) (scalar-quat 1))
        ((null? (cdr qs)) (car qs))
        ((null? (cddr qs)) (apply quat-mul2 qs))
        (else (reduce quat-mul2 qs)))))
  
  ;;;======================================================
  ;;; other operations
  ;;;------------------------------------------------------
  (define quat-conj
    (comp make-quat <- (juxt quat-s 
                             (comp a-neg quat-v))))
  
  (define quat-sqr
    (comp +         <- (juxt (comp sqr quat-s) 
                             (comp a-sqr quat-v))))
  
  (define quat-scale
    (lambda (s q)
      (make-quat (* s (quat-s q))
		 (a-scale s (quat-v q)))))
  
  (define quat-inv
    (comp quat-scale <- (juxt (comp ($ / 1) quat-sqr)
                              quat-conj)))
  
  ;;;======================================================
  ;;; vector conjugation
  ;;;------------------------------------------------------
  (define quat-conjugation
    (lambda (q v)
      (quat-v (quat-mul q (make-quat 0.0 v) 
                        (quat-conj q)))))
)

