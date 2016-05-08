
(in-package :common-lisp-user)


(use-package :frp-signal)

(defparameter signal1 (make-instance 'frp-signal :value 33))


(defparameter signal2 (fmap (lambda (v) (+ 5 v)) signal1))

(defparameter signal3 (fmap (lambda (v) (* 3 v)) signal2))


(set-value signal1 15)

(signal-value signal1)
(signal-value signal2)
(signal-value signal3)
