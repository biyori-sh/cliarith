(in-package :cliarith)

;; only for SBCL
#+sbcl
(defparameter *default-rounding-mode*
  (getf (sb-int:get-floating-point-modes) :ROUNDING-MODE)
  "a keyword of the default rounding mode")
#-sbcl
(error "This package is only for SBCL.")


;; machine epsilon
(defun get-epsilon ()
  "Return a measure of the precision of the floating point type at get-epsilon executed, ((floating-point-type)-negative-epsilon (floating-point-type)-epsilon)."
  (case *read-default-float-format*
    (short-float (list short-float-negative-epsilon short-float-epsilon))
    (single-float (list single-float-negative-epsilon single-float-epsilon))
    (double-float (list double-float-negative-epsilon double-float-epsilon))
    (long-float (list long-float-negative-epsilon long-float-epsilon))
    (t '())))

(defparameter *negative-epsilon* (first (get-epsilon)))
(defparameter *positive-epsilon* (second (get-epsilon)))

(defun set-float-format (floating-point-type)
  "Set *read-default-float-format*, *negative-epsilon* and *positive-epsilon*."
  (when (find floating-point-type
              '(short-float single-float double-float long-float))
    (setf *read-default-float-format* floating-point-type)
    (setf (values *negative-epsilon* *positive-epsilon*)
          (values (first (get-epsilon)) (second (get-epsilon))))
    t))


;; Macro to control rounding-mode
(defun rounding-modep (mode)
  (declare (inline rounding-modep)
           (keyword mode))
  (find mode (list :NEAREST :POSITIVE-INFINITY :NEGATIVE-INFINITY)))

(defmacro round-to (mode-keyword &body body)
  "Evaluate BODY in the rounding mode, :POSITIVE-INFINITY or :NEGATIVE-INFINITY or :NEAREST. After that, the rounding-mode returns to *default-rounding-mode*."
  (when (rounding-modep mode-keyword)
    `(prog2 (sb-int:set-floating-point-modes :ROUNDING-MODE ,mode-keyword)
         (progn ,@body)
       (sb-int:set-floating-point-modes :ROUNDING-MODE *default-rounding-mode*))))

(defmacro round-positive (&body body)
  "Evaluate BODY in the rounding mode: POSITIVE-INFINITY. After that, the rounding-mode returns to *default-rounding-mode*."
  `(prog2 (sb-int:set-floating-point-modes :ROUNDING-MODE :POSITIVE-INFINITY)
       (progn ,@body)
     (sb-int:set-floating-point-modes :ROUNDING-MODE *default-rounding-mode*)))

(defmacro round-negative (&body body)
  "Evaluate BODY in the rounding mode: NEGATIVE-INFINITY. After that, the rounding-mode returns to *default-rounding-mode*."
  `(prog2 (sb-int:set-floating-point-modes :ROUNDING-MODE :NEGATIVE-INFINITY)
       (progn ,@body)
     (sb-int:set-floating-point-modes :ROUNDING-MODE *default-rounding-mode*)))

(defmacro round-nearest (&body body)
  "Evaluate BODY in the rounding mode: NEAREST. After that, the rounding-mode returns to *default-rounding-mode*."
  `(prog2 (sb-int:set-floating-point-modes :ROUNDING-MODE :NEAREST)
       (progn ,@body)
     (sb-int:set-floating-point-modes :ROUNDING-MODE *default-rounding-mode*)))


;; Type
(deftype positive-real ()
  '(real (0) *))

(deftype not-negative-real ()
  '(real 0 *))

(deftype not-positive-real ()
  '(real * 0))

(deftype negative-real ()
  '(real * (0)))


;; Interval
(defstruct ([] (:constructor [] (low high)))
  "Structure type for the interval arithmetic."
  (low 0.0 :type real)
  (high 0.0 :type real))


;; Predicates
(defmethod pointp ((i []))
  (= ([]-high i) ([]-low i)))

(defmethod valid[]p ((i []))
  "Is this a valid interval?"
  (<= ([]-low i) ([]-high i)))

(defmethod element[]p ((point number) (i []))
  (and (valid[]p i)
       (<= ([]-low i) point) (<= point ([]-high i))))

(defmethod sub[]p ((subi []) (superi []))
  (and (valid[]p subi) (valid[]p superi)
       (<= ([]-low superi) ([]-low subi)) (<= ([]-high subi) ([]-high superi))))

(defmethod zero[]p ((i []))
  (and (valid[]p i) (element[]p 0 i)))

(defmethod positive[]p ((i []))
  (and (valid[]p i) (< 0 ([]-low i))))

(defmethod negative[]p ((i []))
  (and (valid[]p i) (< ([]-high i) 0)))

(defmethod =[] ((i1 []) (i2 []))
  (and (= ([]-low i1) ([]-low i2))
       (= ([]-high i1) ([]-high i2))))

(defmethod intersection[]p ((i1 []) (i2 []))
  (or (element[]p ([]-low i1) i2) (element[]p ([]-high i1) i2)))


;; etc
(defun valid[] (low high)
  "Make a valid interval."
  (let ((tmpi ([] low high)))
    (if (valid[]p tmpi) tmpi)))


;; Unary operators
(defmethod diameter ((i []))
  (- ([]-high i) ([]-low i)))

(defmethod radius ((i []))
  (/ (diameter i) 2.0))

(defmethod center ((i []))
  (/ (- ([]-high i) ([]-low i)) 2.0))

(defmethod ->[] ((p real))
  ([] p p))

(defmethod ->[] ((p []))
  p)

(defmethod extend[] ((p real))
  ([] (round-negative (- p *negative-epsilon*))
      (round-positive (+ p *positive-epsilon*))))

(defmethod extend[] ((i []))
  ([] (round-negative (- ([]-low i) *negative-epsilon*))
      (round-positive (+ ([]-high i) *positive-epsilon*))))

(defmethod sign-flip[] ((i []))
  ([] (round-negative (- ([]-high i)))
      (round-positive (- ([]-low i)))))

(defmethod sign-flip[] ((i real))
  ([] (round-negative (- i))
      (round-positive (- i))))

(defmethod intersection[] ((i1 []) (i2 []))
  (if (intersection[]p i1 i2)
      ([] (max ([]-low i1) ([]-low i2))
          (min ([]-high i1) ([]-high i2)))
      nil))


;; Absolute value
(defmethod abs[]-min ((i []))
  (if (zero[]p i) 0
      (min (abs ([]-low i)) (abs ([]-high i)))))

(defmethod abs[]-max ((i []))
  (max (abs ([]-low i)) (abs ([]-high i))))

(defmethod abs[] ((i []))
  ([] (round-negative (abs[]-min i))
      (round-positive (abs[]-max i))))

(defmethod abs[] ((i real))
  (->[] (abs i)))


;; Binary operators
;; Addittion
(defmethod +[]m ((i1 []) (i2 []))
  ([] (round-negative (+ ([]-low i1) ([]-low i2)))
      (round-positive (+ ([]-high i1) ([]-high i2)))))

(defmethod +[]m ((i1 real) (i2 []))
  ([] (round-negative (+ i1 ([]-low i2)))
      (round-positive (+ i1 ([]-high i2)))))

(defmethod +[]m ((i1 []) (i2 real))
  (+[]m i2 i1))

(defmethod +[]m ((i1 real) (i2 real))
  ([] (round-negative (+ i1 i2))
      (round-positive (+ i1 i2))))

(defmacro incf[] (i-place &optional (increment 1))
  `(setf ,i-place (+[]m ,i-place ,increment)))

(defmacro incf[]id (i-place &optional (increment 1))
  (let ((tmp[] (gensym)))
    `(let ((,tmp[] (+[]m ,i-place ,increment)))
       (setf ([]-low ,i-place) ([]-low ,tmp[])
             ([]-high ,i-place) ([]-high ,tmp[]))
       ,i-place)))

(defun +[] (&rest rest)
  (let ((result[] (->[] 0)))
    (declare (type [] result[]))
    (dolist (i rest result[])
      (setf result[] (+[]m result[] i)))))

;; Subtraction
(defmethod -[]m ((i1 []) (i2 []))
  (+[]m i1 (sign-flip[] i2)))

;; (defmethod -[]m ((i1 []) (i2 []))
;;   ([] (round-negative (- ([]-low i1) ([]-high i2)))
;;       (round-positive (- ([]-high i1) ([]-low i2)))))

(defmethod -[]m ((i1 []) (i2 real))
  (+[]m i1 (sign-flip[] i2)))

;; (defmethod -[]m ((i1 []) (i2 real))
;;   ([] (round-negative (- ([]-low i1) i2))
;;       (round-positive (- ([]-high i1) i2))))

(defmethod -[]m ((i1 real) (i2 []))
  (+[]m (->[] i1) (sign-flip[] i2)))

;; (defmethod -[]m ((i1 real) (i2 []))
;;   ([] (round-negative (- i1 ([]-high i2)))
;;       (round-positive (- i1 ([]-low i2)))))

(defmethod -[]m ((i1 real) (i2 real))
  ([] (round-negative (- i1 i2))
      (round-positive (- i1 i2))))

;; (defmethod -[]m ((i1 real) (i2 real))
;;   (+[]m (->[] i1) (->[] (- i2))))

(defmacro decf[] (i-place &optional (decrement 1))
  `(setf ,i-place (-[]m ,i-place ,decrement)))

(defmacro decf[]id (i-place &optional (decrement 1))
  (let ((tmp[] (gensym)))
    `(let ((,tmp[] (-[]m ,i-place ,decrement)))
       (setf ([]-low ,i-place) ([]-low ,tmp[])
             ([]-high ,i-place) ([]-high ,tmp[]))
       ,i-place)))

(defun -[] (i &rest rest)
  (if rest
      (let ((result[] (->[] i)))
        (declare (type [] result[]))
        (dolist (ti rest result[])
          (setf result[] (-[]m result[] ti))))
      (sign-flip[] i)))

;; Multiplication
;; (defmethod *[]m ((i1 []) (i2 []))
;;   ([] (round-negative
;;         (cond ((and (positive[]p i1) (positive[]p i2))
;;                (* ([]-low i1) ([]-low i2)))
;;               ((and (negative[]p i1) (negative[]p i2))
;;                (* ([]-high i1) ([]-high i2)))
;;               (t (min (* ([]-low i1) ([]-high i2))
;;                       (* ([]-high i1) ([]-low i2))))))
;;       (round-positive
;;         (cond ((and (positive[]p i1) (negative[]p i2))
;;                (* ([]-low i1) ([]-high i2)))
;;               ((and (negative[]p i1) (positive[]p i2))
;;                (* ([]-high i1) ([]-low i2)))
;;               (t (max (* ([]-high i1) ([]-high i2))
;;                       (* ([]-low i1) ([]-low i2))))))))

(defmethod *[]m ((i1 []) (i2 []))
  ([] (round-negative
        (min (* ([]-high i1) ([]-high i2))
             (* ([]-low i1) ([]-high i2))
             (* ([]-high i1) ([]-low i2))
             (* ([]-low i1) ([]-low i2))))
      (round-positive
        (max (* ([]-high i1) ([]-high i2))
             (* ([]-low i1) ([]-high i2))
             (* ([]-high i1) ([]-low i2))
             (* ([]-low i1) ([]-low i2))))))

(defmethod *[]m ((i1 real) (i2 []))
  ([] (round-negative
        (min (* i1 ([]-high i2))
             (* i1 ([]-low i2))))
      (round-positive
        (max (* i1 ([]-high i2))
             (* i1 ([]-low i2))))))


(defmethod *[]m ((i1 []) (i2 real))
  ([] (round-negative
        (min (* ([]-high i1) i2)
             (* ([]-low i1) i2)))
      (round-positive
        (max (* ([]-high i1) i2)
             (* ([]-low i1) i2)))))

(defmethod *[]m ((i1 real) (i2 real))
  ([] (round-negative (* i1 i2))
      (round-positive (* i1 i2))))

(defmacro mulf[] (i-place &optional (multiplier 2))
  `(setf ,i-place (*[]m ,i-place ,multiplier)))

(defun *[] (&rest rest)
  (let ((result[] (->[] 1)))
    (declare (type [] result[]))
    (dolist (i rest result[])
      (setf result[] (*[]m result[] i)))))


;; Division
(defmethod /[]m ((i1 []) (i2 []))
  (if (zero[]p i2)
      (error "Not defined for the divisor including zero: ~a~%" i2)
      (*[]m i1 ([] (/ 1.0 ([]-high i2)) (/ 1.0 ([]-low i2))))))

(defmethod /[]m ((i1 real) (i2 []))
  (if (zero[]p i2)
      (error "Not defined for the divisor including zero: ~a~%" i2)
      (*[]m (->[] i1) ([] (/ 1.0 ([]-high i2)) (/ 1.0 ([]-low i2))))))

(defmethod /[]m ((i1 []) (i2 real))
  (*[]m i1 (->[] (/ 1.0 i2))))

(defmethod /[]m ((i1 real) (i2 real))
  (*[]m (->[] i1) (->[] (/ 1.0 i2))))

(defmacro divf[] (i-place &optional (divisor 2))
  `(setf ,i-place (/[]m ,i-place ,divisor)))

(defun /[] (i &rest rest)
  (if rest
      (let ((result[] (->[] i)))
        (declare (type [] result[]))
        (dolist (ti rest result[])
          (setf result[] (/[]m result[] ti))))
      (/[]m 1 i)))


;; Square root
(defmethod sqrt[] ((i []))
  ([] (round-negative (sqrt ([]-low i)))
      (round-positive (sqrt ([]-high i)))))

(defmethod sqrt[] ((i real))
  ([] (round-negative (sqrt i))
      (round-positive (sqrt i))))


;; Exponential(?)
(defmethod exp[] ((i []))
  ([] (round-negative (exp ([]-low i)))
      (round-positive (exp ([]-high i)))))

(defmethod exp[] ((i real))
  ([] (round-negative (exp i))
      (round-positive (exp i))))


;; Power(?)
(defmethod expt[] ((i []) (power real))
  ([] (round-negative (expt ([]-low i) power))
      (round-positive (expt ([]-high i) power))))

(defmethod expt[] ((i real) (power real))
  ([] (round-negative (expt i power))
      (round-positive (expt i power))))
