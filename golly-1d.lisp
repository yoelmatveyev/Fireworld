;; A toolkit for emulating 1D cellular automata with larger neighborhoods in Golly 4.0

;; Copyright by Yoel Matveyev, 2020
;; The GNU General Public License v3.0

; Produce all binary strings of a given length

(defun binary-codes (n)
  (let (s)
    (setf s (concatenate 'string "~" (format nil "~a" n) ",'0B"))
    (loop for x from 0 to (1- (expt 2 n)) collect (format nil s x))))

; Remove mirror reflections from binary strings of a given length 

(defun reflected-binary-codes (n)
  (let ((l1 (binary-codes n))
	l2)
    (loop for x in l1 do
	 (if (or (not (find (reverse x) l2 :test #'equal))
		 (equal x (reverse x)))
	     (push x l2)))
    (reverse l2)))

;; Decode an extended Wolfram code into a rule list

(defun decode-wolfram-to-list (code radius)
  (let* ((n (1+ (ash radius 1)))
	(l1 (binary-codes n))
	l2)
    (loop for x from 0 to (1- (expt 2 n)) do
	 (unless (zerop (mod code 2))
	   (push (nth x l1) l2))
	 (setf code (floor code 2)))
    (reverse l2)))

(defun sort-binary-strings (l)
  (sort l (lambda (x y)
	    (< (parse-integer x :radix 2)
	       (parse-integer y :radix 2)))))

 (defun totalistic-count (s)
	(reduce #'+ (loop for x across s collect (digit-char-p x))))

; Convert a totalistic rule to a weighted rulestring

(defun permute-totalistic (birth-list radius)
  (let ((l (binary-codes (1+ (ash radius 1))))
	l2)
    (loop for x in birth-list do
	 (loop for y in l do
	      (when (= (totalistic-count y) x)
		(push y l2))))
    (sort-binary-strings l2)))

;; Encode a rule list into an extended Wolfram code

(defun encode-list-to-wolfram (list radius)
  (let* ((n (1+ (ash radius 1)))
	 (l (reverse (binary-codes n)))
	 (code 0))
    (loop for x in l do
	 (setf code (ash code 1))
	 (when (find x list :test #'equal)
	   (incf code)))
    code))

;; Enforce reflection to a rule list

(defun make-reflected-list (l)
  (loop for x in l do
       (unless (find (reverse x) l :test #'equal)
	 (push (reverse x) l)))
  l)

;; Check reflectiveness of a given rule

(defun reflected-p (rule radius)
  (let ((f t))
  (when (numberp rule)
    (setf rule (decode-wolfram-to-list rule radius)))
  (loop for x in rule do
       (unless (find (reverse x) rule :test #'equal)
	 (setf f nil)))
  f))

(defparameter weights '(32 8 2 1 4 16 64))

;; Calculate the birth weight based on a binary string

(defun calc-birth (s)
  (let ((weight 0) (l (length s)))
    (when (> l 7)
      (format t "~%Rule too large for Golly 4.0!~%")
      (return-from calc-birth))
    (when (evenp l)
      (format t "~%Error: even number of characters!~%")
      (return-from calc-birth))
    (setf s (ash (parse-integer s :radix 2) (- 3 (floor l 2))))
    (loop for x from 6 downto 0 do
	 (unless (zerop (mod s 2))
	   (setf weight (+ weight (nth x weights))))
	 (setf s (floor s 2)))
    weight))

;; Make a Golly rulestring from an extended Wolfram code or a list of rules

(defparameter b2 "R2,C0,S0-31,B")
(defparameter e2 "NW00000000000802010410000000000000000000000000000000")
(defparameter b3 "R3,C0,S0-127,B")
(defparameter e3 "NW00000000000000000000000000002008020104104000000000000000000000000000000000000000000000000000000000")

(defun make-rulestring (rule radius &key (reflect nil)) 
  (when (> radius 3)
    (format t "~%Radius too large for Golly 4.0!~%")
    (return-from make-rulestring))
  (when (numberp rule)
    (setf rule (decode-wolfram-to-list rule radius)))
  (when reflect
    (setf rule (make-reflected-list rule)))
  (case radius
    (1 (format t "~%W~a~%" (encode-list-to-wolfram rule radius)))
    (2 (format t "~%~a~{~a,~}~a~%" b2 (mapcar #'calc-birth rule) e2))
    (3 (format t "~%~a~{~a,~}~a~%" b3 (mapcar #'calc-birth rule) e3))))

;; Convert a list of totalistic birth counts to a weighted rule string

(defun totalistic-to-rulestring (list radius)
  (make-rulestring (permute-totalistic list radius) radius))
