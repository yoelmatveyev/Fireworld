;;; A generator of Golly rule tables for cyclical rules with 2 to 8 fully symmetrical states

;; Copyright by Yoel Matveyev, 2020
;; The GNU General Public License v3.0

(defun substr (s &optional (n 1))
  (subseq s 0 (- (length s) n)))

(defun arefp (s n)
   (if (<= (length s) n) nil (aref s n)))

(defun concat-lists (seq1 seq2)
   (if (null seq1)
       seq2
       (cons (car seq1)
	     (concat-lists (cdr seq1) seq2))))

(defun add-zeros (l n)
  (if (< (length l) n)
      (concat-lists l
		    (make-list (- n (length l)) :initial-element 0))
      l))

(defun non-zero (l)
  (let ((n 0))
    (loop for x from 1 to (length l) do
         (if (> (nth (1- x) l) 0)
             (setf n x)))
    n))

(defun cycle-digit (c n m)
  (if (zerop c) 0
      (1+ (mod (+ -1 c n) m))))

(defun cycle-list (l n m)
  (loop for x in l collect (cycle-digit x n m)))

;; List all restricted patritions

(defun partitions (n row col)
  (let ((result (list)))
    (labels ((%partitions (n row col acc)
               (cond ((zerop n) (push (reverse acc) result))
                     ((zerop col))
                     (t (loop for i from (min row n) downto
			     (floor (1- (+ n col)) col)
                           do
			     (%partitions (- n i) i (1- col)
					  (cons i acc)))))))
      (%partitions n row col '())
      (nreverse result))))

;; All permutations, including identical ones

(defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations
	     (append (rest lst) (list (first lst)))
	     (rest remain))))))

;; Test lists for cyclical equality

(defun cequal (l1 l2)
  (let ((eq nil))
    (loop for m from 1 to (length l1) do
         (if (equal l1 l2) (setf eq t))
         (setf l1 (append (cdr l1) (list (car l1)))))
    eq))

;; Find the shortest non-zero cyclically equal permutation

(defun shcycle (l)
  (let ((nl l) (nz (non-zero l)) (c (car l)))
    (loop for x from 1 to (length l) do
         (setf l (append (cdr l) (list (car l))))
         (if (< (non-zero l) nz)
             (setf nl l
                   nz (non-zero l) c (car l)))
         (if (and (= (non-zero l) nz) (< (car l) c))
             (setf nl l)))
    nl))

;; Generate all cyclically different permutations

(defun permutation (l)
  (mapcar #'shcycle
	  (remove-duplicates
	   (remove-if
	    (lambda(x)
	      (or (zerop (car x))
		  (= (length (remove-duplicates x)) 1)))
	    (remove-duplicates (all-permutations l)
			       :test #'equal))
	   :test #'cequal)))

;; Generate all different permutations
 
(defun permutation2 (l)
  (mapcar (lambda (x)
	    (subseq x 0 (non-zero x)))
	  (remove-duplicates (all-permutations l)
			     :test #'equal)))

;; Generate all possible birth or survival rules for a given number of states

(defun gen-rules (n &key (b t) (zero-cut t))
  (let ((r
	 (reduce #'concat-lists
		 (mapcar (lambda (x)
			   (if b (permutation x)
			       (permutation2 x)))
			 (mapcar (lambda (x) (add-zeros x n))
				 (reduce #'concat-lists
					 (loop for x from 1 to 8
					    collect  (partitions x 8 n))
					 :from-end t))))))
    (setf  r (if b r (cons (make-list n :initial-element 0) r))
	   r (if zero-cut
		 (mapcar (lambda(x)
			   (setf x (subseq x 0 (non-zero x)))
			   (if x x '(0)))
			 r)
		 r))))

(defun cycle-rule (s n)
   (let ((l (subseq s 0 (1- (length s))))
	 (tr (subseq s (1- (length s)))))
     (if (< (length l) n)
	 (setf l (concatenate 'string l (make-string (- n (length l))
						     :initial-element #\0))))
     (loop for x from 1 to n collect
	  (concatenate 'string (setf l (concatenate 'string
			       (subseq l 1)
			       (subseq l 0 1)))
		       tr))))

(defun decode-rule (l)
  (reduce #'concat-lists (loop for x from 1 to (length l) collect
       (make-list (nth (1- x) l) :initial-element x))))

;; Split a rule string into rules

(defun cut-string (s &optional (l nil))
  (let (sub n p)
    (if (equal s "") (nreverse l)
	(progn (loop for x from 0 to (1- (length s))
		  while (digit-char-p (aref s x))
		  do (setf n x))
	       (if (eq (arefp s (+ 2 n)) #\r)
		   (setf p 3)
		   (setf p 2)) 
	       (push (subseq s 0 (+ n p)) l)
	       (setf sub (subseq s (+ n p)))
	       (cut-string sub l)))))

;; Print one rule (auxiliary function)

 (defun print-rule (s n &key (stream t) (b t))
  (let (r repeat (l (substr s))
	  (tr (coerce (subseq s (1- (length s))) 'character)))
    (when (eq tr #\n)
      (if b (return-from print-rule nil)
	  (return-from print-rule
	    (format stream "~{~a~^,~}~%"
		    (append (cons '|i|
				  (add-zeros (subseq '(|a| |b| |c| |d| |e| |f| |g| |h|)
						     0
						     (digit-char-p (aref l 0)))
					     8))
			     (list '|i|))))))
    (when (eq tr #\r)
      (if b (return-from print-rule nil)
	  (return-from print-rule
	    (progn
	      (setf
	       tr (coerce (subseq s (- (length s) 2) (1- (length s))) 'character)
	       repeat (and (= (length (remove-duplicates (substr l))) 1) (= (length l) (1+ n))))
	    (if (or (eq tr #\a) (eq tr #\z))
		(if b nil
		    (if repeat
			(print-rule l n :stream stream :b b)
		    (mapcar
		     (lambda (x)
		       (format stream "~{~a~^,~}~%" x))
		     (loop for x from 1 to n collect
			  (append (cons '|i|
					(add-zeros
					 (cycle-list
					  (decode-rule
					   (mapcar #'digit-char-p
						   (coerce (substr s 2) 'list)))
					  (1+ (mod x n)) n)
					8))
				  (list (if (eq tr #\z) 0 '|i|)))))))
		(mapcar
		 (lambda (x) (print-rule x n :stream stream :b nil))
		 (cycle-rule l n)))))))
    (setf l (mapcar #'digit-char-p (coerce l 'list))
	  tr (case tr
	      (#\a 1)
	      (#\b 2)
	      (#\c 3)
	      (#\d 4)
	      (#\e 5)
	      (#\f 6)
	      (#\g 7)
	      (#\h 8)
	      (#\z 0))
	  repeat (and (= (length (remove-duplicates l)) 1) (= (length l) n)))
    (if repeat
	(if b (setf r nil)
	    (if (or (= tr 1) (= tr 0))
		(setf r (list (append (cons '|i|
					    (add-zeros (decode-rule l) 8))
				      (list (if (= tr 0) 0 '|i|)))))
		(setf repeat nil))))
    (if (and (= (length l) 1) (or (= tr 1) (= tr 0)))
	(setf r (list (append (cons (if b 0 '|i|)
				    (add-zeros
				     (make-list (car l) :initial-element '|i|) 8))
			      (list (if (= tr 0) 0 '|i|)))))
	(unless repeat (setf r (loop for x from 1 to n collect
				    (append (cons (if b 0 (1+ (mod (1+ x) n)))
						  (add-zeros
						   (cycle-list
						    (decode-rule l) (1+ (mod x n)) n)
						   8))
					    (list (if (= tr 0) 0 (1+ (mod (+ x tr) n)))))))))
    (mapcar (lambda (x)
	      (format stream "~{~a~^,~}~%" x))
	    r)))

;; Print a rule table in REPL

(defun print-rule-table  (string &key comment rulename (stream t))
  (let (n b s c)
  (format stream "@RULE ~a~%~%" (if rulename rulename string))
  (if comment (format stream "# ~a~%~%" comment))
  (format stream "# ~a~%~%" string)
  (if (eq (aref string 0) #\T)
      (setf n 3
	    string (subseq string 1)))
  (if (eq (aref string 0) #\D)
      (setf n 2
	    string (subseq string 1)))
  (if (eq (aref string 1) #\S)
      (setf n (digit-char-p (aref string 0))
	    string (subseq string 2)))
  (case n
    (2  (format stream "@COLORS~%~%0 0 0 0~%1 255 0 0~%2 0 0 255~%~%"))
    (3  (format stream "@COLORS~%~%0 0 0 0~%1 255 0 0~%2 0 255 0~%3 0 0 255~%~%")))
  (format stream "@TABLE~%~%n_states:~a~%neighborhood:Moore~%symmetries:permute~%" (1+ n))
  (format stream "var A={~{~a~^,~}}~%var B=A~%var C=A~%var D=A~%var E=A~%var F=A~%var G=A~%var H=A~%var I=A~%~%var a={~{~a~^,~}}~%var b=a~%var b=a~%var c=a~%var d=a~%var e=a~%var f=a~%var g=a~%var h=a~%var i=a~%~%"
	  (loop for x from 0 to n collect x)
	  (loop for x from 1 to n collect x))
  (setf c (search "-" string)
	b (subseq string 0 c)
	s (if c (subseq string (1+ c)) ""))
  (mapcar (lambda (x) (print-rule x n :stream stream)) (cut-string b))
  (mapcar (lambda (x) (print-rule x n :stream stream :b nil)) (cut-string s))
  (format stream "~%I,A,B,C,D,E,F,G,H,0"))
  t)

;; Create and save a rule table in your '~/.golly/Rules/' directory
;; If the rulename is not given, the file will be names as the ruleline 

(defun save-rule-table (string &key path comment rulename)
  (with-open-file
      (stream (concatenate 'string (if path path "~/.golly/Rules/") (if rulename rulename string) ".rule")
	      :direction :output :if-exists :supersede)
    (print-rule-table string :comment comment :rulename rulename :stream stream))
  t)
