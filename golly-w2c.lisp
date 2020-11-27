;; A tool for producing 2-color symmetrical generalizations of elementary CA in Golly 

;; Copyright by Yoel Matveyev, 2020
;; The GNU General Public License v3.0

;; Additional 2-color patterns

(defparameter addcodes
  '((0 1 2)
    (0 2 1)
    (1 1 2)
    (1 2 1)
    (2 1 1)))

;; Patterns for Wolfram codes

(defparameter wcodes
  '((0 0 0)
    (0 0 1)
    (0 1 0)
    (0 1 1)
    (1 0 0)
    (1 0 1)
    (1 1 0)
    (1 1 1)))

;; Generate a 2-color table for Wolfram codes

(defun w2gen (n &key (stream t))
  (loop for x from 0 to 7 do
       (unless (zerop (mod (ash n (- x)) 2))
	 (format stream "0,~a,~a,0,0,0,0,0,~a,1~%"
		 (cadr (nth x wcodes))
		 (caddr (nth x wcodes))
		 (car (nth x wcodes)))
	 (format stream "0,~a,~a,0,0,0,0,0,~a,2~%"
		 (* (cadr (nth x wcodes)) 2)
		 (* (car (nth x wcodes)) 2)
		 (* (caddr (nth x wcodes)) 2)))))

;; Generate additional rule lines for color interaction

(defun addgen (n &key (stream t))
  (loop for x from 0 to 4 do
       (unless (zerop (mod n 3))
	 (format stream "0,~a,~a,0,0,0,0,0,~a,~a~%"
		 (cadr (nth x addcodes))
		 (caddr (nth x addcodes))
		 (car (nth x addcodes))
		 (mod n 3))
	 (format stream "0,~a,~a,0,0,0,0,0,~a,~a~%"
		 (mod (ash (cadr (nth x addcodes)) 1) 3)
		 (mod (ash (car (nth x addcodes)) 1) 3)
		 (mod (ash (caddr (nth x addcodes)) 1) 3)
		 (mod (ash n 1) 3)))
       (setf n (floor n 3))))

;; Print a rule table in REPL

(defun print-2w-table  (wcode acode &key (stream t))
  (format stream "@RULE W~a-~a~%~%" wcode acode)
  (format stream "@COLORS~%~%0 0 0 0~%1 255 0 0~%2 0 0 255~%~%")
  (format stream "@TABLE~%~%n_states:3~%neighborhood:Moore~%symmetries:none~%~%")
  (w2gen wcode :stream stream)
  (format stream "~%# 2-color interaction:~%~%")
  (addgen acode :stream stream))

;; Create and save a rule table in your '~/.golly/Rules/' directory

;; Rules are named:

;; W<Wolfram code>-<color interaction code>

(defun save-2w-table (wcode acode)
  (with-open-file
      (stream (concatenate 'string "~/.golly/Rules/"
			   "W" (write-to-string wcode) "-"
			   (write-to-string acode) ".rule")
	      :direction :output :if-exists :supersede)
    (print-2w-table wcode acode :stream stream))
  t)
