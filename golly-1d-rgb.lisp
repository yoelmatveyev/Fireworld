;; A toolkit for emulating cyclical multistate 1D CA in Golly 

;; Copyright by Yoel Matveyev, 2020
;; The GNU General Public License v3.0

;; Lists of distinct pattern codes for birth/survival rules

(defparameter rb-b '("01" "11"))
(defparameter rb-s '("00" "01" "02" "11" "12" "22"))
(defparameter rgb-b '("01" "11" "12"))
(defparameter rgb-s '("00" "01" "02" "03" "11" "12" "13" "22" "23" "33"))

(defun shift-color (color color-number &optional (shift 1))
  (if (zerop color) 0
      (1+ (mod (+ color shift -1) color-number))))

(defun code-to-rules (string c birth &key (stream t))
  (let ((l
	 (if birth
	     (if (= c 2) rb-b rgb-b)
	     (if (= c 2) rb-s rgb-s)))
	(stl (1- (length string))))
    (setf string (reverse string))
    (loop for x from 0 to stl do
	 (unless (eq #\0 (elt string x))
	   (loop for y from 0 to (1- c) do
		(format stream "~%0,~a,~a,0,0,0,0,0,~a,~a"
			(if birth 0 (shift-color 1 c y))
			(shift-color (digit-char-p (aref (nth x l) 1)) c y)
			(shift-color (digit-char-p (aref (nth x l) 0)) c y)
			(shift-color (digit-char-p (elt string x)) c y)))))))

;; Print a rule table in REPL

(defun print-1d-rule-table  (string &key comment rulename (stream t))
  (let (dash b s c)
  (format stream "@RULE ~a~%~%" (if rulename rulename string))
  (if comment (format stream "# ~a~%~%" comment))
  (when (eq (search "1DRB" string) 0)
    (setf c 2
	  string (subseq string 4)))
  (when (eq (search "1DRGB" string) 0)
    (setf c 3
	  string (subseq string 5)))
  (unless c
    (format t "~%Malformed rule code!~%")
    (return-from print-1d-rule-table))
  (setf dash (search "-" string)
	b (subseq string 0 dash)
	s (if dash (subseq string (1+ dash)) ""))
  (case c
    (2  (format stream "@COLORS~%~%0 0 0 0~%1 255 0 0~%2 0 0 255~%~%"))
    (3  (format stream "@COLORS~%~%0 0 0 0~%1 255 0 0~%2 0 255 0~%3 0 0 255~%~%")))
  (format stream "@TABLE~%~%n_states:~a~%neighborhood:Moore~%symmetries:reflect_horizontal~%" (1+ c))
  (format stream "~%# Birth:~%")
  (code-to-rules b c t :stream stream)
  (format stream "~%~%# Survival/Mutation:~%")
  (code-to-rules s c nil :stream stream))
  t)

;; Create and save a rule table in your '~/.golly/Rules/' directory
;; If the rulename is not given, the file will be named as the ruleline 

(defun save-1d-rule-table (string &key comment rulename)
  (with-open-file
      (stream (concatenate 'string "~/.golly/Rules/" (if rulename rulename string) ".rule")
	      :direction :output :if-exists :supersede)
    (print-1d-rule-table string :comment comment :rulename rulename :stream stream))
  t)
