(defpackage cl-utils--french-numbers
  (:use :cl))

(in-package :cl-utils--french-numbers)

;; References:
;; -----------
;; https://www.dictionnaire-academie.fr/article/QDL057
;; https://www.academie-francaise.fr/questions-de-langue#58_strong-em-nombres-criture-lecture-accord-em-strong
;; https://www.agathe-redactrice.net/orthographe-simple/nombres-en-lettres/
;; tests below are all verified with https://leconjugueur.lefigaro.fr/frnombre.php

(defun en-toutes-lettres (n)
  "This function converts a number N into its written equivalent in words in French, according to the rules prior to 1990 reform.
For instance : 101 --> 'cent un'.
N shall be <= 999 999 999 999
(v1 as of 2017-02-16, available in occisn/cl-utils GitHub repository)"
  
  (check-type n (integer 0 *))          ; entier positif ou nul
  (assert (<= n 999999999999))          ; 999 milliards etc...

  (let ((unites #("zero" "un" "deux" "trois" "quatre" "cinq" "six" "sept" "huit" "neuf" "dix" "onze" "douze" "treize" "quatorze" "quinze" "seize" "dix-sept" "dix-huit" "dix-neuf"))
	
	(dizaines #("void" "dix" "vingt" "trente" "quarante" "cinquante" "soixante" "soixante-dix" "quatre-vingt" "quatre-vingt-dix")))

    (labels ((sub (n &optional a-la-fin)
	       
	       (cond
		 
		 ((>= n 1000000000)     ; un milliard
		  (let* ((mmm (floor (/ n 1000000000)))
			 (mmm-en-toutes-lettres (sub mmm t))
			 ;; t ci-dessus car milliards est considéré comme un nom
			 ;; donc mmm est "à la fin"
			 (reste (mod n 1000000000))
			 (reste-en-toutes-lettres (sub reste a-la-fin)))
		    (concatenate 'string
				 mmm-en-toutes-lettres
				 " milliard"
				 (when (> mmm 1) "s")
				 (when (>= reste 1) " ")
				 (when (>= reste 1) reste-en-toutes-lettres))))
		 
		 ((>= n 1000000)        ; un million
		  (let* ((mm (floor (/ n 1000000)))
			 (mm-en-toutes-lettres (sub mm t))
			 ;; t ci-dessus car millions est considéré comme un nom
			 ;; donc mm est "à la fin"
			 (reste (mod n 1000000))
			 (reste-en-lettres (sub reste a-la-fin)))
		    (concatenate 'string
				 mm-en-toutes-lettres
				 " million"
				 (when (> mm 1) "s")
				 (when (>= reste 1) " ")
				 (when (>= reste 1) reste-en-lettres))))
		 
		 ((>= n 1000)           ; 
		  (let* ((m (floor (/ n 1000)))
			 (m-en-lettres (sub m nil))
			 (reste (mod n 1000))
			 (reste-en-lettres (sub reste a-la-fin)))
		    (concatenate 'string
				 (when (>= m 2) m-en-lettres)
				 (when (>= m 2) " ")
				 "mille"
				 (when (>= reste 1) " ")
				 (when (>= reste 1) reste-en-lettres))))	      
		 
		 ((<= n 19) (aref unites n))
		 ((<= n 69) (let* ((d (floor (/ n 10)))
				   (d-en-lettres (aref dizaines d))
				   (u (mod n 10)))
			      (cond
				((= u 0) d-en-lettres)
				((= u 1) (concatenate 'string d-en-lettres
						      " et un"))
				(t (concatenate 'string d-en-lettres
						"-" (aref unites u))))))
		 ((= n 70) "soixante-dix")
		 ((= n 71) "soixante et onze")
		 ((<= n 79) (concatenate 'string "soixante-" (aref unites (- n 60))))
		 ((= n 80) (if a-la-fin "quatre-vingts" "quatre-vingt"))
		 ((<= n 99) (concatenate 'string "quatre-vingt-" (aref unites (- n 80))))
		 ((= n 100) "cent")
		 ((<= n 999) (let* ((c (floor (/ n 100)))
				    (c-en-lettres (sub c nil))
				    (du (mod n 100))
				    (du-en-lettres (sub du a-la-fin)))
			       (concatenate 'string
					    (when (<= n 199) "cent")
					    (when (> n 199) c-en-lettres)
					    (when (> n 199) " cent")
					    (when (and a-la-fin (= du 0)) "s")
					    (when (> du 0) " ")
					    (when (> du 0) du-en-lettres)))))))

      (sub n t))))


(defun TEST-en-toutes-lettres ()
  (assert (string= (en-toutes-lettres 3) "trois"))
  (assert (string= (en-toutes-lettres 20) "vingt"))
  (assert (string= (en-toutes-lettres 31) "trente et un"))
  (assert (string= (en-toutes-lettres 46) "quarante-six"))
  (assert (string= (en-toutes-lettres 72) "soixante-douze"))
  (assert (string= (en-toutes-lettres 79) "soixante-dix-neuf"))
  (assert (string= (en-toutes-lettres 80) "quatre-vingts"))
  (assert (string= (en-toutes-lettres 81) "quatre-vingt-un"))
  (assert (string= (en-toutes-lettres 89) "quatre-vingt-neuf"))
  (assert (string= (en-toutes-lettres 90) "quatre-vingt-dix"))
  (assert (string= (en-toutes-lettres 91) "quatre-vingt-onze"))
  (assert (string= (en-toutes-lettres 99) "quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 100) "cent"))
  (assert (string= (en-toutes-lettres 153) "cent cinquante-trois"))
  (assert (string= (en-toutes-lettres 180) "cent quatre-vingts"))
  (assert (string= (en-toutes-lettres 200) "deux cents"))
  (assert (string= (en-toutes-lettres 299) "deux cent quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 300) "trois cents"))
  (assert (string= (en-toutes-lettres 326) "trois cent vingt-six"))
  (assert (string= (en-toutes-lettres 821) "huit cent vingt et un"))
  (assert (string= (en-toutes-lettres 999) "neuf cent quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 1000) "mille"))
  (assert (string= (en-toutes-lettres 1001) "mille un"))
  (assert (string= (en-toutes-lettres 1100) "mille cent"))
  (assert (string= (en-toutes-lettres 1999) "mille neuf cent quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 2000) "deux mille"))
  (assert (string= (en-toutes-lettres 2001) "deux mille un"))
  (assert (string= (en-toutes-lettres 9555) "neuf mille cinq cent cinquante-cinq"))
  (assert (string= (en-toutes-lettres 10000) "dix mille"))
  (assert (string= (en-toutes-lettres 10032) "dix mille trente-deux"))
  (assert (string= (en-toutes-lettres 10200) "dix mille deux cents"))
  (assert (string= (en-toutes-lettres 80000) "quatre-vingt mille"))
  (assert (string= (en-toutes-lettres 100000) "cent mille"))
  (assert (string= (en-toutes-lettres 500000) "cinq cent mille"))
  (assert (string= (en-toutes-lettres 180000) "cent quatre-vingt mille"))
  (assert (string= (en-toutes-lettres 532000) "cinq cent trente-deux mille"))
  (assert (string= (en-toutes-lettres 999999) "neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 1000000) "un million"))
  (assert (string= (en-toutes-lettres 1000022) "un million vingt-deux"))
  (assert (string= (en-toutes-lettres 1000100) "un million cent"))
  (assert (string= (en-toutes-lettres 100000000) "cent millions"))
  (assert (string= (en-toutes-lettres 200000200) "deux cents millions deux cents"))
  (assert (string= (en-toutes-lettres 999999999) "neuf cent quatre-vingt-dix-neuf millions neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf"))
  (assert (string= (en-toutes-lettres 1000000000) "un milliard"))
  (assert (string= (en-toutes-lettres 1001001100) "un milliard un million mille cent")) 
  (assert (string= (en-toutes-lettres 2000000000) "deux milliards"))
  (assert (string= (en-toutes-lettres 80080080080) "quatre-vingts milliards quatre-vingts millions quatre-vingt mille quatre-vingts"))
  (assert (string= (en-toutes-lettres 82082082082) "quatre-vingt-deux milliards quatre-vingt-deux millions quatre-vingt-deux mille quatre-vingt-deux")) 
  (assert (string= (en-toutes-lettres 91091091091) "quatre-vingt-onze milliards quatre-vingt-onze millions quatre-vingt-onze mille quatre-vingt-onze"))
  (assert (string= (en-toutes-lettres 100000100000) "cent milliards cent mille"))
  (assert (string= (en-toutes-lettres 100100100100) "cent milliards cent millions cent mille cent"))
  (assert (string= (en-toutes-lettres 100005300567) "cent milliards cinq millions trois cent mille cinq cent soixante-sept"))
  (assert (string= (en-toutes-lettres 123456789123) "cent vingt-trois milliards quatre cent cinquante-six millions sept cent quatre-vingt-neuf mille cent vingt-trois"))
  (assert (string= (en-toutes-lettres 200200200200) "deux cents milliards deux cents millions deux cent mille deux cents"))
  (assert (string= (en-toutes-lettres 386428075105) "trois cent quatre-vingt-six milliards quatre cent vingt-huit millions soixante-quinze mille cent cinq"))
  (assert (string= (en-toutes-lettres 999999999999) "neuf cent quatre-vingt-dix-neuf milliards neuf cent quatre-vingt-dix-neuf millions neuf cent quatre-vingt-dix-neuf mille neuf cent quatre-vingt-dix-neuf")))

;;; end
