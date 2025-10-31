(in-package :cl-utils)

;; References:
;; -----------
;; https://www.dictionnaire-academie.fr/article/QDL057
;; https://www.academie-francaise.fr/questions-de-langue#58_strong-em-nombres-criture-lecture-accord-em-strong
;; https://www.agathe-redactrice.net/orthographe-simple/nombres-en-lettres/
;; tests below are all verified with https://leconjugueur.lefigaro.fr/frnombre.php

(defun en-toutes-lettres (n)
  "This function converts a number N into its written equivalent in words in French, according to the rules prior to 1990 reform.
For instance : 101 --> 'cent un'.
N shall be an integer >= 0 and <= 999 999 999 999
(v1 as of 2017-02-16, available in occisn/cl-utils GitHub repository)"
    
  (locally

      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      
      (let ((unites #("zero" "un" "deux" "trois" "quatre" "cinq" "six" "sept" "huit" "neuf" "dix" "onze" "douze" "treize" "quatorze" "quinze" "seize" "dix-sept" "dix-huit" "dix-neuf"))
	    
	    (dizaines #("void" "dix" "vingt" "trente" "quarante" "cinquante" "soixante" "soixante-dix" "quatre-vingt" "quatre-vingt-dix")))

        (labels ((sub (n &optional a-la-fin)

                   (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	           
	           (cond
		     
		     ((>= n 1000000000) ; un milliard
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
		     
		     ((>= n 1000000)    ; un million
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
		     
		     ((>= n 1000)       ; 
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

          (sub n t)))))

;;; end
