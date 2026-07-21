;;;; Utilities for format.
;;;;
;;;; Illustrations of various `format` directives.
;;;;
;;;; Reference notes: illustrations and considerations rather than
;;;; exported functions.

(in-package :cl-utils)

(defun SHOW-all-format ()
  ""

  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

  (format t "~%~%======~%=== FORMAT~%======~%")

  ;; general
  ;; -------  
  ;; ~% new line
  ;; ~5% five new lines
  ;; ~& fresh line (newline only if not at start of line)
  ;; ~~ to print a tilde
  ;; ~A - Aesthetic (human-readable, no escape characters)
  ;; ~S - Standard (with escape characters, readably)
  ;;          keep the ""

  (format t "~%")
  (format t "GENERAL:~%")
  (format t "--------~%")
  (format t "~~~~: ~~~%")
  (format t "( ~~A )  ~A~%" "coucou")
  (format t "( ~~S )  ~S~%" "coucou")
  
  ;; integers
  ;; --------
  ;; ~D  for integer
  ;; ~@D to have an apparent '+' sign
  ;; ~6D for right-justificaton
  ;; ~:D to add comma between each group of three digits
  ;; ~B  for binary
  ;; ~O  for octal
  ;; ~X  for hexadecimal
  ;; ~bR for base-b integer
  ;; ~R in English
  ;; ~:R in English with -th
  ;; ~@R for Roman numerals
  
  (format t "~%")
  (format t "INTEGERS:~%")
  (format t "---------~%")
  (format t "( ~~D )     100 --> ~D~%" 100)
  (format t "( ~~D )    -100 --> ~D~%" -100)
  (format t "( ~~@D )    100 --> ~@D~%" 100)
  (format t "( ~~6D )    100 --> |~6D|~%" 100)
  (format t "( ~~6D )   -100 --> |~6D|~%" -100)
  (format t "( ~~6@D )   100 --> |~6@D|~%" 100)
  (format t "( ~~:D )    123456789 --> ~:D~%" 123456789)
  (format t "( ~~B )     100 --> ~B as binary~%" 100)
  (format t "( ~~O )     100 --> ~O as octal~%" 100)
  (format t "( ~~X )     100 --> ~X as hexadecimal~%" 100)
  (format t "( ~~3R )    100 --> ~3R as base-3~%" 100)
  (format t "( ~~R )     100 --> ~R~%" 100)
  (format t "( ~~:R )    100 --> ~:R~%" 100)
  (format t "( ~~@R )    1999 --> ~@R (Roman numeral)~%" 1999)
  
  ;; floats
  ;; ------
  ;; ~F for fixed-format floating point
  ;; ~E for exponential floating point
  ;; ~$ for monetary (2 decimal places by default)
  ;; ~3$ or ~,3f for 3 decimal places
  ;; ~v$ for n decimal places
  
  (format t "~%")
  (format t "FLOATS:~%")
  (format t "-------~%")
  (format t "( ~~F )    1234.56789 --> ~F~%" 1234.56789) ; truncated?
  (format t "( ~~3$ )   1234.56789 --> ~3$~%" 1234.56789)
  (format t "( ~~,3f )  1234.56789 --> ~,3f~%" 1234.56789)
  (format t "( ~~v$ )   1234.56789 --> ~v$~%" 3 1234.56789)
  (format t "( ~~E )    1234.56789 --> ~E~%" 1234.56789)
  (format t "( ~~$ )    1234.56789 --> ~$~%" 1234.56789)
  
  ;; characters
  ;; ----------
  ;; ~C
  ;; ~:C for non-printable character

  (format t "~%")
  (format t "CHARACTERS:~%")
  (format t "-----------~%")
  (format t "( ~~C)    a --> ~C~%" #\a)
  (format t "( ~~:C )  7 --> ~:C~%" (code-char 7))
  
  ;; plural
  ;; ------
  ;; ~P   - plural: output nothing or "s" based on additional argument
  ;; ~:P  - plural: outputs nothing or "s" based on previous arg
  ;; ~@P  - y --> ies with additional argument
  ;; ~:@P - same with previous argument
  
  (format t "~%")
  (format t "PLURAL:~%")
  (format t "-------~%")
  (format t "( ~~P: )    ~D apple~P~%" 1 1)
  (format t "( ~~P: )    ~D apple~P~%" 2 2)
  (format t "( ~~:P: )   ~D apple~:P~%" 1)
  (format t "( ~~:P: )   ~D apple~:P~%" 2)
  (format t "( ~~@P: )   ~D cit~@P~%" 1 1)
  (format t "( ~~@P: )   ~D cit~@P~%" 2 2)
  (format t "( ~~:@P: )  ~D cit~:@P~%" 1)
  (format t "( ~~:@P: )  ~D cit~:@P~%" 2)

  ;; capitalization
  ;; --------------
  ;; ~(...~) to lower case
  ;; ~:@(...~) to upper case
  ;; ~:(...~) to capitalize first letter of each word
  ;; ~@(...~) to capitalize first letter of first word

  (format t "~%")
  (format t "CAPITALIZATION:~%")
  (format t "---------------~%")
  (format t "( ~~(...~~) )  Hello World --> ~(Hello World~)~%")
  (format t "( ~~:@(...~~) )  Hello World --> ~:@(Hello World~)~%")
  (format t "( ~~:(...~~) )  hello world --> ~:(hello world~)~%")
  (format t "( ~~@(...~~) )  hello world --> ~@(hello world~)~%")

  ;; Padding & justfication
  ;; ----------------------
  ;; ~10A - Right-pad to 10 characters
  ;; ~10@A - Left-pad to 10 characters
  ;; ~10,3A - Pad to 10 chars with minimum 3 chars <--- ?
  ;; ~<...~>
  ;; ~; indicate padding (if absent: on the left)
  (format t "~%")
  (format t "PADDING & JUSTIFICATION:~%")
  (format t "------------------------~%")
  (format t "Right padding:~%")
  (format t "   ( ~~6A )   ~6A|~%" 123)
  (format t "   ( ~~6A )   ~6A|~%" 1234)
  (format t "   ( ~~6A )   ~6A|~%" 12345)
  (format t "   ( ~~6A )   ~6A|~%" 123456)
  (format t "   ( ~~6A )   ~6A|~%" 1234567)
  (format t "Left padding:~%")
  (format t "   ( ~~6@A )  |~6@A~%" 123)
  (format t "   ( ~~6@A )  |~6@A~%" 1234)
  (format t "   ( ~~6@A )  |~6@A~%" 12345)
  (format t "   ( ~~6@A )  |~6@A~%" 123456)
  (format t "   ( ~~6@A )  |~6@A~%" 1234567)
  (format t "Right justification:~%")
  (format t "   ( ~~10<...~~> )       |~10<Hello~>|~%")
  (format t "   ( ~~10<...~~> )       |~10<~A~>|~%" 123)
  (format t "Left justification:~%")
  (format t "   (~~10<...~~;~~> )     |~10<Hello~;~>|~%")
  (format t "   ( ~~10<...~~;~~> )     |~10<123~;~>|~%")
  (format t "Centered:~%")
  (format t "   ( ~~10<~~;...~~;~~> )   |~10<~;Hello~;~>|~%")
  (format t "   ( ~~10<~~;...~~;~~> )   |~10<~;123~;~>|~%")
  (format t "Multiple segments evenly distributed:~%")
  (format t "   |~20<~;foo~;bar~;baz~;~>|~%")
  (format t "   |~20<~;1~;2~;3~;~>|~%")
  (format t "Table:~%")
  (format t "   |~10<apple~> | ~10<banana~> | ~10<cherry~>|~%")
  (format t "   |~10<1~> | ~10<2~> | ~10<3~>|~%")
  (format t "Width supplied at runtime with ~~V:~%")
  (format t "   |~V<Hello~>|~%" 15)

  ;; list & iteration
  ;; ----------------
  ;; ~{ ... ~} to iterate over the list
  ;; ~^ to stop before the last element
  (format t "~%")
  (format t "LIST & ITERATION:~%")
  (format t "-----------------~%")
  (format t "space-separated: ~{~A ~}~%" '(1 2 3))
  (format t "comma separated with no trailing comma: ~{~A~^, ~}~%" '(1 2 3))
  (format t "Pairs:~%")
  (format t "~&   Name~20TExtension~{~&   ~A~20T~A~}~%"
          '("Joe" 3215 "Mary" 3246 "Fred" 3222 "Dave" 3232 "Joseph" 3212))
  (format t "Iteration and justification:~%")
  (format t "~{   |~10<~A~>|~%~}" '("cat" "elephant" "fox"))

  ;; conditional
  ;; -----------
  (format t "~%")
  (format t "CONDITIONAL:~%")
  (format t "------------~%")
  (format t "~:[No~;Yes~]~%" t)

  ;; and...
  ;; ~V  - arguments supplied at runtime
  ;; ~T  - tab stop
  ;; ~?  - Recursive format (format string as argument)
  ;; ~*  - Skip an argument
  ;; ~:* - Back up one argument
  ;; ~<  - justification
  ;; ~>  - terminate ~<
  ;; # = nb of remaining arguments:
  ;;           (format t "~#$" pi) ; 1 decimal place

  ;; Reference:
  ;; https://dept-info.labri.fr/~strandh/Teaching/PFS/Common/David-Lamkins/chapter24.html
  
  )

;;; end
