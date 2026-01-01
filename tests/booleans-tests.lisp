(in-package :cl-utils-tests)

(parachute:define-test boolean-value-tests
  
  ;; Test with explicit boolean values
  (parachute:is eq t (boolean-value t)
      "t should coerce to t")
  
  (parachute:is eq nil (boolean-value nil)
      "nil should coerce to nil")
  
  (parachute:is eq t (boolean-value 0)
      "0 should coerce to t (it's not nil)")
  
  (parachute:is eq t (boolean-value 1)
      "1 should coerce to t")
  
  (parachute:is eq t (boolean-value 2)
      "2 should coerce to t")
  
  (parachute:is eq t (boolean-value -1)
      "-1 should coerce to t")
  
  (parachute:is eq t (boolean-value 3.14)
      "3.14 should coerce to t")
  
  (parachute:is eq t (boolean-value "")
      "Empty string should coerce to t")
  
  (parachute:is eq t (boolean-value "hello")
      "Non-empty string should coerce to t")
  
  (parachute:is eq t (boolean-value 'foo)
      "Symbol should coerce to t")
  
  (parachute:is eq t (boolean-value :keyword)
      "Keyword should coerce to t")
  
  (parachute:is eq nil (boolean-value '())
      "Empty list (which is nil) should coerce to nil")
  
  (parachute:is eq t (boolean-value '(1 2 3))
      "Non-empty list should coerce to t")
  
  (parachute:is eq t (boolean-value (cons 1 2))
      "Cons cell should coerce to t")
  
  (parachute:is eq t (boolean-value #\a)
      "Character should coerce to t")
  
  (parachute:is eq t (boolean-value #())
      "Empty vector should coerce to t (it's not nil)")
  
  (parachute:is eq t (boolean-value #(1 2 3))
      "Non-empty vector should coerce to t")
  
  (parachute:is eq t (boolean-value (make-hash-table))
      "Hash table should coerce to t")
  
  (parachute:is eq t (boolean-value (+ 1 1))
      "Expression result should coerce to t")
  
  (parachute:is eq nil (boolean-value (and nil t))
      "Expression evaluating to nil should coerce to nil")
  
  (parachute:is eq t (boolean-value (or nil t))
      "Expression evaluating to non-nil should coerce to t")
  
  ;; Test that the macro only evaluates its argument once
  (let ((counter 0))
    (declare (type fixnum counter))
    (boolean-value (incf counter))
    (parachute:is = 1 counter
        "Argument should be evaluated exactly once")))

;;; end
