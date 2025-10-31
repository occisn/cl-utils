(in-package :cl-utils-tests)

(defmacro test-variants-give-same-result (name fns &key predicate context)
  "Define a parachute test for a bundle of functions supposed to do the same thing.
The objective is to check they yield the same result.
 
NAME: name of the parachute test
FNs: list of functions
PREDICATE: predicate to compare the results of the various functions
CONTEXT: context in which to perform test, containing (compare1 fn...)

Example:
(test-variants-give-same-result
 floor-to-power-of-10--same-results
 (%floor-to-power-of-10-v2
  %floor-to-power-of-10-v3
  %floor-to-power-of-10-v4)
 :predicate =
 :context (loop for i from 1 to 1000 do (compare1 fn i)))"
  (locally

      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))

      (labels ((sexp-replace-sexp-beginning-by (lst prefix replace-builder-fn)
                 "Transform sexp LST in the following way: each time a sexp, called sexp1, is found within LST of format sexp1 = (PREFIX...), it is replaced by (REPLACE-BUILDER-FN sexp1).

Used by macros.

Example:
(sexp-replace-sexp-beginning-by
   '(a (d (submit g h)) c)
   'submit
   (lambda (sexp) (cons 'submit2 (cdr sexp))))
;;; --> (A (D (SUBMIT2 G H)) C)"
                 (declare (type (or atom list) lst)
                          (type function replace-builder-fn))
                 
                 (cond ((atom lst) lst)
                       ((list lst)
                        (if (eq prefix (car lst))
                            (progn
                              (format t "~%lst = ~a~%prefix = ~a~%replace-builder-fn = ~a~%" lst prefix replace-builder-fn)
                              (funcall replace-builder-fn lst))
                            (loop for elt in lst collect (sexp-replace-sexp-beginning-by elt prefix replace-builder-fn)))))))
        
        (let ((fn1 (car fns))
              (fn (gensym))
              (value1 (gensym))
              (valuei (gensym))
              (outer0 (gensym))
              (j (gensym)))
          
          `(parachute:define-test ,name
             (parachute:true
              (block ,outer0
                (locally
                    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                  ,(sexp-replace-sexp-beginning-by
                    context
                    'compare1
                    (lambda (compare-sexp) 
                      `(let ((,value1 (,fn1 ,@(cddr compare-sexp))))
                         (loop for ,j of-type fixnum from 2 
                               for ,fn in (list ,@(loop for fn in (cdr fns) collect (list 'function fn))) ; previously: ',(cdr fns)
                               for ,valuei = (funcall ,fn ,@(cddr compare-sexp))
                               when (not (,predicate ,value1 ,valuei))
                                 do
                                    (format t "~%!!! Not same results : fn #~s yields ~s whereas fn #1 yields ~s~%~%"  ,j ,valuei ,value1)
                                    (return-from ,outer0 nil))))))
                t)                      ; end of outer0
              ))))))

;;; end
