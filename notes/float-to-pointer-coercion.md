
**About "doing float to pointer coercion (cost 13) to \<return value\>" compilation notes...**

*(benchmark at the end)*

When SBCL compiles a function returning a `double-float` with high optimization:  
(i) Unboxed computation:  
  Inside the function, the `double-float` is represented in CPU floating-point registers or stack locations (fast).  
(ii) Possible boxing on return:  
  If the returned value must be passed through a context requiring a Lisp object, SBCL may need to allocate a heap object to box the float.

**Compilation notes should generally be kept during development**, as they provide valuable insight into where type conversions and representation changes may occur.

**1) Boxing inside a tight loop within a function is a real performance problem**

This can usually be avoided by:  
\- precise type declarations  
\- ensuring numeric values remain unboxed throughout the loop  
\- avoiding function calls that force boxed return values in hot paths

**2) Boxing at a function boundary is not necessarily a problem** 

**2.a)** If the returned `double-float`:  
\- is not used in a tight loop, or  
\- is consumed infrequently, or  
\- is passed to I/O or presentation code (e.g., `FORMAT`)  
then the performance impact is typically negligible, and the compilation note can reasonably be ignored or suppressed.

**2.b)** However, if the `double-float` result of function `FOO` is used repeatedly inside a tight loop in function `BAR`, and the value is boxed and unboxed on each iteration, then there *is* an adverse performance impact.

Solution 1:  
Integrate the numeric computation of `FOO` directly into `BAR`’s loop body.  
Not practical, if function `FOO` is also called from elsewhere, since it would duplicate code.

Solution 2:  
Define `FOO` as a local function using `flet` or `labels` inside `BAR`, allowing SBCL to fully analyze the call graph and keep values unboxed.  
Not practical, if function `FOO` is also called from elsewhere, since it would duplicate code.

If you do not want to change program structure:  

Solution 3:

Request inlining with:
```lisp
(declaim (inline foo))
```

and optionally suppress the informational note locally:
```lisp
(locally
  (declare (optimize (sb-ext:compiler-note 0)))
  (defun foo ...))
```

Inlining may eliminate boxing at call sites, but the compilation note may still appear because SBCL must conservatively assume the function can be called in boxed contexts.

Other possible solution:  
Solution 4:  
Having `FOO` mutate a one-element `double-float` array instead of returning a `double-float`.  
While this avoids boxing, it adds indirection and complexity, and degrades performance.

**My preferred solution is #3 (inlining).**

Benchmarks with C and Common Lisp codes contained in this directory:

|   | Description                                                  | Monitored by | Duration | Nb of bytes consed | Compilation note? |
|---|--------------------------------------------------------------|--------------|----------|--------------------|-------------------|
|   | langage C                                                    |              | 0.995 s  |                    |                   |
| A | external function, not inlined                               | (time ...)   | 5.111 s  | 16,000 M           | compilation note  |
| B | external function, inlined                                   | (time ...)   | 0.992 s  | 0                  | compilation note  |
| C | external function (not inlined) returns a 1-element list     | (time ...)   | 7.759 s  | 32,000 M           | compilation note  |
| D | external function (not inlined) modifies a 1-element list    | (time ...)   | 6.132 s  | 16,000 M           | compilation note  |
| E | external function (not inlined) returns a 1-element array    | (time ...)   | 7.855 s  | 32,000 M           |                   |
| F | external function (not inlined) modifies a 1-element array   | (time ...)   | 2.741 s  | 0                  |                   |
| G | flet                                                         | (time ...)   | 1.046 s  | 0                  |                   |
| H | integrated function                                          | (time ...)   | 1.018 s  | 0                  |                   |
| I | integrated function with boxing/unboxing within loop         | (time...)    | 9.217 s  | 32,000 M           | compilation note  |

(end)
