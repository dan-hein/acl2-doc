(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)

(defconst *approx-pi* 22/7
          "An approximation for pi")

(defun factorial (n)
  "Computes the factorial of n."
  (declare (xargs :guard (natp n)))
  (if (zp n)
    1
    (* n (factorial (- n 1)))))

(defun factorial-tail (n r)
  "Computes the factorial of n tail-recursively."
  (declare (xargs :guard (and (natp n) (natp r))))
  (if (zp n)
    r
    (factorial-tail (- n 1) (* n r))))

(defthm factorial-tail-works
  (= (factorial n) (factorial-tail n 1))
  :doc "Proof that factorial is equal to factorial tail.")
