(in-package "ACL2")

(local (include-book "arithmetic-3/top" :dir :system))

(set-state-ok t)

(defconst *newline* (fms-to-string "" nil))

(defmacro cat-str (&rest strs)
  "An abbreviation for (concatenate 'string ...)"
  `(concatenate 'string ,@strs))

(defun read-obj-r (current channel state objcount)
         (declare (xargs :measure (if (natp objcount) objcount 0)))
         (if (zp objcount)
           (mv (reverse current) state)
           (mv-let (eofp obj state)
                   (read-object channel state)
                   (if eofp
                     (mv (reverse current) state)
                     (read-obj-r (cons obj current) 
                                 channel 
                                 state 
                                 (- objcount 1))))))

(defun read-obj (filename state)
  "Read and return all of the objects contained in the file indicated by
  filename.
  
  Returns (mv obj state) where obj is a list of all the forms in filename."
  (mv-let (channel state)
          (open-input-channel filename
                              :object state)
          (mv-let (result state)
                  (read-obj-r nil channel state (expt 2 1000))
                  (let ((state (close-input-channel channel state)))
                    (mv result state)))))

(defun print-to-file (filename state output)
  (mv-let (channel state)
          (open-output-channel (cat-str filename ".html") 
                               :character state)
          (pprogn (princ$ output channel state)
                  (close-output-channel channel state)
                  state)))
