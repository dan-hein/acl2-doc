(in-package "acl2-doc")

(defpkg "acl2-doc" '(generate-docs-for) "docs docs docs")

(include-book "../acl2-html-templater/template")
(include-book "io-utils")

(set-state-ok t)

;(set-guard-checking :none)

(defabbrev symbol->string (symb)
  "Convert a symbol to a lowercase string suitable for display"
  (string-downcase (symbol-name symb)))

(defun append-list-assoc (key new alist)
  (put-assoc key 
             (cons new 
                   (cdr (assoc key alist)))
             alist))

(defmacro alist (&rest key-val-others)
  "An alist (associative list) constructor. Parameters are assumed to alternate
  between keys and values."
  (let ((key (first key-val-others))
        (val (second key-val-others))
        (others (rest (rest key-val-others))))
    (if (endp others)
      `(acons ,key ,val nil)
      `(acons ,key ,val (alist ,@others)))))

(defmacro switch (term &rest maybe-then-others)
  "Similar to c's switch statement. First parameter is the item to be tested.
  Rest of parameters are pairs of values to which the item is compared using
  (equal ...) and the value that the switch statement will return. The last
  pair may optionally start with \"otherwise\", in which case it will always
  match."
  (if (endp maybe-then-others)
    nil
    (let ((maybe (first (first maybe-then-others)))
          (then (second (first maybe-then-others)))
          (others (rest maybe-then-others)))
      (if (equal maybe 'otherwise)
        then
        `(if (equal ,term ,maybe)
           ,then
           (switch ,term ,@others))))))

(defun make-identity-alist (key vals)
  (if (endp vals)
    vals
    (cons (cons (cons key (symbol->string (first vals))) nil)
           (make-identity-alist key (rest vals)))))

(defun add-function-to-subs (evnt subs)
  (append-list-assoc
    'defuns
    (alist 'doc (if (stringp (fourth evnt)) (fourth evnt) "")
           'name (symbol->string (second evnt))
           'params (make-identity-alist 'param (third evnt))
           'hash-name (concatenate 'string "#"
                                   (symbol->string (second evnt))))
    subs))

(defun add-thm-to-subs (evnt subs)
  (append-list-assoc
    'thms
    (alist 'doc "TODO: get docstring from thms"
           'name (symbol->string (second evnt))
           'hash-name (concatenate 'string "#"
                                   (symbol->string (second evnt))))
    subs))

(defun add-macro-to-subs (evnt subs)
  (append-list-assoc
    'macros
    (alist 'doc (if (stringp (fourth evnt)) (fourth evnt) "")
           'name (symbol->string (second evnt))
           'hash-name (concatenate 'string "#"
                                   (symbol->string (second evnt))))
    subs))

(defun add-const-to-subs (evnt subs)
  (append-list-assoc
    'consts
    (alist 'doc (if (stringp (fourth evnt)) (fourth evnt) "")
           'name (symbol->string (second evnt))
           'hash-name (concatenate 'string "#"
                                   (symbol->string (second evnt))))
    subs))

(defun get-subs-from-file-r (filename obj)
  (if (endp obj)
    (list (cons 'filename filename)
          '(package . "none")
          '(books . nil)
          '(consts . nil)
          '(macros . nil)
          '(defuns . nil)
          '(thms . nil))
    (let ((subs (get-subs-from-file-r filename (rest obj)))
          (evnt (first obj)))
      (switch (first evnt)
              ('in-package (put-assoc 'package (second evnt) subs))
              ('defun (add-function-to-subs evnt subs))
              ('defthm (add-thm-to-subs evnt subs))
              ('defabbrev (add-macro-to-subs evnt subs))
              ('defmacro (add-macro-to-subs evnt subs))
              ('defconst (add-const-to-subs evnt subs))
              (otherwise subs)))))

(defun get-subs-from-file (filename state)
  "Get the template substitutions required by book.tmpl from the supplied
  source file."
  (mv-let (obj state) (read-obj filename state)
          (mv (get-subs-from-file-r filename obj) state)))

(defun generate-docs-for (filename state)
  "Generates the documentation for the single lisp source file filename (without the extension). Returns state and writes to filename.html."
  (mv-let (subs state) 
          (get-subs-from-file (cat-str filename ".lisp") state)
          (render-file-to "book" filename subs state)))

(generate-docs-for "generate-docs" state)
(generate-docs-for "demo" state)
(generate-docs-for "io-utils" state)
