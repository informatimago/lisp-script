;;;
;;; clisp initialization for clisp scripts.
;;;

(defmacro defconst (symbol value docstring)
  `(setq ,symbol ,value))

;; Exit status from <sysexit.h>
(defconst  EX_OK           0       "successful termination")
(defconst  EX__BASE        64      "base value for error messages")
(defconst  EX_USAGE        64      "command line usage error")
(defconst  EX_DATAERR      65      "data format error")
(defconst  EX_NOINPUT      66      "cannot open input")
(defconst  EX_NOUSER       67      "addressee unknown")
(defconst  EX_NOHOST       68      "host name unknown")
(defconst  EX_UNAVAILABLE  69      "service unavailable")
(defconst  EX_SOFTWARE     70      "internal software error")
(defconst  EX_OSERR        71      "system error (e.g., can't fork)")
(defconst  EX_OSFILE       72      "critical OS file missing")
(defconst  EX_CANTCREAT    73      "can't create (user) output file")
(defconst  EX_IOERR        74      "input/output error")
(defconst  EX_TEMPFAIL     75      "temp failure; user is invited to retry")
(defconst  EX_PROTOCOL     76      "remote error in protocol")
(defconst  EX_NOPERM       77      "permission denied")
(defconst  EX_CONFIG       78      "configuration error")
(defconst  EX__MAX         78      "maximum listed value")


(defun exit (&optional status)
  "
DOES:   stops the script, exiting with the given status.
"
  (unless status (setq status EX_OK))
  (quit status))

(defun script (file &rest arguments)
  "
DOES:   reads the FILE, skip the first line if it begins with '#!' and
"
  (setq ext:*args* (cons file arguments))
  (let ((pfile (probe-file file))
        script-stream)
    (unless pfile
      (error "Can't read file %S." file))
    (setq script-stream (open pfile
                              :direction         :input
                              :element-type      'character
                              :if-does-not-exist :error))
    (read-line script-stream) ;; eat #!...
    (loop
      (eval (read script-stream)))
    (exit EX_OK)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp / Common-Lisp

(defun add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var (cons element (symbol-value list-var)))))


(defun put (symbol propname value)
  "
Store SYMBOL's PROPNAME property with value VALUE.
It can be retrieved with `(get SYMBOL PROPNAME)'.
"
  (setf (get symbol propname) value))

;;;; THE END ;;;;
