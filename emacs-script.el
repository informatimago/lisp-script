;;;
;;; elisp initialization for emacs scripts.
;;; We try to install some stuff to make it more like Common-Lisp.
;;;

;; (add-to-list 'load-path "")

(require 'cl)
(require 'files)

(defvar *LOAD-PATHNAME*)
(defvar *load-pathname*)
(defvar EXT:*ARGS*)
(defvar ext:*args*)

(defun script (file &rest arguments)
  "
DO:     reads the FILE, skip the first line if it begins with '#!' and
"
  (unless (file-readable-p file)
    (error "Can't read file %S." file))
  (setq ext:*args* arguments)
  (setq *load-pathname* file)
  (setq *LOAD-PATHNAME* *load-pathname*
        EXT:*ARGS*      ext:*args*)
  (find-file file)
  (let ((buf (current-buffer)))
    (unwind-protect
        (progn
          (read-only-mode 1)
          (goto-char (point-min))
          (if (looking-at "#!")
              (forward-line 1))
          (narrow-to-region (point) (point-max))
          (eval-buffer))
      (kill-buffer buf))))


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

(defun ext:exit (&optional status)
  "
DO:     stops the script, exiting with the given status.
"
  (unless status (setq status EX_OK))
  (kill-emacs status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common-Lisp / Elisp

(defun read-line (&optional input-stream eof-error-p eof-value recursive-p)
  "
DO:     Implement partially Common-Lisp read-line function.
"
  (unless input-stream (setq input-stream standard-input))
  (let ( (standard-input input-stream)
         (line nil)
         (char (read-char)) )
    (while (not (member char '(10 13)))
      (push char line)
      (setq char (read-char)) )
    (funcall 'concat  (nreverse line))))


(defalias 'EXT:EXIT  'ext:exit)
(defalias 'READ-LINE 'read-line)

;;;; THE END ;;;;
