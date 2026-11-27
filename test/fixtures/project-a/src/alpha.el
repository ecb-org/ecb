;;; alpha.el --- fixture -*- lexical-binding: t -*-

(defvar alpha-counter 0 "A counter.")

(defcustom alpha-option 'foo "An option." :type 'symbol :group 'alpha)

(defun alpha-one ()
  "First function."
  (setq alpha-counter (1+ alpha-counter)))

(defun alpha-two (x &optional y)
  "Second function."
  (list x y))

(defmacro alpha-with (&rest body)
  "A macro."
  `(progn ,@body))

(provide 'alpha)
;;; alpha.el ends here
