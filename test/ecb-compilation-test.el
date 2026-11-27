;;; ecb-compilation-test.el --- tests for ecb-compilation.el and ecb-cycle.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Which buffers count as "compilation buffers" (and therefore go to the
;; compile window), the compilation-buffer menu cache and cycling through
;; compilation buffers in the compile window.

;;; Code:

(require 'ecb-test-helpers)

(defmacro ecb-comp-test-with-buffer (name mode &rest body)
  "Create buffer NAME in major MODE, run BODY, kill it afterwards."
  (declare (indent 2) (debug (form form body)))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (progn
           (with-current-buffer buf (funcall ,mode))
           ,@body)
       (kill-buffer buf))))

;;; ----------------------------------------------------------------------
;;; Name / mode / predicate sets
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compilation-buffer-names-combines-option-and-internal ()
  (let ((ecb-compilation-buffer-names '(("*a*") ("b.*" . t)))
        (ecb-compilation-buffer-names-internal '(("*internal*"))))
    (should (equal (ecb-compilation-buffer-names)
                   '(("*a*") ("b.*" . t) ("*internal*"))))))

(ert-deftest ecb-compilation-registered-buffer-p-exact-and-regexp ()
  (let ((ecb-compilation-buffer-names '(("*Help*") ("\\*my.*\\*" . t)))
        (ecb-compilation-buffer-names-internal nil))
    (should (equal (ecb-compilation-registered-buffer-p "*Help*") "*Help*"))
    (should (equal (ecb-compilation-registered-buffer-p "*mycompile*") "*mycompile*"))
    ;; exact entries do not match as regexps / substrings
    (should (null (ecb-compilation-registered-buffer-p "*Help*2")))
    (should (null (ecb-compilation-registered-buffer-p "other")))))

(ert-deftest ecb-compilation-default-names-cover-standard-buffers ()
  (dolist (name '("*Help*" "*Messages*" "*Backtrace*" "*Completions*" "*Occur*"
                  "*grep*" "*igrep*" "*compilation*" "*Compilation-foo*"))
    (should (ecb-compilation-registered-buffer-p name)))
  (should-not (ecb-compilation-registered-buffer-p "*scratch*")))

(ert-deftest ecb-compilation-major-modes-and-predicates-combine ()
  (let ((ecb-compilation-major-modes '(compilation-mode))
        (ecb-compilation-major-modes-internal '(my-mode))
        (ecb-compilation-predicates '(comint-check-proc))
        (ecb-compilation-predicates-internal '(my-pred)))
    (should (equal (ecb-compilation-major-modes) '(compilation-mode my-mode)))
    (should (equal (ecb-compilation-predicates) '(comint-check-proc my-pred)))))

;;; ----------------------------------------------------------------------
;;; ecb-compilation-buffer-p
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compilation-buffer-p-by-name ()
  (should (eq (ecb-compilation-buffer-p (get-buffer-create "*Messages*"))
              (get-buffer "*Messages*")))
  (should (eq (ecb-compilation-buffer-p "*Messages*") (get-buffer "*Messages*")))
  (ecb-comp-test-with-buffer "ecb-comp-plain" #'fundamental-mode
    (should (null (ecb-compilation-buffer-p buf)))
    (should (null (ecb-compilation-buffer-p "ecb-comp-plain")))))

(ert-deftest ecb-compilation-buffer-p-unknown-buffer-name-is-nil ()
  (should (null (ecb-compilation-buffer-p "no-such-buffer-anywhere"))))

(ert-deftest ecb-compilation-buffer-p-by-major-mode ()
  (ecb-comp-test-with-buffer "ecb-comp-mode" #'compilation-mode
    (should (eq (ecb-compilation-buffer-p buf) buf)))
  (ecb-comp-test-with-buffer "ecb-comp-text" #'text-mode
    (should (null (ecb-compilation-buffer-p buf)))
    (let ((ecb-compilation-major-modes '(text-mode)))
      (should (eq (ecb-compilation-buffer-p buf) buf)))))

(defun ecb-comp-test-pred-name-starts-with-ecb (buffer)
  (string-prefix-p "ecb-" (buffer-name buffer)))

(ert-deftest ecb-compilation-buffer-p-by-predicate-symbol ()
  "Known bug: `ecb-compilation-buffer-p' evaluates the predicates inside a
plain `dolist' without a result form, which always returns nil, so
`ecb-compilation-predicates' (including the default `comint-check-proc') has
no effect at all."
  :expected-result :failed
  (ecb-comp-test-with-buffer "ecb-comp-pred" #'fundamental-mode
    (let ((ecb-compilation-predicates '(ecb-comp-test-pred-name-starts-with-ecb)))
      (should (eq (ecb-compilation-buffer-p buf) buf)))))

(ert-deftest ecb-compilation-buffer-p-unbound-predicate-is-skipped ()
  (ecb-comp-test-with-buffer "ecb-comp-pred2" #'fundamental-mode
    (let ((ecb-compilation-predicates '(ecb-comp-test-no-such-predicate)))
      (should (null (ecb-compilation-buffer-p buf))))))

(ert-deftest ecb-compilation-buffer-p-accepts-lambda-predicates ()
  "Known bug: the docstring of `ecb-compilation-predicates' allows
lambda-expressions, but `ecb-compilation-buffer-p' calls `fboundp' on each
element, which signals for a lambda."
  :expected-result :failed
  (ecb-comp-test-with-buffer "ecb-comp-lambda" #'fundamental-mode
    (let ((ecb-compilation-predicates
           (list (lambda (b) (equal (buffer-name b) "ecb-comp-lambda")))))
      (should (eq (ecb-compilation-buffer-p buf) buf)))))

;;; ----------------------------------------------------------------------
;;; Buffer list / menu cache
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compilation-get-buffers-sorted-and-indexed ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-comp-test-with-buffer "*mycompile*" #'compilation-mode
      (get-buffer-create "*Help*")
      (let ((result (ecb-compilation-get-buffers)))
        (should (member '("*mycompile*" . 2) result))
        (should (equal (mapcar #'cdr result)
                       (number-sequence 0 (1- (length result)))))
        (should (equal (mapcar #'car result)
                       (sort (mapcar #'car result) #'ecb-string<)))))))

(ert-deftest ecb-compilation-buffer-list-cache-detects-changes ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((ecb-compilation-update-menu-p 'stale)
          (ecb-compilation-buffer-list-cache 'stale))
      (ecb-compilation-buffer-list-init)
      ;; init resets and immediately detects the "change" from nil
      (should ecb-compilation-update-menu-p)
      (should (equal ecb-compilation-buffer-list-cache (buffer-list)))
      (setq ecb-compilation-update-menu-p nil)
      (ecb-compilation-buffer-list-changed-p)
      (should-not ecb-compilation-update-menu-p)
      (ecb-comp-test-with-buffer "ecb-comp-new" #'fundamental-mode
        (ecb-compilation-buffer-list-changed-p)
        (should ecb-compilation-update-menu-p)))))

(ert-deftest ecb-compilation-update-menu-only-when-flagged ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((ecb-compilation-update-menu-p nil))
      (should (null (ecb-compilation-update-menu))))
    (let ((ecb-compilation-update-menu-p t))
      (should (ecb-compilation-update-menu))
      (should-not ecb-compilation-update-menu-p))))

;;; ----------------------------------------------------------------------
;;; ecb-cycle
;;; ----------------------------------------------------------------------

(ert-deftest ecb-cycle-requires-persistent-compile-window ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (should-error (ecb-cycle-through-compilation-buffers))))

(ert-deftest ecb-cycle-through-compilation-buffers-wraps ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit-if-bound proj)
    (ecb-test-with-compile-window
      (ecb-comp-test-with-buffer "*mycompile*" #'compilation-mode
        (let ((ecb-compilation-buffer-names '(("*Messages*")))
              (ecb-compilation-buffer-names-internal nil))
          (should (equal (mapcar #'car (ecb-compilation-get-buffers))
                         '("*Messages*" "*mycompile*")))
          ;; the compile window starts out on *Messages* (index 0) -> next
          (should (equal (buffer-name (window-buffer ecb-compile-window)) "*Messages*"))
          (ecb-cycle-through-compilation-buffers)
          (should (eq (selected-window) ecb-compile-window))
          (should (equal (buffer-name (window-buffer ecb-compile-window)) "*mycompile*"))
          ;; wrap around
          (ecb-cycle-through-compilation-buffers)
          (should (equal (buffer-name (window-buffer ecb-compile-window)) "*Messages*"))
          (ecb-cycle-through-compilation-buffers)
          (should (equal (buffer-name (window-buffer ecb-compile-window)) "*mycompile*"))
          ;; a non-compilation buffer in the compile window -> first one
          (set-window-buffer ecb-compile-window "alpha.el")
          (ecb-cycle-through-compilation-buffers)
          (should (equal (buffer-name (window-buffer ecb-compile-window)) "*Messages*")))))))

(ert-deftest ecb-cycle-choose-buffer-prompts ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-test-with-compile-window
      (ecb-comp-test-with-buffer "*mycompile*" #'compilation-mode
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "*mycompile*")))
          (ecb-cycle-through-compilation-buffers t))
        (should (equal (buffer-name (window-buffer ecb-compile-window)) "*mycompile*"))))))

(ert-deftest ecb-cycle-errors-without-compilation-buffers ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-test-with-compile-window
      (let ((ecb-compilation-buffer-names nil)
            (ecb-compilation-buffer-names-internal nil)
            (ecb-compilation-major-modes nil)
            (ecb-compilation-predicates nil))
        (cl-letf (((symbol-function 'compilation-buffer-p) #'ignore))
          (should-error (ecb-cycle-through-compilation-buffers)))))))

(defun ecb-mb-test-visit-if-bound (proj)
  "Visit alpha.el in the edit window (keeps the edit window on a source)."
  (find-file (expand-file-name "src/alpha.el" proj))
  (ecb-window-sync))

(provide 'ecb-compilation-test)
;;; ecb-compilation-test.el ends here
