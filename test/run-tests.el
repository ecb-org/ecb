;;; run-tests.el --- batch entry point for the ECB test-suite -*- lexical-binding: t -*-

;; Author: Ryan Ware <ware@opensourceware.me>
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Usage (from the ECB root directory):
;;
;;   emacs -Q --batch -l test/run-tests.el                # all tests
;;   emacs -Q --batch -l test/run-tests.el -- unit        # skip integration
;;   emacs -Q --batch -l test/run-tests.el -- tree-buffer # only one file
;;   ECB_TEST_SELECTOR='"layout"' emacs -Q --batch -l test/run-tests.el
;;
;; Exit status is non-zero if any test fails or any test file fails to
;; load.  `make test' and `make test-unit' wrap this.

;;; Code:

(setq debug-on-error nil)
(setq load-prefer-newer t)

;; Tests stub primitives such as `read-key-sequence' and `message' with
;; `cl-letf'. On a native-comp Emacs that triggers on-the-fly compilation
;; of a trampoline, which needs a working C toolchain at test time and is
;; slow. Tests must not depend on that, so turn it off (Emacs >= 28 name
;; and the pre-29 name).
(setq native-comp-enable-subr-trampolines nil)
(setq comp-enable-subr-trampolines nil)

(defvar ecb-run-tests-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(load (expand-file-name "ecb-test-helpers.el" ecb-run-tests-dir) nil t)

(defun ecb-run-tests--files (mode)
  "Return the test files to load for MODE (nil, \"unit\" or a file stem)."
  (let ((all (sort (directory-files ecb-run-tests-dir t "-test\\.el\\'") #'string<)))
    (cond
     ((null mode) all)
     ((string= mode "unit")
      (cl-remove-if (lambda (f) (string-match-p "integration" f)) all))
     (t (cl-remove-if-not (lambda (f) (string-match-p (regexp-quote mode) f)) all)))))

(let* ((args (cdr (member "--" command-line-args)))
       (mode (car args))
       (files (ecb-run-tests--files mode))
       (load-errors nil))
  (setq command-line-args-left nil)
  (when (null files)
    (message "No test files match %S" mode)
    (kill-emacs 2))
  (dolist (f files)
    (condition-case err
        (progn
          (message "Loading %s" (file-name-nondirectory f))
          (load f nil t))
      (error
       (message "ERROR loading %s: %S" f err)
       (push f load-errors))))
  (when load-errors
    (message "%d test file(s) failed to load: %S" (length load-errors) load-errors)
    (kill-emacs 2))
  (let ((selector (if (getenv "ECB_TEST_SELECTOR")
                      (car (read-from-string (getenv "ECB_TEST_SELECTOR")))
                    t)))
    (ert-run-tests-batch-and-exit selector)))

;;; run-tests.el ends here
