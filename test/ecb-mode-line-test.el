;;; ecb-mode-line-test.el --- tests for ecb-mode-line.el -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Mode-line construction for the ECB tree buffers: prefix/data
;; resolution from `ecb-mode-line-prefixes' / `ecb-mode-line-data',
;; window numbers, truncation to the window width.

;;; Code:

(require 'ecb-test-helpers)

(defun ecb-ml-test-format (buffer-name)
  "Return `mode-line-format' of BUFFER-NAME."
  (with-current-buffer buffer-name mode-line-format))

(defun ecb-ml-test-strings (buffer-name)
  "Return the plain strings of the mode-line of BUFFER-NAME.
The first element (window number) is a :eval form and is reported as the
symbol `win-nr' if present, `nil' otherwise."
  (let ((fmt (ecb-ml-test-format buffer-name)))
    (list (and (consp (nth 0 fmt)) (eq (car (nth 0 fmt)) :eval) 'win-nr)
          (substring-no-properties (car (nth 1 fmt)))
          (substring-no-properties (car (nth 2 fmt))))))

(ert-deftest ecb-mode-line-make-modeline-str-applies-face ()
  (let ((result (ecb-mode-line-make-modeline-str "abc" 'bold)))
    (should (equal (car result) "abc"))
    (should (eq (get-text-property 0 'face (car result)) 'bold))))

(ert-deftest ecb-mode-line-update-buffer-sets-format ()
  (with-temp-buffer
    (ecb-mode-line-update-buffer (buffer-name) '("x"))
    (should (equal mode-line-format '("x"))))
  ;; unknown buffer -> only a message
  (should (ecb-mode-line-update-buffer "no-such-ecb-buffer" '("x"))))

(ert-deftest ecb-mode-line-set-prefix-and-text ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-mode-line-set ecb-sources-buffer-name ecb-frame "PFX" "data text")
    (should (equal (ecb-ml-test-strings ecb-sources-buffer-name)
                   '(win-nr " PFX: " "data text")))
    ;; prefix without text has no colon
    (ecb-mode-line-set ecb-sources-buffer-name ecb-frame "PFX" nil)
    (should (equal (ecb-ml-test-strings ecb-sources-buffer-name)
                   '(win-nr " PFX" "")))
    ;; text only
    (ecb-mode-line-set ecb-sources-buffer-name ecb-frame nil "only")
    (should (equal (ecb-ml-test-strings ecb-sources-buffer-name)
                   '(win-nr " " "only")))))

(ert-deftest ecb-mode-line-set-window-number-option ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((ecb-mode-line-display-window-number nil))
      (ecb-mode-line-set ecb-sources-buffer-name ecb-frame nil "only")
      (should (equal (ecb-ml-test-strings ecb-sources-buffer-name)
                     '(nil " " "only"))))
    (let ((ecb-mode-line-display-window-number t))
      (ecb-mode-line-set ecb-sources-buffer-name ecb-frame nil "only" t)
      (should (equal (ecb-ml-test-strings ecb-sources-buffer-name)
                     '(nil " " "only")))
      (ecb-mode-line-set ecb-sources-buffer-name ecb-frame nil "only")
      (should (eq (car (ecb-ml-test-strings ecb-sources-buffer-name)) 'win-nr))
      ;; the :eval form yields W-<n> for the window's position
      (should (string-match-p "^ W-[0-9]+$"
                              (with-selected-window (get-buffer-window ecb-sources-buffer-name)
                                (eval (cadr (car (ecb-ml-test-format ecb-sources-buffer-name))))))))))

(ert-deftest ecb-mode-line-set-truncates-to-window-width ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let* ((win-width (window-width (get-buffer-window ecb-sources-buffer-name)))
           (long (make-string (* 3 win-width) ?x)))
      (ecb-mode-line-set ecb-sources-buffer-name ecb-frame nil long)
      (let ((text (nth 2 (ecb-ml-test-strings ecb-sources-buffer-name))))
        (should (< (length text) win-width))
        (should (string-prefix-p "..." text))))))

(ert-deftest ecb-mode-line-set-ignores-buffers-not-in-frame ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (with-temp-buffer
      (let ((before mode-line-format))
        (ecb-mode-line-set (buffer-name) ecb-frame "P" "T")
        (should (equal mode-line-format before))))))

(ert-deftest ecb-mode-line-format-default-options ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (ecb-mode-line-format)
    ;; sources and directories show the selected directory (shortened to
    ;; fit), the methods buffer the selected source
    (should (string-suffix-p "src" (nth 2 (ecb-ml-test-strings ecb-sources-buffer-name))))
    (should (string-suffix-p "src" (nth 2 (ecb-ml-test-strings ecb-directories-buffer-name))))
    (should (equal (nth 2 (ecb-ml-test-strings ecb-methods-buffer-name)) "alpha.el"))
    ;; no filter -> empty prefix part
    (should (equal (nth 1 (ecb-ml-test-strings ecb-methods-buffer-name)) " "))))

(ert-deftest ecb-mode-line-format-custom-prefix-and-data ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (let ((ecb-mode-line-prefixes
           (list (cons 'ecb-sources-buffer-name "SRC")
                 ;; a string key (buffer name) and a function prefix
                 (cons ecb-methods-buffer-name
                       (lambda (_buf _dir src) (format "M:%s" (cdr src))))))
          (ecb-mode-line-data
           (list (cons 'ecb-sources-buffer-name "fixed")
                 (cons 'ecb-directories-buffer-name
                       (lambda (buf dir _src) (format "%s|%s" buf (file-name-nondirectory
                                                                   (directory-file-name dir)))))
                 (cons 'ecb-methods-buffer-name nil))))
      (ecb-mode-line-format)
      (should (equal (cdr (ecb-ml-test-strings ecb-sources-buffer-name))
                     '(" SRC: " "fixed")))
      (should (string-prefix-p " M:alpha.el"
                               (nth 1 (ecb-ml-test-strings ecb-methods-buffer-name))))
      ;; (truncated at the left to fit the narrow window)
      (should (string-suffix-p "tories*|src"
                               (nth 2 (ecb-ml-test-strings ecb-directories-buffer-name))))
      ;; the default help-echo is only added for non-function data
      (should (get-text-property 0 'help-echo
                                 (car (nth 2 (ecb-ml-test-format ecb-sources-buffer-name)))))
      (should-not (get-text-property 0 'help-echo
                                     (car (nth 2 (ecb-ml-test-format ecb-directories-buffer-name))))))))

(ert-deftest ecb-mode-line-format-rejects-invalid-spec ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((ecb-mode-line-prefixes '((42 . "x"))))
      (should-error (ecb-mode-line-format)))))

(ert-deftest ecb-mode-line-format-methods-filter-prefix ()
  (ecb-test-with-ecb proj
    (let ((buf (progn (find-file (expand-file-name "src/alpha.el" proj))
                      (ecb-window-sync)
                      (current-buffer))))
      (ecb-methods-filter-by-regexp nil buf "one")
      (ecb-mode-line-format)
      (should (string-match-p "\\[Regexp: one\\]"
                              (nth 1 (ecb-ml-test-strings ecb-methods-buffer-name)))))))

(provide 'ecb-mode-line-test)
;;; ecb-mode-line-test.el ends here
