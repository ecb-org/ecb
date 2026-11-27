;;; ecb-symboldef-test.el --- tests for ecb-symboldef.el and ecb-analyse.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The two semantic-analyzer driven ECB buffers: the symbol-definition
;; buffer (layout "left-symboldef") and the analyse buffer (layout
;; "left-analyse").  Both are synchronised explicitly since idle timers
;; never fire in batch.

;;; Code:

(require 'ecb-test-helpers)

(defun ecb-sd-test-buffer-string (name)
  "Contents of buffer NAME without properties."
  (with-current-buffer name (buffer-substring-no-properties (point-min) (point-max))))

;;; ----------------------------------------------------------------------
;;; Symboldef: pure helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-symboldef-get-find-function-by-mode ()
  (let ((ecb-symboldef-find-functions '((emacs-lisp-mode . ecb-symboldef-find-lisp-doc)
                                        (default . ecb-symboldef-find-definition))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (eq (ecb-symboldef-get-find-function) 'ecb-symboldef-find-lisp-doc)))
    (with-temp-buffer
      (c-mode)
      (should (eq (ecb-symboldef-get-find-function) 'ecb-symboldef-find-definition))))
  (let ((ecb-symboldef-find-functions nil))
    (with-temp-buffer
      (should (eq (ecb-symboldef-get-find-function) 'ecb-symboldef-find-null)))))

(ert-deftest ecb-function-at-point-finds-symbol ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(message \"hi\")")
    (goto-char 4)
    (should (eq (ecb-function-at-point) 'message))
    ;; at the end of the symbol
    (goto-char 9)
    (should (eq (ecb-function-at-point) 'message))
    (erase-buffer)
    (insert "(no-such-function-xyz)")
    (goto-char 4)
    (should (null (ecb-function-at-point)))))

(ert-deftest ecb-symboldef-get-doc-for-fsymbol ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((fdoc (ecb-symboldef-get-doc-for-fsymbol 'car (current-buffer))))
      (should (string-match-p "Return the car of LIST" fdoc))
      ;; no help buffer side effects
      (should-not (get-buffer ecb-symboldef-temp-buffer-name)))))

(ert-deftest ecb-symboldef-get-doc-for-vsymbol ()
  "Known bug: redirecting `describe-variable' output via `cl-flet' of
`help-buffer' no longer works with the current help system; the function
returns an empty string."
  :expected-result :failed
  (with-temp-buffer
    (emacs-lisp-mode)
    (unwind-protect
        (let ((vdoc (ecb-symboldef-get-doc-for-vsymbol 'fill-column (current-buffer))))
          (should (string-match-p "fill-column" vdoc)))
      (when (get-buffer ecb-symboldef-temp-buffer-name)
        (kill-buffer ecb-symboldef-temp-buffer-name)))))

;;; ----------------------------------------------------------------------
;;; Symboldef: buffer sync
;;; ----------------------------------------------------------------------

(ert-deftest ecb-symboldef-layout-shows-buffer ()
  (ecb-test-with-layout "left-symboldef" proj
    (ignore proj)
    (should (member ecb-symboldef-buffer-name (ecb-test-visible-ecb-buffers)))
    (ecb-goto-window-symboldef)
    (should (equal (buffer-name) ecb-symboldef-buffer-name))
    (ecb-maximize-window-symboldef)
    (should (equal (ecb-test-visible-ecb-buffers) (list ecb-symboldef-buffer-name)))))

(ert-deftest ecb-symboldef-sync-lisp-doc ()
  (ecb-test-with-layout "left-symboldef" proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (goto-char (point-min))
    (search-forward "(setq ")
    (backward-char 2)                   ; on "setq"
    (ecb-symboldef-buffer-sync t)
    (let ((text (ecb-sd-test-buffer-string ecb-symboldef-buffer-name)))
      (should (string-match-p "^setq is a" text)))
    (should (equal ecb-symboldef-last-symbol "setq"))
    (with-current-buffer ecb-symboldef-buffer-name
      ;; the prefix is truncated to the window width, so only check the start
      (should (string-match-p "Def Lisp Function <<se" (format "%S" mode-line-format))))))

(ert-deftest ecb-symboldef-sync-skips-short-symbols-and-repeats ()
  (ecb-test-with-layout "left-symboldef" proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (setq ecb-symboldef-last-symbol nil)
    ;; "x" is shorter than `ecb-symboldef-minimum-symbol-length'
    (goto-char (point-min))
    (search-forward "(list x y)")
    (backward-char 4)
    (ecb-symboldef-buffer-sync t)
    (should (null ecb-symboldef-last-symbol))
    ;; the same symbol twice does not re-run the finder
    (goto-char (point-min))
    (search-forward "(setq ")
    (backward-char 2)
    (ecb-symboldef-buffer-sync t)
    (should (equal ecb-symboldef-last-symbol "setq"))
    (let ((calls 0))
      (cl-letf (((symbol-function 'ecb-symboldef-find-lisp-doc)
                 (lambda (&rest _) (cl-incf calls) "")))
        (ecb-symboldef-buffer-sync t)
        (should (= calls 0))))))

(ert-deftest ecb-symboldef-sync-c-definition-via-semanticdb ()
  (ecb-test-with-layout "left-symboldef" proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-window-sync)
    (should (eq (ecb-symboldef-get-find-function) 'ecb-symboldef-find-definition))
    (let ((result (ecb-symboldef-find-tag-by-semanticdb "helper" (current-buffer))))
      (should (eq (nth 0 result) (current-buffer)))
      (should (= (nth 1 result) (save-excursion (goto-char (point-min))
                                                (search-forward "static int helper")
                                                (match-beginning 0)))))
    (goto-char (point-min))
    (search-forward "return helper(")
    (backward-char 3)
    (ecb-symboldef-buffer-sync t)
    (let ((text (ecb-sd-test-buffer-string ecb-symboldef-buffer-name)))
      (should (string-match-p "static int helper(int a)" text)))
    (with-current-buffer ecb-symboldef-buffer-name
      ;; the definition line is highlighted
      (goto-char (point-min))
      (search-forward "static int helper")
      (should (eq (get-text-property (match-beginning 0) 'face) 'highlight)))))

(ert-deftest ecb-symboldef-sync-hook-runs ()
  (ecb-test-with-layout "left-symboldef" proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (let* ((ran nil)
           (ecb-symboldef-buffer-sync-hook (list (lambda () (setq ran t)))))
      (ecb-symboldef-buffer-sync t)
      (should ran))))

(ert-deftest ecb-symboldef-find-null-reports-missing-finder ()
  (ecb-test-with-layout "left-symboldef" proj
    (find-file (expand-file-name "README.txt" proj))
    (ecb-window-sync)
    (let ((ecb-symboldef-find-functions nil))
      (goto-char (point-min))
      (forward-word 1)
      (setq ecb-symboldef-last-symbol nil)
      (ecb-symboldef-buffer-sync t)
      (should (string-match-p "No symbol definition function"
                              (ecb-sd-test-buffer-string ecb-symboldef-buffer-name))))))

;;; ----------------------------------------------------------------------
;;; Analyse
;;; ----------------------------------------------------------------------

(ert-deftest ecb-analyse-layout-shows-buffer ()
  (ecb-test-with-layout "left-analyse" proj
    (ignore proj)
    (should (member ecb-analyse-buffer-name (ecb-test-visible-ecb-buffers)))
    (ecb-goto-window-analyse)
    (should (equal (buffer-name) ecb-analyse-buffer-name))
    (ecb-maximize-window-analyse)
    (should (equal (ecb-test-visible-ecb-buffers) (list ecb-analyse-buffer-name)))))

(ert-deftest ecb-analyse-sync-c-context ()
  (ecb-test-with-layout "left-analyse" proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-window-sync)
    (goto-char (point-min))
    (search-forward "return a * 2")
    (backward-char 4)
    (ecb-analyse-buffer-sync t)
    (let ((lines (ecb-test-tree-buffer-lines ecb-analyse-buffer-name)))
      (should (member "[-] Context" lines))
      (should (member " `- -helper (a : int) : int" lines))
      (should (member "[-] Local Variables" lines))
      (should (member " `- a : int" lines))
      (should (member "[-] Prefix" lines))
      (should (member "[-] Completions" lines)))))

(ert-deftest ecb-analyse-sync-elisp-function-and-arguments ()
  (ecb-test-with-layout "left-analyse" proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (goto-char (point-min))
    (search-forward "(list x y)")
    (backward-char 3)                   ; on x
    (ecb-analyse-buffer-sync t)
    (let ((lines (ecb-test-tree-buffer-lines ecb-analyse-buffer-name)))
      (should (member " `- (alpha-two x,y)" lines))
      (should (member "[-] Function" lines))
      (should (member " `- (list &rest,rest)" lines))
      (should (member "[-] Argument # 1" lines))
      (should (member " |  x" lines))
      (should (member " `- y" lines)))))

(ert-deftest ecb-analyse-bucket-visibility-options ()
  (ecb-test-with-layout "left-analyse" proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-window-sync)
    (goto-char (point-min))
    (search-forward "return a * 2")
    (backward-char 4)
    (let ((ecb-analyse-collapsed-buckets '("Completions" "Local Variables")))
      (ecb-analyse-buffer-sync t)
      (let ((lines (ecb-test-tree-buffer-lines ecb-analyse-buffer-name)))
        (should (member "[+] Completions" lines))
        (should (member "[+] Local Variables" lines))
        (should (member "[-] Context" lines))))))

(ert-deftest ecb-analyse-node-click-jumps-to-tag ()
  (ecb-test-with-layout "left-analyse" proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-window-sync)
    (goto-char (point-min))
    (search-forward "return a * 2")
    (backward-char 4)
    (ecb-analyse-buffer-sync t)
    (let ((node (ecb-test-find-displayed-node
                 ecb-analyse-buffer-name
                 (lambda (n) (let ((d (tree-node->data n)))
                               (and (consp d) (semantic-tag-p (car d))
                                    (equal (semantic-tag-name (car d)) "helper")))))))
      (should node)
      (goto-char (point-max))
      (with-current-buffer ecb-analyse-buffer-name
        (ecb-analyse-node-clicked node 1 nil nil nil))
      (should (equal (buffer-name) "gamma.c"))
      (should (looking-at "static int helper")))))

(ert-deftest ecb-analyse-sync-hook-and-empty-for-non-semantic ()
  (ecb-test-with-layout "left-analyse" proj
    (let* ((ran nil)
           (ecb-analyse-buffer-sync-hook (list (lambda () (setq ran t)))))
      (find-file (expand-file-name "src/gamma.c" proj))
      (ecb-window-sync)
      (ecb-analyse-buffer-sync t)
      (should ran))
    (find-file (expand-file-name "README.txt" proj))
    (ecb-window-sync)
    (ecb-analyse-buffer-sync t)
    (should (cl-every #'string-empty-p (ecb-test-tree-buffer-lines ecb-analyse-buffer-name)))))

(provide 'ecb-symboldef-test)
;;; ecb-symboldef-test.el ends here
