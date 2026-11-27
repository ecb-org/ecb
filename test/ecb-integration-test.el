;;; ecb-integration-test.el --- end-to-end scenarios for ECB -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Cross-module scenarios that drive ECB the way a user would: activate,
;; browse directories/sources, open files, watch the methods buffer and
;; history follow, switch layouts, use the compile window, navigate
;; back and forth, and deactivate cleanly.  These are slower than the
;; per-module suites and are skipped by `make test-unit'.

;;; Code:

(require 'ecb-test-helpers)

(defun ecb-int-test-open (proj rel)
  "Visit REL inside PROJ in the edit window and let ECB sync."
  (ecb-select-edit-window)
  (find-file (expand-file-name rel proj))
  (ecb-window-sync)
  (current-buffer))

(defun ecb-int-test-lines (buffer-name)
  (mapcar #'ecb-test-strip (ecb-test-tree-buffer-lines buffer-name)))

(defun ecb-int-test-mode-line-text (buffer-name)
  "The data part of the ECB mode-line of BUFFER-NAME, without properties."
  (let ((fmt (with-current-buffer buffer-name mode-line-format)))
    (substring-no-properties (car (nth 2 fmt)))))

(defun ecb-int-test-mode-line-prefix (buffer-name)
  (let ((fmt (with-current-buffer buffer-name mode-line-format)))
    (substring-no-properties (car (nth 1 fmt)))))

(defun ecb-int-test-frame-sane-p ()
  "All windows live, exactly one edit window selected, no dangling ECB state."
  (and (frame-live-p ecb-frame)
       (eq (selected-frame) ecb-frame)
       (cl-every #'window-live-p (window-list ecb-frame 0))
       (ecb-point-in-edit-window-number)))

;;; ----------------------------------------------------------------------
;;; Full browsing session
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-browse-session ()
  "Open files of three languages and check every ECB buffer follows."
  (ecb-test-with-ecb proj
    ;; directories show the project root; sources list the root's files
    (should (cl-some (lambda (n) (string-suffix-p "project-a" n))
                     (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))
    (should (member "README.txt" (ecb-test-tree-buffer-node-names ecb-sources-buffer-name)))

    ;; 1. elisp
    (ecb-int-test-open proj "src/alpha.el")
    (should (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-sources-buffer-name)))
    (should (member "(alpha-one)" (ecb-int-test-lines ecb-methods-buffer-name)))
    (should (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
    (should (string-suffix-p "src" (ecb-test-strip (ecb-int-test-mode-line-text ecb-sources-buffer-name))))

    ;; 2. C with nested struct members
    (ecb-int-test-open proj "src/gamma.c")
    (should (member "main (argc : int,argv : char**) : int"
                    (ecb-int-test-lines ecb-methods-buffer-name)))
    (should (member "gamma.c" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))

    ;; 3. non-semantic (imenu) source in a subdirectory
    (ecb-int-test-open proj "src/sub/epsilon.rb")
    (should (equal (ecb-test-tree-buffer-node-names ecb-methods-buffer-name) '("run" "stop")))
    (should (member "epsilon.rb" (ecb-test-tree-buffer-node-names ecb-sources-buffer-name)))
    (should (string-suffix-p "sub" (ecb-test-strip (ecb-int-test-mode-line-text ecb-sources-buffer-name))))

    ;; 4. tag sync: moving point in gamma.c highlights the enclosing tag
    (ecb-int-test-open proj "src/gamma.c")
    (goto-char (point-min))
    (search-forward "return a")
    (ecb-tag-sync t)
    (with-current-buffer ecb-methods-buffer-name
      (should (equal (semantic-tag-name (car tree-buffer-highlighted-node)) "helper")))

    (should (ecb-int-test-frame-sane-p))))

;;; ----------------------------------------------------------------------
;;; Clicking through the tree buffers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-click-directory-source-method ()
  "Directory click -> sources update; source click -> file opens; method
click -> point jumps to the tag."
  (ecb-test-with-ecb proj
    ;; expand the project node and click the src directory
    (ecb-expand-directory-nodes 2)
    (let ((src-node (ecb-test-find-displayed-node
                     ecb-directories-buffer-name
                     (lambda (n) (equal (tree-node->name n) "src")))))
      (should src-node)
      (ecb-directory-or-source-clicked src-node 1 nil nil nil)
      (should (member "gamma.c" (ecb-test-tree-buffer-node-names ecb-sources-buffer-name))))
    ;; click gamma.c in the sources buffer
    (let ((gamma-node (ecb-test-find-displayed-node
                       ecb-sources-buffer-name
                       (lambda (n) (equal (tree-node->name n) "gamma.c")))))
      (should gamma-node)
      (ecb-source-clicked gamma-node 1 nil nil nil)
      (should (equal (file-name-nondirectory (buffer-file-name (window-buffer ecb-last-edit-window-with-point)))
                     "gamma.c")))
    ;; click the helper method
    (let ((helper (ecb-test-find-displayed-node
                   ecb-methods-buffer-name
                   (lambda (n) (and (semantic-tag-p (tree-node->data n))
                                    (equal (semantic-tag-name (tree-node->data n)) "helper"))))))
      (should helper)
      (ecb-method-clicked helper 1 nil nil nil)
      (with-current-buffer (window-buffer ecb-last-edit-window-with-point)
        (should (looking-at "static int helper"))))
    ;; navigation history has what we did; go back lands us on gamma.c start
    (ecb-nav-goto-previous)
    (should (ecb-int-test-frame-sane-p))))

;;; ----------------------------------------------------------------------
;;; Layout switching keeps the session
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-switch-through-all-layouts ()
  "Every built-in layout can be switched to while a file is open."
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (let ((failed nil))
      (dolist (type '(left right top left-right))
        (dolist (name (ecb-test-usable-layouts type))
          (condition-case err
              (progn
                (ecb-layout-switch name)
                (should (equal ecb-layout-name name))
                (should (ecb-int-test-frame-sane-p))
                (should (> (length (ecb-test-visible-ecb-buffers)) 0))
                ;; the file is still shown in the edit window
                (should (equal (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list))))
                               "alpha.el")))
            (error (push (cons name err) failed)))))
      (should (null failed)))
    ;; and back to the start layout
    (ecb-layout-switch "left8")
    (should (equal ecb-layout-name "left8"))))

(ert-deftest ecb-int-toggle-layout-returns-to-previous ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-layout-switch "left1")
    (ecb-layout-switch "top1")
    (ecb-toggle-layout t)
    (should (equal ecb-layout-name "left1"))
    (ecb-toggle-layout t)
    (should (equal ecb-layout-name "top1"))
    ;; without prefix the toggle walks `ecb-toggle-layout-sequence'
    (let ((ecb-toggle-layout-sequence '("left9" "left14"))
          (ecb-toggle-layout-state 0))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left9"))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left14")))))

;;; ----------------------------------------------------------------------
;;; Hide/show, maximize/restore, window sizes
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-hide-show-and-maximize-cycle ()
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (let ((all (ecb-test-visible-ecb-buffers)))
      (should (> (length all) 1))
      (ecb-hide-ecb-windows)
      (should (null (ecb-test-visible-ecb-buffers)))
      (should (ecb-windows-all-hidden))
      (ecb-show-ecb-windows)
      (should (equal (ecb-test-visible-ecb-buffers) all))
      ;; maximize each visible buffer in turn and restore
      (dolist (name all)
        (ecb-maximize-ecb-buffer name)
        (should (equal (ecb-test-visible-ecb-buffers) (list name)))
        (ecb-redraw-layout-full)
        (should (equal (ecb-test-visible-ecb-buffers) all)))
      ;; window-size store/restore round-trips
      (let ((ecb-layout-window-sizes nil))
        (cl-letf (((symbol-function 'customize-save-variable)
                   (lambda (sym val) (set sym val))))
          (ecb-store-window-sizes)
          (should (assoc "left8" ecb-layout-window-sizes))
          (ecb-restore-window-sizes)
          (should (equal (ecb-test-visible-ecb-buffers) all)))))))

;;; ----------------------------------------------------------------------
;;; Compile window
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-compile-window-receives-compilation-buffers ()
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (ecb-test-with-compile-window
      ;; display-buffer sends a compilation buffer to the compile window
      (let ((help (get-buffer-create "*Help*")))
        (unwind-protect
            (progn
              (display-buffer help)
              (should (eq (window-buffer ecb-compile-window) help))
              ;; a normal file buffer goes to the edit window
              (ecb-select-edit-window)
              (display-buffer (find-file-noselect (expand-file-name "src/beta.el" proj)))
              (should (eq (window-buffer ecb-compile-window) help))
              ;; cycling through compilation buffers works
              (ecb-cycle-through-compilation-buffers)
              (should (ecb-compilation-buffer-p (window-buffer ecb-compile-window)))
              ;; toggle away and back keeps the layout sane
              (ecb-toggle-compile-window -1)
              (should-not (ecb-compile-window-live-p))
              (ecb-toggle-compile-window 1)
              (should (ecb-compile-window-live-p))
              (should (ecb-int-test-frame-sane-p)))
          (kill-buffer help))))))

(ert-deftest ecb-int-compile-window-survives-layout-switch ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (ecb-test-with-compile-window
      (ecb-layout-switch "right1")
      (should (ecb-compile-window-live-p))
      (ecb-layout-switch "left8")
      (should (ecb-compile-window-live-p))
      (ecb-hide-ecb-windows)
      (should (ecb-compile-window-live-p))
      (ecb-show-ecb-windows)
      (should (ecb-compile-window-live-p)))))

;;; ----------------------------------------------------------------------
;;; History
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-history-filter-and-kill-buffer ()
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (ecb-int-test-open proj "src/gamma.c")
    (ecb-int-test-open proj "README.txt")
    (cl-flet ((files () (cl-remove-if #'file-name-absolute-p
                                      (ecb-test-tree-buffer-node-names ecb-history-buffer-name))))
      (should (equal (sort (files) #'string<) '("README.txt" "alpha.el" "gamma.c")))
      ;; filter by extension
      (ecb-history-filter-by-ext "el")
      (should (equal (files) '("alpha.el")))
      (ecb-history-filter-by-regexp "gamma")
      (should (equal (files) '("gamma.c")))
      (ecb-add-all-buffers-to-history)
      (should (= (length (files)) 3))
      ;; killing a buffer removes it from the history when configured so
      (let ((ecb-kill-buffer-clears-history 'auto))
        (kill-buffer "gamma.c"))
      (should (equal (sort (files) #'string<) '("README.txt" "alpha.el"))))))

;;; ----------------------------------------------------------------------
;;; Edits are reflected after save
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-save-updates-methods-and-sources ()
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (goto-char (point-max))
    (insert "\n(defun alpha-added () 42)\n")
    (save-buffer)
    (ecb-window-sync)
    (should (member "(alpha-added)" (ecb-int-test-lines ecb-methods-buffer-name)))
    ;; a new file created in the project shows up after a rebuild
    (let ((new (expand-file-name "src/zeta.el" proj)))
      (with-temp-file new (insert "(defun zeta () 1)\n"))
      (ecb-int-test-open proj "src/zeta.el")
      (should (member "zeta.el" (ecb-test-tree-buffer-node-names ecb-sources-buffer-name)))
      (should (equal (ecb-test-tree-buffer-node-names ecb-methods-buffer-name) '("(zeta)"))))))

;;; ----------------------------------------------------------------------
;;; Activation lifecycle
;;; ----------------------------------------------------------------------

(ert-deftest ecb-int-activate-deactivate-leaves-no-residue ()
  "Hooks, advices and ECB buffers are gone after deactivation, and ECB can
be re-activated repeatedly in the same session."
  (cl-flet ((ecb-fns (hook)
              (cl-remove-if-not
               (lambda (f) (and (symbolp f) (string-prefix-p "ecb-" (symbol-name f))))
               (default-value hook))))
    (dotimes (_ 3)
      (ecb-test-with-ecb proj
        (ecb-int-test-open proj "src/alpha.el")
        (should (ecb-test-advised-p 'delete-other-windows))
        (should (memq 'ecb-handle-major-mode-visibilty (default-value 'post-command-hook)))))
    (should-not ecb-minor-mode)
    (should-not (ecb-test-advised-p 'delete-other-windows))
    (should (null (ecb-test-ecb-timers)))
    (dolist (hook '(pre-command-hook after-save-hook kill-buffer-hook
                    find-file-hook window-size-change-functions))
      (should (null (ecb-fns hook))))
    ;; see `ecb-int-deactivate-removes-default-tag-filter-hook' for the one
    ;; known leftover on post-command-hook
    (should (null (remq 'ecb-apply-default-tag-filter (ecb-fns 'post-command-hook))))
    (dolist (name (list ecb-directories-buffer-name ecb-sources-buffer-name
                        ecb-methods-buffer-name ecb-history-buffer-name))
      (should-not (get-buffer name)))))

(ert-deftest ecb-int-deactivate-removes-default-tag-filter-hook ()
  "Known bug: `ecb-find-file-hook' pushes the self-removing
`ecb-apply-default-tag-filter' onto `post-command-hook', but
`ecb-deactivate' never removes it. If a file is visited and ECB is
deactivated before the next command, the hook survives deactivation."
  :expected-result :failed
  (remove-hook 'post-command-hook 'ecb-apply-default-tag-filter)
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el"))
  (unwind-protect
      (should-not (memq 'ecb-apply-default-tag-filter (default-value 'post-command-hook)))
    (remove-hook 'post-command-hook 'ecb-apply-default-tag-filter)))

(ert-deftest ecb-int-ecb-buffers-have-correct-modes-and-keymaps ()
  (ecb-test-with-ecb proj
    (ecb-int-test-open proj "src/alpha.el")
    (dolist (name (ecb-test-visible-ecb-buffers))
      (with-current-buffer name
        (should (memq (current-buffer) tree-buffers))
        (should buffer-read-only)
        (should (keymapp (current-local-map)))
        ;; the mode-line carries the ECB prefix
        (should (string-prefix-p " " (ecb-int-test-mode-line-prefix name)))))))

(provide 'ecb-integration-test)
;;; ecb-integration-test.el ends here
