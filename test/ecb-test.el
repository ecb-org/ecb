;;; ecb-test.el --- tests for ecb.el -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Activation and deactivation lifecycle: state, hooks, advices, timers,
;; window configuration, keymap, and failure handling.

;;; Code:

(require 'ecb-test-helpers)

(defconst ecb-test--hooks-installed-by-activation
  '((after-save-hook . ecb-update-methods-after-saving)
    (kill-buffer-hook . ecb-kill-buffer-hook)
    (find-file-hook . ecb-find-file-hook)
    (menu-bar-update-hook . ecb-compilation-update-menu))
  "Global hooks that `ecb-activate' must add and `ecb-deactivate' remove.")

;;; ----------------------------------------------------------------------
;;; Basic lifecycle
;;; ----------------------------------------------------------------------

(ert-deftest ecb-activate-sets-minor-mode-and-frame ()
  (ecb-test-with-ecb _proj
    (should ecb-minor-mode)
    (should (eq ecb-frame (selected-frame)))
    (should (frame-live-p ecb-frame))
    (should ecb-activated-window-configuration)))

(ert-deftest ecb-activate-creates-and-displays-ecb-buffers ()
  (ecb-test-with-ecb _proj
    (dolist (name (list ecb-directories-buffer-name ecb-sources-buffer-name
                        ecb-methods-buffer-name ecb-history-buffer-name))
      (should (get-buffer name))
      (should (get-buffer-window name ecb-frame)))
    ;; exactly one edit window in layout left8
    (should (= (length (ecb-canonical-edit-windows-list)) 1))))

(ert-deftest ecb-activate-installs-hooks-deactivate-removes-them ()
  (ecb-test-with-ecb _proj
    (dolist (h ecb-test--hooks-installed-by-activation)
      (should (memq (cdr h) (symbol-value (car h))))))
  (dolist (h ecb-test--hooks-installed-by-activation)
    (should-not (memq (cdr h) (symbol-value (car h))))))

(ert-deftest ecb-activate-enables-advices-deactivate-disables-them ()
  (ecb-test-with-ecb _proj
    (should (ecb-test-advised-p 'delete-window))
    (should (ecb-test-advised-p 'delete-other-windows))
    (should (ecb-test-advised-p 'display-buffer))
    (should (ecb-test-advised-p 'switch-to-buffer-other-window)))
  (should-not (ecb-test-advised-p 'delete-other-windows))
  (should-not (ecb-test-advised-p 'display-buffer)))

(ert-deftest ecb-deactivate-leaves-no-ecb-timers ()
  (ecb-test-with-ecb _proj
    ;; at least the stealthy / sync timers must be registered while active
    (should (ecb-test-ecb-timers)))
  (should-not (ecb-test-ecb-timers)))

(ert-deftest ecb-deactivate-kills-tree-buffers-and-restores-windows ()
  (ecb-test-prepare-frame)
  (let ((before (ecb-test-window-buffer-names)))
    (ecb-test-with-ecb _proj
      (should (> (length (ecb-test-window-buffer-names)) (length before))))
    (should-not ecb-minor-mode)
    (should (equal (ecb-test-window-buffer-names) before))
    (dolist (name (list ecb-directories-buffer-name ecb-sources-buffer-name
                        ecb-methods-buffer-name ecb-history-buffer-name))
      (should-not (get-buffer name)))
    (should-not (cl-some #'window-dedicated-p (window-list)))))

(ert-deftest ecb-deactivate-records-last-window-config ()
  (ecb-test-with-ecb _proj nil)
  (should ecb-last-window-config-before-deactivation))

(ert-deftest ecb-activate-twice-is-noop ()
  (ecb-test-with-ecb _proj
    (let ((tree (ecb-test-window-tree)))
      (ecb-activate)
      (should ecb-minor-mode)
      (should (equal (ecb-test-window-tree) tree)))))

(ert-deftest ecb-deactivate-twice-is-noop ()
  (ecb-test-with-ecb _proj nil)
  (should-not ecb-minor-mode)
  (ecb-deactivate)
  (should-not ecb-minor-mode))

(ert-deftest ecb-minor-mode-toggles ()
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-tip-of-the-day nil)
          (ecb-layout-name "left8"))
      (ecb-test-no-user-input
        (unwind-protect
            (progn
              (ecb-minor-mode 1)
              (should ecb-minor-mode)
              (ecb-minor-mode 0)
              (should-not ecb-minor-mode)
              (ecb-minor-mode)
              (should ecb-minor-mode)
              (ecb-minor-mode)
              (should-not ecb-minor-mode))
          (ecb-test-force-deactivate))))))

(ert-deftest ecb-activate-restores-temporarily-changed-emacs-variables ()
  (let ((orig-depth max-lisp-eval-depth))
    (ecb-test-with-ecb _proj
      (should (>= max-lisp-eval-depth orig-depth)))
    (should (= max-lisp-eval-depth orig-depth))
    (should-not ecb-temporary-changed-emacs-variables-alist)))

;;; ----------------------------------------------------------------------
;;; Hooks run by activation / deactivation
;;; ----------------------------------------------------------------------

(ert-deftest ecb-activation-hooks-run-in-order ()
  (let ((ecb-before-activate-hook (list (lambda () (ecb-test-record 'before-activate) t)))
        (ecb-activate-before-layout-draw-hook (list (lambda () (ecb-test-record 'before-layout))))
        (ecb-activate-hook (list (lambda () (ecb-test-record 'activate))))
        (ecb-before-deactivate-hook (list (lambda () (ecb-test-record 'before-deactivate) t)))
        (ecb-deactivate-hook (list (lambda () (ecb-test-record 'deactivate)))))
    (should (equal (ecb-test-with-call-log (ecb-test-with-ecb _proj nil))
                   '((before-activate) (before-layout) (activate)
                     (before-deactivate) (deactivate))))))

(ert-deftest ecb-before-activate-hook-can-veto ()
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-before-activate-hook (list (lambda () nil)))
          (ecb-activate-hook (list (lambda () (ecb-test-record 'activate)))))
      (unwind-protect
          (progn
            (should-not (ecb-test-with-call-log (ecb-test-no-user-input (ecb-activate))))
            (should-not ecb-minor-mode)
            (should-not (get-buffer ecb-directories-buffer-name)))
        (ecb-test-force-deactivate)))))

(ert-deftest ecb-before-deactivate-hook-can-veto ()
  (ecb-test-with-ecb _proj
    (let ((ecb-before-deactivate-hook (list (lambda () nil))))
      (ecb-deactivate)
      (should ecb-minor-mode))))

(ert-deftest ecb-activate-hook-error-aborts-and-cleans-up ()
  "An error in `ecb-activate-hook' is treated as an activation failure."
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-tip-of-the-day nil)
          (ecb-activate-hook (list (lambda () (error "hook failure")))))
      (unwind-protect
          (ecb-test-no-user-input
            (let ((err (should-error (ecb-activate))))
              (should (string-match-p "ecb-activate-hook" (cadr err))))
            (should-not ecb-minor-mode)
            (dolist (h ecb-test--hooks-installed-by-activation)
              (should-not (memq (cdr h) (symbol-value (car h)))))
            (should-not (get-buffer ecb-directories-buffer-name)))
        (ecb-test-force-deactivate)))))

(ert-deftest ecb-activation-failure-cleans-up ()
  "If something fails during the core activation ECB must leave no trace."
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-tip-of-the-day nil))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'ecb-redraw-layout-full)
                       (lambda (&rest _) (error "simulated layout failure"))))
              (should-error (ecb-test-no-user-input (ecb-activate))))
            (should-not ecb-minor-mode)
            (dolist (h ecb-test--hooks-installed-by-activation)
              (should-not (memq (cdr h) (symbol-value (car h)))))
            (should-not (ecb-test-advised-p 'delete-other-windows))
            (should-not (ecb-test-ecb-timers)))
        (ecb-test-force-deactivate)))))

;;; ----------------------------------------------------------------------
;;; Keymap and menu
;;; ----------------------------------------------------------------------

(ert-deftest ecb-key-map-entries-are-bound-commands ()
  (should (keymapp ecb-mode-map))
  (dolist (entry (cdr ecb-key-map))
    (let ((cmd (nth 2 entry)))
      (should (fboundp cmd))
      (should (commandp cmd))))
  ;; the prefix leads to a keymap in ecb-mode-map
  (should (keymapp (lookup-key ecb-mode-map (kbd (car ecb-key-map))))))

(ert-deftest ecb-key-map-bindings-resolve-in-mode-map ()
  (ecb-test-with-ecb _proj
    (dolist (entry (cdr ecb-key-map))
      (let ((key (kbd (concat (car ecb-key-map) " " (nth 1 entry)))))
        (should (eq (lookup-key ecb-mode-map key) (nth 2 entry)))))))

(ert-deftest ecb-minor-mode-registered-in-minor-mode-alist ()
  (should (assq 'ecb-minor-mode minor-mode-alist))
  (should (assq 'ecb-minor-mode minor-mode-map-alist))
  (should (eq (cdr (assq 'ecb-minor-mode minor-mode-map-alist)) ecb-mode-map)))

(ert-deftest ecb-menu-bar-is-an-easy-menu ()
  (should (consp ecb-menu-bar))
  (should (equal (car ecb-menu-bar) "ECB"))
  ;; every leaf item names an existing command
  (cl-labels ((walk (items)
                (dolist (item items)
                  (cond ((vectorp item)
                         (let ((cmd (aref item 1)))
                           (when (symbolp cmd)
                             (should (fboundp cmd)))))
                        ((and (consp item) (stringp (car item)))
                         (walk (cdr item)))))))
    (walk (cdr ecb-menu-bar))))

(ert-deftest ecb-menu-item-wraps-vector ()
  (should (equal (ecb-menu-item ["Name" cmd t]) ["Name" cmd t]))
  (should (equal (ecb-menu-item '(x)) '(x))))

;;; ----------------------------------------------------------------------
;;; Misc commands
;;; ----------------------------------------------------------------------

(ert-deftest ecb-window-sync-runs-without-error ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (should (equal (ecb-path-selected-source 'file)
                   (file-truename (expand-file-name "src/alpha.el" proj))))))

(ert-deftest ecb-window-sync-non-semantic-buffer-with-imenu-groups ()
  "KNOWN BUG: `ecb-get-tags-for-non-semantic-files' uses `cl-return' inside a
plain `dolist', which establishes no `cl-block'.  As soon as imenu returns a
grouped tag list (e.g. an elisp buffer without an active semantic parser,
where imenu produces a \"Variables\" group) the function exits
non-locally: a `no-catch' throw up to Emacs 30, a `void-variable' error
on Emacs 31 whose `cl-block' expansion changed.  Either way the sync's
`condition-case' swallows it and the methods buffer stays empty."
  :expected-result :failed
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "README.txt" proj))
    ;; text-mode has no semantic parser; give it a grouped imenu index
    (should-not (ecb--semantic-active-p))
    (setq-local imenu-create-index-function
                (lambda () '(("Variables" ("x" . 1)) ("f" . 2))))
    (let ((ecb-non-semantic-exclude-modes nil))
      (should (eq (condition-case nil
                      (catch '--cl-block-nil-- (ecb-window-sync) 'completed)
                    (error 'errored))
                  'completed))
      (should (member "f" (ecb-test-tree-buffer-node-names ecb-methods-buffer-name))))))

(ert-deftest ecb-kill-buffer-hook-tolerates-non-file-buffers ()
  (ecb-test-with-ecb _proj
    (with-temp-buffer
      (ecb-kill-buffer-hook))))

(ert-deftest ecb-customize-opens-buffers ()
  (ecb-test-with-ecb _proj
    (ecb-customize)
    (should (get-buffer "*Customize Group: Ecb*"))
    (kill-buffer "*Customize Group: Ecb*")
    (ecb-customize-most-important)
    (should (get-buffer "*Customize Group: Ecb Most Important*"))
    (kill-buffer "*Customize Group: Ecb Most Important*")))

(ert-deftest ecb-version-is-a-version-string ()
  (should (stringp ecb-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+" ecb-version)))

(ert-deftest ecb-activation-resets-selected-source-between-sessions ()
  "State selected in one session must not leak into the next activation."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (should (ecb-path-selected-source)))
  ;; previous session is gone, fixture buffers killed; a fresh activation
  ;; starts from scratch
  (ecb-test-with-ecb _proj
    (should-not (ecb-path-selected-source))))

(provide 'ecb-test)
;;; ecb-test.el ends here
