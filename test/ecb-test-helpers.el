;;; ecb-test-helpers.el --- shared fixtures for the ECB test-suite -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;;; Commentary:
;;
;; Shared helpers for the ERT test-suite of ECB.  Every test file
;; `require's this library.  The helpers guarantee that:
;;
;; - nothing touches the real home directory, custom-file or user caches;
;; - nothing ever blocks on stdin (all prompts are stubbed);
;; - ECB is deactivated again even when a test body signals;
;; - the batch frame is large enough for every built-in layout.
;;
;; Run the whole suite with `make test' from the ECB root directory.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load ECB from the parent directory of this file, not from any installed
;; copy that happens to be on the load-path.
(defconst ecb-test-dir
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "ecb-test-helpers.el")))
  "Directory containing the ECB tests.")

(defconst ecb-test-root-dir
  (expand-file-name ".." ecb-test-dir)
  "Root directory of the ECB checkout.")

(defconst ecb-test-fixture-dir
  (expand-file-name "fixtures" ecb-test-dir)
  "Directory containing test fixtures.")

(add-to-list 'load-path ecb-test-root-dir)
(add-to-list 'load-path ecb-test-dir)

;; Make sure semantic is up before ECB is loaded so the method browser can
;; parse the fixture sources.
(require 'semantic)
(require 'semantic/bovine/el)
(semantic-mode 1)
;; The elisp parser is only wired up (via mode-local bindings and
;; `emacs-lisp-mode-hook') once semantic/bovine/el is loaded.  In an
;; interactive session that happens lazily; in batch we must force it before
;; the first fixture buffer is created or elisp buffers are parsed by imenu.
(require 'semantic/bovine/el)
(require 'semantic/bovine/c)
;; Idle timers never fire in batch, so parsing must happen synchronously:
;; with the idle scheduler off ECB calls `semantic-fetch-tags' itself.
(global-semantic-idle-scheduler-mode -1)
(setq ecb-force-reparse-when-semantic-idle-scheduler-off t)

;; ecb-autogen.el (required unconditionally by ecb.el) wipes `find-file-hook'
;; whenever Emacs is non-interactive -- a build-time convenience that leaks into
;; every batch use of ECB and silently disables semantic for visited files.
;; Preserve the hook so tests see the same behaviour as an interactive Emacs.
(let ((saved-find-file-hook find-file-hook))
  (require 'ecb)
  (setq find-file-hook saved-find-file-hook))

;; Never show tips or upgrade dialogs during tests.
(setq ecb-tip-of-the-day nil)
;; Layouts are loaded lazily on first activation; tests inspect the registry
;; before that.
(ecb-load-layouts)
(setq ecb-auto-compatibility-check nil)
(setq ecb-version-check nil)

;;; ----------------------------------------------------------------------
;;; Temporary directories and fixture projects
;;; ----------------------------------------------------------------------

(defun ecb-test-make-temp-dir (&optional prefix)
  "Create and return a fresh temporary directory named with PREFIX."
  (let ((dir (make-temp-file (or prefix "ecb-test-") t)))
    ;; Resolve symlinks (e.g. /var -> /private/var on macOS) so that
    ;; comparisons against `ecb-fix-filename' output are stable.
    (file-truename dir)))

(defun ecb-test-copy-fixture (name target-dir)
  "Copy fixture project NAME below TARGET-DIR and return the new path."
  (let ((src (expand-file-name name ecb-test-fixture-dir))
        (dst (expand-file-name name target-dir)))
    (copy-directory src dst nil t t)
    ;; Backup files are gitignored, so create one here to exercise the
    ;; source-file regexps.
    (when (file-directory-p (expand-file-name "src" dst))
      (with-temp-file (expand-file-name "src/alpha.el~" dst) (insert "backup")))
    (file-truename dst)))

(defmacro ecb-test-with-temp-dir (var &rest body)
  "Bind VAR to a fresh temp directory, run BODY, delete the directory."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,var (ecb-test-make-temp-dir)))
     (unwind-protect
         (progn ,@body)
       (ecb-test-kill-buffers-under ,var)
       (ignore-errors (delete-directory ,var t)))))

(defun ecb-test-kill-buffers-under (dir)
  "Kill every file or dired buffer whose file lives below DIR, without asking."
  (let ((dir (file-name-as-directory (file-truename dir))))
    (dolist (buf (buffer-list))
      (let ((file (or (buffer-file-name buf)
                      (with-current-buffer buf
                        (and (derived-mode-p 'dired-mode) default-directory)))))
        (when (and file (string-prefix-p dir (file-truename file)))
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer buf))))))))

(defmacro ecb-test-with-temp-project (var &rest body)
  "Bind VAR to a temporary copy of fixture project-a and run BODY."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tmp (make-symbol "tmp")))
    `(ecb-test-with-temp-dir ,tmp
       (let ((,var (ecb-test-copy-fixture "project-a" ,tmp)))
         ,@body))))

(defmacro ecb-test-with-temp-projects (var-a var-b &rest body)
  "Bind VAR-A and VAR-B to temp copies of project-a and project-b."
  (declare (indent 2) (debug (symbolp symbolp body)))
  (let ((tmp (make-symbol "tmp")))
    `(ecb-test-with-temp-dir ,tmp
       (let ((,var-a (ecb-test-copy-fixture "project-a" ,tmp))
             (,var-b (ecb-test-copy-fixture "project-b" ,tmp)))
         ,@body))))

;;; ----------------------------------------------------------------------
;;; Stubbing user interaction
;;; ----------------------------------------------------------------------

(defmacro ecb-test-no-user-input (&rest body)
  "Run BODY with every interactive prompt stubbed.
Yes/no questions answer yes, string prompts return \"\", completing-read
returns the first candidate (or \"\"), and `read-number' returns 1.  This
guarantees that no test can block on stdin."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
             ((symbol-function 'read-string) (lambda (&rest _) ""))
             ((symbol-function 'read-number) (lambda (&rest _) 1))
             ((symbol-function 'read-from-minibuffer) (lambda (&rest _) ""))
             ((symbol-function 'completing-read)
              (lambda (_prompt coll &rest _)
                (let ((cands (cond ((functionp coll) (all-completions "" coll))
                                   ((hash-table-p coll) (hash-table-keys coll))
                                   (t (mapcar (lambda (c) (if (consp c) (car c) c))
                                              coll)))))
                  (or (car cands) ""))))
             ((symbol-function 'message-box) (lambda (&rest _) nil))
             ((symbol-function 'sit-for) (lambda (&rest _) t))
             ((symbol-function 'read-key-sequence) (lambda (&rest _) "q")))
     ,@body))

;;; ----------------------------------------------------------------------
;;; Activating and deactivating ECB
;;; ----------------------------------------------------------------------

(defconst ecb-test-frame-width 120)
(defconst ecb-test-frame-height 50)

(defun ecb-test-prepare-frame ()
  "Make the batch frame big enough for every layout."
  (set-frame-size (selected-frame) ecb-test-frame-width ecb-test-frame-height)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create "*ecb-test-scratch*")))

(defun ecb-test-force-deactivate ()
  "Deactivate ECB if active, swallowing errors."
  (when (bound-and-true-p ecb-minor-mode)
    (ignore-errors (ecb-test-no-user-input (ecb-deactivate))))
  ;; Belt and braces: if deactivation blew up half way, remove leftovers.
  (when (bound-and-true-p ecb-minor-mode)
    (setq ecb-minor-mode nil))
  (ignore-errors (ecb-make-windows-not-dedicated (selected-frame)))
  (ignore-errors (delete-other-windows)))

(defmacro ecb-test-with-ecb (project-var &rest body)
  "Activate ECB on a temp copy of project-a bound to PROJECT-VAR, run BODY.
ECB is deactivated afterwards even if BODY signals.  Inside BODY the
`ecb-source-path' is the project directory, no compile window is
configured (tests toggle one explicitly), idle timers are disabled, prompts are stubbed and the standard
layout \"left8\" is active."
  (declare (indent 1) (debug (symbolp body)))
  `(ecb-test-with-temp-project ,project-var
     (ecb-test-prepare-frame)
     ;; the edit buffer's default-directory determines what ECB shows first
     (setq default-directory (file-name-as-directory ,project-var))
     (let ((ecb-source-path (list ,project-var))
           (ecb-compile-window-height nil)
           (ecb-compile-window-width 'frame)
           (ecb-layout-name "left8")
           (ecb-tip-of-the-day nil)
           (ecb-auto-compatibility-check nil)
           (ecb-version-check nil)
           (ecb-stealthy-tasks-delay 1000)
           (ecb-basic-buffer-sync-delay nil)
           (ecb-highlight-tag-with-point-delay nil)
           (ecb-compile-window-temporally-enlarge nil)
           (ecb-split-edit-window-after-start nil)
           (ecb-clear-caches-before-activate t)
           ;; ERT visits the test files to report locations; keep them (and
           ;; anything else from the repo) out of the history buffer.
           (ecb-history-exclude-file-regexps
            (list (concat "^" (regexp-quote ecb-test-root-dir)) "TAGS$"))
           (ecb-major-modes-show-or-hide '(nil))
           (ecb-windows-width 0.25)
           (ecb-windows-height 0.33))
       (ecb-test-no-user-input
         (unwind-protect
             (progn
               (ecb-activate)
               (should ecb-minor-mode)
               ,@body)
           (ecb-test-force-deactivate))))))

(defmacro ecb-test-with-compile-window (&rest body)
  "Run BODY inside an active ECB with a 6-line compile window showing.
Must be used inside `ecb-test-with-ecb'."
  (declare (indent 0) (debug (body)))
  `(let ((ecb-compile-window-height 6))
     (ecb-toggle-compile-window 1)
     (should (ecb-compile-window-live-p))
     ,@body))

(defmacro ecb-test-with-layout (name project-var &rest body)
  "Like `ecb-test-with-ecb' but switch to layout NAME first."
  (declare (indent 2) (debug (form symbolp body)))
  `(ecb-test-with-ecb ,project-var
     (ecb-layout-switch ,name)
     ,@body))

;;; ----------------------------------------------------------------------
;;; Inspecting state
;;; ----------------------------------------------------------------------

(defun ecb-test-window-buffer-names (&optional frame)
  "Return the buffer names of all windows of FRAME in `window-list' order."
  (mapcar (lambda (w) (buffer-name (window-buffer w)))
          (window-list frame 'no-minibuf (frame-first-window frame))))

(defun ecb-test-window-tree (&optional frame)
  "Return a canonical sexp describing the window layout of FRAME.
Leaves are buffer names; internal nodes are (vertical|horizontal CHILD...).
Sizes are deliberately omitted so the result is stable across minor font
or frame differences; use `ecb-test-window-sizes' for sizes."
  (cl-labels ((walk (tree)
                (if (windowp tree)
                    (buffer-name (window-buffer tree))
                  (cons (if (car tree) 'vertical 'horizontal)
                        (mapcar #'walk (cddr tree))))))
    (walk (car (window-tree frame)))))

(defun ecb-test-window-sizes (&optional frame)
  "Return an alist of (BUFFER-NAME WIDTH . HEIGHT) for all windows of FRAME."
  (mapcar (lambda (w) (cons (buffer-name (window-buffer w))
                            (cons (window-total-width w) (window-total-height w))))
          (window-list frame 'no-minibuf (frame-first-window frame))))

(defun ecb-test-visible-ecb-buffers ()
  "Return the names of the ECB special buffers currently displayed."
  (cl-remove-if-not (lambda (name) (ecb-buffer-is-ecb-buffer-of-current-layout-p name))
                    (ecb-test-window-buffer-names)))

(defun ecb-test-tree-buffer-lines (buffer-name)
  "Return the lines of tree-buffer BUFFER-NAME as plain strings.
Text properties are stripped and trailing whitespace removed."
  (with-current-buffer buffer-name
    (mapcar (lambda (l) (string-trim-right (substring-no-properties l)))
            (split-string (buffer-string) "\n"))))

(defun ecb-test-tree-buffer-node-names (buffer-name)
  "Return the names of all displayed nodes of tree-buffer BUFFER-NAME."
  (with-current-buffer buffer-name
    (let (names)
      (tree-buffer-map-displayed-nodes
       (lambda (node) (push (substring-no-properties (tree-node->name node)) names)))
      (nreverse names))))

(defun ecb-test-tree-buffer-node-data (buffer-name)
  "Return the data slot of all displayed nodes of tree-buffer BUFFER-NAME."
  (with-current-buffer buffer-name
    (let (data)
      (tree-buffer-map-displayed-nodes
       (lambda (node) (push (tree-node->data node) data)))
      (nreverse data))))

(defun ecb-test-find-displayed-node (buffer-name pred)
  "Return the first displayed node of BUFFER-NAME for which PRED is non-nil."
  (with-current-buffer buffer-name
    (catch 'found
      (tree-buffer-map-displayed-nodes
       (lambda (node) (when (funcall pred node) (throw 'found node))))
      nil)))

(defun ecb-test-strip (string)
  "Return STRING without text properties."
  (substring-no-properties string))

(defun ecb-test-ecb-timers ()
  "Return all timers whose function is an ECB function."
  (cl-remove-if-not
   (lambda (timer)
     (let ((fn (timer--function timer)))
       (and (symbolp fn) (string-prefix-p "ecb-" (symbol-name fn)))))
   (append timer-list timer-idle-list)))

(defun ecb-test-hook-contains-p (hook pred-or-symbol)
  "Return non-nil if HOOK (a symbol) contains a function matching PRED-OR-SYMBOL."
  (let ((value (and (boundp hook) (symbol-value hook))))
    (cl-some (lambda (f)
               (if (functionp pred-or-symbol)
                   (and (not (symbolp pred-or-symbol)) (funcall pred-or-symbol f))
                 (eq f pred-or-symbol)))
             (if (functionp value) (list value) value))))

(defun ecb-test-advised-p (fn)
  "Return non-nil if FN carries any active ECB advice (old or new style)."
  (or (and (fboundp 'ad-is-advised) (ad-is-advised fn) (ad-is-active fn))
      (let (found)
        (advice-mapc (lambda (advice _props)
                       (when (and (symbolp advice)
                                  (string-prefix-p "ecb-" (symbol-name advice)))
                         (setq found t)))
                     fn)
        found)))

;;; ----------------------------------------------------------------------
;;; Custom-file handling for upgrade tests
;;; ----------------------------------------------------------------------

(defmacro ecb-test-with-custom-file (contents &rest body)
  "Run BODY with `custom-file' pointing at a temp file containing CONTENTS.
`user-init-file' is bound to the same file because `custom-file' refuses
to work under \"emacs -q\" (nil `user-init-file')."
  (declare (indent 1) (debug (form body)))
  (let ((file (make-symbol "file")))
    `(let ((,file (make-temp-file "ecb-test-custom-" nil ".el")))
       (unwind-protect
           (progn
             (with-temp-file ,file (insert ,contents))
             (let ((custom-file ,file)
                   (user-init-file ,file))
               ,@body))
         (ignore-errors (delete-file ,file))))))

(defmacro ecb-test-with-saved-option (option value &rest body)
  "Pretend OPTION was customized (saved) with VALUE while running BODY.
Restores all customize-related symbol properties afterwards."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((saved (make-symbol "saved"))
        (customized (make-symbol "customized"))
        (old-val (make-symbol "old-val")))
    `(let ((,saved (get ',option 'saved-value))
           (,customized (get ',option 'customized-value))
           (,old-val (and (boundp ',option) (symbol-value ',option))))
       (unwind-protect
           (progn
             (put ',option 'saved-value (list (list 'quote ,value)))
             (put ',option 'customized-value nil)
             (when (boundp ',option) (set ',option ,value))
             ,@body)
         (put ',option 'saved-value ,saved)
         (put ',option 'customized-value ,customized)
         (when (boundp ',option) (set ',option ,old-val))))))

;;; ----------------------------------------------------------------------
;;; Recording calls
;;; ----------------------------------------------------------------------

(defvar ecb-test-call-log nil
  "List of recorded calls made through `ecb-test-record'.")

(defun ecb-test-record (&rest args)
  "Append ARGS to `ecb-test-call-log'."
  (setq ecb-test-call-log (append ecb-test-call-log (list args))))

(defmacro ecb-test-with-call-log (&rest body)
  "Run BODY with an empty `ecb-test-call-log' and return the log."
  (declare (indent 0) (debug t))
  `(let ((ecb-test-call-log nil))
     ,@body
     ecb-test-call-log))

;;; ----------------------------------------------------------------------
;;; Emacs-version specific breakage (known ECB bugs, see PLAN.md)
;;; ----------------------------------------------------------------------

(defun ecb-test-speedbar-layout-broken-p ()
  "Non-nil if ECB's speedbar integration cannot start in this Emacs.
Since Emacs 31 `speedbar-set-timer' runs inside `speedbar-buffer', but
`ecb-speedbar-activate' calls `speedbar-disable-update' before that buffer
exists, so any layout containing the speedbar window errors while drawing.
Probed at runtime rather than by version so the skip disappears when
either side is fixed."
  (require 'speedbar)
  (let ((speedbar-buffer nil))
    (condition-case nil
        (progn (speedbar-set-timer nil) nil)
      (error t))))

(defun ecb-test-usable-layouts (&optional type)
  "`ecb-available-layouts-of-type' minus layouts known to be broken here."
  (let ((all (ecb-available-layouts-of-type type)))
    (if (ecb-test-speedbar-layout-broken-p)
        (remove "left-dir-plus-speedbar" all)
      all)))

(defun ecb-test-symbol-all-is-a-function-p ()
  "Non-nil when this Emacs defines `all' as a function (Emacs >= 31).
ECB's `ecb-other-window-behavior' defaults to the symbol `all' and
`ecb-get-other-window' tests it with `functionp' first, so on such an
Emacs `other-window' calls `all' with the wrong arguments."
  (functionp 'all))

(provide 'ecb-test-helpers)
;;; ecb-test-helpers.el ends here
