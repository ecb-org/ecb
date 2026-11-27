;;; ecb-misc-modules-test.el --- tests for the smaller ECB modules -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Covers the modules that have little batch-testable surface of their
;; own: ecb-help, ecb-face, ecb-tod, ecb-speedbar (the non-semantic tag
;; helpers), ecb-compatibility, ecb-winman-support, ecb-create-layout
;; (pure helpers), ecb-buffertab, ecb-multiframe, ecb-jde, ecb-eshell,
;; ecb-semantic-wrapper / ecb-cedet-wrapper and ecb-autogen.
;;
;; Modules that depend on third-party packages (jde, escreen, winring,
;; efc) are only checked for loadability and graceful degradation.

;;; Code:

(require 'ecb-test-helpers)
(require 'ediff)
(require 'eshell)
(require 'em-dirs)
(require 'em-hist)
(require 'ecb-buffertab)

;;; ----------------------------------------------------------------------
;;; Loadability of every shipped module
;;; ----------------------------------------------------------------------

(ert-deftest ecb-all-modules-are-loadable ()
  (dolist (feature '(ecb-util ecb-common-browser ecb-layout ecb-layout-defs
                     ecb-file-browser ecb-method-browser ecb-upgrade ecb-compilation
                     ecb-navigate ecb-cycle ecb-help ecb-face ecb-tod ecb-mode-line
                     ecb-compatibility ecb-winman-support ecb-create-layout
                     ecb-speedbar ecb-eshell ecb-symboldef ecb-analyse
                     ecb-semantic-wrapper ecb-cedet-wrapper ecb-semantic ecb-autogen
                     ecb-buffertab ecb))
    (should (featurep feature)))
  ;; ecb-jde is optional (needs JDEE) but must at least load
  (should (require 'ecb-jde nil t)))

;;; ----------------------------------------------------------------------
;;; ecb-help
;;; ----------------------------------------------------------------------

(ert-deftest ecb-help-problem-report-list-all-variables ()
  "Known bug: uses the removed function `user-variable-p' as the predicate
for `all-completions', so building the variable list (and therefore
`ecb-submit-problem-report') signals `void-function'."
  :expected-result :failed
  (let ((vars (ecb-problem-report-list-all-variables)))
    (should (memq 'ecb-layout-name vars))
    (should (memq 'ecb-minor-mode vars))
    (should (memq 'post-command-hook vars))))

(ert-deftest ecb-help-problem-report-post-hook-appends-messages ()
  (with-temp-buffer
    (insert "report body\n")
    (cl-letf (((symbol-function 'ecb-buffer-obj)
               (lambda (name) (get-buffer name))))
      (ecb-problem-report-post-hook))
    (should (string-match-p "report body" (buffer-string)))
    ;; the *Messages* content is appended after the body
    (should (string-match-p "Messages" (buffer-string)))))

(ert-deftest ecb-help-info-handles-missing-file ()
  (let ((called nil))
    (cl-letf (((symbol-function 'info) (lambda (file) (setq called file))))
      (should-error (ecb-info "/no/such/ecb.info"))
      (should (null (ecb-info "/no/such/ecb.info" t)))
      (should (null called))
      (should (ecb-info (expand-file-name "ecb-util.el" ecb-test-root-dir)))
      (should called))))

(ert-deftest ecb-help-show-help-dispatches-format ()
  (let ((info-called nil) (html-called nil))
    (cl-letf (((symbol-function 'ecb-info)
               (lambda (f &rest _) (setq info-called f)))
              ((symbol-function 'ecb-browse-html-file)
               (lambda (f &rest _) (setq html-called f))))
      (let ((ecb-show-help-format 'info))
        (ecb-show-help)
        (should (string-match-p "ecb\\.info\\'" info-called)))
      (let ((ecb-show-help-format 'html))
        (ecb-show-help)
        (should (string-match-p "ecb\\.html\\'" html-called))))))

;;; ----------------------------------------------------------------------
;;; ecb-face
;;; ----------------------------------------------------------------------

(ert-deftest ecb-face-default-builds-display-specs ()
  (let ((spec (ecb-face-default 1.2 t nil nil "black" "white" "gray" "dimgray")))
    (should (= (length spec) 3))
    (should (equal (car (nth 0 spec)) '((class color) (background light))))
    (should (equal (car (nth 1 spec)) '((class color) (background dark))))
    (should (eq (car (nth 2 spec)) t))
    (let ((light (cadr (nth 0 spec))))
      (should (equal (plist-get light :height) 1.2))
      (should (eq (plist-get light :weight) 'bold))
      (should (null (plist-get light :slant)))
      (should (equal (plist-get light :foreground) "black"))
      (should (equal (plist-get light :background) "gray")))
    (should (equal (plist-get (cadr (nth 1 spec)) :foreground) "white"))))

(ert-deftest ecb-face-all-faces-defined-and-options-point-to-faces ()
  (dolist (face '(ecb-default-general-face ecb-default-highlight-face
                  ecb-directory-face ecb-source-face ecb-method-face ecb-history-face
                  ecb-tag-header-face ecb-tree-guide-line-face ecb-bucket-node-face
                  ecb-mode-line-prefix-face ecb-mode-line-data-face ecb-mode-line-win-nr-face
                  ecb-method-non-semantic-face ecb-type-tag-class-face
                  ecb-type-tag-struct-face ecb-symboldef-symbol-face
                  ecb-analyse-general-face ecb-analyse-face))
    (should (facep face)))
  ;; every *-face option holds a defined face (or nil)
  (mapatoms
   (lambda (sym)
     (when (and (string-match-p "\\`ecb-.*-face\\'" (symbol-name sym))
                (get sym 'custom-type)
                (boundp sym))
       (let ((val (symbol-value sym)))
         (when (and val (not (eq val 'use-font-lock-face)))
           (should (facep val))))))))

(ert-deftest ecb-merge-face-into-text-preserves-text ()
  (let ((text (propertize "abc" 'face 'italic)))
    (should (eq (ecb-merge-face-into-text text nil) text))
    (should (null (ecb-merge-face-into-text nil 'bold)))
    (should (equal (ecb-merge-face-into-text text 'bold) "abc"))))

;;; ----------------------------------------------------------------------
;;; ecb-tod
;;; ----------------------------------------------------------------------

(defmacro ecb-tod-test-with-file (&rest body)
  "Run BODY with `ecb-tip-of-the-day-file' pointing at a temp file."
  (declare (indent 0) (debug t))
  `(ecb-test-with-temp-dir dir
     (let ((ecb-tip-of-the-day-file (expand-file-name "tod.el" dir)))
       ,@body)))

(ert-deftest ecb-tod-move-cursor-writes-and-wraps ()
  (ecb-tod-test-with-file
    (ecb-tod-move-cursor 0)
    (should (file-exists-p ecb-tip-of-the-day-file))
    (load-file ecb-tip-of-the-day-file)
    (should (= ecb-tod-cursor 1))
    (ecb-tod-move-cursor (1- (length ecb-tod-tip-list)))
    (load-file ecb-tip-of-the-day-file)
    (should (= ecb-tod-cursor 0))))

(ert-deftest ecb-show-tip-of-the-day-shows-and-advances ()
  (ecb-tod-test-with-file
    (let ((shown nil))
      (cl-letf (((symbol-function 'ecb-message-box)
                 (lambda (msg &rest _) (setq shown msg) t)))
        (let ((ecb-tip-of-the-day nil))
          (ecb-show-tip-of-the-day)
          (should (null shown)))
        (let ((ecb-tip-of-the-day t))
          (ecb-show-tip-of-the-day)
          (should (equal shown (nth 0 ecb-tod-tip-list)))
          (ecb-show-tip-of-the-day)
          (should (equal shown (nth 1 ecb-tod-tip-list))))))))

(ert-deftest ecb-tod-tips-are-non-empty-strings ()
  (should (> (length ecb-tod-tip-list) 10))
  (dolist (tip ecb-tod-tip-list)
    (should (stringp tip))
    (should (> (length tip) 0))))

;;; ----------------------------------------------------------------------
;;; ecb-speedbar (non-semantic tag helpers)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-speedbar-decorate-tag ()
  (let ((txt (ecb-speedbar-decorate-tag '("name" . 10) 'bold)))
    (should (equal txt "name"))
    (should (eq (get-text-property 0 'face txt) 'bold)))
  (should (null (ecb-speedbar-decorate-tag '(42 . 10) 'bold))))

(ert-deftest ecb-create-non-semantic-tree-builds-nodes ()
  (let ((root (tree-node-new-root)))
    (ecb-create-non-semantic-tree
     root
     '(("flat" . 10)
       ("group" ("g-a" . 20) ("g-b" . 30))
       ("posgroup" 40 ("p-a" . 50))))
    (let ((children (tree-node->children root)))
      (should (equal (mapcar #'tree-node->name children) '("flat" "group" "posgroup")))
      ;; a flat tag carries a speedbar-generated semantic tag
      (should (ecb-speedbar-sb-tag-p (tree-node->data (nth 0 children))))
      (should (equal (semantic-tag-name (tree-node->data (nth 0 children))) "flat"))
      ;; a plain group has no data but children
      (should (null (tree-node->data (nth 1 children))))
      (should (equal (mapcar #'tree-node->name (tree-node->children (nth 1 children)))
                     '("g-a" "g-b")))
      ;; a positioned group has data and children
      (should (ecb-speedbar-sb-tag-p (tree-node->data (nth 2 children))))
      (should (equal (mapcar #'tree-node->name (tree-node->children (nth 2 children)))
                     '("p-a"))))))

(ert-deftest ecb-create-non-semantic-tree-rejects-malformed ()
  (should-error (ecb-create-non-semantic-tree (tree-node-new-root) '(42))))

(ert-deftest ecb-create-non-semantic-tree-initial-expand-option ()
  (with-temp-buffer
    (fundamental-mode)
    (let ((ecb-non-semantic-methods-initial-expand '(fundamental-mode)))
      (let ((root (tree-node-new-root)))
        (ecb-create-non-semantic-tree root '(("group" ("a" . 1))))
        (should (tree-node->expanded (car (tree-node->children root))))))
    (let ((ecb-non-semantic-methods-initial-expand nil))
      (let ((root (tree-node-new-root)))
        (ecb-create-non-semantic-tree root '(("group" ("a" . 1))))
        (should-not (tree-node->expanded (car (tree-node->children root))))))))

(ert-deftest ecb-get-tags-for-non-semantic-files-with-grouped-imenu ()
  "Known bug: `ecb-get-tags-for-non-semantic-files' uses `cl-return' inside a
plain `dolist' (no `cl-dolist'/`cl-block'), so as soon as imenu returns a
grouped index the function exits non-locally with `no-catch'."
  :expected-result :failed
  (ecb-test-with-temp-project proj
    (with-current-buffer (find-file-noselect (expand-file-name "README.txt" proj))
      (unwind-protect
          (let ((ecb-non-semantic-exclude-modes nil))
            (setq-local imenu-create-index-function
                        (lambda () '(("Group" ("one" . 1) ("two" . 5)) ("flat" . 9))))
            (should (eq (catch '--cl-block-nil--
                          (ecb-get-tags-for-non-semantic-files)
                          'completed)
                        'completed)))
        (kill-buffer)))))

(ert-deftest ecb-get-tags-for-non-semantic-files-flat-imenu ()
  (ecb-test-with-temp-project proj
    (with-current-buffer (find-file-noselect (expand-file-name "README.txt" proj))
      (unwind-protect
          (let ((ecb-non-semantic-exclude-modes nil))
            (setq-local imenu-create-index-function
                        (lambda () '(("alpha" . 1) ("beta" . 5))))
            (should (equal (mapcar #'car (ecb-get-tags-for-non-semantic-files))
                           '("alpha" "beta")))
            ;; excluded mode -> nil
            (let ((ecb-non-semantic-exclude-modes '(text-mode)))
              (should (null (ecb-get-tags-for-non-semantic-files)))))
        (kill-buffer)))))

(ert-deftest ecb-speedbar-active-p-without-speedbar ()
  (should-not (ecb-speedbar-active-p)))

;;; ----------------------------------------------------------------------
;;; ecb-compatibility
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compatibility-advices-enabled-with-ecb ()
  (should (assq 'ecb-compatibility-advices ecb-adviced-function-sets))
  (ecb-test-with-ecb proj
    (ignore proj)
    (should (ecb-test-advised-p 'bs-show)))
  (should-not (ecb-test-advised-p 'bs-show)))

(ert-deftest ecb-ad-with-originals-restores-definitions ()
  (defun ecb-misc-test-orig () 'orig)
  (unwind-protect
      (progn
        (should (eq (ecb-ad-with-originals (ecb-misc-test-orig)
                      (fset 'ecb-misc-test-orig (lambda () 'changed))
                      (ecb-misc-test-orig))
                    'changed))
        ;; definition restored on exit
        (should (eq (ecb-misc-test-orig) 'orig))
        ;; unknown functions are tolerated
        (should (eq (ecb-ad-with-originals (ecb-misc-test-no-such-fn) 'ok) 'ok)))
    (fmakunbound 'ecb-misc-test-orig)))

(ert-deftest ecb-ediff-compatibility-hooks ()
  (let ((ediff-before-setup-hook nil)
        (ediff-quit-hook nil))
    (ecb-activate-ediff-compatibility)
    (should (memq 'ecb-ediff-before-setup-hook ediff-before-setup-hook))
    (ecb-deactivate-ediff-compatibility)
    (should-not (memq 'ecb-ediff-before-setup-hook ediff-before-setup-hook))))

(ert-deftest ecb-ediff-quit-hook-restores-window-config ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((ecb-run-ediff-in-ecb-frame t)
          (ecb-before-ediff-window-config nil))
      (ecb-ediff-before-setup-hook)
      (should ecb-before-ediff-window-config)
      (ecb-maximize-window-methods)
      (should (= (length (ecb-test-visible-ecb-buffers)) 1))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (ecb-ediff-quit-hook))
      (should (> (length (ecb-test-visible-ecb-buffers)) 1))
      (should (null ecb-before-ediff-window-config)))))

;;; ----------------------------------------------------------------------
;;; ecb-winman-support
;;; ----------------------------------------------------------------------

(ert-deftest ecb-winman-advice-sets-exist ()
  (should (assq 'ecb-winman-escreen-adviced-functions ecb-adviced-function-sets))
  (should (assq 'ecb-winman-winring-adviced-functions ecb-adviced-function-sets)))

(ert-deftest ecb-winman-enable-without-library-errors-cleanly ()
  (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
    (should-error (ecb-winman-escreen-enable-support))
    (should-error (ecb-winman-winring-enable-support)))
  ;; disabling is always safe
  (ecb-winman-escreen-disable-support)
  (ecb-winman-winring-disable-support)
  (should-not (ecb-test-advised-p 'escreen-save-current-screen-configuration)))

;;; ----------------------------------------------------------------------
;;; ecb-create-layout (pure helpers)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-create-layout-buf-types-management ()
  (let ((ecb-create-layout-buf-types nil))
    (ecb-create-layout-add-to-buf-types "methods")
    (ecb-create-layout-add-to-buf-types "directories")
    (ecb-create-layout-add-to-buf-types "not-a-type")
    (ecb-create-layout-add-to-buf-types 'methods)
    (should (equal ecb-create-layout-buf-types '("directories" "methods")))
    (ecb-create-layout-remove-from-buf-type "directories")
    (should (equal ecb-create-layout-buf-types '("methods")))))

(ert-deftest ecb-create-layout-all-buf-types-have-setters ()
  (dolist (type ecb-create-layout-all-buf-types)
    (should (fboundp (intern (format "ecb-set-%s-buffer" type))))))

(ert-deftest ecb-create-layout-gen-lisp-accumulates ()
  (let ((ecb-create-layout-generated-lisp nil))
    (ecb-create-layout-gen-lisp '(a))
    (ecb-create-layout-gen-lisp '(b))
    (should (equal ecb-create-layout-generated-lisp '((b) (a))))
    (ecb-create-layout-gen-lisp-for-buffer-type "methods")
    (should (equal (car ecb-create-layout-generated-lisp)
                   '(if (fboundp 'ecb-set-methods-buffer)
                        (ecb-set-methods-buffer)
                      (ecb-set-default-ecb-buffer))))))

(ert-deftest ecb-create-layout-buffer-type-and-factor-properties ()
  (with-temp-buffer
    (insert "x")
    (should (null (ecb-create-layout-buffer-type)))
    (ecb-create-layout-set-buffer-type "methods")
    (ecb-create-layout-set-buffer-factor 0.5)
    (should (equal (ecb-create-layout-buffer-type) "methods"))
    (should (equal (ecb-create-layout-buffer-factor) 0.5))))

(ert-deftest ecb-create-layout-file-header-and-save-readiness ()
  (with-temp-buffer
    (ecb-create-layout-insert-file-header)
    (should (string-match-p "ecb-user-layouts" (buffer-string)))
    (should (string-match-p "DO NOT EDIT" (buffer-string))))
  (let ((ecb-create-layout-buf-types nil))
    (cl-letf (((symbol-function 'ecb-create-layout-edit-window-selected-p) (lambda () t)))
      ;; nothing typed yet -> all types still available -> not ready
      (should-not (ecb-create-layout-ready-for-save-p)))))

;;; ----------------------------------------------------------------------
;;; ecb-buffertab / ecb-multiframe / ecb-jde / ecb-eshell
;;; ----------------------------------------------------------------------

(ert-deftest ecb-buffertab-make-menu-lists-compilation-buffers ()
  (ecb-test-with-ecb proj
    (ignore proj)
    (get-buffer-create "*Help*")
    (let ((menu (ecb-buffertab-make-menu "Compilation")))
      (should (memq 'keymap menu))
      (should (member "Compilation" menu))
      (should (cl-some (lambda (e) (and (consp e) (equal (car-safe (car e)) "*Help*")))
                       menu)))))

(ert-deftest ecb-buffertab-make-menu-is-a-valid-keymap ()
  "Known bug: the APPEND argument of `add-to-list' is outside the call's
parens, so entries are prepended and the result no longer starts with
`keymap'."
  :expected-result :failed
  (ecb-test-with-ecb proj
    (ignore proj)
    (let ((menu (ecb-buffertab-make-menu "Compilation")))
      (should (keymapp menu))
      (should (equal (cadr menu) "Compilation")))))

(defun ecb-misc-test-run-in-subprocess (form)
  "Evaluate FORM in a fresh batch Emacs with ECB on the load-path.
Return (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((code (call-process
                 (concat invocation-directory invocation-name) nil t nil
                 "-Q" "--batch"
                 "-L" ecb-test-root-dir "-L" ecb-test-dir
                 "--eval" (prin1-to-string form))))
      (cons code (buffer-string)))))

(ert-deftest ecb-multiframe-loads-and-overrides-deactivation ()
  "ecb-multiframe is an opt-in experiment that redefines
`ecb-deactivate-internal' at load time, so exercise it in a subprocess."
  (let ((result (ecb-misc-test-run-in-subprocess
                 '(progn
                    (require 'ecb)
                    (require 'ecb-multiframe)
                    (princ (format "HOOK=%s\n"
                                   (and (memq 'ecb-multiframe-activate-hook
                                              ecb-activate-hook) t)))
                    (princ (format "DEACT-ARITY=%S\n"
                                   (func-arity 'ecb-deactivate-internal)))))))
    (should (zerop (car result)))
    (should (string-match-p "HOOK=t" (cdr result)))
    (should (string-match-p "DEACT-ARITY=(0 \\. 0)" (cdr result)))))

(ert-deftest ecb-multiframe-setup-buffer-name-references-unbound-frame ()
  "Known bug: `ecb-multiframe-setup-buffer-name' calls
`set-frame-parameter' with the free variable `frame', so it signals
`void-variable' whenever it is called."
  :expected-result :failed
  (let ((result (ecb-misc-test-run-in-subprocess
                 '(progn
                    (require 'ecb)
                    (require 'ecb-multiframe)
                    (princ (ecb-multiframe-setup-buffer-name
                            'ecb-misc-test-buffer-name "*T <%s>*"))))))
    (should (zerop (car result)))
    (should (string-match-p "\\*T <[0-9]+>\\*" (cdr result)))))

(ert-deftest ecb-jde-source-path-hook-management ()
  (defvar jde-sourcepath)
  (let ((ecb-source-path-functions nil)
        (jde-sourcepath '("/a" "/b")))
    (cl-letf (((symbol-function 'jde-normalize-path) #'identity)
              ((symbol-function 'ecb-update-directories-buffer) #'ignore))
      (should (equal (ecb-jde-get-source-path) '("/a" "/b")))
      (let ((ecb-jde-set-directories-buffer-to-jde-sourcepath 'add))
        (ecb-jde-update-ecb-source-paths)
        (should (memq 'ecb-jde-get-source-path ecb-source-path-functions)))
      (let ((ecb-jde-set-directories-buffer-to-jde-sourcepath nil))
        (ecb-jde-update-ecb-source-paths)
        (should-not (memq 'ecb-jde-get-source-path ecb-source-path-functions)))
      (let ((ecb-jde-set-directories-buffer-to-jde-sourcepath 'replace)
            (ecb-source-path nil))
        (ecb-jde-update-ecb-source-paths)
        (should (equal ecb-source-path '("/a" "/b")))))))

(ert-deftest ecb-eshell-integration-enables-and-disables-advices ()
  (should (assq 'ecb-eshell-adviced-functions ecb-adviced-function-sets))
  (ecb-eshell-activate-integration)
  (unwind-protect
      (should (ecb-test-advised-p 'eshell))
    (ecb-eshell-deactivate-integration))
  (should-not (ecb-test-advised-p 'eshell)))

(ert-deftest ecb-eshell-buffer-sync-noop-without-compile-window ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    ;; no compile window -> nothing happens and nothing errors
    (should-not (ecb-compile-window-live-p))
    (ecb-eshell-buffer-sync t)))

(ert-deftest ecb-eshell-buffer-sync-changes-eshell-directory ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (ecb-test-with-compile-window
      (let* ((eshell-dir (ecb-test-make-temp-dir "ecb-eshell"))
             (eshell-directory-name (file-name-as-directory eshell-dir))
             (eshell-buffer-name "*ecb-test-eshell*")
             (eshell-history-file-name nil)
             (eshell-last-dir-ring-file-name nil)
             (eshell-login-script nil)
             (eshell-rc-script nil)
             (eshell-modules-list '(eshell-dirs eshell-hist eshell-prompt))
             (esh (save-window-excursion (eshell))))
        (unwind-protect
            (progn
              (should (memq esh ecb-eshell-buffer-list))
              (set-window-buffer ecb-compile-window esh)
              (ecb-eshell-buffer-sync t)
              (should (equal (file-truename (with-current-buffer esh default-directory))
                             (file-truename (file-name-as-directory
                                             (expand-file-name "src" proj))))))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer esh))
          (delete-directory eshell-dir t))))))

;;; ----------------------------------------------------------------------
;;; ecb-semantic-wrapper / ecb-cedet-wrapper / ecb-semantic / ecb-autogen
;;; ----------------------------------------------------------------------

(defconst ecb-misc-test-stale-semantic-aliases '(semantic-tag-type-superclass)
  "Semantic 2.X names in the wrapper alists that no longer exist.")

(ert-deftest ecb-semantic-wrapper-aliases-resolve-to-semantic-2 ()
  (dolist (alist (list ecb--semantic-function-alist
                       ecb--semantic-format-function-alist
                       ecb--semanticdb-function-alist))
    (dolist (elem alist)
      (unless (memq (cdr elem) ecb-misc-test-stale-semantic-aliases)
        (let ((wrapper (intern (concat "ecb--" (symbol-name (cdr elem))))))
          (should (fboundp wrapper))
          ;; every semantic 2.X function must exist in this Emacs
          (should (fboundp (cdr elem)))
          (should (eq (indirect-function wrapper)
                      (indirect-function (cdr elem)))))))))

(ert-deftest ecb-semantic-wrapper-alist-has-no-stale-entries ()
  "Known bug: `ecb--semantic-function-alist' maps
`semantic-token-type-parent-superclass' to the nonexistent
`semantic-tag-type-superclass' (the real name is plural), so the generated
`ecb--semantic-tag-type-superclass' alias points at a void function."
  :expected-result :failed
  (dolist (elem ecb--semantic-function-alist)
    (should (fboundp (cdr elem)))))

(ert-deftest ecb-cedet-wrapper-function-list-available ()
  (dolist (fn ecb--cedet-function-list)
    (should (fboundp fn)))
  (dolist (fn ecb--semantic-format-function-list)
    (should (fboundp fn))))

(ert-deftest ecb-cedet-require-maps-old-names ()
  (should (eq (ecb-cedet-require 'semantic) 'semantic))
  (should (featurep 'semantic))
  (should-error (ecb-cedet-require 'ecb-no-such-cedet-lib)))

(ert-deftest ecb-semantic-helpers-on-c-source ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-window-sync)
    (let ((tag (car (semantic-find-tags-by-name "helper" (semantic-fetch-tags)))))
      (should tag)
      (should (ecb--semantic-tag-with-position-p tag))
      (should (equal (ecb--semantic-tag-name tag) "helper"))
      (should (eq (ecb--semantic-tag-class tag) 'function))
      (should (eq (ecb-semantic-tag-buffer tag) (current-buffer)))
      (should (= (ecb-semantic-tag-start tag)
                 (save-excursion (goto-char (point-min))
                                 (search-forward "static int helper")
                                 (match-beginning 0)))))))

(ert-deftest ecb-autogen-update-autoloads-generates-file ()
  (ecb-test-with-temp-dir dir
    ;; work on a copy of the two sources that carry autoload cookies
    (dolist (f '("ecb.el" "ecb-util.el"))
      (copy-file (expand-file-name f ecb-test-root-dir) (expand-file-name f dir)))
    (let ((default-directory (file-name-as-directory dir))
          (out (expand-file-name ecb-autogen-file dir)))
      (ecb-update-autoloads)
      (should (file-exists-p out))
      (ecb-test-kill-buffers-under dir)
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "ecb-activate" (buffer-string)))
        (should (string-match-p "ecb-byte-compile" (buffer-string)))))))

(ert-deftest ecb-autogen-does-not-install-find-file-hook ()
  "Known bug: loading ecb-autogen.el does `(set 'find-file-hook nil)' at
top level, clobbering the user's hook list."
  :expected-result :failed
  (let ((marker (lambda () nil))
        (saved find-file-hook))
    (add-hook 'find-file-hook marker)
    (unwind-protect
        (progn
          (load (expand-file-name "ecb-autogen.el" ecb-test-root-dir) nil t)
          (should (memq marker find-file-hook)))
      ;; undo the clobbering so later tests are unaffected
      (setq find-file-hook saved))))

(provide 'ecb-misc-modules-test)
;;; ecb-misc-modules-test.el ends here
