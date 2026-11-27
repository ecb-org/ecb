;;; ecb-upgrade-test.el --- tests for ecb-upgrade.el -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Version string handling, the individual option-conversion functions,
;; the upgrade driver (`ecb-option-upgrade', not-compatible / renamed
;; option handling) and the options-version bookkeeping.  Nothing here
;; touches the real custom-file: `ecb-custom-file-writeable-p' is stubbed
;; to nil so saving degrades to `customize-set-variable'.

;;; Code:

(require 'ecb-test-helpers)

(defmacro ecb-up-test-preserving (symbols &rest body)
  "Run BODY, then restore value and customize properties of SYMBOLS."
  (declare (indent 1) (debug (form body)))
  (let ((snap (make-symbol "snap")))
    `(let ((,snap (mapcar (lambda (s)
                            (list s (boundp s) (and (boundp s) (symbol-value s))
                                  (get s 'saved-value) (get s 'customized-value)))
                          ,symbols)))
       (unwind-protect
           (cl-letf (((symbol-function 'ecb-custom-file-writeable-p) (lambda () nil)))
             ,@body)
         (dolist (e ,snap)
           (cl-destructuring-bind (s bound val saved customized) e
             (if bound (set s val) (makunbound s))
             (put s 'saved-value saved)
             (put s 'customized-value customized)))))))

;;; ----------------------------------------------------------------------
;;; Version strings
;;; ----------------------------------------------------------------------

(ert-deftest ecb-package-version-str2list-parses ()
  (should (equal (ecb-package-version-str2list "2.52") '(2 52 3 0)))
  (should (equal (ecb-package-version-str2list "2.40.1") '(2 40 4 1)))
  (should (equal (ecb-package-version-str2list "2.32alpha3") '(2 32 0 3)))
  (should (equal (ecb-package-version-str2list "2.32beta1") '(2 32 1 1)))
  (should (equal (ecb-package-version-str2list "2.32pre2") '(2 32 2 2)))
  (should (equal (ecb-package-version-str2list "1.4") '(1 4 3 0)))
  (should (null (ecb-package-version-str2list "2")))
  (should (null (ecb-package-version-str2list "2.a")))
  (should (null (ecb-package-version-str2list "v2.52"))))

(ert-deftest ecb-package-version-list<-ordering ()
  (should (ecb-package-version-list< '(2 40 3 0) '(2 52 3 0)))
  (should (ecb-package-version-list< '(1 99 3 0) '(2 0 3 0)))
  ;; alpha < beta < pre < release < dotted
  (should (ecb-package-version-list< '(2 32 0 9) '(2 32 1 0)))
  (should (ecb-package-version-list< '(2 32 1 0) '(2 32 2 0)))
  (should (ecb-package-version-list< '(2 32 2 0) '(2 32 3 0)))
  (should (ecb-package-version-list< '(2 32 3 0) '(2 32 4 1)))
  (should (ecb-package-version-list< '(2 32 4 1) '(2 32 4 2)))
  (should-not (ecb-package-version-list< '(2 52 3 0) '(2 52 3 0)))
  (should-not (ecb-package-version-list< '(2 52 3 0) '(2 40 3 0))))

(ert-deftest ecb-package-version-string<-and-roundtrip ()
  (should (ecb-package-version-string< "2.40" "2.52"))
  (should (ecb-package-version-string< "2.52pre1" "2.52"))
  (should-not (ecb-package-version-string< "2.52" "2.52"))
  (dolist (v '("2.52" "2.40.1" "2.32alpha3" "2.32beta1" "2.32pre2"))
    (should (equal (ecb-package-version-list2str (ecb-package-version-str2list v)) v))))

(ert-deftest ecb-version-is-well-formed ()
  (should (ecb-package-version-str2list ecb-version))
  (should (equal ecb-options-version ecb-version)))

(ert-deftest ecb-upgrade-news-is-sorted-and-parsable ()
  (let ((versions (mapcar #'car ecb-upgrade-news)))
    (dolist (v versions) (should (ecb-package-version-str2list v)))
    ;; newest first
    (cl-loop for (a b) on versions while b
             do (should (ecb-package-version-string< b a)))))

;;; ----------------------------------------------------------------------
;;; Conversion functions
;;; ----------------------------------------------------------------------

(ert-deftest ecb-upgrade-compile-window-temporally-enlarge ()
  (should (eq (ecb-upgrade-compile-window-temporally-enlarge t) 'after-display))
  (should (eq (ecb-upgrade-compile-window-temporally-enlarge 'after-compilation) 'after-display))
  (should (null (ecb-upgrade-compile-window-temporally-enlarge nil)))
  (should (eq (ecb-upgrade-compile-window-temporally-enlarge 'both) 'both))
  (should (eq (ecb-upgrade-compile-window-temporally-enlarge 'after-selection) 'after-selection))
  (should (eq (ecb-upgrade-compile-window-temporally-enlarge 'bogus) 'ecb-no-upgrade-conversion)))

(ert-deftest ecb-upgrade-window-sync ()
  (should (equal (ecb-upgrade-window-sync t)
                 (ecb-option-get-value 'ecb-window-sync 'standard-value)))
  (should (null (ecb-upgrade-window-sync nil))))

(ert-deftest ecb-upgrade-layout-nr ()
  (should (equal (ecb-upgrade-layout-nr2name nil) "left8"))
  (should (equal (ecb-upgrade-layout-nr2name 0) "left1"))
  (should (equal (ecb-upgrade-layout-nr2name 20) "speedbar1"))
  (should (null (ecb-upgrade-layout-nr2name 99)))
  (should (equal (ecb-upgrade-layout-nr 9) "left8"))
  (should (eq (ecb-upgrade-layout-nr 99) 'ecb-no-upgrade-conversion))
  (should (equal (ecb-upgrade-toggle-layout-sequence '(0 1 18)) '("left1" "left2" "leftright1")))
  ;; every target layout name (except nr. 20, see next test) must exist
  (dolist (n (number-sequence 0 19))
    (should (ecb-available-layouts-member-p (ecb-upgrade-layout-nr2name n)))))

(ert-deftest ecb-upgrade-layout-nr-20-maps-to-missing-layout ()
  "Known bug: old layout number 20 is mapped to \"speedbar1\", which is not
defined in ecb-layout-defs.el anymore, so the upgrade produces an unusable
`ecb-layout-name'."
  :expected-result :failed
  (should (ecb-available-layouts-member-p (ecb-upgrade-layout-nr2name 20))))

(ert-deftest ecb-upgrade-major-modes-activate ()
  (should (eq (ecb-upgrade-major-modes-activate 'foo) 'foo))
  (let ((old '((c-mode . 0) (java-mode . "left8") emacs-lisp-mode)))
    (should (equal (ecb-upgrade-major-modes-activate old)
                   '((c-mode . "left1") (java-mode . "left8") emacs-lisp-mode)))
    ;; input not mutated
    (should (equal old '((c-mode . 0) (java-mode . "left8") emacs-lisp-mode)))))

(ert-deftest ecb-upgrade-use-speedbar-for-directories ()
  (should (eq (ecb-upgrade-use-speedbar-for-directories t) 'dir))
  (should (null (ecb-upgrade-use-speedbar-for-directories nil))))

(ert-deftest ecb-upgrade-cache-directory-contents ()
  (should (equal (ecb-upgrade-cache-directory-contents '(("/usr" 50) ("/tmp" 10)))
                 '(("/usr" . 50) ("/tmp" . 10)))))

(ert-deftest ecb-upgrade-truncate-lines ()
  (let ((all '(ecb-directories-buffer-name ecb-sources-buffer-name
               ecb-methods-buffer-name ecb-history-buffer-name)))
    (should (equal (ecb-upgrade-truncate-lines t) all))
    (should (null (ecb-upgrade-truncate-lines nil)))
    (should (equal (ecb-upgrade-truncate-lines '(t nil t nil))
                   '(ecb-methods-buffer-name ecb-directories-buffer-name)))
    (should (equal (ecb-upgrade-truncate-lines 'anything) all))))

(ert-deftest ecb-upgrade-alway-operate-in-edit-window ()
  (let ((old '(switch-to-buffer switch-to-buffer-other-window display-buffer)))
    (should (equal (ecb-upgrade-alway-operate-in-edit-window old)
                   '(switch-to-buffer display-buffer)))
    (should (equal old '(switch-to-buffer switch-to-buffer-other-window display-buffer)))))

(ert-deftest ecb-upgrade-mode-line-prefixes-and-data ()
  (should (equal (ecb-upgrade-mode-line-prefixes '(a b c d))
                 '((ecb-directories-buffer-name . a) (ecb-sources-buffer-name . b)
                   (ecb-methods-buffer-name . c) (ecb-history-buffer-name . d))))
  (should (equal (ecb-upgrade-mode-line-data '(selected selected selected x))
                 '((ecb-directories-buffer-name . sel-dir) (ecb-sources-buffer-name . sel-dir)
                   (ecb-methods-buffer-name . sel-source) (ecb-history-buffer-name . x))))
  (should (equal (ecb-upgrade-mode-line-data '(nil y nil nil))
                 '((ecb-directories-buffer-name) (ecb-sources-buffer-name . y)
                   (ecb-methods-buffer-name) (ecb-history-buffer-name)))))

(ert-deftest ecb-upgrade-menu-extensions ()
  (should (equal (ecb-upgrade-menu-extension '((fn "Name") (fn2 "Other")))
                 '(("Name" fn) ("Other" fn2))))
  (dolist (fn '(ecb-upgrade-directories-menu-ext ecb-upgrade-sources-menu-ext
                ecb-upgrade-methods-menu-ext ecb-upgrade-history-menu-ext))
    (let ((result (funcall fn '((my-fn "Mine")))))
      (should (equal (car result) '("Mine" my-fn))))))

(ert-deftest ecb-upgrade-token-display-function ()
  (let ((old '((default . semantic-uml-prototype-nonterminal)
               (c-mode . ecb-name-nonterminal)
               (java-mode . my-own-fn))))
    (should (equal (ecb-upgrade-token-display-function old)
                   '((default . ecb--semantic-format-tag-uml-prototype)
                     (c-mode . ecb-format-tag-name)
                     (java-mode . my-own-fn))))
    (should (eq (cdar old) 'semantic-uml-prototype-nonterminal))))

(ert-deftest ecb-upgrade-type-token-display ()
  (let ((old '((default (class nil ecb-type-token-class-face)
                        (struct t ecb-type-token-struct-face)
                        (enum nil other-face)))))
    (should (equal (ecb-upgrade-type-token-display old)
                   '((default (class nil ecb-type-tag-class-face)
                              (struct t ecb-type-tag-struct-face)
                              (enum nil other-face)))))))

(ert-deftest ecb-upgrade-post-process-semantic-tokenlist-and-taglist ()
  (should (equal (ecb-upgrade-post-process-semantic-tokenlist
                  '((c++-mode . ecb-group-function-tokens-with-parents) (x . y)))
                 '((c++-mode . ecb-group-function-tags-with-parents) (x . y))))
  (should (equal (ecb-upgrade-post-process-semantic-taglist
                  '((c++-mode . ecb-group-function-tags-with-parents) (x)))
                 '((c++-mode ecb-group-function-tags-with-parents) (x)))))

(ert-deftest ecb-upgrade-token-visit-post-actions ()
  (should (equal (ecb-upgrade-token-visit-post-actions
                  '((default ecb-token-visit-smart-token-start ecb-token-visit-recenter)
                    (java-mode my-fn ecb-token-visit-narrow-token)))
                 '((default ecb-tag-visit-smart-tag-start ecb-tag-visit-recenter)
                   (java-mode my-fn ecb-tag-visit-narrow-tag)))))

(ert-deftest ecb-upgrade-token-header-face ()
  (should (eq (ecb-upgrade-token-header-face 'ecb-token-header-face) 'ecb-tag-header-face))
  (should (eq (ecb-upgrade-token-header-face 'highlight) 'highlight)))

(ert-deftest ecb-upgrade-simple-value-conversions ()
  (should (eq (ecb-upgrade-split-edit-window t) 'before-activation))
  (should (eq (ecb-upgrade-split-edit-window 'horizontal) 'horizontal))
  (should (eq (ecb-upgrade-other-window-jump-behavior 'all) 'all))
  (should (equal (ecb-upgrade-other-window-jump-behavior 'foo)
                 (ecb-option-get-value 'ecb-other-window-behavior 'standard-value)))
  (should (equal (ecb-upgrade-show-tags 'whatever)
                 (ecb-option-get-value 'ecb-show-tags 'standard-value)))
  (should (eq (ecb-upgrade-sort-history-items t) ecb-sources-sort-method))
  (should (null (ecb-upgrade-sort-history-items nil)))
  (should (equal (ecb-upgrade-excluded-directories-regexp "^CVS$") '("^CVS$")))
  (should (equal (ecb-upgrade-exclude-parents-regexp "^Object$") '("^Object$")))
  (should (null (ecb-upgrade-exclude-parents-regexp nil)))
  (should (eq (ecb-upgrade-auto-expand-tag-tree-collapse-other t) 'only-if-on-tag))
  (should (null (ecb-upgrade-auto-expand-tag-tree-collapse-other nil)))
  (dolist (fn '(ecb-upgrade-prescan-directories-for-emptyness
                ecb-upgrade-sources-perform-read-only-check
                ecb-upgrade-vc-enable-support))
    (should (eq (funcall fn t) 'unless-remote))
    (should (null (funcall fn nil)))))

(ert-deftest ecb-upgrade-source-file-regexps ()
  (should (equal (ecb-upgrade-source-file-regexps '((".*" "~$" "^Makefile")))
                 '((".*" ("~$") ("^Makefile"))))))

(ert-deftest ecb-upgrade-tree-image-icons-directories ()
  (should (equal (ecb-upgrade-tree-image-icons-directories '("/def" "/dir" nil "/meth" nil))
                 '("/def" (ecb-directories-buffer-name . "/dir")
                   (ecb-methods-buffer-name . "/meth"))))
  (should (equal (ecb-upgrade-tree-image-icons-directories '("/def" nil nil nil nil))
                 '("/def"))))

(ert-deftest ecb-upgrade-tree-RET-selects-edit-window ()
  "Buffers *not* in the old list become the ones that do not leave the window."
  (should (equal (ecb-upgrade-tree-RET-selects-edit-window
                  (list ecb-directories-buffer-name ecb-methods-buffer-name))
                 '(ecb-sources-buffer-name ecb-history-buffer-name ecb-analyse-buffer-name)))
  (should (equal (ecb-upgrade-tree-RET-selects-edit-window nil)
                 '(ecb-directories-buffer-name ecb-sources-buffer-name
                   ecb-methods-buffer-name ecb-history-buffer-name
                   ecb-analyse-buffer-name))))

;;; ----------------------------------------------------------------------
;;; The upgradable-option table itself
;;; ----------------------------------------------------------------------

(ert-deftest ecb-upgradable-option-alist-is-consistent ()
  (dolist (elem ecb-upgradable-option-alist)
    (let ((old (car elem)) (new (nth 1 elem)) (fn (nth 2 elem)))
      (should (symbolp old))
      (should (fboundp fn))
      ;; the target must be a real option of the current ECB
      (should (get new 'custom-type))
      ;; and a same-named entry must really be a type change (needs a converter)
      (when (eq old new)
        (should-not (eq fn 'identity))))))

;;; ----------------------------------------------------------------------
;;; Option compatibility and upgrade driver
;;; ----------------------------------------------------------------------

(ert-deftest ecb-option-compatible-p ()
  (ecb-up-test-preserving '(ecb-windows-width ecb-layout-name)
    (should (ecb-option-compatible-p 'ecb-windows-width))
    (setq ecb-windows-width "wide")
    (should-not (ecb-option-compatible-p 'ecb-windows-width))
    (setq ecb-layout-name 7)
    (should-not (ecb-option-compatible-p 'ecb-layout-name))))

(ert-deftest ecb-get-all-ecb-options-collects-defcustoms ()
  (let ((ecb-all-options nil))
    (ecb-get-all-ecb-options)
    (should (memq 'ecb-layout-name ecb-all-options))
    (should (memq 'ecb-source-path ecb-all-options))
    (should-not (memq 'ecb-minor-mode ecb-all-options))
    (should (cl-every (lambda (s) (get s 'custom-type)) ecb-all-options))))

(ert-deftest ecb-check-not-compatible-options-finds-bad-values ()
  (ecb-up-test-preserving '(ecb-windows-width)
    (let ((ecb-all-options nil) (ecb-not-compatible-options nil))
      (ecb-check-not-compatible-options)
      (should (null ecb-not-compatible-options))
      (setq ecb-windows-width "wide")
      (ecb-check-not-compatible-options)
      (should (equal ecb-not-compatible-options '((ecb-windows-width . "wide")))))))

(ert-deftest ecb-option-upgrade-no-saved-value-is-noop ()
  (ecb-up-test-preserving '(ecb-token-jump-sets-mark ecb-tag-jump-sets-mark)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      (should (null (ecb-option-upgrade 'ecb-token-jump-sets-mark)))
      (should (null (ecb-option-upgrade 'not-an-ecb-option))))))

(ert-deftest ecb-option-upgrade-renamed-option-copies-saved-value ()
  (ecb-up-test-preserving '(ecb-token-jump-sets-mark ecb-tag-jump-sets-mark)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      (ecb-test-with-saved-option ecb-token-jump-sets-mark nil
        (put 'ecb-tag-jump-sets-mark 'saved-value nil)
        (should (equal (ecb-option-upgrade 'ecb-token-jump-sets-mark) '(nil)))
        (should (null ecb-tag-jump-sets-mark))
        ;; `customize-set-variable' leaves a customized-value behind
        (should (get 'ecb-tag-jump-sets-mark 'customized-value))))))

(ert-deftest ecb-option-upgrade-renamed-option-skipped-when-new-already-saved ()
  (ecb-up-test-preserving '(ecb-token-jump-sets-mark ecb-tag-jump-sets-mark)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      (ecb-test-with-saved-option ecb-token-jump-sets-mark nil
        (ecb-test-with-saved-option ecb-tag-jump-sets-mark t
          (should (null (ecb-option-upgrade 'ecb-token-jump-sets-mark)))
          (should (eq ecb-tag-jump-sets-mark t)))))))

(ert-deftest ecb-option-upgrade-renamed-with-conversion ()
  (ecb-up-test-preserving '(ecb-layout-nr ecb-layout-name)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      (ecb-test-with-saved-option ecb-layout-nr 0
        (put 'ecb-layout-name 'saved-value nil)
        (should (equal (ecb-option-upgrade 'ecb-layout-nr) '("left1")))
        (should (equal ecb-layout-name "left1"))))))

(ert-deftest ecb-option-upgrade-failed-conversion-returns-marker ()
  (ecb-up-test-preserving '(ecb-layout-nr ecb-layout-name)
    (let ((ecb-all-options nil) (before ecb-layout-name))
      (ecb-get-all-ecb-options)
      (ecb-test-with-saved-option ecb-layout-nr 999
        (put 'ecb-layout-name 'saved-value nil)
        (should (equal (ecb-option-upgrade 'ecb-layout-nr) '(ecb-no-upgrade-conversion)))
        (should (equal ecb-layout-name before))))))

(ert-deftest ecb-option-upgrade-same-name-type-change ()
  "An option whose type changed is upgraded only while its value is incompatible."
  (ecb-up-test-preserving '(ecb-cache-directory-contents)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      (ecb-test-with-saved-option ecb-cache-directory-contents '(("/usr" 50))
        (should-not (ecb-option-compatible-p 'ecb-cache-directory-contents))
        (should (equal (ecb-option-upgrade 'ecb-cache-directory-contents)
                       '((("/usr" . 50)))))
        (should (equal ecb-cache-directory-contents '(("/usr" . 50))))
        (should (ecb-option-compatible-p 'ecb-cache-directory-contents)))
      ;; compatible saved value -> nothing to do (no double upgrade)
      (ecb-test-with-saved-option ecb-cache-directory-contents '(("/usr" . 50))
        (should (null (ecb-option-upgrade 'ecb-cache-directory-contents)))))))

(ert-deftest ecb-option-upgrade-converter-error-is-no-conversion ()
  (ecb-up-test-preserving '(ecb-cache-directory-contents)
    (let ((ecb-all-options nil))
      (ecb-get-all-ecb-options)
      ;; (nth 0 42) signals inside the converter
      (ecb-test-with-saved-option ecb-cache-directory-contents '(42)
        (should (equal (ecb-option-upgrade 'ecb-cache-directory-contents)
                       '(ecb-no-upgrade-conversion)))))))

(ert-deftest ecb-upgrade-not-compatible-options-resets-or-converts ()
  (ecb-up-test-preserving '(ecb-windows-width ecb-cache-directory-contents)
    (let ((ecb-all-options nil)
          (ecb-not-compatible-options nil))
      (ecb-test-with-saved-option ecb-cache-directory-contents '(("/usr" 50))
        (setq ecb-windows-width "wide")
        (ecb-check-not-compatible-options)
        (should (= (length ecb-not-compatible-options) 2))
        (ecb-upgrade-not-compatible-options)
        ;; no converter -> reset to the standard value
        (should (equal ecb-windows-width
                       (ecb-option-get-value 'ecb-windows-width 'standard-value)))
        ;; converter -> upgraded
        (should (equal ecb-cache-directory-contents '(("/usr" . 50))))))))

(ert-deftest ecb-upgrade-not-compatible-options-downgrade-always-resets ()
  (ecb-up-test-preserving '(ecb-cache-directory-contents)
    (let ((ecb-all-options nil)
          (ecb-not-compatible-options nil)
          (ecb-options-version "9.99"))
      (ecb-test-with-saved-option ecb-cache-directory-contents '(("/usr" 50))
        (ecb-check-not-compatible-options)
        (ecb-upgrade-not-compatible-options)
        (should (equal ecb-cache-directory-contents
                       (ecb-option-get-value 'ecb-cache-directory-contents
                                             'standard-value)))))))

(ert-deftest ecb-upgrade-renamed-options-records-upgrades ()
  (ecb-up-test-preserving '(ecb-token-jump-sets-mark ecb-tag-jump-sets-mark
                            ecb-layout-nr ecb-layout-name)
    (let ((ecb-all-options nil)
          (ecb-not-compatible-options nil)
          (ecb-renamed-options nil))
      (ecb-test-with-saved-option ecb-token-jump-sets-mark nil
        (ecb-test-with-saved-option ecb-layout-nr 2
          (put 'ecb-tag-jump-sets-mark 'saved-value nil)
          (put 'ecb-layout-name 'saved-value nil)
          (ecb-upgrade-renamed-options)
          (should (member '(ecb-token-jump-sets-mark nil ecb-tag-jump-sets-mark nil)
                          ecb-renamed-options))
          (should (member '(ecb-layout-nr 2 ecb-layout-name "left3")
                          ecb-renamed-options))
          (should (equal ecb-layout-name "left3"))
          (should (ecb-not-compatible-or-renamed-options-detected)))))))

(ert-deftest ecb-upgrade-renamed-options-skips-on-downgrade ()
  (ecb-up-test-preserving '(ecb-token-jump-sets-mark ecb-tag-jump-sets-mark)
    (let ((ecb-all-options nil)
          (ecb-not-compatible-options nil)
          (ecb-renamed-options '(stale))
          (ecb-options-version "9.99"))
      (ecb-test-with-saved-option ecb-token-jump-sets-mark nil
        (put 'ecb-tag-jump-sets-mark 'saved-value nil)
        (ecb-upgrade-renamed-options)
        (should (null ecb-renamed-options))))))

(ert-deftest ecb-upgrade-options-is-idempotent ()
  (ecb-up-test-preserving '(ecb-layout-nr ecb-layout-name ecb-cache-directory-contents)
    (let ((ecb-all-options nil)
          (ecb-not-compatible-options nil)
          (ecb-renamed-options nil))
      (ecb-test-with-saved-option ecb-layout-nr 0
        (ecb-test-with-saved-option ecb-cache-directory-contents '(("/usr" 50))
          (put 'ecb-layout-name 'saved-value nil)
          (cl-letf (((symbol-function 'ecb-display-upgraded-options) #'ignore))
            (ecb-upgrade-options)
            (should (equal ecb-layout-name "left1"))
            (should (equal ecb-cache-directory-contents '(("/usr" . 50))))
            (should (ecb-not-compatible-or-renamed-options-detected))
            ;; simulate the user accepting the "save" button: the upgraded
            ;; values become saved values
            (put 'ecb-layout-name 'saved-value (get 'ecb-layout-name 'customized-value))
            (put 'ecb-cache-directory-contents 'saved-value
                 (get 'ecb-cache-directory-contents 'customized-value))
            ;; second run: everything is compatible and the renamed option
            ;; already has a saved value -> nothing reported
            (ecb-upgrade-options)
            (should (null ecb-not-compatible-options))
            (should (null ecb-renamed-options))
            (should-not (ecb-not-compatible-or-renamed-options-detected))))))))

(ert-deftest ecb-display-upgraded-options-builds-report-buffer ()
  (ecb-up-test-preserving '(ecb-windows-width)
    (let ((ecb-not-compatible-options '((ecb-windows-width . "wide")))
          (ecb-renamed-options '((ecb-layout-nr 0 ecb-layout-name "left1"))))
      (unwind-protect
          (progn
            (should (ecb-display-upgraded-options))
            (let ((buf (get-buffer "*ECB upgraded options*")))
              (should buf)
              (with-current-buffer buf
                (should (string-match-p "ecb-windows-width" (buffer-string)))
                (should (string-match-p "ecb-layout-nr" (buffer-string)))
                (should (string-match-p "left1" (buffer-string))))))
        (when (get-buffer "*ECB upgraded options*")
          (kill-buffer "*ECB upgraded options*"))))
    (let ((ecb-not-compatible-options nil) (ecb-renamed-options nil))
      (should-not (ecb-display-upgraded-options)))))

;;; ----------------------------------------------------------------------
;;; Options-version bookkeeping and news
;;; ----------------------------------------------------------------------

(ert-deftest ecb-options-version-bookkeeping ()
  (ecb-up-test-preserving '(ecb-options-version)
    (let ((ecb-old-ecb-version nil))
      (ecb-test-with-saved-option ecb-options-version "2.40"
        (should-not (ecb-options-version=ecb-version-p))
        (ecb-store-current-options-version)
        (should (equal ecb-old-ecb-version "2.40"))
        (should (equal ecb-options-version ecb-version))))
    (let ((ecb-old-ecb-version nil))
      (ecb-test-with-saved-option ecb-options-version ecb-version
        (should (ecb-options-version=ecb-version-p))
        (ecb-store-current-options-version)
        (should (null ecb-old-ecb-version))))))

(ert-deftest ecb-display-news-for-upgrade-only-after-upgrade ()
  (let ((ecb-old-ecb-version nil))
    (put 'ecb-display-news-for-upgrade 'ecb-news-for-upgrade-displayed nil)
    (ecb-display-news-for-upgrade)
    (should-not (get-buffer "*News for the new ECB-version*")))
  (let ((ecb-old-ecb-version "2.32"))
    (unwind-protect
        (progn
          (put 'ecb-display-news-for-upgrade 'ecb-news-for-upgrade-displayed nil)
          (ecb-display-news-for-upgrade)
          (let ((buf (get-buffer "*News for the new ECB-version*")))
            (should buf)
            (with-current-buffer buf
              (should (string-match-p "upgraded ECB from version 2.32" (buffer-string)))
              ;; news of versions newer than 2.32 are included
              (should (string-match-p "^\\* " (buffer-string)))))
          (should (get 'ecb-display-news-for-upgrade 'ecb-news-for-upgrade-displayed)))
      (put 'ecb-display-news-for-upgrade 'ecb-news-for-upgrade-displayed nil)
      (when (get-buffer "*News for the new ECB-version*")
        (kill-buffer "*News for the new ECB-version*")))))

(ert-deftest ecb-customize-save-variable-falls-back-without-custom-file ()
  (ecb-up-test-preserving '(ecb-tip-of-the-day)
    (ecb-customize-save-variable 'ecb-tip-of-the-day nil)
    (should (null ecb-tip-of-the-day))
    (should (get 'ecb-tip-of-the-day 'customized-value))
    (should (null (get 'ecb-tip-of-the-day 'saved-value)))))

(ert-deftest ecb-custom-file-writeable-p-checks-extension-and-permissions ()
  (ecb-test-with-custom-file ";; empty\n"
    (should (ecb-custom-file-writeable-p)))
  (cl-letf (((symbol-function 'ecb-custom-file) (lambda () "/nonexistent/dir/custom.elc")))
    (should-not (ecb-custom-file-writeable-p)))
  (cl-letf (((symbol-function 'ecb-custom-file) (lambda () nil)))
    (should-not (ecb-custom-file-writeable-p))))

(ert-deftest ecb-upgrade-make-copy-of-custom-file-creates-backup ()
  (ecb-test-with-custom-file ";; my settings\n"
    (let ((backup (format "%s.before_ecb_%s" custom-file ecb-version)))
      (unwind-protect
          (progn
            (ecb-upgrade-make-copy-of-custom-file)
            (should (file-exists-p backup))
            ;; a second backup gets a numbered suffix
            (ecb-upgrade-make-copy-of-custom-file)
            (should (file-exists-p (concat backup "__1"))))
        (ignore-errors (delete-file backup))
        (ignore-errors (delete-file (concat backup "__1")))))))

(provide 'ecb-upgrade-test)
;;; ecb-upgrade-test.el ends here
