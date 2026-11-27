;;; ecb-common-browser-test.el --- tests for ecb-common-browser.el -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; The advice-set machinery (legacy `defecb-advice' and the new
;; `ecb-add-to-advice-set'), the ECB buffer registry, source objects,
;; `ecb-fix-filename' and friends.  The advice tests are the gate for the
;; defadvice -> advice-add migration: both entry formats must keep working
;; side by side until every advice is converted.

;;; Code:

(require 'ecb-test-helpers)
(require 'ecb-common-browser)

;;; ----------------------------------------------------------------------
;;; Advice machinery: fixtures
;;; ----------------------------------------------------------------------

;; Plain functions to advise.  Their bodies record into the call log so
;; tests can see whether the advice or the original ran.

(defun ecb-cb-test-orig-1 (x)
  (ecb-test-record 'orig-1 x)
  (* x 2))

(defun ecb-cb-test-orig-2 (x)
  (ecb-test-record 'orig-2 x)
  (+ x 1))

(defun ecb-cb-test-orig-3 (x)
  (ecb-test-record 'orig-3 x)
  x)

(defecb-advice-set ecb-cb-test-set
  "Advice-set used by ecb-common-browser-test.")

(defecb-advice-set ecb-cb-test-permanent-set
  "Permanent advice-set used by ecb-common-browser-test."
  t)

(defvar ecb-cb-test-permanent-pred-result nil)
(defun ecb-cb-test-permanent-pred (_set) ecb-cb-test-permanent-pred-result)

(defecb-advice-set ecb-cb-test-pred-set
  "Advice-set whose permanence is decided by a predicate."
  'ecb-cb-test-permanent-pred)

;; Legacy style advice (defadvice based).
(defecb-advice ecb-cb-test-orig-1 around ecb-cb-test-set
  "Doubles the result."
  (ecb-test-record 'legacy-around (ad-get-arg 0))
  ad-do-it
  (setq ad-return-value (* ad-return-value 10)))

;; New style advice (advice-add based).
(defun ecb-cb-test-new-before (&rest args)
  (ecb-test-record 'new-before (car args)))
(ecb-add-to-advice-set 'ecb-cb-test-orig-2 :before 'ecb-cb-test-new-before 'ecb-cb-test-set)

(defun ecb-cb-test-new-around (orig &rest args)
  (ecb-test-record 'new-around (car args))
  (* 100 (apply orig args)))
(ecb-add-to-advice-set 'ecb-cb-test-orig-3 :around 'ecb-cb-test-new-around 'ecb-cb-test-permanent-set)

(defun ecb-cb-test-new-after (&rest args)
  (ecb-test-record 'new-after (car args)))
(ecb-add-to-advice-set 'ecb-cb-test-orig-3 :after 'ecb-cb-test-new-after 'ecb-cb-test-pred-set)

;; An always-disabled advice for `ecb-with-ecb-advice'.
(defecb-advice ecb-cb-test-orig-2 around ecb-always-disabled-advices
  "Only on demand."
  (ecb-test-record 'always-disabled-around (ad-get-arg 0))
  (setq ad-return-value 'on-demand))

(defun ecb-cb-test-reset-advices ()
  "Make sure every test starts with all test advices disabled."
  (ecb-disable-advices 'ecb-cb-test-set t)
  (ecb-disable-advices 'ecb-cb-test-permanent-set t)
  (ecb-disable-advices 'ecb-cb-test-pred-set t)
  (ecb-enable-ecb-advice 'ecb-cb-test-orig-2 'around -1))

;;; ----------------------------------------------------------------------
;;; Advice machinery: registration
;;; ----------------------------------------------------------------------

(ert-deftest ecb-advice-set-registers-in-global-lists ()
  (should (assq 'ecb-cb-test-set ecb-adviced-function-sets))
  (should (assq 'ecb-cb-test-permanent-set ecb-adviced-function-sets))
  (should (eq (cdr (assq 'ecb-cb-test-permanent-set ecb-adviced-permanent-function-sets)) t))
  (should (eq (cdr (assq 'ecb-cb-test-pred-set ecb-adviced-permanent-function-sets))
              'ecb-cb-test-permanent-pred))
  (should-not (assq 'ecb-cb-test-set ecb-adviced-permanent-function-sets)))

(ert-deftest ecb-advice-entries-have-expected-formats ()
  ;; legacy: (FN . CLASS); new: (FN :CLASS ADVICE-FN)
  (should (member '(ecb-cb-test-orig-1 . around) ecb-cb-test-set))
  (should (member '(ecb-cb-test-orig-2 :before ecb-cb-test-new-before) ecb-cb-test-set))
  (should (member '(ecb-cb-test-orig-1 . around) ecb-adviced-functions))
  (should (member '(ecb-cb-test-orig-2 :before ecb-cb-test-new-before) ecb-adviced-functions)))

(ert-deftest ecb-add-to-advice-set-rejects-unknown-set ()
  (should-error (ecb-add-to-advice-set 'car :before 'ignore 'ecb-cb-no-such-set)))

(ert-deftest ecb-add-to-advice-set-rejects-illegal-class-without-mutating ()
  "KNOWN DEFECT: the class check happens after the entry has already been
pushed onto the set, so an illegal class leaves garbage behind.  Remove
:expected-result once the validation is moved before the mutation."
  :expected-result :failed
  (let ((before (copy-sequence ecb-cb-test-set))
        (before-all (copy-sequence ecb-adviced-functions)))
    (unwind-protect
        (progn
          (should-error (ecb-add-to-advice-set 'car :sideways 'ignore 'ecb-cb-test-set))
          (should (equal ecb-cb-test-set before)))
      ;; undo the pollution so later tests are not affected
      (setq ecb-cb-test-set before
            ecb-adviced-functions before-all))))

(ert-deftest ecb-defecb-advice-rejects-unknown-set-and-class ()
  (let ((before (copy-sequence ecb-cb-test-set))
        (before-all (copy-sequence ecb-adviced-functions)))
    (unwind-protect
        (progn
          (should-error (eval '(defecb-advice car around ecb-cb-no-such-set "d" ad-do-it) t))
          (should-error (eval '(defecb-advice car sideways ecb-cb-test-set "d" ad-do-it) t)))
      (setq ecb-cb-test-set before
            ecb-adviced-functions before-all))))

;;; ----------------------------------------------------------------------
;;; Advice machinery: enabling / disabling
;;; ----------------------------------------------------------------------

(ert-deftest ecb-advices-are-disabled-after-load ()
  (ecb-cb-test-reset-advices)
  (should (equal (ecb-test-with-call-log (should (= (ecb-cb-test-orig-1 2) 4)))
                 '((orig-1 2))))
  (should (equal (ecb-test-with-call-log (should (= (ecb-cb-test-orig-2 2) 3)))
                 '((orig-2 2)))))

(ert-deftest ecb-enable-advices-enables-legacy-and-new-entries ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-set)
        ;; legacy around advice is active
        (should (equal (ecb-test-with-call-log (should (= (ecb-cb-test-orig-1 2) 40)))
                       '((legacy-around 2) (orig-1 2))))
        ;; new before advice is active
        (should (equal (ecb-test-with-call-log (should (= (ecb-cb-test-orig-2 2) 3)))
                       '((new-before 2) (orig-2 2))))
        (should (advice-member-p 'ecb-cb-test-new-before 'ecb-cb-test-orig-2)))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-disable-advices-disables-legacy-and-new-entries ()
  (ecb-cb-test-reset-advices)
  (ecb-enable-advices 'ecb-cb-test-set)
  (ecb-disable-advices 'ecb-cb-test-set)
  (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-1 1)) '((orig-1 1))))
  (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-2 1)) '((orig-2 1))))
  (should-not (advice-member-p 'ecb-cb-test-new-before 'ecb-cb-test-orig-2)))

(ert-deftest ecb-enable-disable-are-idempotent ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-set)
        (ecb-enable-advices 'ecb-cb-test-set)
        ;; the new-style advice must not be stacked twice
        (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-2 1))
                       '((new-before 1) (orig-2 1))))
        (ecb-disable-advices 'ecb-cb-test-set)
        (ecb-disable-advices 'ecb-cb-test-set)
        (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-2 1)) '((orig-2 1)))))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-enable-advices-rejects-unknown-and-always-disabled-sets ()
  (should-error (ecb-enable-advices 'ecb-cb-no-such-set))
  (should-error (ecb-enable-advices 'ecb-always-disabled-advices))
  (should-error (ecb-disable-advices 'ecb-cb-no-such-set)))

(ert-deftest ecb-permanent-set-survives-plain-disable ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-permanent-set)
        (should (= (ecb-cb-test-orig-3 1) 100))
        (ecb-disable-advices 'ecb-cb-test-permanent-set)
        (should (= (ecb-cb-test-orig-3 1) 100))
        (ecb-disable-advices 'ecb-cb-test-permanent-set t)
        (should (= (ecb-cb-test-orig-3 1) 1)))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-predicate-permanent-set ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-pred-set)
        (let ((ecb-cb-test-permanent-pred-result t))
          (ecb-disable-advices 'ecb-cb-test-pred-set)
          (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-3 5))
                         '((orig-3 5) (new-after 5)))))
        (let ((ecb-cb-test-permanent-pred-result nil))
          (ecb-disable-advices 'ecb-cb-test-pred-set)
          (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-3 5))
                         '((orig-3 5))))))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-with-original-adviced-function-set-disables-temporarily ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-set)
        (should (= (ecb-cb-test-orig-1 2) 40))
        (ecb-with-original-adviced-function-set 'ecb-cb-test-set
          (should (= (ecb-cb-test-orig-1 2) 4))
          (should (equal (ecb-test-with-call-log (ecb-cb-test-orig-2 2)) '((orig-2 2))))
          ;; nested call: still original, and leaving the inner call must
          ;; NOT re-enable the advices
          (ecb-with-original-adviced-function-set 'ecb-cb-test-set
            (should (= (ecb-cb-test-orig-1 2) 4)))
          (should (= (ecb-cb-test-orig-1 2) 4)))
        ;; outermost exit re-enables
        (should (= (ecb-cb-test-orig-1 2) 40))
        (should (null (cdr (assq 'ecb-cb-test-set ecb-adviced-function-sets)))))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-with-original-adviced-function-set-restores-on-error ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-set)
        (should-error
         (ecb-with-original-adviced-function-set 'ecb-cb-test-set
           (error "boom")))
        (should (= (ecb-cb-test-orig-1 2) 40))
        (should (null (cdr (assq 'ecb-cb-test-set ecb-adviced-function-sets)))))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-with-original-adviced-function-set-forces-permanent-sets ()
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (progn
        (ecb-enable-advices 'ecb-cb-test-permanent-set)
        (ecb-with-original-adviced-function-set 'ecb-cb-test-permanent-set
          (should (= (ecb-cb-test-orig-3 1) 1)))
        (should (= (ecb-cb-test-orig-3 1) 100)))
    (ecb-cb-test-reset-advices)))

(ert-deftest ecb-with-ecb-advice-enables-on-demand-and-nests ()
  (ecb-cb-test-reset-advices)
  (should (= (ecb-cb-test-orig-2 1) 2))
  (should (eq (ecb-with-ecb-advice 'ecb-cb-test-orig-2 'around
                (ecb-with-ecb-advice 'ecb-cb-test-orig-2 'around
                  (should (eq (ecb-cb-test-orig-2 1) 'on-demand)))
                ;; inner exit must not have disabled it
                (ecb-cb-test-orig-2 1))
              'on-demand))
  (should (= (ecb-cb-test-orig-2 1) 2))
  (should-not (get 'ecb-cb-test-orig-2 'ecb-with-ecb-advice)))

(ert-deftest ecb-with-ecb-advice-restores-on-error ()
  (ecb-cb-test-reset-advices)
  (should-error (ecb-with-ecb-advice 'ecb-cb-test-orig-2 'around (error "boom")))
  (should (= (ecb-cb-test-orig-2 1) 2))
  (should-not (get 'ecb-cb-test-orig-2 'ecb-with-ecb-advice)))

(ert-deftest ecb-with-ecb-advice-rejects-unregistered-advice ()
  (should-error (ecb-with-ecb-advice 'ecb-cb-test-orig-1 'around nil)))

(ert-deftest ecb-advices-debug-error-only-logs-when-enabled ()
  (let ((ecb-advices-debug-error nil))
    (should-not (ecb-advices-debug-error 'f 'around 'calling)))
  (let ((ecb-advices-debug-error t))
    (should (string-match-p "debug calling of .around. advice f"
                            (ecb-advices-debug-error 'f 'around 'calling)))))

(ert-deftest ecb-enable-ecb-advice2-does-not-spam-messages ()
  "KNOWN DEFECT: `ecb-enable-ecb-advice2' still contains a stray
`message' that prints \"Advice enable : ...\" on every activation.
Remove :expected-result once it is deleted."
  :expected-result :failed
  (ecb-cb-test-reset-advices)
  (unwind-protect
      (let ((msgs nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) msgs) nil)))
          (ecb-enable-advices 'ecb-cb-test-set))
        (should-not (cl-some (lambda (m) (string-match-p "Advice enable" m)) msgs)))
    (ecb-cb-test-reset-advices)))

;;; ----------------------------------------------------------------------
;;; Source objects and selected source
;;; ----------------------------------------------------------------------

(ert-deftest ecb-source-make-and-accessors ()
  (should (equal (ecb-source-make "/f") "/f"))
  (should (equal (ecb-source-make "/f" "buf") '("/f" . "buf")))
  (with-temp-buffer
    (should (equal (ecb-source-make "/f" (current-buffer)) (cons "/f" (buffer-name)))))
  (should (equal (ecb-source-get-filename "/f") "/f"))
  (should (equal (ecb-source-get-filename '("/f" . "b")) "/f"))
  (should-not (ecb-source-get-buffername "/f"))
  (should (equal (ecb-source-get-buffername '("/f" . "b")) "b")))

(ert-deftest ecb-source-get-buffer-prefers-live-buffer ()
  (ecb-test-with-temp-project proj
    (let* ((file (expand-file-name "README.txt" proj))
           (buf (find-file-noselect file)))
      (unwind-protect
          (progn
            (should (eq (ecb-source-get-buffer (ecb-source-make file buf)) buf))
            (should (eq (ecb-source-get-buffer file) buf)))
        (kill-buffer buf)))))

(ert-deftest ecb-path-selected-source-set-and-get ()
  (let ((ecb-path-selected-source nil))
    (should-error (ecb-path-selected-source-set nil "b"))
    (with-temp-buffer
      (ecb-path-selected-source-set "/x/y.el" (current-buffer))
      (should (equal (ecb-path-selected-source 'file) "/x/y.el"))
      (should (equal (ecb-path-selected-source 'buffername) (buffer-name)))
      (should (eq (ecb-path-selected-source 'buffer) (current-buffer)))
      (should (equal (ecb-path-selected-source) (cons "/x/y.el" (buffer-name)))))))

;;; ----------------------------------------------------------------------
;;; Buffer registry
;;; ----------------------------------------------------------------------

(ert-deftest ecb-ecb-buffer-registry-add-and-query ()
  (let ((ecb-ecb-buffer-registry nil))
    (ecb-ecb-buffer-registry-add " *T1*" 'ecb-t1-name t 'ecb-set-t1)
    (ecb-ecb-buffer-registry-add " *T2*" 'ecb-t2-name nil)
    (should (equal (sort (ecb-ecb-buffer-registry-name-list) #'string<) '(" *T1*" " *T2*")))
    (should (equal (ecb-ecb-buffer-registry-name-list t) '(" *T1*")))
    (should (equal (ecb-ecb-buffer-registry-symbol-list t) '(ecb-t1-name)))
    (should (eq (ecb-ecb-buffer-registry-get-symbol " *T2*") 'ecb-t2-name))
    (should (eq (ecb-ecb-buffer-registry-get-set-fcn " *T1*") 'ecb-set-t1))
    (should-not (ecb-ecb-buffer-registry-get-set-fcn " *T2*"))
    ;; re-adding an existing name only updates the set-fcn
    (ecb-ecb-buffer-registry-add " *T2*" 'ecb-t2-name nil 'ecb-set-t2)
    (should (= (length ecb-ecb-buffer-registry) 2))
    (should (eq (ecb-ecb-buffer-registry-get-set-fcn " *T2*") 'ecb-set-t2))
    (ecb-ecb-buffer-registry-init)
    (should-not ecb-ecb-buffer-registry)))

(ert-deftest ecb-ecb-buffer-registry-buffer-list-only-live-buffers ()
  (let ((ecb-ecb-buffer-registry nil)
        (b (get-buffer-create " *ecb-registry-live*")))
    (unwind-protect
        (progn
          (ecb-ecb-buffer-registry-add " *ecb-registry-live*" 'x t)
          (ecb-ecb-buffer-registry-add " *ecb-registry-dead*" 'y t)
          (should (equal (ecb-ecb-buffer-registry-buffer-list) (list b))))
      (kill-buffer b))))

(ert-deftest ecb-tree-buffer-callbacks-add-and-lookup ()
  (let ((ecb-tree-buffer-callbacks (list (cons 'expand nil) (cons 'select nil))))
    (ecb-tree-buffer-callbacks-add 'select 'ecb-foo-buffer-name 'ecb-foo-select)
    (ecb-tree-buffer-callbacks-add 'expand 'ecb-foo-buffer-name 'ecb-foo-expand)
    (should (equal (ecb-tree-buffer-callbacks-alist-of-type 'select)
                   '((ecb-foo-buffer-name . ecb-foo-select))))
    (should (equal (ecb-tree-buffer-callbacks-alist-of-type 'expand)
                   '((ecb-foo-buffer-name . ecb-foo-expand))))
    ;; adding the same buffer again replaces, does not duplicate
    (ecb-tree-buffer-callbacks-add 'select 'ecb-foo-buffer-name 'ecb-foo-select2)
    (should (= (length (ecb-tree-buffer-callbacks-alist-of-type 'select)) 1))
    (should-error (ecb-tree-buffer-callbacks-add 'bogus 'x 'y))))

(ert-deftest ecb-tree-buffer-creators-register-and-run ()
  (let ((ecb-tree-buffer-creators nil))
    (ecb-tree-buffer-creators-init)
    (ecb-tree-buffer-creators-register 'ecb-test-name-1 (lambda () (ecb-test-record 'c1)))
    (ecb-tree-buffer-creators-register 'ecb-test-name-2 (lambda () (ecb-test-record 'c2)))
    ;; registering the identical (name . fn) pair twice is a no-op
    (ecb-tree-buffer-creators-register 'ecb-test-name-2 (cdr (assq 'ecb-test-name-2 ecb-tree-buffer-creators)))
    (should (= (length ecb-tree-buffer-creators) 2))
    (should (equal (sort (mapcar #'car (ecb-test-with-call-log (ecb-tree-buffer-creators-run)))
                         (lambda (l r) (string< (symbol-name l) (symbol-name r))))
                   '(c1 c2)))))

;;; ----------------------------------------------------------------------
;;; Filenames
;;; ----------------------------------------------------------------------

(ert-deftest ecb-fix-filename-strips-trailing-separator ()
  (ecb-filename-cache-init)
  (should (string= (ecb-fix-filename "/tmp/") "/tmp"))
  (should (string= (ecb-fix-filename "/tmp") "/tmp"))
  (should (string= (ecb-fix-filename "/") "/")))

(ert-deftest ecb-fix-filename-appends-filename ()
  (ecb-filename-cache-init)
  (should (string= (ecb-fix-filename "/tmp/" "x.el") "/tmp/x.el"))
  (should (string= (ecb-fix-filename "/tmp" "/other/dir/x.el") "/tmp/x.el"))
  (should (string= (ecb-fix-filename "/" "x") "/x")))

(ert-deftest ecb-fix-filename-expands-and-normalises ()
  (ecb-filename-cache-init)
  (should (string= (ecb-fix-filename "/tmp//a/../b/") "/tmp/b"))
  (should (string= (ecb-fix-filename "~") (directory-file-name (expand-file-name "~"))))
  (should-not (ecb-fix-filename nil))
  (should-not (ecb-fix-filename 42)))

(ert-deftest ecb-fix-filename-substitutes-env-vars-on-request ()
  (ecb-filename-cache-init)
  (let ((process-environment (cons "ECB_TEST_DIR=/tmp/ecbenv" process-environment)))
    (should (string= (ecb-fix-filename "$ECB_TEST_DIR/sub/" nil t) "/tmp/ecbenv/sub"))
    (should (string= (ecb-fix-filename "/tmp" "$ECB_TEST_DIR/f.el" t) "/tmp/f.el"))
    ;; without the flag the variable is left alone
    (should (string-match-p "ECB_TEST_DIR" (ecb-fix-filename "/tmp/$ECB_TEST_DIR")))))

(ert-deftest ecb-fix-filename-uses-cache ()
  (ecb-filename-cache-init)
  (ecb-fix-filename "/tmp/cache-me/" "f")
  (should (string= (ecb-fixed-filename-cache-get "/tmp/cache-me/" "f") "/tmp/cache-me/f"))
  (ecb-filename-cache-init)
  (should-not (ecb-fixed-filename-cache-get "/tmp/cache-me/" "f")))

(ert-deftest ecb-fix-filename-remote-paths-are-left-unexpanded ()
  (ecb-filename-cache-init)
  (require 'tramp)
  ;; host is unreachable from the test, so the path must not be expanded
  ;; (that would trigger a tramp connection); only the trailing slash goes.
  (cl-letf (((symbol-function 'ecb-host-accessible-p) (lambda (_host) nil)))
    (should (string= (ecb-fix-filename "/ssh:nohost.invalid:/a/b/") "/ssh:nohost.invalid:/a/b"))
    (should (string= (ecb-fix-filename "/ssh:nohost.invalid:/a" "c") "/ssh:nohost.invalid:/a/c"))))

(ert-deftest ecb-directory-sep-char-and-string ()
  (should (= (ecb-directory-sep-char "/x/y") ?/))
  (should (string= (ecb-directory-sep-string "/x/y") "/")))

(ert-deftest ecb-expand-file-name-and-directory-files-wrappers ()
  (ecb-test-with-temp-project proj
    (should (string= (ecb-expand-file-name "README.txt" proj) (expand-file-name "README.txt" proj)))
    (should (member "README.txt" (ecb-directory-files proj)))))

;;; ----------------------------------------------------------------------
;;; Misc
;;; ----------------------------------------------------------------------

(ert-deftest ecb-format-bucket-name-uses-prefix-suffix ()
  (let ((ecb-bucket-node-display '("[" "]" ecb-bucket-node-face)))
    (should (string= (substring-no-properties (ecb-format-bucket-name "Vars")) "[Vars]"))
    (should (string= (substring-no-properties (ecb-format-bucket-name "Vars" t)) "Vars"))
    (should (string= (substring-no-properties (ecb-format-bucket-name "Vars" nil t)) "[Vars]"))))

(ert-deftest ecb-generate-node-name-keeps-text ()
  (let ((existed (get-buffer ecb-sources-buffer-name)))
    (get-buffer-create ecb-sources-buffer-name)
    (unwind-protect
        (let ((s (ecb-generate-node-name "name" 0 nil ecb-sources-buffer-name)))
          (should (string= (substring-no-properties s) "name")))
      (unless existed (kill-buffer ecb-sources-buffer-name)))))

(ert-deftest ecb-window-select-with-window-object ()
  "KNOWN BUG: `ecb-window-select' documents a buffer-or-name argument but
`ecb-maximize-ecb-buffer' and friends pass the window object
`ecb-compile-window'.  Either the callers or this function need to change;
this test pins the behaviour the callers rely on.  Remove :expected-result
once fixed."
  :expected-result :failed
  (ecb-test-prepare-frame)
  (let ((ecb-frame (selected-frame)))
    (split-window)
    (unwind-protect
        (let ((w (cadr (window-list))))
          (should (eq (ecb-window-select w) w))
          (should (eq (selected-window) w)))
      (delete-other-windows))))

(ert-deftest ecb-stealthy-function-state-bookkeeping ()
  (let ((ecb-stealthy-function-list nil)
        (ecb-stealthy-function-state-alist nil))
    (ecb-stealthy-function-list-add 'ecb-cb-test-stealthy)
    (ecb-stealthy-function-state-alist-add 'ecb-cb-test-stealthy)
    (should (ecb-stealthy-function-p 'ecb-cb-test-stealthy))
    (should-not (ecb-stealthy-function-p 'ecb-cb-test-other))
    ;; freshly added functions start in state 'done
    (should (eq (ecb-stealthy-function-state-get 'ecb-cb-test-stealthy) 'done))
    (ecb-stealthy-function-state-init)
    (should (eq (ecb-stealthy-function-state-get 'ecb-cb-test-stealthy) 'restart))
    (ecb-stealthy-function-state-init 'ecb-cb-test-stealthy 'done)
    (should (eq (ecb-stealthy-function-state-get 'ecb-cb-test-stealthy) 'done))))

(provide 'ecb-common-browser-test)
;;; ecb-common-browser-test.el ends here
