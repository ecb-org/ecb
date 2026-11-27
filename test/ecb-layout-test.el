;;; ecb-layout-test.el --- tests for ecb-layout.el -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Window management: layout switching, maximize/undo, hiding ECB windows,
;; the compile window, window classification (`ecb-where-is-point' & co),
;; window sizes and the advised window commands.  Golden window trees for
;; every shipped layout live in ecb-layout-defs-test.el.

;;; Code:

(require 'ecb-test-helpers)

(defconst ecb-layout-test-edit-buffer "*ecb-test-scratch*"
  "Name of the buffer `ecb-test-prepare-frame' shows in the edit window.")

(defconst ecb-layout-test-left8-tree
  `(horizontal (vertical ,ecb-directories-buffer-name ,ecb-sources-buffer-name
                         ,ecb-methods-buffer-name ,ecb-history-buffer-name)
               ,ecb-layout-test-edit-buffer))

;;; ----------------------------------------------------------------------
;;; Layout registry
;;; ----------------------------------------------------------------------

(ert-deftest ecb-layout-types-are-known ()
  (dolist (type '(left right top left-right))
    (should (ecb-layout-type-p type)))
  (should-not (ecb-layout-type-p 'bottom))
  (should-error (ecb-layout-type-p 'bottom t)))

(ert-deftest ecb-available-layouts-registry ()
  (should (ecb-available-layouts-member-p "left8"))
  (should-not (ecb-available-layouts-member-p "no-such-layout"))
  (should (member "left8" (ecb-available-layouts-of-type 'left)))
  (should (member "right1" (ecb-available-layouts-of-type 'right)))
  (should (member "top1" (ecb-available-layouts-of-type 'top)))
  (should (member "leftright1" (ecb-available-layouts-of-type 'left-right)))
  (should-not (member "left8" (ecb-available-layouts-of-type 'right)))
  ;; the list is kept sorted
  (let ((names (ecb-available-layouts-of-type nil)))
    (should (equal names (sort (copy-sequence names) #'string<)))))

(ert-deftest ecb-layout-define-and-undefine ()
  (unwind-protect
      (progn
        (eval '(ecb-layout-define "ecb-test-layout" left
                 "A throwaway layout."
                 (ecb-set-directories-buffer)
                 (ecb-split-ver 0.5)
                 (ecb-set-methods-buffer)
                 (select-window (next-window)))
              t)
        (should (ecb-available-layouts-member-p "ecb-test-layout"))
        (should (fboundp 'ecb-layout-function-ecb-test-layout))
        (should (eq (ecb-get-layout-type "ecb-test-layout") 'left))
        (ecb-test-with-layout "ecb-test-layout" _proj
          (should (equal (ecb-test-window-tree)
                         `(horizontal (vertical ,ecb-directories-buffer-name
                                                ,ecb-methods-buffer-name)
                                      ,ecb-layout-test-edit-buffer)))))
    (ecb-layout-undefine "ecb-test-layout")
    (should-not (ecb-available-layouts-member-p "ecb-test-layout"))
    (should-not (fboundp 'ecb-layout-function-ecb-test-layout))))

(ert-deftest ecb-layout-switch-unknown-name-errors ()
  (ecb-test-with-ecb _proj
    (should-error (ecb-layout-switch "no-such-layout"))
    ;; still intact
    (should (equal ecb-layout-name "left8"))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-layout-switch-changes-layout-and-back ()
  (ecb-test-with-ecb _proj
    (ecb-layout-switch "left9")
    (should (equal ecb-layout-name "left9"))
    (should (equal (ecb-test-window-tree)
                   `(horizontal ,ecb-methods-buffer-name ,ecb-layout-test-edit-buffer)))
    (ecb-layout-switch "left8")
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-toggle-layout-cycles-through-toggle-layout-sequence ()
  (ecb-test-with-ecb _proj
    (let ((ecb-toggle-layout-sequence '("left9" "left13" "left8"))
          (ecb-toggle-layout-state 0))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left9"))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left13"))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left8"))
      (ecb-toggle-layout)
      (should (equal ecb-layout-name "left9"))
      ;; LAST-ONE jumps back to the previously used layout
      (ecb-toggle-layout 'last)
      (should (equal ecb-layout-name "left8")))))

(ert-deftest ecb-change-layout-reads-name-from-user ()
  (ecb-test-with-ecb _proj
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "left10")))
      (ecb-change-layout))
    (should (equal ecb-layout-name "left10"))
    (should (equal (ecb-test-window-tree)
                   `(horizontal (vertical ,ecb-methods-buffer-name
                                          (horizontal ,ecb-sources-buffer-name
                                                      ,ecb-history-buffer-name))
                                ,ecb-layout-test-edit-buffer)))))

;;; ----------------------------------------------------------------------
;;; Window classification
;;; ----------------------------------------------------------------------

(ert-deftest ecb-where-is-point-classifies-windows ()
  (ecb-test-with-ecb _proj
    (should (equal (ecb-where-is-point) '(edit . 1)))
    (should (= (ecb-point-in-edit-window-number) 1))
    (should-not (ecb-point-in-ecb-window-number))
    (should-not (ecb-point-in-compile-window))
    (ecb-goto-window-directories)
    (should (equal (ecb-where-is-point) '(ecb . 1)))
    (should (= (ecb-point-in-ecb-window-number) 1))
    (should (ecb-point-in-ecb-tree-buffer))
    (should (ecb-point-in-dedicated-special-buffer))
    (ecb-goto-window-history)
    (should (equal (ecb-where-is-point) '(ecb . 4)))
    (ecb-goto-window-edit-last)
    (should (equal (ecb-where-is-point) '(edit . 1)))))

(ert-deftest ecb-canonical-window-lists ()
  (ecb-test-with-ecb _proj
    (should (equal (mapcar (lambda (w) (buffer-name (window-buffer w)))
                           (ecb-canonical-ecb-windows-list))
                   (list ecb-directories-buffer-name ecb-sources-buffer-name
                         ecb-methods-buffer-name ecb-history-buffer-name)))
    (should (= (length (ecb-canonical-edit-windows-list)) 1))
    ;; residual windows = everything that is neither ecb- nor compile-window
    (should (equal (ecb-canonical-residual-windows-list)
                   (ecb-canonical-edit-windows-list)))
    (should-not (ecb-edit-window-splitted))))

(ert-deftest ecb-goto-window-commands-select-correct-windows ()
  (ecb-test-with-ecb _proj
    (dolist (pair `((ecb-goto-window-directories . ,ecb-directories-buffer-name)
                    (ecb-goto-window-sources . ,ecb-sources-buffer-name)
                    (ecb-goto-window-methods . ,ecb-methods-buffer-name)
                    (ecb-goto-window-history . ,ecb-history-buffer-name)
                    (ecb-goto-window-edit1 . ,ecb-layout-test-edit-buffer)))
      (funcall (car pair))
      (should (equal (buffer-name (window-buffer (selected-window))) (cdr pair))))))

(ert-deftest ecb-goto-window-edit2-on-unsplit-edit-area-stays-in-edit1 ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-edit2)
    (should (equal (ecb-where-is-point) '(edit . 1)))))

(ert-deftest ecb-buffer-is-ecb-buffer-of-current-layout-p ()
  (ecb-test-with-ecb _proj
    (should (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-directories-buffer-name))
    (should-not (ecb-buffer-is-ecb-buffer-of-current-layout-p "*scratch*"))
    (ecb-layout-switch "left9")
    (should (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-methods-buffer-name))
    (should-not (ecb-buffer-is-ecb-buffer-of-current-layout-p ecb-directories-buffer-name))))

;;; ----------------------------------------------------------------------
;;; Maximizing
;;; ----------------------------------------------------------------------

(ert-deftest ecb-maximize-and-undo-ecb-buffer ()
  (ecb-test-with-ecb _proj
    (should-not (ecb-buffer-is-maximized-p))
    (ecb-toggle-maximize-ecb-window ecb-methods-buffer-name)
    (should (ecb-buffer-is-maximized-p))
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-methods-buffer-name))
    (should (equal (ecb-test-window-tree)
                   `(horizontal ,ecb-methods-buffer-name ,ecb-layout-test-edit-buffer)))
    (ecb-toggle-maximize-ecb-window ecb-methods-buffer-name)
    (should-not (ecb-buffer-is-maximized-p))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-maximize-non-ecb-buffer-is-noop ()
  (ecb-test-with-ecb _proj
    (ecb-toggle-maximize-ecb-window "*scratch*")
    (should-not (ecb-buffer-is-maximized-p))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-maximize-preserves-selected-ecb-window ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-sources)
    (ecb-maximize-ecb-buffer ecb-sources-buffer-name t)
    (should (equal (buffer-name) ecb-sources-buffer-name))
    (should (equal (ecb-where-is-point) '(ecb . 1)))
    (ecb-undo-maximize-ecb-buffer t)
    (should (equal (buffer-name) ecb-sources-buffer-name))
    (should (equal (ecb-where-is-point) '(ecb . 2)))))

(ert-deftest ecb-maximize-without-preserve-selects-edit-window ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-sources)
    (ecb-maximize-ecb-buffer ecb-sources-buffer-name)
    (should (equal (ecb-where-is-point) '(edit . 1)))))

(ert-deftest ecb-maximize-with-compile-window-from-compile-window ()
  "KNOWN BUG: `ecb-maximize-ecb-buffer' and `ecb-undo-maximize-ecb-buffer'
call `ecb-window-select' with the window object `ecb-compile-window' when
point was in the compile window, but `ecb-window-select' only accepts a
buffer name (ecb-layout.el, the `compile' branches of the `cl-case' forms).
Result: (wrong-type-argument stringp #<window>)."
  :expected-result :failed
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (ecb-goto-window-compilation)
      (should (equal (ecb-where-is-point) '(compile . 1)))
      (ecb-maximize-ecb-buffer ecb-methods-buffer-name t)
      (should (equal (ecb-where-is-point) '(compile . 1)))
      (ecb-undo-maximize-ecb-buffer t)
      (should (equal (ecb-where-is-point) '(compile . 1))))))

(ert-deftest ecb-maximize-with-compile-window-from-edit-window ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (ecb-toggle-maximize-ecb-window ecb-methods-buffer-name)
      (should (equal (ecb-test-window-tree)
                     `(vertical (horizontal ,ecb-methods-buffer-name ,ecb-layout-test-edit-buffer)
                                "*Messages*")))
      (ecb-toggle-maximize-ecb-window ecb-methods-buffer-name)
      (should (equal (ecb-test-window-tree)
                     `(vertical ,ecb-layout-test-left8-tree "*Messages*"))))))

(ert-deftest ecb-cycle-maximized-ecb-buffers-walks-layout-order ()
  (ecb-test-with-ecb _proj
    (ecb-cycle-maximized-ecb-buffers)
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-directories-buffer-name))
    (ecb-cycle-maximized-ecb-buffers)
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-sources-buffer-name))
    (ecb-cycle-maximized-ecb-buffers)
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-methods-buffer-name))
    (ecb-cycle-maximized-ecb-buffers)
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-history-buffer-name))
    (ecb-cycle-maximized-ecb-buffers)
    (should (equal (ecb-maximized-ecb-buffer-name) ecb-directories-buffer-name))
    (ecb-undo-maximize-ecb-buffer)
    (should-not (ecb-buffer-is-maximized-p))))

(ert-deftest ecb-redraw-layout-undoes-maximize ()
  (ecb-test-with-ecb _proj
    (ecb-toggle-maximize-ecb-window ecb-methods-buffer-name)
    (ecb-redraw-layout-full)
    (should-not (ecb-buffer-is-maximized-p))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

;;; ----------------------------------------------------------------------
;;; Hiding / showing the ECB windows
;;; ----------------------------------------------------------------------

(ert-deftest ecb-toggle-ecb-windows-hides-and-shows ()
  (ecb-test-with-ecb _proj
    (should-not (ecb-windows-all-hidden))
    (ecb-toggle-ecb-windows)
    (should (ecb-windows-all-hidden))
    (should (equal (ecb-test-window-tree) ecb-layout-test-edit-buffer))
    (should-not (ecb-canonical-ecb-windows-list))
    (ecb-toggle-ecb-windows)
    (should-not (ecb-windows-all-hidden))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-hide-and-show-ecb-windows-are-idempotent ()
  (ecb-test-with-ecb _proj
    (ecb-hide-ecb-windows)
    (ecb-hide-ecb-windows)
    (should (ecb-windows-all-hidden))
    (ecb-show-ecb-windows)
    (ecb-show-ecb-windows)
    (should-not (ecb-windows-all-hidden))
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-toggle-ecb-windows-with-numeric-arg ()
  (ecb-test-with-ecb _proj
    (ecb-toggle-ecb-windows -1)
    (should (ecb-windows-all-hidden))
    (ecb-toggle-ecb-windows -1)
    (should (ecb-windows-all-hidden))
    (ecb-toggle-ecb-windows 1)
    (should-not (ecb-windows-all-hidden))))

(ert-deftest ecb-hidden-windows-survive-redraw-and-sync ()
  (ecb-test-with-ecb proj
    (ecb-hide-ecb-windows)
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (should (ecb-windows-all-hidden))
    (should (equal (ecb-test-window-tree) "alpha.el"))))

;;; ----------------------------------------------------------------------
;;; Compile window
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compile-window-toggle ()
  (ecb-test-with-ecb _proj
    (should-not (ecb-compile-window-live-p))
    (ecb-test-with-compile-window
      (should (window-live-p ecb-compile-window))
      (should (equal (ecb-test-window-tree)
                     `(vertical ,ecb-layout-test-left8-tree "*Messages*")))
      (should (= (window-total-height ecb-compile-window) 6))
      (ecb-toggle-compile-window -1)
      (should-not (ecb-compile-window-live-p))
      (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree)))))

(ert-deftest ecb-toggle-compile-window-without-configured-height-is-noop ()
  (ecb-test-with-ecb _proj
    (should-not ecb-compile-window-height)
    (ecb-toggle-compile-window 1)
    (should-not (ecb-compile-window-live-p))))

(defun ecb-layout-test-activate-with-compile-window (split-after-start)
  "Activate ECB with a configured compile window and SPLIT-AFTER-START.
Return (LIVE-P . TREE) and deactivate again."
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-compile-window-height 6)
          (ecb-layout-name "left8")
          (ecb-split-edit-window-after-start split-after-start)
          (ecb-tip-of-the-day nil))
      (unwind-protect
          (ecb-test-no-user-input
            (ecb-activate)
            (cons (ecb-compile-window-live-p) (ecb-test-window-tree)))
        (ecb-test-force-deactivate)))))

(ert-deftest ecb-compile-window-created-at-activation-when-configured ()
  (let ((result (ecb-layout-test-activate-with-compile-window 'before-activation)))
    (should (car result))
    (should (equal (cdr result)
                   `(vertical ,ecb-layout-test-left8-tree "*Messages*")))))

(ert-deftest ecb-compile-window-survives-split-edit-window-after-start ()
  "KNOWN BUG: when `ecb-split-edit-window-after-start' is nil, `vertical' or
`horizontal', activation runs `delete-other-windows' in the edit window
after drawing the layout, which also removes the freshly created compile
window although `ecb-compile-window-height' asks for one."
  :expected-result :failed
  (dolist (split '(nil vertical horizontal))
    (should (car (ecb-layout-test-activate-with-compile-window split)))))

(ert-deftest ecb-goto-window-compilation-and-where-is-point ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (ecb-goto-window-compilation)
      (should (equal (ecb-where-is-point) '(compile . 1)))
      (should (ecb-point-in-compile-window))
      (ecb-goto-window-edit1)
      (should-not (ecb-point-in-compile-window)))))

(ert-deftest ecb-toggle-compile-window-height-enlarges-and-shrinks ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (let ((base (window-total-height ecb-compile-window)))
        (ecb-goto-window-compilation)
        (ecb-toggle-compile-window-height 1)
        (should (> (window-total-height ecb-compile-window) base))
        (ecb-toggle-compile-window-height -1)
        (should (= (window-total-height ecb-compile-window) base))))))

(ert-deftest ecb-compile-window-displays-compilation-buffers ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (let ((buf (get-buffer-create "*compilation*")))
        (with-current-buffer buf (compilation-mode))
        (display-buffer buf)
        (should (eq (window-buffer ecb-compile-window) buf))
        ;; edit window untouched
        (should (equal (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list))))
                       ecb-layout-test-edit-buffer))
        (kill-buffer buf)))))

(ert-deftest ecb-compile-window-survives-layout-redraw ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (ecb-redraw-layout-full)
      (should (ecb-compile-window-live-p))
      (ecb-layout-switch "left9")
      (should (ecb-compile-window-live-p))
      (should (equal (ecb-test-window-tree)
                     `(vertical (horizontal ,ecb-methods-buffer-name ,ecb-layout-test-edit-buffer)
                                "*Messages*"))))))

;;; ----------------------------------------------------------------------
;;; Edit area and advised window commands
;;; ----------------------------------------------------------------------

(ert-deftest ecb-split-edit-window-tracked ()
  (ecb-test-with-ecb _proj
    (split-window-vertically)
    (should (eq (ecb-edit-window-splitted) 'vertical))
    (should (= (length (ecb-canonical-edit-windows-list)) 2))
    (should (equal (ecb-test-window-tree)
                   `(horizontal (vertical ,ecb-directories-buffer-name ,ecb-sources-buffer-name
                                          ,ecb-methods-buffer-name ,ecb-history-buffer-name)
                                (vertical ,ecb-layout-test-edit-buffer ,ecb-layout-test-edit-buffer))))
    (ecb-goto-window-edit2)
    (should (equal (ecb-where-is-point) '(edit . 2)))
    (delete-window)
    (should-not (ecb-edit-window-splitted))))

(ert-deftest ecb-split-edit-window-horizontally-tracked ()
  (ecb-test-with-ecb _proj
    (split-window-horizontally)
    (should (eq (ecb-edit-window-splitted) 'horizontal))
    (should (= (length (ecb-canonical-edit-windows-list)) 2))))

(ert-deftest ecb-delete-other-windows-in-edit-area-keeps-ecb-windows ()
  (ecb-test-with-ecb _proj
    (split-window-vertically)
    (delete-other-windows)
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))
    (should-not (ecb-edit-window-splitted))))

(ert-deftest ecb-delete-window-in-ecb-window-is-refused ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-directories)
    (delete-window)
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-delete-window-on-sole-edit-window-is-refused ()
  (ecb-test-with-ecb _proj
    (delete-window)
    (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree))))

(ert-deftest ecb-switch-to-buffer-in-ecb-window-uses-edit-window ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-methods)
    (switch-to-buffer "*scratch*")
    (should (equal (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list))))
                   "*scratch*"))
    (should (equal (buffer-name (window-buffer (nth 2 (ecb-canonical-ecb-windows-list))))
                   ecb-methods-buffer-name))))

(ert-deftest ecb-display-buffer-from-edit-window-splits-edit-area ()
  (ecb-test-with-ecb _proj
    (let ((buf (get-buffer-create "*ecb-layout-tmp*")))
      (unwind-protect
          (progn
            (display-buffer buf)
            (should (= (length (ecb-canonical-edit-windows-list)) 2))
            (should (equal (ecb-test-visible-ecb-buffers)
                           (list ecb-directories-buffer-name ecb-sources-buffer-name
                                 ecb-methods-buffer-name ecb-history-buffer-name))))
        (kill-buffer buf)))))

(ert-deftest ecb-other-window-cycles-through-edit-and-ecb-windows ()
  "Known bug (Emacs >= 31): `ecb-other-window-behavior' defaults to the
symbol `all', and `ecb-get-other-window' checks `functionp' on it before
its `cl-case'. Emacs 31 added a builtin function `all', so every
`other-window' with the default setting now signals
wrong-number-of-arguments."
  :expected-result (if (ecb-test-symbol-all-is-a-function-p) :failed :passed)
  (ecb-test-with-ecb _proj
    (let ((ecb-other-window-behavior 'all))
      (let (seen)
        (dotimes (_ 5)
          (push (ecb-where-is-point) seen)
          (other-window 1))
        (should (equal (ecb-where-is-point) '(edit . 1)))
        (should (= (length (delete-dups seen)) 5))))))

(ert-deftest ecb-other-window-cycles-with-custom-function ()
  "Cycling through all windows via a user function works on every Emacs
and is what `all' is supposed to do."
  (ecb-test-with-ecb _proj
    (let ((ecb-other-window-behavior
           (lambda (win-list _edit _ecb _comp _minibuf _point-loc nth)
             (ecb-next-listelem win-list (selected-window) nth))))
      (let (seen)
        (dotimes (_ 5)
          (push (ecb-where-is-point) seen)
          (other-window 1))
        (should (equal (ecb-where-is-point) '(edit . 1)))
        (should (= (length (delete-dups seen)) 5))))))

(ert-deftest ecb-other-window-edit-only-behaviour ()
  (ecb-test-with-ecb _proj
    (let ((ecb-other-window-behavior 'only-edit))
      (split-window-vertically)
      (other-window 1)
      (should (equal (ecb-where-is-point) '(edit . 2)))
      (other-window 1)
      (should (equal (ecb-where-is-point) '(edit . 1))))))

(ert-deftest ecb-redraw-layout-preserves-edit-buffers ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (split-window-vertically)
    (other-window 1)
    (find-file (expand-file-name "src/beta.el" proj))
    (ecb-redraw-layout-full)
    (should (equal (mapcar (lambda (w) (buffer-name (window-buffer w)))
                           (ecb-canonical-edit-windows-list))
                   '("alpha.el" "beta.el")))))

;;; ----------------------------------------------------------------------
;;; Window sizes
;;; ----------------------------------------------------------------------

(ert-deftest ecb-get-window-size-returns-fractions ()
  (ecb-test-with-ecb _proj
    (let ((size (ecb-get-window-size (selected-window))))
      (should (consp size))
      (should (< 0 (car size) 1))
      (should (< 0 (cdr size) 1)))))

(ert-deftest ecb-store-window-sizes-records-current-layout ()
  (ecb-test-with-ecb _proj
    (let ((ecb-layout-window-sizes nil))
      (ecb-test-no-user-input (ecb-store-window-sizes))
      (let ((entry (assoc "left8" ecb-layout-window-sizes)))
        (should entry)
        (should (equal (mapcar #'car (cdr entry))
                       '(ecb-directories-buffer-name ecb-sources-buffer-name
                         ecb-methods-buffer-name ecb-history-buffer-name)))
        (dolist (size (cdr entry))
          (should (floatp (cadr size)))
          (should (floatp (cddr size))))))))

(ert-deftest ecb-restore-window-sizes-applies-stored-sizes ()
  (ecb-test-with-ecb _proj
    (let ((ecb-layout-window-sizes nil))
      ;; make the directories window one line taller than default, store that
      (ecb-goto-window-directories)
      (enlarge-window 1)
      (let ((wanted (window-total-height (selected-window))))
        (ecb-test-no-user-input (ecb-store-window-sizes))
        (ecb-goto-window-edit1)
        (ecb-redraw-layout-full)
        ;; sizes are stored as fractions, so allow one line of rounding
        (should (<= (abs (- (window-total-height (car (ecb-canonical-ecb-windows-list)))
                            wanted))
                    1))
        ;; shrink again and restore explicitly
        (ecb-goto-window-directories)
        (shrink-window 2)
        (ecb-goto-window-edit1)
        (ecb-restore-window-sizes)
        (should (<= (abs (- (window-total-height (car (ecb-canonical-ecb-windows-list)))
                            wanted))
                    1))))))

(ert-deftest ecb-windows-width-option-is-respected ()
  (ecb-test-with-temp-project proj
    (ecb-test-prepare-frame)
    (let ((ecb-source-path (list proj))
          (ecb-windows-width 0.5)
          (ecb-layout-name "left8")
          (ecb-tip-of-the-day nil))
      (unwind-protect
          (ecb-test-no-user-input
            (ecb-activate)
            (should (= (window-total-width (car (ecb-canonical-ecb-windows-list)))
                       (/ (frame-width) 2))))
        (ecb-test-force-deactivate)))))

;;; ----------------------------------------------------------------------
;;; Window configuration snapshots
;;; ----------------------------------------------------------------------

(ert-deftest ecb-current-window-configuration-roundtrip ()
  (ecb-test-with-ecb _proj
    (let ((conf (ecb-current-window-configuration)))
      (should-not (ecb-window-configuration-invalidp conf))
      (ecb-layout-switch "left9")
      (ecb-set-window-configuration conf)
      (should (equal (ecb-test-window-tree) ecb-layout-test-left8-tree)))))

(ert-deftest ecb-window-configuration-invalid-after-layout-change ()
  (ecb-test-with-ecb _proj
    (let ((conf (ecb-current-window-configuration)))
      (should-not (ecb-window-configuration-invalidp conf))
      (ecb-layout-switch "left9")
      (should (ecb-window-configuration-invalidp conf))
      ;; a snapshot under one windows-width is stale under another
      (let ((conf2 (ecb-current-window-configuration)))
        (should-not (ecb-window-configuration-invalidp conf2))
        (let ((ecb-windows-width 0.4))
          (should (ecb-window-configuration-invalidp conf2)))))))

(provide 'ecb-layout-test)
;;; ecb-layout-test.el ends here
