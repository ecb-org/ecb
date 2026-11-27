;;; ecb-compatibility-test.el --- behaviour of ECB's package-compatibility advices -*- lexical-binding: t -*-

;; Author: Ryan Ware <ware@opensourceware.me>
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; ecb-compatibility.el advises functions of bs, electric, master,
;; scroll-all, tmm and view so they behave inside the ECB frame.  These
;; tests call the *advised public functions* and assert the observable
;; result, so they hold no matter how the advice is implemented
;; (`defecb-advice' today, `ecb-add-to-advice-set' after the migration).
;;
;; Every advice has two branches: "inside the ECB frame" and "not ECB"
;; (other frame or ECB inactive), where the original must run untouched.
;; Batch Emacs has a single frame, so the second branch is exercised by
;; binding `ecb-minor-mode' to nil while the advices stay enabled; this
;; is exactly the condition the advices test first.

;;; Code:

(require 'ecb-test-helpers)
(require 'bs)
(require 'electric)
(require 'ebuff-menu)
(require 'echistory)
(require 'master)
(require 'scroll-all)
(require 'tmm)
(require 'view)

(defmacro ecb-compat-test-as-if-not-ecb (&rest body)
  "Run BODY with ECB's advices enabled but `ecb-minor-mode' nil.
This is the path taken for frames other than `ecb-frame'."
  (declare (indent 0) (debug t))
  `(let ((ecb-minor-mode nil))
     ,@body))

(defun ecb-compat-test-two-edit-windows (proj)
  "Show alpha.el and beta.el in two edit windows; return (WIN-A . WIN-B)."
  (ecb-select-edit-window)
  (find-file (expand-file-name "src/alpha.el" proj))
  (let ((a (selected-window))
        (b (split-window-vertically)))
    (set-window-buffer b (find-file-noselect (expand-file-name "src/beta.el" proj)))
    (select-window a)
    (cons a b)))

;;; ----------------------------------------------------------------------
;;; Advice set bookkeeping
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-advice-set-covers-all-packages ()
  "Every compatibility advice is enabled with ECB and gone afterwards."
  (let ((fns '(bs-show Electric-pop-up-window electric-command-history
               electric-buffer-list master-says scroll-all-function-all
               tmm-prompt view-mode-exit)))
    (ecb-test-with-ecb _proj
      (dolist (f fns)
        (should (ecb-test-advised-p f))))
    (dolist (f fns)
      (should-not (ecb-test-advised-p f)))
    ;; one-window-p is on-demand only: never enabled outside ecb-with-ecb-advice
    (ecb-test-with-ecb _proj
      (should-not (ecb-test-advised-p 'one-window-p)))))

;;; ----------------------------------------------------------------------
;;; one-window-p (on demand via ecb-with-ecb-advice)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-one-window-p-counts-only-edit-windows ()
  (ecb-test-with-ecb proj
    (ecb-select-edit-window)
    ;; plain one-window-p sees all the ECB windows
    (should-not (one-window-p))
    ;; the adviced version sees one edit window
    (should (ecb-with-ecb-advice 'one-window-p 'around (one-window-p)))
    ;; and is disabled again afterwards
    (should-not (ecb-test-advised-p 'one-window-p))
    (should-not (one-window-p))
    ;; two edit windows -> nil
    (ecb-compat-test-two-edit-windows proj)
    (should-not (ecb-with-ecb-advice 'one-window-p 'around (one-window-p)))))

(ert-deftest ecb-compat-one-window-p-not-ecb-falls-through ()
  (ecb-test-with-ecb _proj
    (ecb-select-edit-window)
    (ecb-compat-test-as-if-not-ecb
      ;; original semantics: many windows -> nil, and NOMINI arg accepted
      (should-not (ecb-with-ecb-advice 'one-window-p 'around (one-window-p)))
      (should-not (ecb-with-ecb-advice 'one-window-p 'around (one-window-p t))))))

(ert-deftest ecb-compat-one-window-p-advice-disabled-after-error ()
  (ecb-test-with-ecb _proj
    (should-error (ecb-with-ecb-advice 'one-window-p 'around (error "boom")))
    (should-not (ecb-test-advised-p 'one-window-p))))

;;; ----------------------------------------------------------------------
;;; Electric-pop-up-window
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-electric-pop-up-window-uses-compile-window ()
  "A compilation buffer goes to the visible compile window."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-test-with-compile-window
      (let ((help (get-buffer-create "*Help*"))
            (before (ecb-test-visible-ecb-buffers)))
        (unwind-protect
            (progn
              (ecb-select-edit-window)
              (Electric-pop-up-window help)
              (should (eq (window-buffer ecb-compile-window) help))
              (should (equal (ecb-test-visible-ecb-buffers) before)))
          (kill-buffer help))))))

(ert-deftest ecb-compat-electric-pop-up-window-uses-edit-window ()
  "Without a compile window the buffer appears in an edit window and the
ECB windows are untouched. The original would split based on
`one-window-p', which must see the edit area only."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((buf (get-buffer-create "*ecb-compat-plain*"))
          (before (ecb-test-visible-ecb-buffers)))
      (unwind-protect
          (progn
            (ecb-select-edit-window)
            (Electric-pop-up-window buf)
            (should (get-buffer-window buf))
            (should (member (get-buffer-window buf) (ecb-canonical-edit-windows-list)))
            (should (equal (ecb-test-visible-ecb-buffers) before))
            ;; the on-demand one-window-p advice was cleaned up
            (should-not (ecb-test-advised-p 'one-window-p)))
        (kill-buffer buf)))))

(ert-deftest ecb-compat-electric-pop-up-window-not-ecb-runs-original ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((buf (get-buffer-create "*ecb-compat-plain*")))
      (unwind-protect
          (progn
            (ecb-select-edit-window)
            (ecb-compat-test-as-if-not-ecb
              (Electric-pop-up-window buf)
              (should (get-buffer-window buf))
              (should (eq (current-buffer) buf))))
        (kill-buffer buf)))))

;;; ----------------------------------------------------------------------
;;; electric-command-history / electric-buffer-list (before/after advices)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-electric-command-history-leaves-ecb-window ()
  "Called from an ECB tree window, point is moved to an edit window first.
With an empty `command-history' the command then signals, which is fine:
the before-advice has already run."
  (ecb-test-with-ecb _proj
    (let ((command-history nil))
      (ecb-goto-window-methods)
      (should (ecb-point-in-dedicated-special-buffer))
      (condition-case nil (electric-command-history) (error nil))
      (should (ecb-point-in-edit-window-number)))))

(ert-deftest ecb-compat-electric-buffer-list-leaves-ecb-window-and-buries ()
  (ecb-test-with-ecb _proj
    (ecb-goto-window-methods)
    (should (ecb-point-in-dedicated-special-buffer))
    ;; The command reads from the terminal via `read-event' and
    ;; `read-key-sequence' (primitives, so `cl-letf' stubs are not seen by
    ;; native-compiled callers); feed it a "q" through the event queue
    ;; instead, which both primitives honour and which quits the loop.
    (let ((unread-command-events (list ?q)))
      (electric-buffer-list nil))
    (should (ecb-point-in-edit-window-number))
    ;; after-advice buries the list buffer
    (when (get-buffer "*Buffer List*")
      (should (eq (car (last (buffer-list))) (get-buffer "*Buffer List*"))))))

(ert-deftest ecb-compat-electric-before-advices-noop-when-not-ecb ()
  (ecb-test-with-ecb _proj
    (let ((command-history nil))
      (ecb-goto-window-methods)
      (let ((w (selected-window)))
        (ecb-compat-test-as-if-not-ecb
          (condition-case nil (electric-command-history) (error nil))
          (should (eq (selected-window) w)))))))

;;; ----------------------------------------------------------------------
;;; master-says
;;; ----------------------------------------------------------------------

(defmacro ecb-compat-test-with-master-slave (proj &rest body)
  "Set alpha.el as master of beta.el, both shown in edit windows."
  (declare (indent 1) (debug t))
  `(let* ((wins (ecb-compat-test-two-edit-windows ,proj))
          (master (window-buffer (car wins)))
          (slave (window-buffer (cdr wins))))
     (with-current-buffer master
       (master-mode 1)
       (master-set-slave slave))
     (with-current-buffer slave (goto-char (point-min)))
     (unwind-protect
         (progn ,@body)
       (with-current-buffer master (master-mode -1)))))

(ert-deftest ecb-compat-master-says-operates-on-slave-and-returns ()
  (ecb-test-with-ecb proj
    (ecb-compat-test-with-master-slave proj
      (select-window (car wins))
      (master-says 'forward-line '(2))
      ;; the slave moved, point is back in the master window
      (should (= (with-current-buffer slave (line-number-at-pos)) 3))
      (should (eq (selected-window) (car wins)))
      (should (eq (current-buffer) master))
      ;; starting from an ECB window returns there too
      (ecb-goto-window-methods)
      (let ((w (selected-window)))
        (with-current-buffer master
          (master-says 'forward-line '(1)))
        (should (= (with-current-buffer slave (line-number-at-pos)) 4))
        (should (eq (selected-window) w))))))

(ert-deftest ecb-compat-master-says-not-ecb-runs-original ()
  (ecb-test-with-ecb proj
    (ecb-compat-test-with-master-slave proj
      (select-window (car wins))
      (ecb-compat-test-as-if-not-ecb
        (master-says 'forward-line '(2)))
      (should (= (with-current-buffer slave (line-number-at-pos)) 3)))))

;;; ----------------------------------------------------------------------
;;; scroll-all-function-all
;;; ----------------------------------------------------------------------

(defun ecb-compat-test-make-long (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dotimes (i 300) (insert (format "line %d\n" i)))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(ert-deftest ecb-compat-scroll-all-scrolls-edit-windows-only ()
  (ecb-test-with-ecb proj
    (let* ((wins (ecb-compat-test-two-edit-windows proj))
           (ecb-starts (mapcar #'window-start (ecb-canonical-ecb-windows-list))))
      (ecb-compat-test-make-long (window-buffer (car wins)))
      (ecb-compat-test-make-long (window-buffer (cdr wins)))
      (select-window (car wins))
      (scroll-all-function-all 'scroll-up-command nil)
      ;; both edit windows scrolled
      (should (> (window-start (car wins)) 1))
      (should (> (window-start (cdr wins)) 1))
      ;; the ECB windows did not
      (should (equal (mapcar #'window-start (ecb-canonical-ecb-windows-list)) ecb-starts))
      ;; point returned to where it started
      (should (eq (selected-window) (car wins))))))

(ert-deftest ecb-compat-scroll-all-not-ecb-runs-original ()
  (ecb-test-with-ecb proj
    (let ((wins (ecb-compat-test-two-edit-windows proj)))
      (ecb-compat-test-make-long (window-buffer (car wins)))
      (ecb-compat-test-make-long (window-buffer (cdr wins)))
      (select-window (car wins))
      (ecb-compat-test-as-if-not-ecb
        (scroll-all-function-all 'scroll-up-command nil))
      ;; original semantics: every *other* window is scrolled (errors in
      ;; the small ECB windows are ignored), the selected one is left to
      ;; the calling command, and point ends up where it started.
      (should (= (window-start (car wins)) 1))
      (should (> (window-start (cdr wins)) 1))
      (should (eq (selected-window) (car wins))))))

;;; ----------------------------------------------------------------------
;;; tmm-prompt
;;; ----------------------------------------------------------------------

(defvar ecb-compat-test-tmm-ran nil)

(defun ecb-compat-test-tmm-menu (&optional command)
  "A one-item menu keymap; the item runs COMMAND or records that it ran."
  (let ((map (make-sparse-keymap "Test menu")))
    (define-key map [item]
      `(menu-item "Only item"
                  ,(or command
                       (lambda () (interactive)
                         (setq ecb-compat-test-tmm-ran t)))))
    map))

(defmacro ecb-compat-test-tmm-choosing-first (&rest body)
  "Run BODY with `tmm-prompt's minibuffer read answering the first item.
tmm calls `completing-read-default' directly, bypassing the generic stub."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'completing-read-default)
              (lambda (_prompt coll &rest _)
                (car (all-completions "" coll)))))
     ,@body))

(ert-deftest ecb-compat-tmm-prompt-runs-item-inside-ecb ()
  "The completion stub picks the only item; it must run, the layout must
survive and no ECB window may be used for *Completions*."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((before (ecb-test-visible-ecb-buffers))
          (ecb-compat-test-tmm-ran nil))
      (ecb-select-edit-window)
      (ecb-compat-test-tmm-choosing-first
        (tmm-prompt (ecb-compat-test-tmm-menu)))
      (should ecb-compat-test-tmm-ran)
      (should (equal (ecb-test-visible-ecb-buffers) before))
      (should (ecb-point-in-edit-window-number)))))

(ert-deftest ecb-compat-tmm-prompt-other-window-behavior-is-callable ()
  "While tmm-prompt runs, ECB binds `ecb-other-window-behavior' to a
function; `other-window' must still work with it."
  (ecb-test-with-ecb proj
    (ecb-compat-test-two-edit-windows proj)
    (let ((ecb-compat-test-tmm-ran nil))
      (ecb-compat-test-tmm-choosing-first
        (tmm-prompt
         (ecb-compat-test-tmm-menu
          (lambda () (interactive)
            (other-window 1)
            (setq ecb-compat-test-tmm-ran (ecb-where-is-point))))))
      (should (consp ecb-compat-test-tmm-ran))
      (should (eq (car ecb-compat-test-tmm-ran) 'edit)))))

(ert-deftest ecb-compat-tmm-prompt-not-ecb-runs-original ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((ecb-compat-test-tmm-ran nil))
      (ecb-select-edit-window)
      (ecb-compat-test-tmm-choosing-first
        (ecb-compat-test-as-if-not-ecb
          (tmm-prompt (ecb-compat-test-tmm-menu))))
      (should ecb-compat-test-tmm-ran))))

;;; ----------------------------------------------------------------------
;;; view-mode-exit
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-view-mode-exit-in-compile-window ()
  "Leaving view-mode in the compile window shrinks it and selects the
last edit window instead of letting view-mode delete/restore windows."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-test-with-compile-window
      (let ((buf (get-buffer-create "*ecb-compat-view*")))
        (unwind-protect
            (progn
              (ecb-compat-test-make-long buf)
              (set-window-buffer ecb-compile-window buf)
              (select-window ecb-compile-window)
              (view-mode 1)
              ;; enlarge, so the shrink is observable
              (ecb-toggle-compile-window-height 1)
              (let ((enlarged (window-height ecb-compile-window)))
                (view-mode-exit)
                (should-not (with-current-buffer buf view-mode))
                (should (ecb-compile-window-live-p))
                (should (< (window-height ecb-compile-window) enlarged))
                (should (ecb-point-in-edit-window-number))))
          (kill-buffer buf))))))

(ert-deftest ecb-compat-view-mode-exit-outside-compile-window-is-original ()
  (ecb-test-with-ecb proj
    (ecb-select-edit-window)
    (find-file (expand-file-name "src/alpha.el" proj))
    (view-mode 1)
    (view-mode-exit)
    (should-not view-mode)
    (should (ecb-point-in-edit-window-number))
    ;; and when ECB is "not active" the original runs, with its argument
    (view-mode 1)
    (ecb-compat-test-as-if-not-ecb
      (view-mode-exit t))
    (should-not view-mode)))

;;; ----------------------------------------------------------------------
;;; bs-show
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-bs-show-from-ecb-window ()
  "Called from a tree window, bs-show first moves to an edit window; when
*buffer-selection* counts as a compilation buffer and a compile window
exists, the selection buffer is shown there."
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((before (ecb-test-visible-ecb-buffers)))
      (unwind-protect
          (progn
            (ecb-goto-window-methods)
            (bs-show nil)
            (should (get-buffer-window "*buffer-selection*"))
            (should-not (member (get-buffer-window "*buffer-selection*")
                                (ecb-canonical-ecb-windows-list)))
            (should (equal (ecb-test-visible-ecb-buffers) before))
            (bs-kill)
            (ecb-test-with-compile-window
              (let ((ecb-compilation-buffer-names '(("*buffer-selection*" . nil))))
                (ecb-goto-window-methods)
                (bs-show nil)
                (should (eq (window-buffer ecb-compile-window)
                            (get-buffer "*buffer-selection*")))
                (should (equal (ecb-test-visible-ecb-buffers) before))
                (bs-kill))))
        (when (get-buffer "*buffer-selection*")
          (kill-buffer "*buffer-selection*"))))))

;;; ----------------------------------------------------------------------
;;; scroll-other-window (ecb-layout, but a compatibility concern)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-compat-scroll-other-window-targets-other-edit-window ()
  (ecb-test-with-ecb proj
    (let ((wins (ecb-compat-test-two-edit-windows proj))
          (ecb-scroll-other-window-scrolls-compile-window nil)
          (ecb-other-window-behavior 'only-edit))
      (ecb-compat-test-make-long (window-buffer (cdr wins)))
      (select-window (car wins))
      (let ((ecb-starts (mapcar #'window-start (ecb-canonical-ecb-windows-list))))
        (scroll-other-window 5)
        (should (> (window-start (cdr wins)) 1))
        (should (= (window-start (car wins)) 1))
        (should (equal (mapcar #'window-start (ecb-canonical-ecb-windows-list)) ecb-starts))))))

(ert-deftest ecb-compat-scroll-other-window-targets-compile-window-when-asked ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-test-with-compile-window
      (let ((buf (get-buffer-create "*ecb-compat-scroll*"))
            (ecb-scroll-other-window-scrolls-compile-window t))
        (unwind-protect
            (progn
              (ecb-compat-test-make-long buf)
              (set-window-buffer ecb-compile-window buf)
              (ecb-select-edit-window)
              (scroll-other-window 3)
              (should (> (window-start ecb-compile-window) 1)))
          (kill-buffer buf))))))

(ert-deftest ecb-compat-scroll-other-window-not-ecb-runs-original ()
  (ecb-test-with-ecb proj
    (let ((wins (ecb-compat-test-two-edit-windows proj)))
      (ecb-compat-test-make-long (window-buffer (cdr wins)))
      (select-window (car wins))
      (ecb-compat-test-as-if-not-ecb
        (scroll-other-window 5))
      ;; some other window scrolled, this one did not
      (should (= (window-start (car wins)) 1)))))

(provide 'ecb-compatibility-test)
;;; ecb-compatibility-test.el ends here
