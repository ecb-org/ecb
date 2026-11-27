;;; ecb-layout-defs-test.el --- golden tests for ecb-layout-defs.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Every shipped layout is drawn once and its window tree compared against a
;; golden sexp.  Leaves are buffer names; EDIT stands for the edit window.
;; If a layout definition is changed on purpose, update the golden here.

;;; Code:

(require 'ecb-test-helpers)

(defconst ecb-layout-defs-test-goldens
  '(("left1" left
     (horizontal (vertical DIRS (horizontal SOURCES HISTORY) METHODS) EDIT))
    ("left2" left
     (horizontal (vertical DIRS SOURCES) EDIT))
    ("left3" left
     (horizontal (vertical DIRS SOURCES METHODS) EDIT))
    ("left4" left
     (horizontal (vertical DIRS (horizontal SOURCES HISTORY)) EDIT))
    ("left5" left
     (horizontal (vertical DIRS SOURCES HISTORY) EDIT))
    ("left6" left
     (horizontal (vertical SOURCES METHODS HISTORY) EDIT))
    ("left7" left
     (horizontal (vertical DIRS HISTORY METHODS) EDIT))
    ("left8" left
     (horizontal (vertical DIRS SOURCES METHODS HISTORY) EDIT))
    ("left9" left
     (horizontal METHODS EDIT))
    ("left10" left
     (horizontal (vertical METHODS (horizontal SOURCES HISTORY)) EDIT))
    ("left11" left
     (horizontal (vertical METHODS HISTORY) EDIT))
    ("left12" left
     (horizontal HISTORY EDIT))
    ("left13" left
     (horizontal DIRS EDIT))
    ("left14" left
     (horizontal (vertical DIRS HISTORY) EDIT))
    ("left15" left
     (horizontal (vertical DIRS METHODS) EDIT))
    ("right1" right
     (horizontal EDIT (vertical DIRS SOURCES METHODS)))
    ("right2" right
     (horizontal EDIT (vertical DIRS METHODS)))
    ("top1" top
     (vertical (horizontal DIRS SOURCES METHODS) EDIT))
    ("top2" top
     (vertical METHODS EDIT))
    ("leftright1" left-right
     (horizontal (vertical DIRS SOURCES HISTORY) EDIT METHODS))
    ("leftright2" left-right
     (horizontal (vertical DIRS SOURCES) EDIT (vertical METHODS HISTORY)))
    ("leftright3" left-right
     (horizontal DIRS EDIT METHODS))
    ("left-dir-plus-speedbar" left
     (horizontal (vertical DIRS SPEEDBAR) EDIT))
    ("left-analyse" left
     (horizontal (vertical DIRS SOURCES METHODS ANALYSE) EDIT))
    ("leftright-analyse" left-right
     (horizontal (vertical DIRS SOURCES HISTORY) EDIT (vertical METHODS ANALYSE)))
    ("left-symboldef" left
     (horizontal (vertical DIRS SOURCES METHODS SYMBOLDEF) EDIT)))
  "Golden (NAME TYPE TREE) for every shipped layout.")

(defun ecb-layout-defs-test-resolve (tree)
  "Replace the placeholder symbols in TREE with real buffer names."
  (cond ((consp tree) (mapcar #'ecb-layout-defs-test-resolve tree))
        ((eq tree 'EDIT) "*ecb-test-scratch*")
        ((eq tree 'DIRS) ecb-directories-buffer-name)
        ((eq tree 'SOURCES) ecb-sources-buffer-name)
        ((eq tree 'METHODS) ecb-methods-buffer-name)
        ((eq tree 'HISTORY) ecb-history-buffer-name)
        ((eq tree 'ANALYSE) ecb-analyse-buffer-name)
        ((eq tree 'SYMBOLDEF) ecb-symboldef-buffer-name)
        ((eq tree 'SPEEDBAR) ecb-speedbar-buffer-name)
        (t tree)))

(ert-deftest ecb-layout-defs-golden-list-is-complete ()
  "Every registered layout has a golden and vice versa."
  (should (equal (sort (mapcar #'car ecb-layout-defs-test-goldens) #'string<)
                 (ecb-available-layouts-of-type nil))))

(ert-deftest ecb-layout-defs-types-match ()
  (dolist (golden ecb-layout-defs-test-goldens)
    (should (eq (ecb-get-layout-type (car golden)) (cadr golden)))))

(ert-deftest ecb-layout-defs-all-layouts-render-to-golden-trees ()
  "Switch through every layout in one session and compare the window trees."
  (ecb-test-with-ecb _proj
    (dolist (golden ecb-layout-defs-test-goldens)
      (ert-info ((format "layout %s" (car golden)))
        (ecb-layout-switch (car golden))
        (should (equal ecb-layout-name (car golden)))
        (should (equal (ecb-test-window-tree)
                       (ecb-layout-defs-test-resolve (nth 2 golden))))
        ;; point always ends up in an edit window after a switch
        (should (eq (car (ecb-where-is-point)) 'edit))
        ;; every ecb window is dedicated, the edit window is not
        (dolist (w (ecb-canonical-ecb-windows-list))
          (should (window-dedicated-p w)))
        (dolist (w (ecb-canonical-edit-windows-list))
          (should-not (window-dedicated-p w)))))))

(ert-deftest ecb-layout-defs-each-layout-activates-from-scratch ()
  "Each layout can be the one ECB starts with."
  (dolist (golden ecb-layout-defs-test-goldens)
    (ert-info ((format "layout %s" (car golden)))
      (ecb-test-with-temp-project proj
        (ecb-test-prepare-frame)
        (let ((ecb-source-path (list proj))
              (ecb-layout-name (car golden))
              (ecb-compile-window-height nil)
              (ecb-tip-of-the-day nil)
              (ecb-windows-width 0.25)
              (ecb-windows-height 0.33))
          (unwind-protect
              (ecb-test-no-user-input
                (ecb-activate)
                (should (equal (ecb-test-window-tree)
                               (ecb-layout-defs-test-resolve (nth 2 golden)))))
            (ecb-test-force-deactivate)))))))

(ert-deftest ecb-layout-defs-maximize-works-in-single-column-layouts ()
  "Maximizing is implemented for left, right and top layouts."
  (ecb-test-with-ecb _proj
    (dolist (golden ecb-layout-defs-test-goldens)
      (unless (eq (cadr golden) 'left-right)
        (ert-info ((format "layout %s" (car golden)))
          (ecb-layout-switch (car golden))
          (let ((ecb-bufs (mapcar (lambda (w) (buffer-name (window-buffer w)))
                                  (ecb-canonical-ecb-windows-list))))
            (dolist (name ecb-bufs)
              (ecb-maximize-ecb-buffer name)
              (should (equal (ecb-maximized-ecb-buffer-name) name))
              (should (equal (ecb-test-visible-ecb-buffers) (list name))))
            (ecb-undo-maximize-ecb-buffer)
            (should (equal (ecb-test-visible-ecb-buffers) ecb-bufs))))))))

(ert-deftest ecb-layout-defs-maximize-is-noop-in-left-right-layouts ()
  "`ecb-layout-define' deliberately ignores the windows-creator for
left-right layouts, so maximizing leaves the window tree untouched."
  (ecb-test-with-ecb _proj
    (dolist (golden ecb-layout-defs-test-goldens)
      (when (eq (cadr golden) 'left-right)
        (ert-info ((format "layout %s" (car golden)))
          (ecb-layout-switch (car golden))
          (ecb-maximize-ecb-buffer ecb-directories-buffer-name)
          (should (equal (ecb-test-window-tree)
                         (ecb-layout-defs-test-resolve (nth 2 golden))))
          (ecb-undo-maximize-ecb-buffer))))))

(ert-deftest ecb-layout-defs-maximize-state-consistent-in-left-right-layouts ()
  "KNOWN BUG: in left-right layouts `ecb-maximize-ecb-buffer' changes
nothing on screen but still records the buffer as maximized, so
`ecb-buffer-is-maximized-p' and the toggle commands report a state that
does not exist."
  :expected-result :failed
  (ecb-test-with-layout "leftright1" _proj
    (ecb-maximize-ecb-buffer ecb-directories-buffer-name)
    (should (equal (ecb-test-visible-ecb-buffers)
                   (list ecb-directories-buffer-name ecb-sources-buffer-name
                         ecb-history-buffer-name ecb-methods-buffer-name)))
    (should-not (ecb-buffer-is-maximized-p))))

(ert-deftest ecb-layout-defs-hide-and-show-in-every-layout ()
  (ecb-test-with-ecb _proj
    (dolist (golden ecb-layout-defs-test-goldens)
      (ert-info ((format "layout %s" (car golden)))
        (ecb-layout-switch (car golden))
        (ecb-hide-ecb-windows)
        (should (equal (ecb-test-window-tree) "*ecb-test-scratch*"))
        (ecb-show-ecb-windows)
        (should (equal (ecb-test-window-tree)
                       (ecb-layout-defs-test-resolve (nth 2 golden))))))))

(ert-deftest ecb-layout-defs-compile-window-in-every-layout ()
  (ecb-test-with-ecb _proj
    (ecb-test-with-compile-window
      (dolist (golden ecb-layout-defs-test-goldens)
        (ert-info ((format "layout %s" (car golden)))
          (ecb-layout-switch (car golden))
          (should (ecb-compile-window-live-p))
          (let ((tree (ecb-layout-defs-test-resolve (nth 2 golden))))
            ;; nested vertical splits are flattened by `window-tree'
            (should (equal (ecb-test-window-tree)
                           (if (eq (car tree) 'vertical)
                               (append tree '("*Messages*"))
                             `(vertical ,tree "*Messages*"))))))))))

(provide 'ecb-layout-defs-test)
;;; ecb-layout-defs-test.el ends here
