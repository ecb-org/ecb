;;; ecb-file-browser-test.el --- tests for ecb-file-browser.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Directories, sources and history buffers against the project-a fixture:
;; directory scanning and filtering, source-path handling, selection and
;; synchronisation, source filters, history bucketing/filters and the
;; per-directory caches.

;;; Code:

(require 'ecb-test-helpers)

(defun ecb-fb-test-lines (buffer-name)
  "Non-empty lines of tree-buffer BUFFER-NAME."
  (cl-remove-if #'string-empty-p (ecb-test-tree-buffer-lines buffer-name)))

(defun ecb-fb-test-source-names ()
  "Source file names currently shown in the sources buffer."
  (ecb-test-tree-buffer-node-names ecb-sources-buffer-name))

(defun ecb-fb-test-dir-names ()
  "Node names shown in the directories buffer (shrunk root names excluded)."
  (cdr (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))

;;; ----------------------------------------------------------------------
;;; Pure helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-get-sources-sort-function-variants ()
  "Sorting honours `ecb-sources-sort-ignore-case'.
Note: the IGNORE-CASE parameter of `ecb-get-sources-sort-function' is dead;
only the option is consulted."
  (let ((files (list "b.el" "a.c" "C.el" "a.el")))
    (let ((ecb-sources-sort-ignore-case nil))
      (should (equal (sort (copy-sequence files) (ecb-get-sources-sort-function 'name))
                     '("C.el" "a.c" "a.el" "b.el")))
      (should (equal (sort (copy-sequence files) (ecb-get-sources-sort-function 'extension))
                     '("a.c" "C.el" "a.el" "b.el"))))
    (let ((ecb-sources-sort-ignore-case t))
      (should (equal (sort (copy-sequence files) (ecb-get-sources-sort-function 'name))
                     '("a.c" "a.el" "b.el" "C.el")))
      (should (equal (sort (copy-sequence files) (ecb-get-sources-sort-function 'extension))
                     '("a.c" "a.el" "b.el" "C.el"))))
    ;; nil means keep order
    (should (equal (sort (copy-sequence files) (ecb-get-sources-sort-function nil))
                   files))))

(ert-deftest ecb-check-directory-for-source-regexps-uses-default ()
  (ecb-test-with-temp-project proj
    (let ((ecb-source-file-regexps '((".*" . (("~$") ("^keep"))))))
      (should (equal (ecb-check-directory-for-source-regexps proj)
                     '(("~$") ("^keep")))))
    (let ((ecb-source-file-regexps '(("src$" . (("~$") nil))
                                     (".*" . (("x") nil)))))
      (should (equal (ecb-check-directory-for-source-regexps
                      (expand-file-name "src" proj))
                     '(("~$") nil)))
      (should (equal (ecb-check-directory-for-source-regexps proj)
                     '(("x") nil))))))

(ert-deftest ecb-check-directory-for-caching-thresholds ()
  (ecb-test-with-temp-project proj
    (let ((ecb-cache-directory-contents '((".*" . 50)))
          (ecb-cache-directory-contents-not nil))
      (should-not (ecb-check-directory-for-caching proj 10))
      ;; threshold is exclusive
      (should-not (ecb-check-directory-for-caching proj 50))
      (should (ecb-check-directory-for-caching proj 51))
      (should (ecb-check-directory-for-caching proj 500)))
    (let ((ecb-cache-directory-contents '(("project-b" . 0)))
          (ecb-cache-directory-contents-not nil))
      (should-not (ecb-check-directory-for-caching proj 500)))))

(ert-deftest ecb-cache-directory-contents-not-accepts-regexp-list ()
  "KNOWN BUG: `ecb-cache-directory-contents-not' is declared as a plain list
of regexps (see its :type), but `ecb-check-directory-for-caching' calls
`car' on every element, so any customized value signals
wrong-type-argument."
  :expected-result :failed
  (ecb-test-with-temp-project proj
    (let ((ecb-cache-directory-contents '((".*" . 0)))
          (ecb-cache-directory-contents-not '("project-a$")))
      (should-not (ecb-check-directory-for-caching proj 500)))))

(ert-deftest ecb-check-emptyness-of-dir ()
  "\"Empty\" depends on whether sources are shown in the directories buffer."
  (ecb-test-with-temp-project proj
    (ecb-directory-empty-cache-clear)
    (let ((ecb-layout-name "left8")
          (ecb-show-sources-in-directories-buffer 'never))
      (should (ecb-check-emptyness-of-dir (expand-file-name "empty" proj)))
      (should-not (ecb-check-emptyness-of-dir (expand-file-name "src" proj)))
      ;; files only, no subdirs: empty as far as the directories tree goes
      (should (ecb-check-emptyness-of-dir (expand-file-name "src/sub" proj))))
    (ecb-directory-empty-cache-clear)
    (let ((ecb-layout-name "left8")
          (ecb-show-sources-in-directories-buffer 'always))
      ;; .keep is a file, so with sources shown the dir is not empty
      (should-not (ecb-check-emptyness-of-dir (expand-file-name "empty" proj)))
      (should-not (ecb-check-emptyness-of-dir (expand-file-name "src/sub" proj))))
    ;; the result is cached together with the show-sources flag
    (should (equal (ecb-directory-empty-cache-get (expand-file-name "empty" proj))
                   '(nil . t)))))

(ert-deftest ecb-get-source-name-is-basename ()
  (should (equal (ecb-get-source-name "/a/b/c.el") "c.el"))
  (should (equal (ecb-get-source-name "c.el") "c.el")))

(ert-deftest ecb-show-sources-in-directories-buffer-p-variants ()
  (let ((ecb-layout-name "left8"))
    (let ((ecb-show-sources-in-directories-buffer 'always))
      (should (ecb-show-sources-in-directories-buffer-p)))
    (let ((ecb-show-sources-in-directories-buffer 'never))
      (should-not (ecb-show-sources-in-directories-buffer-p)))
    (let ((ecb-show-sources-in-directories-buffer '("left7" "left8")))
      (should (ecb-show-sources-in-directories-buffer-p)))
    (let ((ecb-show-sources-in-directories-buffer '("left7")))
      (should-not (ecb-show-sources-in-directories-buffer-p)))))

;;; ----------------------------------------------------------------------
;;; Directory scanning
;;; ----------------------------------------------------------------------

(ert-deftest ecb-get-files-and-subdirs-splits-and-filters ()
  (ecb-test-with-ecb proj
    (should (equal (ecb-get-files-and-subdirs (expand-file-name "src" proj))
                   '(("alpha.el" "beta.el" "gamma.c") "sub")))
    ;; hidden directory and object/backup files are excluded by default
    (should (equal (ecb-get-files-and-subdirs proj)
                   '(("README.txt") "docs" "empty" "src")))))

(ert-deftest ecb-get-files-and-subdirs-honours-excluded-directories ()
  (ecb-test-with-ecb proj
    (let ((ecb-excluded-directories-regexps '("^\\(docs\\|\\.[^xX]*\\)$")))
      (ecb-files-and-subdirs-cache-clear)
      (should (equal (cdr (ecb-get-files-and-subdirs proj)) '("empty" "src"))))
    (let ((ecb-excluded-directories-regexps nil))
      (ecb-files-and-subdirs-cache-clear)
      (should (member ".hidden" (cdr (ecb-get-files-and-subdirs proj)))))))

(ert-deftest ecb-get-files-and-subdirs-honours-source-file-regexps ()
  (ecb-test-with-ecb proj
    (let ((ecb-source-file-regexps '((".*" . (("\\.c$") nil)))))
      (ecb-files-and-subdirs-cache-clear)
      (should (equal (car (ecb-get-files-and-subdirs (expand-file-name "src" proj)))
                     '("alpha.el" "alpha.el~" "beta.el" "gamma.o"))))
    ;; include-regexp wins over exclude-regexp
    (let ((ecb-source-file-regexps '((".*" . (("\\.el$") ("^alpha"))))))
      (ecb-files-and-subdirs-cache-clear)
      (should (equal (car (ecb-get-files-and-subdirs (expand-file-name "src" proj)))
                     '("alpha.el" "alpha.el~" "gamma.c" "gamma.o"))))))

(ert-deftest ecb-get-files-and-subdirs-sorting ()
  (ecb-test-with-ecb proj
    (let ((ecb-sources-sort-method 'extension))
      (ecb-files-and-subdirs-cache-clear)
      (should (equal (car (ecb-get-files-and-subdirs (expand-file-name "src" proj)))
                     '("gamma.c" "alpha.el" "beta.el"))))))

(ert-deftest ecb-files-and-subdirs-cache-is-used ()
  (ecb-test-with-ecb proj
    (let ((ecb-cache-directory-contents '((".*" . 0))))
      (ecb-files-and-subdirs-cache-clear)
      (ecb-get-files-and-subdirs (expand-file-name "src" proj))
      (should (ecb-files-and-subdirs-cache-get (expand-file-name "src" proj)))
      ;; a new file is invisible until the cache entry is removed
      (with-temp-file (expand-file-name "src/new.el" proj) (insert ";;"))
      (should-not (member "new.el" (car (ecb-get-files-and-subdirs (expand-file-name "src" proj)))))
      (ecb-files-and-subdirs-cache-remove (expand-file-name "src" proj))
      (should (member "new.el" (car (ecb-get-files-and-subdirs (expand-file-name "src" proj))))))))

(ert-deftest ecb-files-and-subdirs-cache-can-be-disabled-per-dir ()
  (ecb-test-with-ecb proj
    (let ((ecb-cache-directory-contents '(("project-a$" . 0))))
      (ecb-files-and-subdirs-cache-clear)
      (ecb-get-files-and-subdirs (expand-file-name "src" proj))
      (should-not (ecb-files-and-subdirs-cache-get (expand-file-name "src" proj)))
      (ecb-get-files-and-subdirs proj)
      (should (ecb-files-and-subdirs-cache-get proj)))))

;;; ----------------------------------------------------------------------
;;; Source paths
;;; ----------------------------------------------------------------------

(ert-deftest ecb-normed-source-paths-expands-and-keeps-alias ()
  (ecb-test-with-ecb proj
    (let ((ecb-source-path (list proj (list (expand-file-name "src" proj) "Sources"))))
      (let ((normed (ecb-normed-source-paths)))
        (should (= (length normed) 2))
        (should (equal (substring-no-properties (car (car normed))) proj))
        (should (equal (substring-no-properties (car (cadr normed)))
                       (expand-file-name "src" proj)))
        (should (equal (substring-no-properties (cdr (cadr normed))) "Sources"))))))

(ert-deftest ecb-normed-source-paths-removes-duplicates-on-request ()
  (ecb-test-with-ecb proj
    (let ((ecb-source-path (list proj proj)))
      (should (= (length (ecb-normed-source-paths)) 2))
      (should (= (length (ecb-normed-source-paths t)) 1)))))

(ert-deftest ecb-matching-and-best-matching-source-path ()
  (ecb-test-with-ecb proj
    (let* ((src (expand-file-name "src" proj))
           (ecb-source-path (list proj src)))
      (let ((matching (ecb-matching-source-paths (expand-file-name "src/sub/delta.py" proj) t)))
        (should (= (length matching) 2))
        ;; sorted: longest (most specific) first
        (should (equal (substring-no-properties (car (car matching))) src)))
      (should (equal (substring-no-properties
                      (car (ecb-get-best-matching-source-path (expand-file-name "src/alpha.el" proj))))
                     src))
      (should (equal (substring-no-properties
                      (car (ecb-get-best-matching-source-path (expand-file-name "README.txt" proj))))
                     proj))
      (should-not (ecb-get-best-matching-source-path "/nowhere/file.el")))))

(ert-deftest ecb-source-path-functions-are-consulted ()
  (ecb-test-with-ecb proj
    (let ((ecb-source-path-functions
           (list (lambda () (list (expand-file-name "docs" proj))))))
      (should (equal (ecb-get-source-paths-from-functions)
                     (list (expand-file-name "docs" proj))))
      (ecb-update-directories-buffer)
      (should (member "docs" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name))))))

(ert-deftest ecb-add-source-path-updates-directories-buffer ()
  (ecb-test-with-ecb proj
    (let ((b (ecb-test-copy-fixture "project-b" (file-name-directory (directory-file-name proj)))))
      (cl-letf (((symbol-function 'ecb-customize-save-variable) #'ignore)
                ((symbol-function 'customize-set-variable) (lambda (sym val) (set sym val))))
        (ecb-add-source-path b "B" t))
      (should (equal (cadr ecb-source-path) (list b "B")))
      (should (member "B" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))
      ;; the alias node carries the real path as data
      (let ((node (ecb-test-find-displayed-node ecb-directories-buffer-name
                                                (lambda (n) (equal (substring-no-properties (tree-node->name n)) "B")))))
        (should node)
        (should (equal (tree-node->data node) b))))))

(ert-deftest ecb-update-directories-buffer-reflects-source-path-changes ()
  (ecb-test-with-ecb proj
    (let ((b (ecb-test-copy-fixture "project-b" (file-name-directory (directory-file-name proj)))))
      (let ((ecb-source-path (list (list proj "A") (list b "B"))))
        (ecb-update-directories-buffer)
        ;; A is the selected directory and therefore expanded
        (should (equal (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)
                       '("A" "docs" "empty" "src" "B"))))
      (let ((ecb-source-path (list b)))
        (ecb-update-directories-buffer)
        (should (= (length (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)) 1))))))

;;; ----------------------------------------------------------------------
;;; Directories buffer
;;; ----------------------------------------------------------------------

(ert-deftest ecb-directories-buffer-initial-content ()
  (ecb-test-with-ecb proj
    ;; root expanded because it is the selected directory; children collapsed
    (should (equal (ecb-fb-test-dir-names) '("docs" "empty" "src")))
    (should (equal (cdr (ecb-fb-test-lines ecb-directories-buffer-name))
                   '(" |  [+] docs" " |  [+] empty" " `- [+] src")))
    (should (equal (substring-no-properties ecb-path-selected-directory) proj))))

(ert-deftest ecb-expand-directory-nodes-and-emptyness-markers ()
  (ecb-test-with-ecb proj
    (ecb-expand-directory-nodes 1)
    (should (equal (ecb-fb-test-dir-names) '("docs" "empty" "src" "sub")))
    ;; after expanding, dirs without subdirs are marked [x]
    (should (equal (cdr (ecb-fb-test-lines ecb-directories-buffer-name))
                   '(" |  [x] docs" " |  [x] empty" " `- [-] src" "     `- [x] sub")))
    (ecb-expand-directory-nodes 0)
    (should (equal (ecb-fb-test-dir-names) '("docs" "empty" "src")))))

(ert-deftest ecb-set-selected-directory-updates-sources-and-expands ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src/sub" proj))
    (should (equal (substring-no-properties ecb-path-selected-directory)
                   (expand-file-name "src/sub" proj)))
    (should (equal (ecb-fb-test-source-names) '("delta.py" "epsilon.rb")))
    ;; the directory tree got expanded down to the selection
    (should (member "sub" (ecb-fb-test-dir-names)))
    ;; and the node exists in the tree with the full path as data
    (should (ecb-test-find-displayed-node
             ecb-directories-buffer-name
             (lambda (n) (equal (tree-node->data n) (expand-file-name "src/sub" proj)))))))

(ert-deftest ecb-sync-with-file-outside-source-path-adds-path ()
  "Visiting a file outside `ecb-source-path' adds a temporary source path
according to `ecb-add-path-for-not-matching-files'."
  (ecb-test-with-ecb proj
    (let ((b (ecb-test-copy-fixture "project-b" (file-name-directory (directory-file-name proj)))))
      (cl-letf (((symbol-function 'ecb-customize-save-variable) #'ignore))
        ;; (nil . nil): add the file's directory, do not ask
        (let ((ecb-add-path-for-not-matching-files '(nil . nil)))
          (find-file (expand-file-name "lib/util.el" b))
          (ecb-window-sync)
          (should (equal (ecb-fb-test-source-names) '("util.el")))
          (should (member (expand-file-name "lib" b)
                          (mapcar (lambda (p) (if (consp p) (car p) p)) ecb-source-path))))))))

(ert-deftest ecb-sync-with-file-outside-source-path-adds-root ()
  (ecb-test-with-ecb proj
    (let ((b (ecb-test-copy-fixture "project-b" (file-name-directory (directory-file-name proj)))))
      (cl-letf (((symbol-function 'ecb-customize-save-variable) #'ignore))
        ;; (t . nil): add the filesystem root instead
        (let ((ecb-add-path-for-not-matching-files '(t . nil)))
          (find-file (expand-file-name "lib/util.el" b))
          (ecb-window-sync)
          (should (member "/" (mapcar (lambda (p) (if (consp p) (car p) p)) ecb-source-path))))))))

(ert-deftest ecb-show-sources-in-directories-buffer ()
  (ecb-test-with-layout "left13" proj
    (should (ecb-show-sources-in-directories-buffer-p))
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (should (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))
    (should (member "gamma.c" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))
    (should-not (member "gamma.o" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name)))))

(ert-deftest ecb-directory-node-click-selects-directory ()
  (ecb-test-with-ecb proj
    (let ((node (ecb-test-find-displayed-node ecb-directories-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "src")))))
      (should node)
      (with-current-buffer ecb-directories-buffer-name
        (ecb-directory-or-source-clicked node 1 nil nil nil))
      (should (equal (substring-no-properties ecb-path-selected-directory)
                     (expand-file-name "src" proj)))
      (should (equal (ecb-fb-test-source-names) '("alpha.el" "beta.el" "gamma.c"))))))

(ert-deftest ecb-directory-node-expansion-callback ()
  (ecb-test-with-ecb proj
    (let ((node (ecb-test-find-displayed-node ecb-directories-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "src")))))
      (with-current-buffer ecb-directories-buffer-name
        (ecb-update-directory-node node)
        (should (= (length (tree-node->children node)) 1))
        (should (equal (substring-no-properties (tree-node->name (car (tree-node->children node))))
                       "sub"))))))

;;; ----------------------------------------------------------------------
;;; Sources buffer
;;; ----------------------------------------------------------------------

(ert-deftest ecb-sources-buffer-lists-filtered-files ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (should (equal (ecb-fb-test-source-names) '("alpha.el" "beta.el" "gamma.c")))
    (should (equal (ecb-test-tree-buffer-node-data ecb-sources-buffer-name)
                   (mapcar (lambda (f) (expand-file-name f (expand-file-name "src" proj)))
                           '("alpha.el" "beta.el" "gamma.c"))))))

(ert-deftest ecb-sources-filter-by-regexp-and-ext ()
  (ecb-test-with-ecb proj
    (let ((src (expand-file-name "src" proj)))
      (ecb-set-selected-directory src)
      (ecb-sources-filter-by-regexp "^a")
      (should (equal (ecb-fb-test-source-names) '("alpha.el")))
      (should (equal (ecb-sources-filter-modeline-prefix nil src nil) "[Filter: ^a]"))
      (ecb-sources-filter-by-ext "c")
      (should (equal (ecb-fb-test-source-names) '("gamma.c")))
      (should (equal (ecb-sources-filter-modeline-prefix nil src nil) "[Filter: *.c]"))
      (ecb-apply-filter-to-sources-buffer nil)
      (should (equal (ecb-fb-test-source-names) '("alpha.el" "beta.el" "gamma.c")))
      (should-not (ecb-sources-filter-modeline-prefix nil src nil)))))

(ert-deftest ecb-sources-filter-is-per-directory ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (ecb-sources-filter-by-regexp "^a")
    (ecb-set-selected-directory proj)
    (should (equal (ecb-fb-test-source-names) '("README.txt")))
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (should (equal (ecb-fb-test-source-names) '("alpha.el")))))

(ert-deftest ecb-sources-filter-with-empty-result-is-not-applied ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (ecb-sources-filter-by-regexp "nothing-matches")
    (should (= (length (ecb-fb-test-source-names)) 3))
    (should-not (ecb-sources-filter-modeline-prefix nil (expand-file-name "src" proj) nil))))

(ert-deftest ecb-sources-filter-interactive-choices ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (cl-letf (((symbol-function 'ecb-query-string) (lambda (&rest _) "extension"))
              ((symbol-function 'read-string) (lambda (&rest _) "el")))
      (ecb-sources-filter))
    (should (equal (ecb-fb-test-source-names) '("alpha.el" "beta.el")))
    (cl-letf (((symbol-function 'ecb-query-string) (lambda (&rest _) "nothing")))
      (ecb-sources-filter))
    (should (= (length (ecb-fb-test-source-names)) 3))))

(ert-deftest ecb-source-node-click-opens-file ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (let ((node (ecb-test-find-displayed-node ecb-sources-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "beta.el")))))
      (should node)
      (with-current-buffer ecb-sources-buffer-name
        (ecb-source-clicked node 1 nil nil nil))
      (should (equal (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list)))) "beta.el"))
      (should (equal (ecb-path-selected-source 'file) (expand-file-name "src/beta.el" proj))))))

(ert-deftest ecb-select-source-and-path-selected-source ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/gamma.c" proj))
    (ecb-path-selected-source-set (expand-file-name "src/gamma.c" proj) (current-buffer))
    (ecb-select-source)
    (should (equal (ecb-path-selected-source 'file) (expand-file-name "src/gamma.c" proj)))
    (should (equal (buffer-name (ecb-path-selected-source 'buffer)) "gamma.c"))
    ;; sources buffer highlights the file
    (with-current-buffer ecb-sources-buffer-name
      (should (tree-buffer-highlighted-node-matches-data/name-p
               (expand-file-name "src/gamma.c" proj) nil)))))

;;; ----------------------------------------------------------------------
;;; Synchronisation
;;; ----------------------------------------------------------------------

(ert-deftest ecb-window-sync-follows-visited-file ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/sub/delta.py" proj))
    (ecb-window-sync)
    (should (equal (substring-no-properties ecb-path-selected-directory)
                   (expand-file-name "src/sub" proj)))
    (should (equal (ecb-fb-test-source-names) '("delta.py" "epsilon.rb")))
    (should (member "sub" (ecb-fb-test-dir-names)))))

(ert-deftest ecb-window-sync-ignores-non-file-buffers ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (switch-to-buffer "*scratch*")
    (ecb-window-sync)
    ;; selection unchanged
    (should (equal (ecb-path-selected-source 'file) (expand-file-name "src/alpha.el" proj)))))

(ert-deftest ecb-window-sync-dired-buffer-selects-directory ()
  (ecb-test-with-ecb proj
    (dired (expand-file-name "src" proj))
    (ecb-window-sync)
    (should (equal (substring-no-properties ecb-path-selected-directory)
                   (expand-file-name "src" proj)))
    (should (equal (ecb-fb-test-source-names) '("alpha.el" "beta.el" "gamma.c")))))

(ert-deftest ecb-basic-buffer-sync-respects-exclude-option ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (let ((ecb-basic-buffer-sync '(text-mode)))
      (find-file (expand-file-name "README.txt" proj))
      (ecb-basic-buffer-sync)
      ;; text-mode buffers are excluded from auto sync
      (should (equal (ecb-path-selected-source 'file) (expand-file-name "src/alpha.el" proj)))
      ;; but a forced sync still works
      (ecb-basic-buffer-sync t)
      (should (equal (ecb-path-selected-source 'file) (expand-file-name "README.txt" proj))))))

;;; ----------------------------------------------------------------------
;;; History buffer
;;; ----------------------------------------------------------------------

(defun ecb-fb-test-visit-three (proj)
  "Visit three fixture files and sync; return nothing."
  (find-file (expand-file-name "src/alpha.el" proj))
  (find-file (expand-file-name "src/beta.el" proj))
  (find-file (expand-file-name "README.txt" proj))
  (ecb-window-sync))

(ert-deftest ecb-history-buckets-by-directory ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (should (equal (cl-remove-if (lambda (l) (string-match-p "^\\[-\\]" l))
                                 (ecb-fb-test-lines ecb-history-buffer-name))
                   '(" `- README.txt" " |  alpha.el" " `- beta.el")))
    ;; bucket nodes carry the directory as data
    (should (member (expand-file-name "src" proj)
                    (mapcar (lambda (d) (if (stringp d) d ""))
                            (ecb-test-tree-buffer-node-data ecb-history-buffer-name))))))

(ert-deftest ecb-history-bucket-modes ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (let ((ecb-history-make-buckets 'never))
      (ecb-add-all-buffers-to-history)
      (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                     '("alpha.el" "beta.el" "README.txt"))))
    (let ((ecb-history-make-buckets 'extension))
      (ecb-add-all-buffers-to-history)
      (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                     '("[-] .el" " |  alpha.el" " `- beta.el" "[-] .txt" " `- README.txt"))))
    (let ((ecb-history-make-buckets 'mode))
      (ecb-add-all-buffers-to-history)
      (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                     '("[-] emacs-lisp-mode" " |  alpha.el" " `- beta.el"
                       "[-] text-mode" " `- README.txt"))))
    (let ((ecb-history-make-buckets '("^[a-m]" "^[n-z]")))
      (ecb-add-all-buffers-to-history)
      (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                     '("[-] ^[a-m]" " |  alpha.el" " `- beta.el" "[-] ^[n-z]" " `- README.txt"))))))

(ert-deftest ecb-history-sort-method ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/beta.el" proj))
    (find-file (expand-file-name "src/gamma.c" proj))
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((ecb-history-make-buckets 'never))
      (let ((ecb-history-sort-method 'name))
        (ecb-add-all-buffers-to-history)
        (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                       '("alpha.el" "beta.el" "gamma.c"))))
      (let ((ecb-history-sort-method 'extension))
        (ecb-add-all-buffers-to-history)
        (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                       '("gamma.c" "alpha.el" "beta.el"))))
      (let ((ecb-history-sort-method nil))
        (ecb-add-all-buffers-to-history)
        ;; see `ecb-history-unsorted-is-most-recent-first' for the order
        (should (equal (sort (ecb-fb-test-lines ecb-history-buffer-name) #'string<)
                       '("alpha.el" "beta.el" "gamma.c")))))))

(ert-deftest ecb-history-unsorted-is-most-recent-first ()
  "KNOWN BUG: `ecb-history-sort-method' nil promises the most recently used
buffers on top, but `ecb-add-buffers-to-history-new' calls
`(reverse (buffer-list))' -- `buffer-list' is already most-recent-first, so
the history ends up least-recent-first."
  :expected-result :failed
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/beta.el" proj))
    (find-file (expand-file-name "src/gamma.c" proj))
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((ecb-history-make-buckets 'never)
          (ecb-history-sort-method nil))
      (ecb-add-all-buffers-to-history)
      (should (equal (ecb-fb-test-lines ecb-history-buffer-name)
                     '("alpha.el" "gamma.c" "beta.el"))))))

(ert-deftest ecb-history-filter-by-regexp-and-ext ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (ecb-history-filter-by-regexp "\\.el$")
    (should (equal (ecb-test-tree-buffer-node-names ecb-history-buffer-name)
                   (list (ecb-test-strip (format "%s" (expand-file-name "src" proj))) "alpha.el" "beta.el")))
    (should-not (ecb-history-filter-reset-p))
    (should (string-match-p "Filter" (ecb-history-filter-modeline-prefix nil nil nil)))
    (ecb-history-filter-by-ext "txt")
    (should (equal (cdr (ecb-test-tree-buffer-node-names ecb-history-buffer-name)) '("README.txt")))
    (ecb-reset-history-filter)
    (should (ecb-history-filter-reset-p))
    (ecb-add-all-buffers-to-history)
    (should (= (length (cl-remove-if-not (lambda (n) (string-match-p "\\.\\(el\\|txt\\)$" n))
                                         (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
               3))))

(ert-deftest ecb-history-exclude-file-regexps ()
  (ecb-test-with-ecb proj
    (let ((ecb-history-exclude-file-regexps '("README")))
      (ecb-fb-test-visit-three proj)
      (should-not (member "README.txt" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
      (should (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name))))))

(ert-deftest ecb-kill-buffer-clears-history-auto ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (let ((ecb-kill-buffer-clears-history 'auto))
      (kill-buffer "beta.el")
      (should-not (member "beta.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
      (should (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name))))))

(ert-deftest ecb-kill-buffer-keeps-history-entry-when-nil ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (let ((ecb-kill-buffer-clears-history nil))
      (kill-buffer "beta.el")
      (should (member "beta.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
      ;; the entry is tracked as dead and survives a normal sync
      (should (equal (mapcar #'car (ecb-history-content-all-dead-buffers-alist)) '("beta.el")))
      (ecb-window-sync)
      (should (member "beta.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
      ;; rebuilding from the live buffers drops it
      (ecb-add-all-buffers-to-history)
      (should-not (member "beta.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name))))))

(ert-deftest ecb-kill-buffer-clears-history-ask ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (let ((ecb-kill-buffer-clears-history 'ask))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (kill-buffer "beta.el"))
      (should (member "beta.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (kill-buffer "alpha.el"))
      (should-not (member "alpha.el" (ecb-test-tree-buffer-node-names ecb-history-buffer-name))))))

(ert-deftest ecb-history-node-click-switches-buffer ()
  (ecb-test-with-ecb proj
    (ecb-fb-test-visit-three proj)
    (let ((node (ecb-test-find-displayed-node ecb-history-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "alpha.el")))))
      (should node)
      (with-current-buffer ecb-history-buffer-name
        (ecb-history-clicked node 1 nil nil nil))
      (should (equal (buffer-name (window-buffer (car (ecb-canonical-edit-windows-list)))) "alpha.el")))))

(ert-deftest ecb-history-indirect-buffers-stick-to-base ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (let ((ind (make-indirect-buffer "alpha.el" "alpha-ind" t)))
      (unwind-protect
          (progn
            (switch-to-buffer ind)
            (ecb-window-sync)
            (let ((ecb-history-stick-indirect-buffers-to-basebuffer t))
              (ecb-add-all-buffers-to-history)
              (let ((names (ecb-test-tree-buffer-node-names ecb-history-buffer-name)))
                (should (member "alpha.el" names))
                (should (member "alpha-ind" names))
                (should (< (cl-position "alpha.el" names :test #'equal)
                           (cl-position "alpha-ind" names :test #'equal)))))
            (should (equal (mapcar #'buffer-name (ecb-indirect-buffers-of-buffer (get-buffer "alpha.el")))
                           '("alpha-ind"))))
        (kill-buffer ind)))))

;;; ----------------------------------------------------------------------
;;; Caches
;;; ----------------------------------------------------------------------

(ert-deftest ecb-sources-cache-roundtrip ()
  (ecb-test-with-ecb proj
    (let ((src (expand-file-name "src" proj))
          (ecb-cache-directory-contents '((".*" . 0))))
      (ecb-set-selected-directory src t)
      (should (ecb-sources-cache-get-full src))
      (should-not (ecb-sources-cache-get-filtered src))
      (ecb-sources-filter-by-regexp "^a")
      (should (ecb-sources-cache-get-filtered src))
      (ecb-sources-cache-remove src)
      (should-not (ecb-sources-cache-get-full src))
      (should-not (ecb-sources-cache-get-filtered src)))))

(ert-deftest ecb-directory-empty-cache ()
  (ecb-test-with-ecb proj
    (ecb-directory-empty-cache-clear)
    (ecb-directory-empty-cache-add (expand-file-name "empty" proj) t)
    (ecb-directory-empty-cache-add (expand-file-name "src" proj) nil)
    (should (eq (ecb-directory-empty-cache-get (expand-file-name "empty" proj)) t))
    (should (eq (ecb-directory-empty-cache-get (expand-file-name "src" proj)) nil))
    (ecb-directory-empty-cache-add (expand-file-name "src" proj) 'x)
    (should (eq (ecb-directory-empty-cache-get (expand-file-name "src" proj)) 'x))
    (ecb-directory-empty-cache-remove (expand-file-name "src" proj))
    (should-not (ecb-directory-empty-cache-get (expand-file-name "src" proj)))
    (ecb-directory-empty-cache-remove-all proj)
    (should-not (ecb-directory-empty-cache-get (expand-file-name "empty" proj)))))

(ert-deftest ecb-remove-dir-from-caches-clears-everything ()
  (ecb-test-with-ecb proj
    (let ((src (expand-file-name "src" proj)))
      (ecb-set-selected-directory src)
      (ecb-get-files-and-subdirs src)
      (ecb-remove-dir-from-caches src)
      (should-not (ecb-sources-cache-get-full src))
      (should-not (ecb-files-and-subdirs-cache-get src)))))

(ert-deftest ecb-file-browser-initialize-clears-selection ()
  (ecb-test-with-ecb proj
    (find-file (expand-file-name "src/alpha.el" proj))
    (ecb-window-sync)
    (ecb-file-browser-initialize)
    (should-not ecb-path-selected-directory)
    (should-not (ecb-path-selected-source))
    ;; and the sources cache is gone too
    (should-not (ecb-sources-cache-get-full (expand-file-name "src" proj)))))

;;; ----------------------------------------------------------------------
;;; Popup commands that are safe to run headless
;;; ----------------------------------------------------------------------

(ert-deftest ecb-create-directory-makes-dir-and-refreshes ()
  (ecb-test-with-ecb proj
    (let ((node (ecb-test-find-displayed-node ecb-directories-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "src")))))
      (cl-letf (((symbol-function 'read-from-minibuffer) (lambda (&rest _) "newdir")))
        (with-current-buffer ecb-directories-buffer-name
          (ecb-create-directory node)))
      (should (file-directory-p (expand-file-name "src/newdir" proj)))
      (ecb-expand-directory-nodes 1)
      (should (member "newdir" (ecb-test-tree-buffer-node-names ecb-directories-buffer-name))))))

(ert-deftest ecb-delete-source-removes-file ()
  (ecb-test-with-ecb proj
    (ecb-set-selected-directory (expand-file-name "src" proj))
    (let ((node (ecb-test-find-displayed-node ecb-sources-buffer-name
                                              (lambda (n) (equal (substring-no-properties (tree-node->name n)) "beta.el")))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (with-current-buffer ecb-sources-buffer-name
          (ecb-delete-source node)))
      (should-not (file-exists-p (expand-file-name "src/beta.el" proj)))
      (should (equal (ecb-fb-test-source-names) '("alpha.el" "gamma.c"))))))

(provide 'ecb-file-browser-test)
;;; ecb-file-browser-test.el ends here
