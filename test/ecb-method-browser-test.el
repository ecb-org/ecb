;;; ecb-method-browser-test.el --- tests for ecb-method-browser.el -*- lexical-binding: t -*-

;; Author: Ryan Ware <ware@opensourceware.me>
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; The Methods buffer against the project-a fixture: semantic sources
;; (alpha.el, gamma.c), the imenu/speedbar fallback for non-semantic
;; sources (epsilon.rb), node display options, expansion, tag filters,
;; tag synchronisation with point, rebuilding and the tag-tree cache.
;;
;; Batch notes: semantic's idle scheduler never runs in batch, so the
;; helpers switch it off and set
;; `ecb-force-reparse-when-semantic-idle-scheduler-off' so ECB parses
;; synchronously.  `ecb-expand-methods-nodes' reads the level via
;; `ecb-read-number', which loops until it gets a number, so it is stubbed.

;;; Code:

(require 'ecb-test-helpers)

(defconst ecb-mb-test-alpha-lines
  '("[+] Variables" "(alpha-one)" "(alpha-two x y)" "(alpha-with body)" "[+] Provides")
  "Methods buffer for alpha.el with the default options.")

(defconst ecb-mb-test-gamma-lines
  '("[+] Includes" "[-] point : struct" " `- [+] Attributes"
    "main (argc : int,argv : char**) : int" "-helper (a : int) : int")
  "Methods buffer for gamma.c with the default options.")

(defun ecb-mb-test-lines ()
  "Non-empty lines of the methods buffer."
  (cl-remove-if #'string-empty-p (ecb-test-tree-buffer-lines ecb-methods-buffer-name)))

(defun ecb-mb-test-visit (proj rel)
  "Visit REL under PROJ in the edit window and synchronise ECB."
  (find-file (expand-file-name rel proj))
  (ecb-window-sync)
  (current-buffer))

(defun ecb-mb-test-highlighted-tag-name ()
  "Name of the semantic tag currently highlighted in the methods buffer."
  (with-current-buffer ecb-methods-buffer-name
    ;; the car of `tree-buffer-highlighted-node' is the node's data
    (and tree-buffer-highlighted-node
         (let ((data (car tree-buffer-highlighted-node)))
           (and (semantic-tag-p data) (semantic-tag-name data))))))

(defun ecb-mb-test-tag-node (name)
  "Displayed node of the methods buffer whose semantic tag is called NAME."
  (ecb-test-find-displayed-node
   ecb-methods-buffer-name
   (lambda (n) (let ((d (tree-node->data n)))
                 (and (semantic-tag-p d) (equal (semantic-tag-name d) name))))))

(defun ecb-mb-test-expand (level)
  "Run `ecb-expand-methods-nodes' with LEVEL answered to the prompt."
  (cl-letf (((symbol-function 'ecb-read-number) (lambda (&rest _) level)))
    (ecb-expand-methods-nodes)))

(defun ecb-mb-test-prefix ()
  "Current filter modeline prefix of the methods buffer (string or nil)."
  (let ((p (ecb-methods-filter-modeline-prefix ecb-methods-buffer-name nil
                                               (ecb-path-selected-source))))
    (and p (substring-no-properties p))))

;;; ----------------------------------------------------------------------
;;; Pure helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-string-make-singular-strips-trailing-s ()
  (should (equal (ecb-string-make-singular "Variables") "Variable"))
  (should (equal (ecb-string-make-singular "class") "clas"))
  (should (equal (ecb-string-make-singular "type") "type")))

(ert-deftest ecb-normalize-expand-spec ()
  (should (eq (ecb-normalize-expand-spec 'all) 'all))
  (should (equal (ecb-normalize-expand-spec '(Types Variables function))
                 '(type variable function))))

(ert-deftest ecb-access-order-ranks-protection ()
  (should (< (ecb-access-order 'public) (ecb-access-order 'protected)))
  (should (< (ecb-access-order 'protected) (ecb-access-order 'private)))
  (should (numberp (ecb-access-order nil))))

(ert-deftest ecb-sort-tags-by-name-and-access ()
  (let ((tags (list (semantic-tag "zeta" 'function)
                    (semantic-tag "alpha" 'function)
                    (semantic-tag "mid" 'function))))
    (should (equal (mapcar #'semantic-tag-name (ecb-sort-tags 'name (copy-sequence tags)))
                   '("alpha" "mid" "zeta")))
    (should (equal (mapcar #'semantic-tag-name (ecb-sort-tags nil (copy-sequence tags)))
                   '("zeta" "alpha" "mid")))
    (should (equal (mapcar #'semantic-tag-name (ecb-sort-tags 'access (copy-sequence tags)))
                   '("alpha" "mid" "zeta")))))

(ert-deftest ecb-get-show-tags-list-default ()
  "The default spec is used for modes without an own entry."
  (let ((ecb-show-tags '((default (function flattened nil) (t hidden nil))
                         (c-mode (type collapsed nil)))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal (ecb-get-show-tags-list)
                     '((function flattened nil) (t hidden nil)))))
    (with-temp-buffer
      (c-mode)
      (should (equal (ecb-get-show-tags-list) '((type collapsed nil)))))))

(ert-deftest ecb-get-tag-display-function-default-and-mode ()
  (let ((ecb-tag-display-function '((default . ecb-format-tag-uml-prototype)
                                    (c-mode . ecb-format-tag-name))))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (eq (ecb-get-tag-display-function) 'ecb-format-tag-uml-prototype)))
    (with-temp-buffer
      (c-mode)
      (should (eq (ecb-get-tag-display-function) 'ecb-format-tag-name)))))

(ert-deftest ecb-tag-tree-cache-clear ()
  (let ((ecb-tag-tree-cache (list (cons "a.el" 'x) (cons "b.el" 'y))))
    (ecb-clear-tag-tree-cache "a.el")
    (should (equal ecb-tag-tree-cache '(("b.el" . y))))
    (ecb-clear-tag-tree-cache)
    (should (null ecb-tag-tree-cache))))

(ert-deftest ecb-current-tag-table-is-buffer-local ()
  (with-temp-buffer
    (should (null (ecb-get-current-tag-table)))
    (should (equal (ecb-set-current-tag-table '(tag)) '(tag)))
    (should (equal (ecb-get-current-tag-table) '(tag))))
  (with-temp-buffer
    (should (null (ecb-get-current-tag-table)))))

(ert-deftest ecb-default-tag-filter-for-current-source-matches-mode-and-file ()
  (ecb-test-with-temp-project proj
    (with-current-buffer (find-file-noselect (expand-file-name "src/gamma.c" proj))
      (unwind-protect
          (progn
            (let ((ecb-default-tag-filter '(((c-mode . "gamma") (tag-class . function)))))
              (should (equal (ecb-default-tag-filter-for-current-source)
                             '((tag-class . function)))))
            (let ((ecb-default-tag-filter '(((c-mode . "other") (tag-class . function)))))
              (should (null (ecb-default-tag-filter-for-current-source))))
            (let ((ecb-default-tag-filter '(((emacs-lisp-mode . "") (tag-class . function)))))
              (should (null (ecb-default-tag-filter-for-current-source)))))
        (kill-buffer)))))

;;; ----------------------------------------------------------------------
;;; Semantic sources: display
;;; ----------------------------------------------------------------------

(ert-deftest ecb-methods-buffer-shows-elisp-tags-bucketed ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (should (ecb--semantic-active-p))
    (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))
    (should (equal (ecb-test-tree-buffer-node-names ecb-methods-buffer-name)
                   '("Variables" "(alpha-one)" "(alpha-two x y)" "(alpha-with body)" "Provides")))))

(ert-deftest ecb-methods-buffer-shows-c-tags-with-type-members ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (should (equal (ecb-mb-test-lines) ecb-mb-test-gamma-lines))))

(ert-deftest ecb-methods-buffer-current-tag-table-after-sync ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (should (equal (mapcar #'semantic-tag-name (ecb-get-current-tag-table))
                   '("alpha-counter" "alpha-option" "alpha-one" "alpha-two" "alpha-with" "alpha")))
    (should (equal (buffer-name (ecb-methods-get-data-store 'source-buffer)) "alpha.el"))
    (should (assoc "alpha.el" ecb-tag-tree-cache))))

(ert-deftest ecb-methods-buffer-honours-tag-display-function ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (let ((ecb-tag-display-function '((default . ecb-format-tag-name))))
      (ecb-rebuild-methods-buffer-fully)
      (should (equal (ecb-mb-test-lines)
                     '("[+] Includes" "[-] point" " `- [+] Attributes" "main" "helper"))))))

(ert-deftest ecb-methods-buffer-honours-show-tags ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (let ((ecb-show-tags '((default (function flattened nil) (variable flattened nil)
                                    (t hidden nil)))))
      (ecb-rebuild-methods-buffer-fully)
      (should (equal (ecb-mb-test-lines)
                     '("(alpha-one)" "(alpha-two x y)" "(alpha-with body)"
                       "alpha-counter" "alpha-option"))))
    (let ((ecb-show-tags '((default (function collapsed name) (t hidden nil)))))
      (ecb-rebuild-methods-buffer-fully)
      (should (equal (ecb-mb-test-lines) '("[+] Defuns"))))
    (let ((ecb-show-tags '((default (function flattened name) (t hidden nil)))))
      (ecb-rebuild-methods-buffer-fully)
      (should (equal (ecb-mb-test-lines)
                     '("(alpha-one)" "(alpha-two x y)" "(alpha-with body)"))))))

(ert-deftest ecb-methods-buffer-c-functions-in-bucket ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (let ((ecb-show-tags '((default (include collapsed nil) (type flattened nil)
                                    (variable collapsed access) (function collapsed name)
                                    (t collapsed nil)))))
      (ecb-rebuild-methods-buffer-fully)
      (should (equal (ecb-mb-test-lines)
                     '("[+] Includes" "[-] point : struct" " `- [+] Attributes" "[+] Functions"))))))

(ert-deftest ecb-methods-buffer-switching-sources-rebuilds ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))
    (ecb-mb-test-visit proj "src/gamma.c")
    (should (equal (ecb-mb-test-lines) ecb-mb-test-gamma-lines))
    (ecb-mb-test-visit proj "src/alpha.el")
    (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))
    (should (assoc "gamma.c" ecb-tag-tree-cache))
    (should (assoc "alpha.el" ecb-tag-tree-cache))))

(ert-deftest ecb-methods-buffer-empty-for-excluded-non-semantic-mode ()
  "text-mode is excluded by default, so README.txt shows no tags."
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "README.txt")
    (should-not (ecb--semantic-active-p))
    (should (null (ecb-mb-test-lines)))))

;;; ----------------------------------------------------------------------
;;; Expansion
;;; ----------------------------------------------------------------------

(ert-deftest ecb-expand-methods-nodes-expands-and-collapses ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (ecb-mb-test-expand 2)
    (should (equal (ecb-mb-test-lines)
                   '("[-] Variables" " |  alpha-counter" " `- alpha-option"
                     "(alpha-one)" "(alpha-two x y)" "(alpha-with body)" "[+] Provides")))
    (ecb-mb-test-expand -1)
    (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))))

(ert-deftest ecb-expand-methods-nodes-c-nested ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (ecb-mb-test-expand 3)
    (should (equal (ecb-mb-test-lines)
                   '("[+] Includes" "[-] point : struct" " `- [-] Attributes"
                     "     |  +x : int" "     `- +y : int"
                     "main (argc : int,argv : char**) : int" "-helper (a : int) : int")))))

(ert-deftest ecb-expand-methods-nodes-switches-off-auto-expand ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (let ((ecb-expand-methods-switch-off-auto-expand t)
          (ecb-auto-expand-tag-tree 'expand-spec))
      (ecb-mb-test-expand 1)
      (should (null ecb-auto-expand-tag-tree)))
    (let ((ecb-expand-methods-switch-off-auto-expand nil)
          (ecb-auto-expand-tag-tree 'expand-spec))
      (ecb-mb-test-expand 1)
      (should (eq ecb-auto-expand-tag-tree 'expand-spec)))))

(ert-deftest ecb-toggle-auto-expand-tag-tree ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (let ((ecb-auto-expand-tag-tree 'expand-spec))
      (ecb-toggle-auto-expand-tag-tree -1)
      (should (null ecb-auto-expand-tag-tree))
      (ecb-toggle-auto-expand-tag-tree 1)
      (should (eq ecb-auto-expand-tag-tree 'expand-spec))
      (ecb-toggle-auto-expand-tag-tree)
      (should (null ecb-auto-expand-tag-tree))
      (ecb-toggle-auto-expand-tag-tree)
      (should (eq ecb-auto-expand-tag-tree 'expand-spec)))))

;;; ----------------------------------------------------------------------
;;; Tag synchronisation / navigation
;;; ----------------------------------------------------------------------

(ert-deftest ecb-tag-sync-highlights-tag-at-point ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-min))
    (search-forward "(list x y)")
    (should (equal (semantic-tag-name (ecb-get-real-curr-tag)) "alpha-two"))
    (ecb-tag-sync t)
    (should (equal (ecb-mb-test-highlighted-tag-name) "alpha-two"))))

(ert-deftest ecb-tag-sync-auto-expands-to-nested-tag ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (goto-char (point-min))
    (search-forward "int x")
    (ecb-tag-sync t)
    (should (equal (ecb-mb-test-highlighted-tag-name) "x"))
    (should (equal (ecb-get-type-name-hierarchy-of-current-tag) '("x" "point")))
    ;; the Attributes bucket got expanded to reveal the member
    (should (member "     |  +x : int" (ecb-mb-test-lines)))))

(ert-deftest ecb-tag-sync-point-outside-tag-or-in-include ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/gamma.c")
    (goto-char (point-min))
    (should (equal (semantic-tag-class (ecb-get-real-curr-tag)) 'include))
    (goto-char (point-min))
    (search-forward "return a * 2")
    (ecb-tag-sync t)
    (should (equal (ecb-mb-test-highlighted-tag-name) "helper"))
    (should (equal (ecb-get-type-name-hierarchy-of-current-tag) '("helper")))))

(ert-deftest ecb-tag-sync-respects-highlight-option ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-min))
    (search-forward "(list x y)")
    (let ((ecb-highlight-tag-with-point nil))
      (ecb-tag-sync)
      (should (null (ecb-mb-test-highlighted-tag-name))))
    (ecb-tag-sync t)
    (should (equal (ecb-mb-test-highlighted-tag-name) "alpha-two"))))

(ert-deftest ecb-method-clicked-jumps-to-tag ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-max))
    (let ((node (ecb-mb-test-tag-node "alpha-two")))
      (should node)
      (with-current-buffer ecb-methods-buffer-name
        (ecb-method-clicked node 1 nil nil nil)))
    (should (equal (buffer-name) "alpha.el"))
    (should (looking-at "(defun alpha-two"))
    (should (ecb-point-in-edit-window-number))))

(ert-deftest ecb-method-clicked-sets-mark-when-configured ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-max))
    (let ((start (point))
          (node (ecb-mb-test-tag-node "alpha-one")))
      (let ((ecb-tag-jump-sets-mark t))
        (with-current-buffer ecb-methods-buffer-name
          (ecb-method-clicked node 1 nil nil nil)))
      (should (= (marker-position (mark-marker)) start)))))

(ert-deftest ecb-methods-node-get-semantic-type ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (let ((bucket (ecb-test-find-displayed-node
                   ecb-methods-buffer-name
                   (lambda (n) (equal (tree-node->name n) "Variables"))))
          (tag (ecb-mb-test-tag-node "alpha-one")))
      (should (eq (ecb-methods-node-get-semantic-type bucket) 'variable))
      (should (eq (ecb-methods-node-get-semantic-type tag) 'function)))))

;;; ----------------------------------------------------------------------
;;; Filters
;;; ----------------------------------------------------------------------

(ert-deftest ecb-methods-filter-by-regexp-and-modeline ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el")))
      (should (null (ecb-mb-test-prefix)))
      (should (ecb-methods-filter-by-regexp nil buf "one"))
      (should (equal (ecb-mb-test-lines) '("(alpha-one)")))
      (should (equal (ecb-mb-test-prefix) "[Regexp: one]"))
      (ecb-methods-filter-nofilter)
      (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))
      (should (null (ecb-mb-test-prefix))))))

(ert-deftest ecb-methods-filter-stack-and-inverse ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el"))
          (ecb-methods-filter-replace-existing 'never))
      (ecb-methods-filter-by-regexp t buf "one")
      (should (equal (ecb-mb-test-lines)
                     '("[+] Variables" "(alpha-two x y)" "(alpha-with body)" "[+] Provides")))
      (should (equal (ecb-mb-test-prefix) "[^Regexp: one]"))
      (ecb-methods-filter-by-regexp t buf "two")
      (should (equal (ecb-mb-test-lines)
                     '("[+] Variables" "(alpha-with body)" "[+] Provides")))
      (should (equal (ecb-mb-test-prefix) "[^Regexp(2): two]"))
      (should (equal (mapcar (lambda (f) (list (nth 0 f) (nth 1 f) (nth 2 f)))
                             (cdr (assoc buf ecb-methods-user-filter-alist)))
                     '((regexp ("one") not) (regexp ("two") not))))
      (ecb-methods-filter-delete-last)
      (should (equal (ecb-mb-test-prefix) "[^Regexp: one]"))
      (ecb-methods-filter-delete-last)
      (should (null (ecb-mb-test-prefix)))
      (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines)))))

(ert-deftest ecb-methods-filter-rejected-when-everything-filtered ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el")))
      (should (ecb-methods-filter-by-regexp nil buf "one"))
      ;; inverse "one" on top would leave nothing -> rejected, stack unchanged
      (should-not (ecb-methods-filter-by-regexp t buf "one"))
      (should (equal (ecb-mb-test-lines) '("(alpha-one)")))
      (should (equal (ecb-mb-test-prefix) "[Regexp: one]")))))

(ert-deftest ecb-methods-filter-by-tag-class ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el")))
      (ecb-methods-filter-by-tag-class nil buf "variable")
      (should (equal (ecb-mb-test-lines) '("[+] Variables")))
      (should (equal (ecb-mb-test-prefix) "[Tagclass: Variables]"))
      ;; a second tag-class filter replaces the first (non-inverse)
      (ecb-methods-filter-by-tag-class nil buf "function")
      (should (equal (ecb-mb-test-lines)
                     '("(alpha-one)" "(alpha-two x y)" "(alpha-with body)")))
      (should (equal (ecb-mb-test-prefix) "[Tagclass: Defuns]")))))

(ert-deftest ecb-methods-filter-by-prot-c ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/gamma.c")))
      (ecb-methods-filter-by-prot nil buf "private")
      (should (equal (ecb-mb-test-lines)
                     '("[+] Includes" "point : struct"
                       "main (argc : int,argv : char**) : int" "-helper (a : int) : int")))
      (should (equal (ecb-mb-test-prefix) "[Prot: private]")))))

(ert-deftest ecb-methods-filter-by-function ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el")))
      (defun ecb-mb-test-only-with (tag _buf)
        (string-match-p "with" (semantic-tag-name tag)))
      (unwind-protect
          (progn
            (ecb-methods-filter-by-function nil buf "ecb-mb-test-only-with")
            (should (equal (ecb-mb-test-lines) '("(alpha-with body)")))
            (should (equal (ecb-mb-test-prefix) "[Function: ecb-mb-test-only-with]")))
        (fmakunbound 'ecb-mb-test-only-with)))))

(ert-deftest ecb-methods-filter-semantic-only-filters-rejected-for-non-semantic ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/sub/epsilon.rb")))
      (should-not (ecb--semantic-active-p))
      (should-error (ecb-methods-filter-apply 'tag-class '(function) nil "Tagclass" "x" buf)))))

(ert-deftest ecb-methods-filter-internal-dispatch ()
  (ecb-test-with-ecb proj
    (let ((buf (ecb-mb-test-visit proj "src/alpha.el")))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "two")))
        (ecb-methods-filter-internal nil 'regexp))
      (should (equal (ecb-mb-test-lines) '("(alpha-two x y)")))
      (ecb-methods-filter-internal nil 'no-filter)
      (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines))
      (should (equal (ecb-get-source-buffer-for-tag-filter) buf)))))

(ert-deftest ecb-methods-filter-is-per-source-buffer ()
  (ecb-test-with-ecb proj
    (let ((alpha (ecb-mb-test-visit proj "src/alpha.el")))
      (ecb-methods-filter-by-regexp nil alpha "one")
      (ecb-mb-test-visit proj "src/gamma.c")
      (should (equal (ecb-mb-test-lines) ecb-mb-test-gamma-lines))
      (should (null (ecb-mb-test-prefix)))
      (ecb-mb-test-visit proj "src/alpha.el")
      (should (equal (ecb-mb-test-lines) '("(alpha-one)")))
      (should (equal (ecb-mb-test-prefix) "[Regexp: one]")))))

(ert-deftest ecb-methods-filter-on-empty-buffer-is-noop ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "README.txt")
    (should (null (ecb-mb-test-lines)))
    (ecb-methods-filter-internal nil 'regexp)
    (should (null ecb-methods-user-filter-alist))))

(ert-deftest ecb-default-tag-filter-applied-on-visit ()
  "`ecb-find-file-hook' defers the default filter to `post-command-hook'
\(which never runs in batch), so it is invoked explicitly here."
  (ecb-test-with-ecb proj
    (let ((ecb-default-tag-filter '(((emacs-lisp-mode . "alpha") (regexp "one" nil)))))
      (should (memq 'ecb-find-file-hook find-file-hook))
      (let ((post-command-hook nil))
        (find-file (expand-file-name "src/alpha.el" proj))
        (should (memq 'ecb-apply-default-tag-filter (default-value 'post-command-hook)))
        (ecb-window-sync)
        (ecb-apply-default-tag-filter)
        (should-not (memq 'ecb-apply-default-tag-filter (default-value 'post-command-hook))))
      (should (equal (ecb-mb-test-lines) '("(alpha-one)")))
      (should (equal (ecb-mb-test-prefix) "[Regexp: one]")))))

;;; ----------------------------------------------------------------------
;;; Rebuilding / updating
;;; ----------------------------------------------------------------------

(ert-deftest ecb-rebuild-methods-buffer-picks-up-edits ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-max))
    (insert "\n(defun alpha-three () nil)\n")
    (ecb-rebuild-methods-buffer)
    (should (member "(alpha-three)" (ecb-mb-test-lines)))
    (ecb-rebuild-methods-buffer t)
    (should (member "(alpha-three)" (ecb-mb-test-lines)))
    (set-buffer-modified-p nil)))

(ert-deftest ecb-update-methods-after-saving-respects-option ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-max))
    (insert "\n(defun alpha-three () nil)\n")
    (let ((ecb-auto-update-methods-after-save nil))
      (save-buffer)
      (should-not (member "(alpha-three)" (ecb-mb-test-lines))))
    (let ((ecb-auto-update-methods-after-save t))
      (insert "\n(defun alpha-four () nil)\n")
      (save-buffer)
      (should (member "(alpha-three)" (ecb-mb-test-lines)))
      (should (member "(alpha-four)" (ecb-mb-test-lines))))))

(ert-deftest ecb-update-methods-after-saving-ignores-non-current-buffer ()
  (ecb-test-with-ecb proj
    (let ((beta (find-file-noselect (expand-file-name "src/beta.el" proj))))
      (ecb-mb-test-visit proj "src/alpha.el")
      (with-current-buffer beta
        (goto-char (point-max))
        (insert "\n(defun beta-new () nil)\n")
        (let ((ecb-auto-update-methods-after-save t))
          (save-buffer)))
      (should (equal (ecb-mb-test-lines) ecb-mb-test-alpha-lines)))))

(ert-deftest ecb-rebuild-methods-buffer-outside-edit-window-is-noop ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (goto-char (point-max))
    (insert "\n(defun alpha-three () nil)\n")
    (select-window (get-buffer-window ecb-methods-buffer-name))
    (ecb-rebuild-methods-buffer-fully)
    (should-not (member "(alpha-three)" (ecb-mb-test-lines)))
    (with-current-buffer "alpha.el" (set-buffer-modified-p nil))))

(ert-deftest ecb-methods-buffer-after-create-hook-runs ()
  (let ((log (ecb-test-with-call-log
               (let ((ecb-methods-buffer-after-create-hook
                      (list (lambda () (ecb-test-record 'created (buffer-name))))))
                 (ecb-test-with-ecb proj
                   (ignore proj))))))
    (should (equal log (list (list 'created ecb-methods-buffer-name))))))

;;; ----------------------------------------------------------------------
;;; Non-semantic sources (imenu fallback)
;;; ----------------------------------------------------------------------

(ert-deftest ecb-non-semantic-source-uses-imenu ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/sub/epsilon.rb")
    (should-not (ecb--semantic-active-p))
    (should (equal (mapcar #'car (ecb-get-tags-for-non-semantic-files)) '("run" "stop")))
    (should (equal (ecb-mb-test-lines) '("run" "stop")))))

(ert-deftest ecb-non-semantic-node-click-jumps-to-position ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/sub/epsilon.rb")
    (goto-char (point-min))
    (let ((node (ecb-test-find-displayed-node
                 ecb-methods-buffer-name
                 (lambda (n) (equal (tree-node->name n) "stop")))))
      (should node)
      (with-current-buffer ecb-methods-buffer-name
        (ecb-method-clicked node 1 nil nil nil)))
    (should (equal (buffer-name) "epsilon.rb"))
    (should (looking-at "def stop"))))

(ert-deftest ecb-non-semantic-source-excluded-mode-shows-nothing ()
  (ecb-test-with-ecb proj
    (let ((ecb-non-semantic-exclude-modes '(ruby-mode text-mode)))
      (ecb-mb-test-visit proj "src/sub/epsilon.rb")
      (should (null (ecb-get-tags-for-non-semantic-files)))
      (should (null (ecb-mb-test-lines))))))

(ert-deftest ecb-non-semantic-source-disabled-shows-nothing ()
  (ecb-test-with-ecb proj
    (let ((ecb-process-non-semantic-files nil))
      (ecb-mb-test-visit proj "src/sub/epsilon.rb")
      (should (null (ecb-mb-test-lines))))))

(ert-deftest ecb-non-semantic-rebuild-picks-up-edits-and-hook-can-veto ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/sub/epsilon.rb")
    (goto-char (point-max))
    (insert "\ndef added\n  3\nend\n")
    (let ((imenu-auto-rescan t))
      (let ((ecb-rebuild-non-semantic-methods-before-hook (list (lambda (_file) nil))))
        (ecb-rebuild-methods-buffer-fully)
        (should (equal (ecb-mb-test-lines) '("run" "stop"))))
      (let* ((seen nil)
             (ecb-rebuild-non-semantic-methods-before-hook
             (list (lambda (file) (setq seen file) t))))
        (ecb-rebuild-methods-buffer-fully)
        (should (equal (file-name-nondirectory seen) "epsilon.rb"))
        (should (equal (ecb-mb-test-lines) '("run" "stop" "added")))))
    (set-buffer-modified-p nil)))

(defun ecb-mb-test-custom-parser (_file)
  "Parsing function for `ecb-non-semantic-parsing-function' tests."
  '(("alpha_x" . 1) ("zz" . 5)))

(ert-deftest ecb-non-semantic-custom-parsing-function ()
  "The configured parser must be a symbol (speedbar checks `fboundp')."
  (ecb-test-with-ecb proj
    (let ((ecb-non-semantic-parsing-function
           (list (cons 'ruby-mode 'ecb-mb-test-custom-parser))))
      (ecb-mb-test-visit proj "src/sub/epsilon.rb")
      (should (equal (ecb-mb-test-lines) '("alpha_x" "zz"))))))

;;; ----------------------------------------------------------------------
;;; Maximize / goto
;;; ----------------------------------------------------------------------

(ert-deftest ecb-goto-and-maximize-window-methods ()
  (ecb-test-with-ecb proj
    (ecb-mb-test-visit proj "src/alpha.el")
    (ecb-goto-window-methods)
    (should (equal (buffer-name) ecb-methods-buffer-name))
    (ecb-maximize-window-methods)
    (should (equal (ecb-test-visible-ecb-buffers) (list ecb-methods-buffer-name)))
    (ecb-undo-maximize-ecb-buffer)
    (should (> (length (ecb-test-visible-ecb-buffers)) 1))))

(provide 'ecb-method-browser-test)
;;; ecb-method-browser-test.el ends here
