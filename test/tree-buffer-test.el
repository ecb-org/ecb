;;; tree-buffer-test.el --- tests for tree-buffer.el -*- lexical-binding: t -*-

;; Author: Ryan Ware <ware@opensourceware.me>
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Covers the pure tree-node data layer and the rendering layer of
;; tree-buffer.el using a real buffer in batch mode.

;;; Code:

(require 'ecb-test-helpers)
(require 'tree-buffer)

;;; ----------------------------------------------------------------------
;;; Sequence helpers
;;; ----------------------------------------------------------------------

(ert-deftest tree-buffer-copy-list-copies-structure ()
  (let* ((orig (list 1 2 3))
         (copy (tree-buffer-copy-list orig)))
    (should (equal orig copy))
    (should-not (eq orig copy))))

(ert-deftest tree-buffer-member-default-and-custom-test ()
  (should (equal (tree-buffer-member 2 '(1 2 3)) '(2 3)))
  (should-not (tree-buffer-member 9 '(1 2 3)))
  (should (equal (tree-buffer-member "B" '("a" "b" "c")
                                     (lambda (a b) (string= (downcase a) b)))
                 '("b" "c")))
  (let ((sym (list 'x)))
    (should (eq (car (tree-buffer-member sym (list (list 'x) sym) 'eq)) sym))))

(ert-deftest tree-buffer-position-returns-index ()
  (should (= (tree-buffer-position '(a b c) 'b) 1))
  (should-not (tree-buffer-position '(a b c) 'z))
  (should (= (tree-buffer-position [a b c] 'c) 2)))

(ert-deftest tree-buffer-first-and-last ()
  (should (eq (tree-buffer-first '(a b c)) 'a))
  (should (eq (tree-buffer-last '(a b c)) 'c))
  (should (eq (tree-buffer-first [x y]) 'x))
  (should (eq (tree-buffer-last [x y]) 'y))
  (should-not (tree-buffer-first nil))
  (should-not (tree-buffer-last [])))

(ert-deftest tree-buffer-set-and-remove-elt ()
  (should (equal (tree-buffer-set-elt (list 1 2 3) 1 'x) '(1 x 3)))
  (should (equal (tree-buffer-remove-elt (list 1 2 3) 1) '(1 3)))
  (should (equal (tree-buffer-remove-elt (list 1 2 3) 0) '(2 3)))
  (should (equal (tree-buffer-remove-elt (list 1 2 3) 2) '(1 2))))

;;; ----------------------------------------------------------------------
;;; tree-node data layer
;;; ----------------------------------------------------------------------

(defun tree-buffer-test--make-tree ()
  "Return a root node with a small, deterministic tree below it.
root
  A (type 0, data \"a\")
    A1 (type 1, data \"a1\")
    A2 (type 1, data \"a2\")
      A2x (type 2, data \"a2x\")
  B (type 0, data \"b\", leaf)
  C (type 0, data \"c\")"
  (let* ((root (tree-node-new-root))
         (a (tree-node-new "A" 0 "a" nil root))
         (a1 (tree-node-new "A1" 1 "a1" t a))
         (a2 (tree-node-new "A2" 1 "a2" nil a))
         (a2x (tree-node-new "A2x" 2 "a2x" t a2))
         (b (tree-node-new "B" 0 "b" t root))
         (c (tree-node-new "C" 0 "c" nil root)))
    (ignore a1 a2x b c)
    root))

(ert-deftest tree-node-new-sets-slots-and-parent-link ()
  (let* ((root (tree-node-new-root))
         (n (tree-node-new "name" 7 'data nil root 'beginning '(1 ">>" 3))))
    (should (tree-node-p n))
    (should (string= (tree-node->name n) "name"))
    (should (= (tree-node->type n) 7))
    (should (eq (tree-node->data n) 'data))
    (should (tree-node->expandable n))
    (should-not (tree-node->expanded n))
    (should (eq (tree-node->parent n) root))
    (should (eq (tree-node->shrink-name n) 'beginning))
    (should (equal (tree-node->shrink-name-spec n) '(1 ">>" 3)))
    (should (memq n (tree-node->children root)))))

(ert-deftest tree-node-new-not-expandable-flag ()
  (let ((n (tree-node-new "leaf" 0 nil t)))
    (should-not (tree-node->expandable n))
    (should-not (tree-node->parent n))
    (should (equal (tree-node->shrink-name-spec n) '(0 "..." 5)))))

(ert-deftest tree-node-ids-are-unique-and-increasing ()
  (let ((a (tree-node-new "a" 0 nil))
        (b (tree-node-new "b" 0 nil))
        (c (tree-node-new "c" 0 nil)))
    (should (< (tree-node->id a) (tree-node->id b) (tree-node->id c)))
    (should (= (length (cl-remove-duplicates (list (tree-node->id a)
                                                   (tree-node->id b)
                                                   (tree-node->id c))))
               3))))

(ert-deftest tree-node-new-root-has-type-minus-one ()
  (let ((root (tree-node-new-root)))
    (should (= (tree-node->type root) -1))
    (should (string= (tree-node->name root) "root"))
    (should (tree-node->expandable root))
    (should-not (tree-node->parent root))))

(ert-deftest tree-buffer-set-root-marks-root-expanded ()
  (tree-buffer-test-with-buffer buf
    (let ((root (tree-node-new-root)))
      (should-not (tree-node->expanded root))
      (tree-buffer-set-root root)
      (should (tree-node->expanded root)))))

(ert-deftest tree-node-add-children-single-and-list-and-at-beginning ()
  (let ((root (tree-node-new-root))
        (x (tree-node-new "x" 0 nil))
        (y (tree-node-new "y" 0 nil))
        (z (tree-node-new "z" 0 nil)))
    (tree-node-add-children root x)
    (tree-node-add-children root (list y))
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("x" "y")))
    (tree-node-add-children root z t)
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("z" "x" "y")))
    (dolist (n (list x y z)) (should (eq (tree-node->parent n) root)))
    (should-error (tree-node-add-children root 42))))

(ert-deftest tree-node-remove-child-clears-parent ()
  (let* ((root (tree-buffer-test--make-tree))
         (b (tree-node-find-child-by-name root "B")))
    (tree-node-remove-child root b)
    (should-not (tree-node->parent b))
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("A" "C")))))

(ert-deftest tree-node-sort-children-sorts-in-place ()
  (let ((root (tree-buffer-test--make-tree)))
    (tree-node-sort-children root (lambda (l r) (string> (tree-node->name l) (tree-node->name r))))
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("C" "B" "A")))))

(ert-deftest tree-node-find-child-by-name ()
  (let ((root (tree-buffer-test--make-tree)))
    (should (string= (tree-node->name (tree-node-find-child-by-name root "B")) "B"))
    (should-not (tree-node-find-child-by-name root "A1"))
    (should-not (tree-node-find-child-by-name root "nope"))))

(ert-deftest tree-node-find-child-by-data-and-name ()
  ;; data comparison goes through the data-equal-fn of the current
  ;; tree-buffer, so this needs a tree-buffer context.
  (tree-buffer-test-with-buffer buf
   (let* ((root (tree-buffer-test--make-tree))
         (a (tree-node-find-child-by-name root "A")))
    (should (eq (tree-node-find-child-by-data/name root "b")
                (tree-node-find-child-by-name root "B")))
    (should (eq (tree-node-find-child-by-data/name a "a2" "A2")
                (tree-node-find-child-by-name a "A2")))
    ;; data matches but name does not
    (should-not (tree-node-find-child-by-data/name a "a2" "wrong"))
    ;; only direct children are searched
    (should-not (tree-node-find-child-by-data/name root "a2x")))))

(ert-deftest tree-node-remove-child-by-data-name-returns-cell-and-unlinks ()
  (tree-buffer-test-with-buffer buf
   (let* ((root (tree-buffer-test--make-tree))
         (b (tree-node-find-child-by-name root "B"))
         (removed (tree-node-remove-child-by-data/name root "b")))
    (should (eq (car removed) b))
    (should-not (cdr removed))
    (should-not (tree-node->parent b))
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("A" "C")))
    ;; removing the first child updates the children slot of the parent
    (tree-node-remove-child-by-data/name root "a")
    (should (equal (mapcar #'tree-node->name (tree-node->children root)) '("C")))
    (should-not (tree-node-remove-child-by-data/name root "does-not-exist")))))

(ert-deftest tree-node-indent-level-counts-ancestors ()
  ;; level 0 is defined relative to the root of the current tree-buffer
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A"))
           (a2 (tree-node-find-child-by-name a "A2"))
           (a2x (tree-node-find-child-by-name a2 "A2x")))
      (should (= (tree-node-indent-level a) 0))
      (should (= (tree-node-indent-level a2) 1))
      (should (= (tree-node-indent-level a2x) 2)))))

(ert-deftest tree-node-update-replaces-slots ()
  (let ((n (tree-node-new "old" 0 'olddata)))
    (tree-node-update n "new" 5 'newdata nil 'end)
    (should (string= (tree-node->name n) "new"))
    (should (= (tree-node->type n) 5))
    (should (eq (tree-node->data n) 'newdata))
    (should-not (tree-node->expandable n))
    (should (eq (tree-node->shrink-name n) 'end))))

(ert-deftest tree-node-toggle-expanded ()
  (let ((n (tree-node-new "n" 0 nil)))
    (should-not (tree-node->expanded n))
    (tree-node-toggle-expanded n)
    (should (tree-node->expanded n))
    (tree-node-toggle-expanded n)
    (should-not (tree-node->expanded n))))

(ert-deftest tree-node-map-subtree-is-depth-first-and-skips-root ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A")))
      ;; the root of the current tree-buffer is skipped ...
      (should (equal (tree-node-map-subtree root #'tree-node->name)
                     '("A" "A1" "A2" "A2x" "B" "C")))
      ;; ... any other start node is included
      (should (equal (tree-node-map-subtree a #'tree-node->name)
                     '("A" "A1" "A2" "A2x")))))
  ;; outside a tree-buffer there is no root, so the start node is included
  (with-temp-buffer
    (let ((root (tree-buffer-test--make-tree)))
      (should (equal (tree-node-map-subtree root #'tree-node->name)
                     '("root" "A" "A1" "A2" "A2x" "B" "C"))))))

(ert-deftest tree-node-search-subtree-by-data-and-id ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A"))
           (a2 (tree-node-find-child-by-name a "A2"))
           (a2x (tree-node-find-child-by-name a2 "A2x")))
      (should (eq (tree-node-search-subtree-by-data root "a2x") a2x))
      (should (eq (tree-node-search-subtree-by-data a "a2") a2))
      (should-not (tree-node-search-subtree-by-data a "c"))
      (should (eq (tree-node-search-subtree-by-id root (tree-node->id a2x)) a2x))
      (should-not (tree-node-search-subtree-by-id root -999)))))

;;; ----------------------------------------------------------------------
;;; Rendering layer
;;; ----------------------------------------------------------------------

(defvar tree-buffer-test--expanded-calls nil)

(defun tree-buffer-test--populate (buffer)
  "Fill tree-buffer BUFFER with the standard test tree and redisplay it."
  (with-current-buffer buffer
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-new "A" 0 "a" nil root)))
      (tree-node-new "A1" 1 "a1" t a)
      (let ((a2 (tree-node-new "A2" 1 "a2" nil a)))
        (tree-node-new "A2x" 2 "a2x" t a2))
      (tree-node-new "B" 0 "b" t root)
      (tree-node-new "C" 0 "c" nil root)
      (tree-buffer-update))))

(defmacro tree-buffer-test-with-buffer (var &rest body)
  "Create an ASCII-guide tree-buffer bound to VAR, populate it, run BODY."
  (declare (indent 1))
  `(let ((,var nil)
         (tree-buffer-test--expanded-calls nil))
     (unwind-protect
         (progn
           (setq ,var (tree-buffer-create
                       "*tree-buffer-test*"
                       :frame (selected-frame)
                       :mouse-action-trigger 'button-release
                       :is-click-valid-fn (lambda (&rest _) t)
                       :node-selected-fn (lambda (&rest _) nil)
                       :node-expanded-fn (lambda (node &rest _)
                                           (push (tree-node->name node)
                                                 tree-buffer-test--expanded-calls))
                       :node-collapsed-fn (lambda (&rest _) nil)
                       :node-mouse-over-fn (lambda (&rest _) "")
                       :mouse-highlight-fn nil
                       :node-data-equal-fn #'equal
                       :maybe-empty-node-types '(0)
                       :leaf-node-types '(1 2)
                       :menu-creator nil
                       :menu-titles nil
                       :modeline-menu-creator nil
                       :sticky-parent-p nil
                       :trunc-lines t
                       :read-only t
                       :tree-indent 2
                       :incr-search-p t
                       :incr-search-additional-pattern nil
                       :arrow-navigation 'expand-collapse
                       :hor-scroll-step nil
                       :default-images-dir nil
                       :additional-images-dir nil
                       :image-file-prefix nil
                       :tree-style 'ascii-guides
                       :ascii-guide-face nil
                       :type-facer nil
                       :expand-symbol-before-p t
                       :highlight-node-face 'highlight
                       :general-face nil
                       :after-create-hook nil
                       :after-update-hook nil))
           (tree-buffer-test--populate ,var)
           (with-current-buffer ,var
             ,@body))
       (when ,var (tree-buffer-destroy (buffer-name ,var))))))

(ert-deftest tree-buffer-create-registers-buffer-and-root ()
  (tree-buffer-test-with-buffer buf
    (should (memq buf tree-buffers))
    (should (tree-node-p (tree-buffer-get-root)))
    (should (= (tree-node->type (tree-buffer-get-root)) -1))
    (should (tree-buffer-spec-p tree-buffer-spec))
    (should buffer-read-only)))

(ert-deftest tree-buffer-destroy-removes-buffer ()
  (let ((name "*tree-buffer-destroy-test*"))
    (tree-buffer-create name :frame (selected-frame) :node-data-equal-fn #'equal
                        :tree-style 'ascii-guides :expand-symbol-before-p t
                        :tree-indent 2)
    (should (get-buffer name))
    (tree-buffer-destroy name)
    (should-not (get-buffer name))
    (should-not (cl-some (lambda (b) (and (buffer-live-p b) (string= (buffer-name b) name)))
                         tree-buffers))
    ;; destroying a non tree-buffer is a no-op
    (with-temp-buffer
      (tree-buffer-destroy (buffer-name))
      (should (buffer-live-p (current-buffer))))))

(ert-deftest tree-buffer-update-renders-only-top-level-when-collapsed ()
  (tree-buffer-test-with-buffer buf
    (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf)) '("A" "B" "C")))
    (should (= (tree-buffer-number-of-displayed-nodes) 3))
    (let ((lines (ecb-test-tree-buffer-lines (buffer-name buf))))
      ;; A and C are expandable (closed), B is a not-expandable type 0 node
      ;; and so is shown with the empty symbol.
      (should (string-match-p "^\\[\\+\\] A$" (nth 0 lines)))
      (should (string-match-p "^\\[x\\] B$" (nth 1 lines)))
      (should (string-match-p "^\\[\\+\\] C$" (nth 2 lines))))))

(ert-deftest tree-buffer-expand-node-levels ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A")))
      ;; level 0 on A: expand A itself only
      (tree-buffer-expand-node a 0)
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "A1" "A2" "B" "C")))
      ;; the node-expanded-fn is consulted for every not-yet-expanded
      ;; expandable node that is visited, A itself included.
      (should (member "A" tree-buffer-test--expanded-calls))
      ;; level 1 on A: A and its direct children
      (tree-buffer-expand-node a 1)
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "A1" "A2" "A2x" "B" "C")))
      ;; negative level collapses
      (tree-buffer-expand-node a -1)
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "B" "C")))
      ;; expanding the root itself is a no-op by contract
      (tree-buffer-expand-node root 5)
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "B" "C"))))))

(ert-deftest tree-buffer-expand-node-honours-predicates ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A")))
      ;; expand-pred refuses to expand A2
      (tree-buffer-expand-node a 3 (lambda (node _lvl) (not (string= (tree-node->name node) "A2"))))
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "A1" "A2" "B" "C")))
      ;; collapse-pred refuses to collapse A when asked to collapse everything
      (tree-buffer-expand-node a -1 nil (lambda (node _lvl) (not (string= (tree-node->name node) "A"))))
      (tree-buffer-update)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf))
                     '("A" "A1" "A2" "B" "C"))))))

(ert-deftest tree-buffer-indentation-and-guides ()
  (tree-buffer-test-with-buffer buf
    (let ((a (tree-node-find-child-by-name (tree-buffer-get-root) "A")))
      (tree-buffer-expand-node a 1)
      (tree-buffer-update)
      (let ((lines (ecb-test-tree-buffer-lines (buffer-name buf))))
        (should (= (length (cl-remove-if #'string-empty-p lines)) 6))
        ;; Children are indented relative to their parent.
        (should (< (string-match-p "A1" (nth 1 lines))
                   (string-match-p "A2x" (nth 3 lines))))
        (should (string-match-p "A1" (nth 1 lines)))
        (should (string-match-p "A2" (nth 2 lines)))
        (should (string-match-p "A2x" (nth 3 lines)))))))

(ert-deftest tree-buffer-get-node-at-point-per-line ()
  (tree-buffer-test-with-buffer buf
    (goto-char (point-min))
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "A"))
    (forward-line 1)
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "B"))
    (forward-line 1)
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "C"))
    (should (string= (tree-node->name (tree-buffer-get-node-at-point (point-min))) "A"))))

(ert-deftest tree-buffer-displayed-node-nr-and-linenr ()
  (tree-buffer-test-with-buffer buf
    (let ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B")))
      (should (= (tree-buffer-displayed-node-nr b) 1))
      (should (= (tree-buffer-displayed-node-linenr b) 2))
      (should (eq (tree-buffer-nth-displayed-node 1) b)))))

(ert-deftest tree-buffer-find-displayed-node-by-data-name ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A"))
           (b (tree-node-find-child-by-name root "B")))
      (should (eq (tree-buffer-find-displayed-node-by-data/name "b") b))
      (should (eq (tree-buffer-find-displayed-node-by-data/name "b" "B") b))
      (should-not (tree-buffer-find-displayed-node-by-data/name "b" "X"))
      ;; hidden (collapsed) nodes are not displayed
      (should-not (tree-buffer-find-displayed-node-by-data/name "a1"))
      (tree-buffer-expand-node a 0)
      (tree-buffer-update)
      (should (tree-buffer-find-displayed-node-by-data/name "a1"))
      ;; start-node restricts the search to nodes from that point on
      (should-not (tree-buffer-find-displayed-node-by-data/name "a" nil b))
      (should (tree-buffer-find-displayed-node-by-data/name "c" nil b)))))

(ert-deftest tree-buffer-search-displayed-node-list ()
  (tree-buffer-test-with-buffer buf
    (should (string= (tree-node->name
                      (tree-buffer-search-displayed-node-list
                       (lambda (n) (= (tree-node->type n) 0))))
                     "A"))
    (should-not (tree-buffer-search-displayed-node-list (lambda (_n) nil)))))

(ert-deftest tree-buffer-node-name-start-and-end-points ()
  (tree-buffer-test-with-buffer buf
    (let* ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B"))
           (start (tree-buffer-get-node-name-start-point b))
           (end (tree-buffer-get-node-name-end-point b)))
      (should (string= (buffer-substring-no-properties start end) "B"))
      (should (= (tree-buffer-get-node-name-start-column b)
                 (save-excursion (goto-char start) (current-column))))
      (should (tree-buffer-point-at-node-name-p b start))
      (should-not (tree-buffer-point-at-node-name-p b (1- start))))))

(ert-deftest tree-buffer-highlight-node-moves-overlay ()
  (tree-buffer-test-with-buffer buf
    (let ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B")))
      (should (tree-buffer-highlight-node-by-data/name "b" nil nil t))
      (should (eq (nth 2 tree-buffer-highlighted-node) b))
      (should (= (overlay-start tree-buffer-highlight-overlay)
                 (tree-buffer-get-node-name-start-point b)))
      (should (= (overlay-end tree-buffer-highlight-overlay)
                 (tree-buffer-get-node-name-end-point b)))
      ;; highlighting survives a redisplay
      (tree-buffer-update)
      (should (= (overlay-start tree-buffer-highlight-overlay)
                 (tree-buffer-get-node-name-start-point
                  (tree-buffer-find-displayed-node-by-data/name "b"))))
      ;; unknown data removes the highlight and returns nil
      (should-not (tree-buffer-highlight-node-by-data/name "zzz" nil nil t))
      (should-not (overlay-buffer tree-buffer-highlight-overlay))
      ;; nil data removes it too
      (tree-buffer-highlight-node-by-data/name "b" nil nil t)
      (should-not (tree-buffer-highlight-node-by-data/name nil))
      (should-not (overlay-buffer tree-buffer-highlight-overlay)))))

(ert-deftest tree-buffer-highlighted-node-matches-data-name-p ()
  (tree-buffer-test-with-buffer buf
    (tree-buffer-highlight-node-by-data/name "b" "B" nil t)
    (should (tree-buffer-highlighted-node-matches-data/name-p "b" "B"))
    (should-not (tree-buffer-highlighted-node-matches-data/name-p "c" "C"))))

(ert-deftest tree-buffer-remove-node-with-redisplay ()
  (tree-buffer-test-with-buffer buf
    (let ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B")))
      (tree-buffer-remove-node b t)
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf)) '("A" "C")))
      (should-not (tree-node-find-child-by-name (tree-buffer-get-root) "B"))
      (should (= (length (cl-remove-if #'string-empty-p
                                       (ecb-test-tree-buffer-lines (buffer-name buf))))
                 2)))))

(ert-deftest tree-buffer-remove-node-empty-parent-types ()
  (tree-buffer-test-with-buffer buf
    (let* ((root (tree-buffer-get-root))
           (a (tree-node-find-child-by-name root "A"))
           (a2 (tree-node-find-child-by-name a "A2"))
           (a2x (tree-node-find-child-by-name a2 "A2x")))
      ;; A2x is the only child of A2 (type 1); removing it with type 1 in
      ;; EMPTY-PARENT-TYPES removes A2 too.  A (type 0) still has A1 so it
      ;; stays.
      (tree-buffer-remove-node a2x nil '(1))
      (should-not (tree-node-find-child-by-name a "A2"))
      (should (tree-node-find-child-by-name a "A1"))
      (should (tree-node-find-child-by-name root "A"))
      ;; root is never removed
      (tree-buffer-remove-node root nil)
      (should (eq (tree-buffer-get-root) root)))))

(ert-deftest tree-buffer-clear-tree-empties-root ()
  (tree-buffer-test-with-buffer buf
    (tree-buffer-clear-tree)
    (should-not (tree-node->children (tree-buffer-get-root)))
    (tree-buffer-update)
    (should (tree-buffer-empty-p))
    (should (= (tree-buffer-number-of-displayed-nodes) 0))))

(ert-deftest tree-buffer-update-node-changes-display ()
  (tree-buffer-test-with-buffer buf
    (let ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B")))
      (tree-buffer-update-node b "Bee" nil 0 "bee" nil t)
      (should (string= (tree-node->name b) "Bee"))
      (should (equal (tree-node->data b) "bee"))
      (should (cl-some (lambda (l) (string-match-p "Bee" l))
                       (ecb-test-tree-buffer-lines (buffer-name buf)))))))

(ert-deftest tree-buffer-set-root-replaces-tree ()
  (tree-buffer-test-with-buffer buf
    (let ((new-root (tree-node-new-root)))
      (tree-node-new "Only" 0 "only" t new-root)
      (tree-buffer-set-root new-root)
      (tree-buffer-update)
      (should (eq (tree-buffer-get-root) new-root))
      (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf)) '("Only"))))))

(ert-deftest tree-buffer-data-store-round-trip ()
  (tree-buffer-test-with-buffer buf
    (tree-buffer-set-data-store '(1 2 3))
    (should (equal (tree-buffer-get-data-store) '(1 2 3)))))

(ert-deftest tree-buffer-real-style-falls-back-to-ascii-in-batch ()
  (tree-buffer-test-with-buffer buf
    ;; In batch there is no image support so 'image must degrade.
    (should (memq (tree-buffer-real-style 'image) '(ascii-guides ascii-no-guides)))
    (should (eq (tree-buffer-real-style 'ascii-no-guides) 'ascii-no-guides))))

(ert-deftest tree-buffer-add-image-icon-maybe-keeps-string-in-batch ()
  (tree-buffer-test-with-buffer buf
    (let ((s (tree-buffer-add-image-icon-maybe 0 1 "x name" "open")))
      (should (string= (substring-no-properties s) "x name")))))

(ert-deftest tree-buffer-node-display-name-no-shrink-when-fits ()
  (tree-buffer-test-with-buffer buf
    (let ((b (tree-node-find-child-by-name (tree-buffer-get-root) "B")))
      (should (string= (tree-buffer-node-display-name b) "B"))
      (should (string= (tree-node->displayed-name b) "B")))))

(ert-deftest tree-buffer-node-display-name-shrinks-from-beginning ()
  (tree-buffer-test-with-buffer buf
    (let* ((long (make-string 300 ?n))
           (n (tree-node-new long 1 "long" t (tree-buffer-get-root) 'beginning)))
      (tree-buffer-update)
      (let ((shown (tree-buffer-node-display-name n)))
        (should (< (length shown) (length long)))
        (should (string-prefix-p "..." shown))))))

(ert-deftest tree-buffer-node-display-name-shrinks-at-end-for-expandable ()
  ;; shrink 'end only applies when the expand symbol comes after the name,
  ;; so this needs its own buffer with :expand-symbol-before-p nil.
  (let ((name "*tree-buffer-shrink-end-test*")
        (long (make-string 300 ?e)))
    (unwind-protect
        (let ((buf (tree-buffer-create name :frame (selected-frame)
                                       :node-data-equal-fn #'equal
                                       :tree-style 'ascii-guides
                                       :expand-symbol-before-p nil
                                       :tree-indent 2)))
          (with-current-buffer buf
            (let ((n (tree-node-new long 0 "long" nil (tree-buffer-get-root) 'end)))
              (tree-buffer-update)
              (let ((shown (tree-buffer-node-display-name n)))
                (should (< (length shown) (length long)))
                (should (string-suffix-p "..." shown))))))
      (tree-buffer-destroy name))))

(ert-deftest tree-buffer-find-common-substring ()
  (tree-buffer-test-with-buffer buf
    (should (string= (tree-buffer-find-common-substring '("foobar" "foobaz" "fooqux") "foo" t)
                     "foo"))
    (should (string= (tree-buffer-find-common-substring '("foobar" "foobaz") "foo" t)
                     "fooba"))
    (should (eq (tree-buffer-find-common-substring '("foo") "foo" t) t))))

(ert-deftest tree-buffer-gen-searchpattern-indent-prefix ()
  (tree-buffer-test-with-buffer buf
    (let ((pat (tree-buffer-gen-searchpattern-indent-prefix)))
      (should (stringp pat))
      (should (string-prefix-p "^" pat))
      (should (string-suffix-p "*" pat)))
    (should (string-suffix-p "\\{2\\}" (tree-buffer-gen-searchpattern-indent-prefix 2)))))

(ert-deftest tree-buffer-incremental-search-narrows-to-matching-node ()
  (tree-buffer-test-with-buffer buf
    (goto-char (point-min))
    ;; Simulate typing "C" by binding last-command-event.
    (let ((last-command-event ?C))
      (tree-buffer-incremental-node-search))
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "C"))
    (should (string= (downcase tree-buffer-incr-searchpattern) "c"))
    ;; a non-matching key is appended to the pattern but point stays put
    (let ((last-command-event ?Z))
      (tree-buffer-incremental-node-search))
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "C"))
    (should (string= (downcase tree-buffer-incr-searchpattern) "cz"))
    ;; backspace removes the last char again
    (let ((last-command-event 'backspace))
      (tree-buffer-incremental-node-search))
    (should (string= (downcase tree-buffer-incr-searchpattern) "c"))
    ;; home clears the pattern
    (let ((last-command-event 'home))
      (tree-buffer-incremental-node-search))
    (should (string= tree-buffer-incr-searchpattern ""))))

(ert-deftest tree-buffer-tab-pressed-toggles-expansion ()
  (tree-buffer-test-with-buffer buf
    (goto-char (point-min))
    (tree-buffer-tab-pressed)
    (should (tree-node->expanded (tree-node-find-child-by-name (tree-buffer-get-root) "A")))
    (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf)) '("A" "A1" "A2" "B" "C")))
    (goto-char (point-min))
    (tree-buffer-tab-pressed)
    (should (equal (ecb-test-tree-buffer-node-names (buffer-name buf)) '("A" "B" "C")))))

(ert-deftest tree-buffer-count-subnodes-to-display ()
  (tree-buffer-test-with-buffer buf
    (let ((a (tree-node-find-child-by-name (tree-buffer-get-root) "A")))
      (should (= (tree-buffer-count-subnodes-to-display a) 0))
      (tree-buffer-expand-node a 0)
      (should (= (tree-buffer-count-subnodes-to-display a) 2))
      (tree-buffer-expand-node a 1)
      (should (= (tree-buffer-count-subnodes-to-display a) 3)))))

(ert-deftest tree-buffer-node-data-equal-p-uses-spec-function ()
  (tree-buffer-test-with-buffer buf
    (should (tree-buffer-node-data-equal-p "x" "x"))
    (should-not (tree-buffer-node-data-equal-p "x" "y"))))

(ert-deftest tree-buffer-create-menu-builds-keymap ()
  (tree-buffer-test-with-buffer buf
    (let ((menu (tree-buffer-create-menu '((ignore "Item one")
                                           ("---")
                                           ("Sub" (ignore "Sub item"))))))
      (should (keymapp menu)))))

(ert-deftest tree-buffer-goto-line-and-current-line ()
  (tree-buffer-test-with-buffer buf
    (tree-buffer-goto-line 2)
    (should (= (tree-buffer-current-line) 2))
    (should (string= (tree-node->name (tree-buffer-get-node-at-point)) "B"))))

(provide 'tree-buffer-test)
;;; tree-buffer-test.el ends here
