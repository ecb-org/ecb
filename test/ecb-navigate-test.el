;;; ecb-navigate-test.el --- tests for ecb-navigate.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The doubly linked list, the history item classes and the navigation
;; history (back/forward) used after tag/file jumps.

;;; Code:

(require 'ecb-test-helpers)

(defmacro ecb-nav-test-with-fresh-history (&rest body)
  "Run BODY with a fresh navigation history, restoring the old one afterwards."
  (declare (indent 0) (debug t))
  `(let ((ecb-nav-first-node ecb-nav-first-node)
         (ecb-nav-current-node ecb-nav-current-node))
     (ecb-nav-initialize)
     ,@body))

(defun ecb-nav-test-history-data ()
  "Return the `ecb-nav-history-item' objects from first to last."
  (let ((node ecb-nav-first-node) (result nil))
    (while node
      (push (ecb-get-data node) result)
      (setq node (ecb-get-next node)))
    (nreverse result)))

;;; ----------------------------------------------------------------------
;;; Doubly linked list
;;; ----------------------------------------------------------------------

(ert-deftest ecb-dlist-node-new-and-accessors ()
  (let ((n (ecb-dlist-node-new 'payload)))
    (should (eq (ecb-get-data n) 'payload))
    (should (null (ecb-get-next n)))
    (should (null (ecb-get-previous n)))
    (ecb-set-data n 'other)
    (should (eq (ecb-get-data n) 'other))))

(ert-deftest ecb-dlist-set-next-links-both-directions ()
  (let ((a (ecb-dlist-node-new 'a))
        (b (ecb-dlist-node-new 'b))
        (c (ecb-dlist-node-new 'c)))
    (ecb-set-next a b)
    (should (eq (ecb-get-next a) b))
    (should (eq (ecb-get-previous b) a))
    ;; replacing the successor unlinks the old one
    (ecb-set-next a c)
    (should (eq (ecb-get-next a) c))
    (should (eq (ecb-get-previous c) a))
    (should (null (ecb-get-previous b)))))

(ert-deftest ecb-dlist-set-previous-links-both-directions ()
  (let ((a (ecb-dlist-node-new 'a))
        (b (ecb-dlist-node-new 'b)))
    (ecb-set-previous b a)
    (should (eq (ecb-get-previous b) a))
    (should (eq (ecb-get-next a) b))
    (ecb-set-previous b nil)
    (should (null (ecb-get-previous b)))))

;;; ----------------------------------------------------------------------
;;; History items
;;; ----------------------------------------------------------------------

(ert-deftest ecb-nav-history-item-basics ()
  (let ((item (ecb-nav-history-item :pos 5 :window-start 2)))
    (should (= (ecb-nav-get-pos item) 5))
    (should (= (ecb-nav-get-window-start item) 2))
    (ecb-nav-set-pos item 7)
    (ecb-nav-set-window-start item 3)
    (should (equal (ecb-nav-to-string item) "7:3"))
    (should (ecb-nav-save item))
    (should (ecb-nav-is-valid item))))

(ert-deftest ecb-nav-file-history-item-new-captures-position ()
  (ecb-test-with-temp-project proj
    (with-current-buffer (find-file-noselect (expand-file-name "src/alpha.el" proj))
      (unwind-protect
          (progn
            (goto-char 10)
            (let ((item (ecb-nav-file-history-item-new)))
              (should (equal (ecb-nav-get-file item) (expand-file-name "src/alpha.el" proj)))
              (should (null (ecb-nav-get-indirect-buffer-name item)))
              (should (= (ecb-nav-get-pos item) 10))
              (should (ecb-nav-is-valid item))
              (should (string-prefix-p (expand-file-name "src/alpha.el" proj)
                                       (ecb-nav-to-string item)))))
        (kill-buffer)))))

(ert-deftest ecb-nav-file-history-item-indirect-buffer ()
  (ecb-test-with-temp-project proj
    (let* ((base (find-file-noselect (expand-file-name "src/alpha.el" proj)))
           (ind (make-indirect-buffer base "alpha-indirect")))
      (unwind-protect
          (with-current-buffer ind
            (let ((item (ecb-nav-file-history-item-new)))
              (should (equal (ecb-nav-get-indirect-buffer-name item) "alpha-indirect"))
              (should (ecb-nav-is-valid item))
              (kill-buffer ind)
              (should-not (ecb-nav-is-valid item))))
        (when (buffer-live-p ind) (kill-buffer ind))
        (kill-buffer base)))))

(ert-deftest ecb-nav-tag-history-item-save-and-validity ()
  (ecb-test-with-temp-project proj
    (with-current-buffer (find-file-noselect (expand-file-name "src/alpha.el" proj))
      (unwind-protect
          (let* ((start (copy-marker 20))
                 (end (copy-marker 60))
                 (item (ecb-nav-tag-history-item-new "alpha-one" (current-buffer) start end)))
            (should (equal (ecb-nav-get-tag-name item) "alpha-one"))
            (should (eq (ecb-nav-get-tag-buffer item) (current-buffer)))
            (should (ecb-nav-is-valid item))
            (goto-char 25)
            (should (ecb-nav-save item))
            (should (= (ecb-nav-get-pos item) 5))
            ;; buffer is not shown in a window -> no window-start saved
            (should (null (ecb-nav-get-window-start item)))
            ;; after the buffer dies the item is neither savable nor valid
            (kill-buffer)
            (should-not (ecb-nav-save item))
            (should-not (ecb-nav-is-valid item)))
        (when (buffer-live-p (current-buffer)) (kill-buffer))))))

;;; ----------------------------------------------------------------------
;;; History list management
;;; ----------------------------------------------------------------------

(ert-deftest ecb-nav-initialize-creates-sentinel ()
  (ecb-nav-test-with-fresh-history
    (should (eq ecb-nav-first-node ecb-nav-current-node))
    (should (equal (ecb-nav-get-pos (ecb-get-data ecb-nav-first-node)) "First item"))
    (should (null (ecb-get-next ecb-nav-first-node)))))

(ert-deftest ecb-nav-add-item-inserts-after-current ()
  (ecb-nav-test-with-fresh-history
    (let ((a (ecb-nav-history-item :pos 1))
          (b (ecb-nav-history-item :pos 2))
          (c (ecb-nav-history-item :pos 3)))
      (ecb-nav-add-item a)
      (ecb-nav-add-item b)
      (should (equal (mapcar #'ecb-nav-get-pos (ecb-nav-test-history-data))
                     '("First item" 1 2)))
      (should (eq (ecb-get-data ecb-nav-current-node) b))
      ;; go back one, add c -> c is spliced in after a, b is kept after c
      (setq ecb-nav-current-node (ecb-get-previous ecb-nav-current-node))
      (ecb-nav-add-item c)
      (should (equal (mapcar #'ecb-nav-get-pos (ecb-nav-test-history-data))
                     '("First item" 1 3 2)))
      (should (eq (ecb-get-data ecb-nav-current-node) c)))))

(ert-deftest ecb-nav-remove-last-node-and-sentinel ()
  (ecb-nav-test-with-fresh-history
    (let ((a (ecb-nav-history-item :pos 1))
          (b (ecb-nav-history-item :pos 2)))
      (ecb-nav-add-item a) (ecb-nav-add-item b)
      ;; removing the current (last) node moves current to the previous node
      (ecb-nav-remove-current-node)
      (should (eq (ecb-get-data ecb-nav-current-node) a))
      (should (equal (mapcar #'ecb-nav-get-pos (ecb-nav-test-history-data))
                     '("First item" 1)))
      ;; removing the first (sentinel) node promotes its successor
      (ecb-nav-remove-node ecb-nav-first-node)
      (should (eq (ecb-get-data ecb-nav-first-node) a))
      (should (null (ecb-get-previous ecb-nav-first-node)))
      ;; removing the last remaining node re-initialises
      (ecb-nav-remove-node ecb-nav-first-node)
      (should (equal (ecb-nav-get-pos (ecb-get-data ecb-nav-first-node)) "First item"))
      (should (eq ecb-nav-first-node ecb-nav-current-node)))))

(ert-deftest ecb-nav-remove-middle-node-keeps-tail ()
  "Known bug: `ecb-nav-remove-node' first calls `ecb-set-next' on the
predecessor (which clears the removed node's `previous' slot) and then
`ecb-set-previous' on the successor with that now-nil value, which in turn
unlinks the successor from the predecessor.  The tail of the history is lost."
  :expected-result :failed
  (ecb-nav-test-with-fresh-history
    (let ((a (ecb-nav-history-item :pos 1))
          (b (ecb-nav-history-item :pos 2))
          (c (ecb-nav-history-item :pos 3)))
      (ecb-nav-add-item a) (ecb-nav-add-item b) (ecb-nav-add-item c)
      (ecb-nav-remove-node (ecb-get-previous ecb-nav-current-node))
      (should (equal (mapcar #'ecb-nav-get-pos (ecb-nav-test-history-data))
                     '("First item" 1 3))))))

(ert-deftest ecb-nav-remove-invalid-nodes-drops-dead-items ()
  (ecb-nav-test-with-fresh-history
    (let* ((buf (generate-new-buffer "ecb-nav-dead"))
           (dead (ecb-nav-tag-history-item-new "x" buf (with-current-buffer buf (point-marker))
                                               (with-current-buffer buf (point-marker))))
           (alive (ecb-nav-history-item :pos 1)))
      (ecb-nav-add-item alive)
      (ecb-nav-add-item dead)
      (kill-buffer buf)
      (ecb-nav-remove-invalid-nodes)
      (should (equal (mapcar #'ecb-nav-get-pos (ecb-nav-test-history-data))
                     '("First item" 1)))
      (should (eq (ecb-get-data ecb-nav-current-node) alive)))))

(ert-deftest ecb-nav-save-current-removes-unsavable-items ()
  (ecb-nav-test-with-fresh-history
    (let* ((buf (generate-new-buffer "ecb-nav-dead2"))
           (dead (ecb-nav-tag-history-item-new "x" buf (with-current-buffer buf (point-marker))
                                               (with-current-buffer buf (point-marker)))))
      (ecb-nav-add-item (ecb-nav-history-item :pos 1))
      (ecb-nav-add-item dead)
      (kill-buffer buf)
      (ecb-nav-save-current)
      (should (= (ecb-nav-get-pos (ecb-get-data ecb-nav-current-node)) 1)))))

(ert-deftest ecb-nav-dump-history-lists-items ()
  "Known bug: the sentinel item stores the string \"First item\" as pos, and
`ecb-nav-to-string' calls `int-to-string' on it, so dumping always signals."
  :expected-result :failed
  (ecb-nav-test-with-fresh-history
    (ecb-nav-add-item (ecb-nav-history-item :pos 1 :window-start 0))
    (with-temp-buffer
      (ecb-nav-dump-history)
      (should (string-match-p "1:0" (buffer-string))))))

;;; ----------------------------------------------------------------------
;;; Navigation inside ECB
;;; ----------------------------------------------------------------------

(ert-deftest ecb-nav-goto-previous-and-next-between-files ()
  (ecb-test-with-ecb proj
    (ecb-nav-test-with-fresh-history
      (find-file (expand-file-name "src/alpha.el" proj))
      (ecb-window-sync)
      (goto-char (point-min)) (forward-line 3)
      (ecb-nav-add-item (ecb-nav-file-history-item-new))
      (find-file (expand-file-name "src/beta.el" proj))
      (ecb-window-sync)
      (goto-char (point-max))
      (ecb-nav-add-item (ecb-nav-file-history-item-new))
      (ecb-nav-goto-previous)
      (should (equal (buffer-name) "alpha.el"))
      (should (= (line-number-at-pos) 4))
      ;; cannot go before the first real item
      (ecb-nav-goto-previous)
      (should (equal (buffer-name) "alpha.el"))
      (ecb-nav-goto-next)
      (should (equal (buffer-name) "beta.el"))
      (should (= (point) (point-max)))
      ;; nothing after the last item
      (ecb-nav-goto-next)
      (should (equal (buffer-name) "beta.el")))))

(ert-deftest ecb-nav-goto-tag-item-widens-and-positions ()
  (ecb-test-with-ecb proj
    (ecb-nav-test-with-fresh-history
      (find-file (expand-file-name "src/alpha.el" proj))
      (ecb-window-sync)
      (let* ((buf (current-buffer))
             (start (progn (goto-char (point-min)) (search-forward "(defun alpha-two")
                           (copy-marker (match-beginning 0))))
             (end (progn (forward-sexp) (point-marker)))
             (item (ecb-nav-tag-history-item-new "alpha-two" buf start end t)))
        (ecb-nav-add-item item)
        (goto-char (point-min))
        (ecb-nav-add-item (ecb-nav-file-history-item-new))
        (ecb-nav-goto-previous)
        (should (eq (current-buffer) buf))
        (should (= (point) (marker-position start)))
        (should (buffer-narrowed-p))
        (widen)))))

(ert-deftest ecb-method-click-records-navigation-history ()
  "Clicking a tag in the methods buffer pushes a tag item on the history."
  (ecb-test-with-ecb proj
    (ecb-nav-test-with-fresh-history
      (find-file (expand-file-name "src/alpha.el" proj))
      (ecb-window-sync)
      (let ((node (ecb-test-find-displayed-node
                   ecb-methods-buffer-name
                   (lambda (n) (let ((d (tree-node->data n)))
                                 (and (semantic-tag-p d)
                                      (equal (semantic-tag-name d) "alpha-with")))))))
        (with-current-buffer ecb-methods-buffer-name
          (ecb-method-clicked node 1 nil nil nil)))
      (let ((items (ecb-nav-test-history-data)))
        (should (= (length items) 2))
        (should (ecb-nav-tag-history-item-p (cadr items)))
        (should (equal (ecb-nav-get-tag-name (cadr items)) "alpha-with"))
        (should (eq (ecb-get-data ecb-nav-current-node) (cadr items))))
      (should (looking-at "(defmacro alpha-with"))
      ;; only the sentinel is before it -> stays put
      (ecb-nav-goto-previous)
      (should (looking-at "(defmacro alpha-with")))))

(provide 'ecb-navigate-test)
;;; ecb-navigate-test.el ends here
