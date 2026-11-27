;;; ecb-util-test.el --- tests for ecb-util.el -*- lexical-binding: t -*-

;; Author: Ryan Ware <ware@opensourceware.me>
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Pure helpers in ecb-util.el: list/sequence utilities, string helpers,
;; option access, multicaches, time helpers, buffer/window helpers.

;;; Code:

(require 'ecb-test-helpers)
(require 'ecb-util)

;;; ----------------------------------------------------------------------
;;; Option access
;;; ----------------------------------------------------------------------

(defcustom ecb-util-test-option 'standard
  "Throw-away option for tests."
  :type 'symbol
  :group 'ecb)

(ert-deftest ecb-option-get-value-precedence ()
  (should (eq (ecb-option-get-value 'ecb-util-test-option) 'standard))
  (should (eq (ecb-option-get-value 'ecb-util-test-option 'standard-value) 'standard))
  (ecb-test-with-saved-option ecb-util-test-option 'saved
    (should (eq (ecb-option-get-value 'ecb-util-test-option) 'saved))
    (should (eq (ecb-option-get-value 'ecb-util-test-option 'saved-value) 'saved))
    (should (eq (ecb-option-get-value 'ecb-util-test-option 'standard-value) 'standard))
    (unwind-protect
        (progn
          (put 'ecb-util-test-option 'customized-value '((quote customized)))
          (should (eq (ecb-option-get-value 'ecb-util-test-option) 'customized)))
      (put 'ecb-util-test-option 'customized-value nil))))

(ert-deftest ecb-option-get-value-evaluates-non-quoted-forms ()
  (ecb-test-with-saved-option ecb-util-test-option 'x
    (put 'ecb-util-test-option 'saved-value '((list 1 2)))
    (should (equal (ecb-option-get-value 'ecb-util-test-option) '(1 2)))))

(ert-deftest ecb-custom-file-does-not-signal ()
  ;; With -Q there is no custom-file; the helper must return nil, not error.
  (should (or (null (ecb-custom-file)) (stringp (ecb-custom-file)))))

;;; ----------------------------------------------------------------------
;;; Assoc helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-assoc-helpers ()
  (let ((al '((a . 1) (b . 2) (c . 3))))
    (should (equal (ecb-remove-assoc 'b al) '((a . 1) (c . 3))))
    (should (equal (ecb-add-assoc '(z . 26) al) '((z . 26) (a . 1) (b . 2) (c . 3))))
    (should (= (ecb-find-assoc-value 'c al) 3))
    (should-not (ecb-find-assoc-value 'q al))
    (should (equal (ecb-find-assoc 'a al) '(a . 1)))))

;;; ----------------------------------------------------------------------
;;; List / sequence helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-filter-keeps-matching-elements ()
  (should (equal (ecb-filter '(1 2 3 4) #'cl-evenp) '(2 4)))
  (should (equal (ecb-filter '(nil 1 nil 2) nil) '(1 2)))
  (should-not (ecb-filter nil #'identity)))

(ert-deftest ecb-some-returns-first-true-value ()
  (should (= (ecb-some (lambda (x) (and (> x 2) (* x 10))) '(1 2 3 4)) 30))
  (should-not (ecb-some #'cl-evenp '(1 3 5))))

(ert-deftest ecb-some-with-multiple-sequences ()
  "KNOWN BUG: the multi-sequence branch of `ecb-some' calls `map', which
does not exist in GNU Emacs (it is `cl-map').  No caller in ECB uses that
branch today, so this is latent.  Remove :expected-result once fixed."
  :expected-result :failed
  (should (ecb-some #'< '(1 2) '(2 3)))
  (should (ecb-some #'identity [nil t])))

(ert-deftest ecb-copy-list-is-shallow-copy ()
  (let* ((inner (list 'i))
         (orig (list inner 2))
         (copy (ecb-copy-list orig)))
    (should (equal orig copy))
    (should-not (eq orig copy))
    (should (eq (car orig) (car copy)))))

(ert-deftest ecb-set-difference ()
  (should (equal (sort (ecb-set-difference '(a b c d) '(b d)) #'string<) '(a c)))
  (should (equal (ecb-set-difference '(a b) nil) '(a b)))
  (should-not (ecb-set-difference nil '(a)))
  (should (equal (ecb-set-difference '("a" "b") '("a")
                                     (lambda (item lst) (member item lst)))
                 '("b"))))

(ert-deftest ecb-member-variants ()
  (should (equal (ecb-member "b" '("a" "b" "c")) '("b" "c")))
  (should-not (ecb-member "z" '("a")))
  (should (equal (ecb-member 2 '(1 2 3) (lambda (i e) (= i e))) '(2 3)))
  (let ((cell (list 'x)))
    (should (eq (car (ecb-member cell (list (list 'x) cell) 'eq)) cell))))

(ert-deftest ecb-delete-duplicates-default-keeps-rightmost ()
  (should (equal (ecb-delete-duplicates (list 1 2 1 3 2)) '(1 3 2)))
  (should (equal (ecb-delete-duplicates (list 1 2 1 3 2) nil nil nil t) '(1 2 3)))
  (should (equal (ecb-delete-duplicates (list "a" "A" "b")
                                        (lambda (l r) (string= (downcase l) (downcase r)))
                                        nil nil t)
                 '("a" "b"))))

(ert-deftest ecb-delete-duplicates-copy-leaves-original ()
  (let* ((orig (list 1 1 2))
         (res (ecb-delete-duplicates orig nil nil nil nil t)))
    (should (equal res '(1 2)))
    (should (equal orig '(1 1 2)))))

(ert-deftest ecb-position ()
  (should (= (ecb-position 'b '(a b c)) 1))
  (should-not (ecb-position 'z '(a b c)))
  (should (= (ecb-position 'a '(a b a) nil nil nil t) 2))
  (should (= (ecb-position "B" '("a" "b")
                           (lambda (i e) (string= (downcase i) e)))
             1))
  (should (= (ecb-position 'x [a x]) 1)))

(ert-deftest ecb-set-elt-and-remove-elt ()
  (should (equal (ecb-set-elt (list 1 2 3) 0 'x) '(x 2 3)))
  (should (equal (ecb-set-elt (vector 1 2) 1 'y) [1 y]))
  (should (equal (ecb-remove-elt (list 1 2 3) 1) '(1 3))))

(ert-deftest ecb-replace-occurrences ()
  (should (equal (ecb-replace-first-occurence (list 1 2 1) 1 'x) '(x 2 1)))
  (should (equal (ecb-replace-all-occurences (list 1 2 1) 1 'x) '(x 2 x)))
  (should (equal (ecb-replace-first-occurence (list 1 2) 9 'x) '(1 2))))

(ert-deftest ecb-delete-occurrences-from-list ()
  (should (equal (ecb-delete-first-occurence-from-list (list 1 2 1) 1) '(2 1)))
  (should (equal (ecb-delete-all-occurences-from-list (list 1 2 1) 1) '(2)))
  (should (equal (ecb-delete-all-occurences-from-list (list 1 2) 9) '(1 2))))

(ert-deftest ecb-subseq-lists-vectors-strings-negative-indices ()
  (should (equal (ecb-subseq '(a b c d) 1) '(b c d)))
  (should (equal (ecb-subseq '(a b c d) 1 3) '(b c)))
  (should (equal (ecb-subseq '(a b c d) -2) '(c d)))
  (should (equal (ecb-subseq '(a b c d) 0 -1) '(a b c)))
  (should (equal (ecb-subseq [1 2 3] 1) [2 3]))
  (should (equal (ecb-subseq "hello" 1 3) "el"))
  (should (equal (ecb-subseq '(a) 1) nil)))

(ert-deftest ecb-concatenate ()
  (should (equal (ecb-concatenate 'list '(1) '(2 3)) '(1 2 3)))
  (should (equal (ecb-concatenate 'vector [1] [2]) [1 2]))
  (should (equal (ecb-concatenate 'string "a" "b") "ab"))
  (should-error (ecb-concatenate 'hash "a")))

(ert-deftest ecb-rotate ()
  (should (equal (ecb-rotate '(a b c d e f) 'c) '(c d e f a b)))
  (should (equal (ecb-rotate '(a b c) 'a) '(a b c)))
  (should-not (ecb-rotate '(a b c) 'z))
  (should (equal (ecb-rotate [1 2 3] 3) [3 1 2]))
  (should (equal (ecb-rotate "abc" ?c) "cab")))

(ert-deftest ecb-first-and-last ()
  (should (eq (ecb-first '(a b)) 'a))
  (should (eq (ecb-last '(a b)) 'b))
  (should (eq (ecb-first [x y]) 'x))
  (should (eq (ecb-last [x y]) 'y))
  (should-not (ecb-first nil))
  (should-not (ecb-last [])))

(ert-deftest ecb-next-listelem-wraps-and-steps ()
  (let ((l '(a b c d)))
    (should (eq (ecb-next-listelem l 'a) 'b))
    (should (eq (ecb-next-listelem l 'd) 'a))
    (should (eq (ecb-next-listelem l 'c 3) 'b))
    (should (eq (ecb-next-listelem l 'c 7) 'b))
    (should (eq (ecb-next-listelem l 'c -1) 'b))
    (should-not (ecb-next-listelem l 'z))))

(ert-deftest ecb-aggregate-alist-groups-and-sorts ()
  (should (equal (ecb-aggregate-alist
                  '((a . a1) (a . a2) (b . b1) (c . c3) (a . a4) (a . a3) (b . b3) (b . b2))
                  (lambda (l r) (eq l r))
                  (lambda (i1 i2) (string< (symbol-name (car i1)) (symbol-name (car i2)))))
                 '((a a1 a2 a4 a3) (b b1 b3 b2) (c c3))))
  (should-not (ecb-aggregate-alist nil #'eq #'ignore)))

(ert-deftest ecb-values-of-symbol-value-list ()
  (let ((ecb-util-test-sym "resolved"))
    (ignore ecb-util-test-sym)
    (defvar ecb-util-test-dyn "dyn")
    (should (equal (ecb-values-of-symbol/value-list '(ecb-util-test-dyn "lit")) '("dyn" "lit")))
    (should (equal (ecb-values-of-symbol/value-list '((ecb-util-test-dyn . 1) ("x" . 2)) #'car)
                   '("dyn" "x")))))

(ert-deftest ecb-member-of-symbol-value-list ()
  (defvar ecb-util-test-dyn2 "dyn2")
  (let ((lst '(ecb-util-test-dyn2 "lit")))
    (should (eq (ecb-member-of-symbol/value-list "dyn2" lst) 'ecb-util-test-dyn2))
    (should (equal (ecb-member-of-symbol/value-list "lit" lst) "lit"))
    (should-not (ecb-member-of-symbol/value-list "nope" lst)))
  (let ((alist '((ecb-util-test-dyn2 . "d") ("lit" . "l"))))
    (should (equal (ecb-member-of-symbol/value-list "dyn2" alist #'car #'cdr) "d"))
    (should (equal (ecb-member-of-symbol/value-list "LIT" alist #'car #'cdr
                                                    (lambda (v e) (string= (downcase v) e)))
                   "l"))))

;;; ----------------------------------------------------------------------
;;; Regexp helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-match-regexp-list ()
  (should (equal (ecb-match-regexp-list "foo.el" '("\\.c$" "\\.el$")) "\\.el$"))
  (should-not (ecb-match-regexp-list "foo.py" '("\\.c$" "\\.el$")))
  ;; case-insensitive by design
  (should (ecb-match-regexp-list "FOO.EL" '("\\.el$")))
  (should (equal (ecb-match-regexp-list "x.c" '(("\\.el$" . el) ("\\.c$" . c)) #'car #'cdr) 'c))
  (should-not (ecb-match-regexp-list "x" nil)))

;;; ----------------------------------------------------------------------
;;; Multicache
;;; ----------------------------------------------------------------------

(defecb-multicache ecb-util-test-cache 10 'equal '(one two)
  "Throw-away cache for tests.")

(ert-deftest ecb-multicache-put-get-clear ()
  (ecb-multicache-clear 'ecb-util-test-cache)
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "k" 'one))
  (ecb-multicache-put-value 'ecb-util-test-cache "k" 'one 1)
  (ecb-multicache-put-value 'ecb-util-test-cache "k" 'two 2)
  (should (ecb-multicache-p 'ecb-util-test-cache))
  (should (= (ecb-multicache-get-value 'ecb-util-test-cache "k" 'one) 1))
  (should (= (ecb-multicache-get-value 'ecb-util-test-cache "k" 'two) 2))
  (should (equal (ecb-multicache-get-values 'ecb-util-test-cache "k") '((one . 1) (two . 2))))
  (ecb-multicache-clear-value 'ecb-util-test-cache "k" 'one)
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "k" 'one))
  (should (= (ecb-multicache-get-value 'ecb-util-test-cache "k" 'two) 2))
  (ecb-multicache-remove 'ecb-util-test-cache "k")
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "k" 'two))
  (ecb-multicache-put-value 'ecb-util-test-cache "a" 'one 'x)
  (ecb-multicache-clear 'ecb-util-test-cache)
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "a" 'one)))

(ert-deftest ecb-multicache-apply-and-map ()
  (ecb-multicache-clear 'ecb-util-test-cache)
  (ecb-multicache-put-value 'ecb-util-test-cache "a" 'one 1)
  (ecb-multicache-put-value 'ecb-util-test-cache "b" 'one 2)
  (ecb-multicache-apply-to-value 'ecb-util-test-cache "a" 'one (lambda (v) (* v 10)))
  (should (= (ecb-multicache-get-value 'ecb-util-test-cache "a" 'one) 10))
  (let (seen)
    (ecb-multicache-mapsubcache 'ecb-util-test-cache 'one
                                (lambda (k v) (push (cons k v) seen) v))
    (should (equal (sort seen (lambda (l r) (string< (car l) (car r))))
                   '(("a" . 10) ("b" . 2)))))
  (ecb-multicache-clear-subcache 'ecb-util-test-cache 'one)
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "a" 'one))
  (should-not (ecb-multicache-get-value 'ecb-util-test-cache "b" 'one)))

(ert-deftest ecb-multicache-print-subcache-returns-buffer-with-content ()
  (ecb-multicache-clear 'ecb-util-test-cache)
  (ecb-multicache-put-value 'ecb-util-test-cache "key1" 'one 'val1)
  (ecb-test-prepare-frame)
  (unwind-protect
      (progn
        (ecb-multicache-print-subcache 'ecb-util-test-cache 'one)
        (with-current-buffer "*ecb-multicache - subcache: one*"
          (should (string-match-p "key1" (buffer-string)))
          (should (string-match-p "val1" (buffer-string)))))
    (delete-other-windows)
    (ignore-errors (kill-buffer "*ecb-multicache - subcache: one*"))))

;;; ----------------------------------------------------------------------
;;; String helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-string=-and-string< ()
  (should (ecb-string= "a" "a"))
  (should-not (ecb-string= "a" "A"))
  (should (ecb-string= "a" "A" t))
  (should (ecb-string= 'sym "sym"))
  (should (ecb-string< "a" "b"))
  (should-not (ecb-string< "b" "a"))
  (should-not (ecb-string< "a" "a"))
  (should (ecb-string< "a" "B" t))
  (should (ecb-string< 'a "b")))

(ert-deftest ecb-trim-functions ()
  (should (string= (ecb-left-trim "  \t x ") "x "))
  (should (string= (ecb-right-trim " x \n\t") " x"))
  (should (string= (ecb-trim "  x  ") "x"))
  (should (string= (ecb-excessive-trim "a   b \t c") "a b c"))
  (should (string= (ecb-full-trim "  a   b  ") "a b"))
  (should (string= (ecb-trim "") ""))
  (should (string= (ecb-trim "   ") "")))

(ert-deftest ecb-fit-str-to-width ()
  (should (string= (ecb-fit-str-to-width "short" 10 'left) "short"))
  (should (string= (ecb-fit-str-to-width "abcdefghij" 7 'left) "...ghij"))
  (should (string= (ecb-fit-str-to-width "abcdefghij" 7 'right) "abcd..."))
  (should (= (length (ecb-fit-str-to-width "abcdefghij" 7 'left)) 7))
  (should (string= (ecb-fit-str-to-width "abcdefghij" 4 'left) ""))
  (should (string= (ecb-fit-str-to-width "abcd" 2 'left) "")))

(ert-deftest ecb-merge-face-into-text-returns-text ()
  (should (string= (substring-no-properties (ecb-merge-face-into-text "hello" 'bold)) "hello"))
  (should (string= (ecb-merge-face-into-text "hello" nil) "hello"))
  (should-not (ecb-merge-face-into-text nil 'bold)))

(ert-deftest ecb-merge-face-into-text-adds-face ()
  "KNOWN REGRESSION: commit e147547 reduced `ecb-merge-face' to a no-op
while stripping XEmacs code, so bucket/tag/VC faces are no longer applied
to tree-buffer text.  Remove :expected-result once the GNU Emacs branch
(`alter-text-property' or `add-face-text-property') is restored."
  :expected-result :failed
  (let ((s (ecb-merge-face-into-text (copy-sequence "hello") 'bold)))
    (should (memq 'bold (let ((f (get-text-property 0 'face s)))
                          (if (listp f) f (list f))))))
  ;; merging must keep an existing face and put the new one in front
  (let ((s (propertize "hello" 'face 'italic)))
    (setq s (ecb-merge-face-into-text s 'bold))
    (should (equal (get-text-property 0 'face s) '(bold italic)))))

;;; ----------------------------------------------------------------------
;;; Numbers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-normalize-number ()
  (should (= (ecb-normalize-number 0.5 100) 50))
  (should (= (ecb-normalize-number -0.25 100) -25))
  (should (= (ecb-normalize-number 0.5) 0))
  (should (= (ecb-normalize-number 10 100) 10))
  (should (= (ecb-normalize-number 1.0 100) 1))
  (should (= (ecb-normalize-number 7.9) 7)))

;;; ----------------------------------------------------------------------
;;; Time helpers
;;; ----------------------------------------------------------------------

(ert-deftest ecb-time-helpers ()
  (should (= (ecb-time-to-seconds '(1 0 0)) 65536.0))
  (should (equal (ecb-seconds-to-time 65537.5) '(1 1 500000)))
  (should (equal (ecb-subtract-time '(2 10) '(1 20)) '(0 65526)))
  (should (= (ecb-time-diff '(0 10 0) '(0 4 0) t) 6))
  (should (ecb-time-less-p '(0 1) '(0 2)))
  (should (ecb-time-less-p '(0 9) '(1 0)))
  (should-not (ecb-time-less-p '(1 0) '(0 9))))

;;; ----------------------------------------------------------------------
;;; Rings
;;; ----------------------------------------------------------------------

(ert-deftest ecb-ring-elements ()
  (let ((r (ecb-make-ring 3)))
    (should (ecb-ring-empty-p r))
    (ecb-ring-insert r 'a)
    (ecb-ring-insert r 'b)
    (should (equal (length (ecb-ring-elements r)) 3))
    (should (eq (ecb-ring-ref r 0) 'b))))

;;; ----------------------------------------------------------------------
;;; Buffers, windows, misc
;;; ----------------------------------------------------------------------

(ert-deftest ecb-with-readonly-buffer-restores-read-only ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (let ((buf (current-buffer)))
      (ecb-with-readonly-buffer buf
        (should-not buffer-read-only)
        (insert "x"))
      (should buffer-read-only)
      (should (string= (buffer-string) "x"))
      (should-error (ecb-with-readonly-buffer buf (error "boom")))
      (should buffer-read-only)))
  (should-error (ecb-with-readonly-buffer (get-buffer "no such buffer at all"))))

(ert-deftest ecb-buffer-name-and-obj-accept-buffer-or-window ()
  (with-temp-buffer
    (should (string= (ecb-buffer-name (current-buffer)) (buffer-name)))
    (should (eq (ecb-buffer-obj (buffer-name)) (current-buffer)))
    (should (eq (ecb-buffer-obj (current-buffer)) (current-buffer)))
    (let ((w (selected-window)))
      (should (eq (ecb-buffer-obj w) (window-buffer w))))))

(ert-deftest ecb-buffer-local-value ()
  (with-temp-buffer
    (setq-local fill-column 42)
    (should (= (ecb-buffer-local-value 'fill-column (current-buffer)) 42))))

(ert-deftest ecb-buffer-file-name-handles-indirect-buffers ()
  (ecb-test-with-temp-project proj
    (let* ((file (expand-file-name "README.txt" proj))
           (base (find-file-noselect file))
           (indirect (make-indirect-buffer base "ecb-util-test-indirect")))
      (unwind-protect
          (progn
            (should (string= (ecb-buffer-file-name base) (file-truename file)))
            (should (string= (ecb-buffer-file-name indirect) (file-truename file)))
            (should-not (ecb-buffer-file-name indirect t)))
        (kill-buffer indirect)
        (kill-buffer base)))))

(ert-deftest ecb-file-content-as-string ()
  (ecb-test-with-temp-project proj
    (should (string= (ecb-file-content-as-string (expand-file-name "README.txt" proj))
                     "readme\n"))))

(ert-deftest ecb-delete-file-ignores-missing ()
  (ecb-test-with-temp-dir dir
    (let ((f (expand-file-name "x" dir)))
      (with-temp-file f (insert "x"))
      (ecb-delete-file f)
      (should-not (file-exists-p f))
      ;; deleting again does not signal
      (ecb-delete-file f))))

(ert-deftest ecb-window-select-by-buffer-and-name ()
  (ecb-test-prepare-frame)
  (let* ((b1 (get-buffer-create "*ecb-util-w1*"))
         (b2 (get-buffer-create "*ecb-util-w2*"))
         (ecb-frame (selected-frame)))
    (unwind-protect
        (progn
          (switch-to-buffer b1)
          (split-window)
          (other-window 1)
          (switch-to-buffer b2)
          (should (eq (window-buffer (ecb-window-select b1)) b1))
          (should (eq (window-buffer (selected-window)) b1))
          (should (eq (window-buffer (ecb-window-select "*ecb-util-w2*")) b2))
          (should-not (ecb-window-select (get-buffer-create "*ecb-util-not-shown*"))))
      (delete-other-windows)
      (kill-buffer b1) (kill-buffer b2)
      (kill-buffer "*ecb-util-not-shown*"))))

(ert-deftest ecb-exec-in-window-runs-body-in-window-and-restores ()
  (ecb-test-prepare-frame)
  (let* ((b1 (get-buffer-create "*ecb-util-e1*"))
         (b2 (get-buffer-create "*ecb-util-e2*"))
         (ecb-frame (selected-frame)))
    (unwind-protect
        (progn
          (switch-to-buffer b1)
          (split-window)
          (other-window 1)
          (switch-to-buffer b2)
          (let ((before (selected-window)))
            (should (eq (ecb-exec-in-window b1 (current-buffer)) b1))
            (should (eq (selected-window) before))))
      (delete-other-windows)
      (kill-buffer b1) (kill-buffer b2))))

(ert-deftest ecb-canonical-windows-list-excludes-minibuffer ()
  (ecb-test-prepare-frame)
  (let ((ecb-frame (selected-frame)))
    (split-window)
    (unwind-protect
        (progn
          (should (= (length (ecb-canonical-windows-list)) 2))
          (should-not (cl-some #'window-minibuffer-p (ecb-canonical-windows-list))))
      (delete-other-windows))))

(ert-deftest ecb-window-in-window-list-number ()
  (ecb-test-prepare-frame)
  (let ((ecb-frame (selected-frame)))
    (split-window)
    (unwind-protect
        (let ((wl (ecb-canonical-windows-list)))
          (should (= (ecb-window-in-window-list-number wl (car wl)) 1))
          (should (= (ecb-window-in-window-list-number wl (cadr wl)) 2)))
      (delete-other-windows))))

(ert-deftest ecb-set-windows-dedicated-state ()
  (ecb-test-prepare-frame)
  (let ((b (get-buffer-create "*ecb-util-ded*")))
    (unwind-protect
        (progn
          (switch-to-buffer b)
          (ecb-set-windows-dedicated-state (list "*ecb-util-ded*") t)
          (should (window-dedicated-p (selected-window)))
          (ecb-make-windows-not-dedicated (selected-frame))
          (should-not (window-dedicated-p (selected-window))))
      (set-window-dedicated-p (selected-window) nil)
      (kill-buffer b))))

(ert-deftest ecb-create-menu-user-ext-type-nests-to-max-level ()
  (let ((type (ecb-create-menu-user-ext-type 1 2)))
    (should (eq (car type) 'repeat))
    ;; level 1 contains a submenu entry, level 2 (max) does not
    (should (cl-some (lambda (e) (and (consp e) (eq (car e) 'cons))) (cadr type)))
    (let ((inner (ecb-create-menu-user-ext-type 2 2)))
      (should-not (cl-some (lambda (e) (and (consp e) (eq (car e) 'cons))) (cadr inner))))))

(ert-deftest ecb-noninteractive-is-true-in-batch ()
  (should (ecb-noninteractive)))

(ert-deftest ecb-error-and-warning-format ()
  (should-error (ecb-error "oops %d" 1) :type 'error)
  (condition-case err
      (ecb-error "oops %d" 1)
    (error (should (string-match-p "ECB .* oops 1" (cadr err)))))
  (should (string-match-p "warn" (ecb-warning "warn %s" "x"))))

(ert-deftest ecb-goto-line-and-current-line ()
  (with-temp-buffer
    (insert "1\n2\n3\n")
    (ecb-goto-line 2)
    (should (= (ecb-current-line) 2))
    (should (looking-at "2"))))

(provide 'ecb-util-test)
;;; ecb-util-test.el ends here
