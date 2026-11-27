;;; ecb-advice-scenario-test.el --- ERT port of the original advice-backbone test -*- lexical-binding: t -*-

;; Author: Ryan Ware
;; Assisted-by: Claude:claude-fable-5-1

;;; Commentary:
;;
;; Klaus Berndl's original `ecb-advice-test.el' was a manual test: run
;; `ecb-test-with-original-advice-set' and compare *Messages* with the
;; expected output in the file.  This is the same scenario, but every
;; original function / advice records into a log and the resulting event
;; sequence is compared against the expected output automatically.  The
;; "ECB 2.52: debug ..." messages are covered by
;; `ecb-common-browser-test.el'; here only the observable call order
;; matters.

;;; Code:

(require 'ecb-test-helpers)

(defecb-advice-set ecb-advice-port-test-set
  "An advice-set only for testing the advice-mechanism of ECB")

(defun ecb-advice-port-defun-1 ()
  (ecb-test-record 'original-1))

(defecb-advice ecb-advice-port-defun-1 around ecb-advice-port-test-set
  "An advice"
  (ecb-test-record 'around-1))

(defun ecb-advice-port-defun-2 ()
  (ecb-test-record 'original-2))

(defecb-advice ecb-advice-port-defun-2 before ecb-advice-port-test-set
  "An advice"
  (ecb-test-record 'before-2))

(defecb-advice ecb-advice-port-defun-2 after ecb-advice-port-test-set
  "An advice"
  (ecb-test-record 'after-2))

(defun ecb-advice-port-defun-3 ()
  (ecb-test-record 'original-3))

(defecb-advice ecb-advice-port-defun-3 around ecb-always-disabled-advices
  "An always disabled advice"
  (ecb-test-record 'around-3))

(defecb-advice ecb-advice-port-defun-3 after ecb-always-disabled-advices
  "An always disabled advice"
  (ecb-test-record 'after-3))

(defun ecb-advice-port-defun-4 ()
  (ecb-test-record 'original-4))

(defecb-advice ecb-advice-port-defun-4 around ecb-always-disabled-advices
  "An always disabled advice"
  (ecb-test-record 'around-4))

(defun ecb-advice-port-run-scenario ()
  "Run the scenario of the original `ecb-test-with-original-advice-set'.
Returns the recorded event list."
  (ecb-test-with-call-log
    (let ((ecb-advices-debug-error nil))
      (unwind-protect
          (progn
            (ecb-test-record 'begin)
            (ecb-enable-advices 'ecb-advice-port-test-set)
            (ecb-disable-advices 'ecb-always-disabled-advices)
            (ecb-advice-port-defun-1)
            (ecb-advice-port-defun-2)
            (ecb-advice-port-defun-3)
            (ecb-advice-port-defun-4)
            (ecb-with-original-adviced-function-set 'ecb-advice-port-test-set
              (ecb-advice-port-defun-1)
              (ecb-test-record 'loc-0.1)
              (ecb-with-ecb-advice 'ecb-advice-port-defun-3 'around
                (ecb-advice-port-defun-1)
                (ecb-advice-port-defun-2)
                (ecb-advice-port-defun-3)
                (ecb-advice-port-defun-4)
                (ecb-with-ecb-advice 'ecb-advice-port-defun-3 'around
                  (ecb-advice-port-defun-3)
                  (ecb-advice-port-defun-4)
                  (ecb-with-ecb-advice 'ecb-advice-port-defun-3 'after
                    (ecb-advice-port-defun-3)
                    (ecb-advice-port-defun-4)
                    (ecb-with-ecb-advice 'ecb-advice-port-defun-4 'around
                      (ecb-advice-port-defun-3)
                      (ecb-advice-port-defun-4))
                    (ecb-test-record 'loc-0.2)
                    (ecb-advice-port-defun-3)
                    (ecb-advice-port-defun-4))
                  (ecb-test-record 'loc-0.3)
                  (ecb-advice-port-defun-3)
                  (ecb-advice-port-defun-4))
                (ecb-test-record 'loc-0.4)
                (ecb-advice-port-defun-3)
                (ecb-advice-port-defun-4))
              (ecb-advice-port-defun-2)
              (ecb-advice-port-defun-3)
              (ecb-advice-port-defun-4)
              (ecb-test-record 'loc-1)
              (ecb-with-original-adviced-function-set 'ecb-advice-port-test-set
                (ecb-advice-port-defun-1)
                (ecb-advice-port-defun-2))
              (ecb-test-record 'loc-2)
              (ecb-advice-port-defun-1)
              (ecb-advice-port-defun-2)
              (ecb-test-record 'loc-3)
              (ecb-with-original-adviced-function-set 'ecb-advice-port-test-set
                (ecb-advice-port-defun-1)
                (ecb-advice-port-defun-2)
                (ecb-test-record 'loc-4)
                (ecb-with-original-adviced-function-set 'ecb-advice-port-test-set
                  (ecb-advice-port-defun-1)
                  (ecb-advice-port-defun-2)))
              (ecb-test-record 'loc-5))
            (ecb-advice-port-defun-1)
            (ecb-advice-port-defun-2)
            (ecb-test-record 'loc-6))
        (ecb-disable-advices 'ecb-advice-port-test-set)
        (ecb-advice-port-defun-1)
        (ecb-advice-port-defun-2)
        (ecb-advice-port-defun-3)
        (ecb-advice-port-defun-4)
        (ecb-test-record 'end)))))

(defconst ecb-advice-port-expected-events
  '(begin
    ;; test-set enabled, always-disabled set disabled
    around-1
    before-2 original-2 after-2
    original-3
    original-4
    ;; with original advice-set
    original-1
    loc-0.1
    ;; with always-disabled around advice of defun-3
    original-1 original-2
    around-3
    original-4
    ;; nested, same advice again (already enabled)
    around-3
    original-4
    ;; plus after advice of defun-3
    around-3 after-3
    original-4
    ;; plus around advice of defun-4
    around-3 after-3
    around-4
    loc-0.2
    around-3 after-3
    original-4
    loc-0.3
    around-3
    original-4
    loc-0.4
    around-3
    original-4
    ;; left innermost with-ecb-advice, still inside the original advice-set
    original-2 original-3 original-4
    loc-1
    original-1 original-2
    loc-2
    original-1 original-2
    loc-3
    original-1 original-2
    loc-4
    original-1 original-2
    loc-5
    ;; original advice-set left -> test-set re-enabled
    around-1
    before-2 original-2 after-2
    loc-6
    ;; unwind: test-set disabled
    original-1 original-2 original-3 original-4
    end)
  "Event sequence derived from the expected output of the original test.")

(ert-deftest ecb-advice-original-scenario-call-order ()
  "The full scenario of Klaus Berndl's manual advice test, checked automatically."
  (let ((events (mapcar #'car (ecb-advice-port-run-scenario))))
    (should (equal events ecb-advice-port-expected-events))))

(ert-deftest ecb-advice-original-scenario-leaves-everything-disabled ()
  (ecb-advice-port-run-scenario)
  (dolist (fn '(ecb-advice-port-defun-1 ecb-advice-port-defun-2
                ecb-advice-port-defun-3 ecb-advice-port-defun-4))
    (should-not (ecb-test-advised-p fn)))
  ;; a plain call runs only the originals
  (should (equal (mapcar #'car (ecb-test-with-call-log
                                 (ecb-advice-port-defun-1)
                                 (ecb-advice-port-defun-2)))
                 '(original-1 original-2))))

(ert-deftest ecb-advice-original-scenario-is-repeatable ()
  (should (equal (ecb-advice-port-run-scenario) (ecb-advice-port-run-scenario))))

(ert-deftest ecb-advice-original-scenario-with-debug-messages ()
  "Running with `ecb-advices-debug-error' produces the documented debug lines."
  (let ((messages nil))
    (cl-letf* ((orig (symbol-function 'message))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) messages))
                  (apply orig fmt args))))
      (let ((ecb-advices-debug-error t))
        (ecb-with-original-adviced-function-set 'ecb-advice-port-test-set
          (ecb-advice-port-defun-1))))
    (setq messages (nreverse messages))
    (should (cl-some (lambda (m) (string-match-p "debug with original advice-set: ecb-advice-port-test-set - ENTRY" m))
                     messages))
    (should (cl-some (lambda (m) (string-match-p "debug with original advice-set: ecb-advice-port-test-set - EXIT" m))
                     messages))))

(provide 'ecb-advice-scenario-test)
;;; ecb-advice-scenario-test.el ends here
