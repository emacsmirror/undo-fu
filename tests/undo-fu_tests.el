;;; undo-fu_tests.el --- Testing -*- lexical-binding: t; coding: utf-8 -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-undo-fu
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Integration tests for undo-fu undo/redo commands.
;; Tests run in batch mode via undo-fu_tests.py wrapper.

;;; Usage

;; Run via: python tests/undo-fu_tests.py

;;; Code:

;; ---------------------------------------------------------------------------
;; Message Capture (suppress minibuffer noise during tests)

(defvar undo-fu-test--captured-messages nil
  "List of messages captured during test execution (newest first).")

(defun undo-fu-test--message-capture (format-string &rest args)
  "Capture message instead of displaying.
Stores formatted message in `undo-fu-test--captured-messages'."
  (when format-string
    (push (apply #'format format-string args) undo-fu-test--captured-messages))
  ;; Return nil like `message' does when format-string is nil.
  nil)

(defun undo-fu-test--set-message-handler (message)
  "Handler for `set-message-function' that suppresses MESSAGE display.
Returns t to indicate the message was handled (prevents echo area display)."
  (when message
    (push message undo-fu-test--captured-messages))
  t)

(defmacro with-undo-fu-test-message-capture (&rest body)
  "Execute BODY with messages captured instead of displayed.
Messages are stored in `undo-fu-test--captured-messages'."
  (declare (indent 0))
  `(let ((undo-fu-test--captured-messages nil)
         (inhibit-message t)
         (echo-keystrokes 0)
         ;; Intercept echo area messages including interactive prompts.
         (set-message-function #'undo-fu-test--set-message-handler)
         (clear-message-function #'ignore)
         (orig-message (symbol-function 'message))
         ;; Save tooltip-mode state to restore later.
         ;; When enabled, `execute-kbd-macro' calls `tooltip-hide' which calls
         ;; `(message "")' to clear the echo area, polluting captured messages.
         (orig-tooltip-mode (bound-and-true-p tooltip-mode)))
     (unwind-protect
         (progn
           (fset 'message #'undo-fu-test--message-capture)
           (when (fboundp 'tooltip-mode)
             (tooltip-mode 0))
           ,@body)
       (fset 'message orig-message)
       (when (fboundp 'tooltip-mode)
         (tooltip-mode
          (cond
           (orig-tooltip-mode
            1)
           (t
            0)))))))

(defmacro should-error-with-message (form error-type expected-message)
  "Assert FORM signals an error of ERROR-TYPE with EXPECTED-MESSAGE."
  (declare (indent 1))
  (let ((err-sym (make-symbol "err")))
    `(let ((,err-sym (should-error ,form :type ,error-type)))
       (should (equal ,expected-message (error-message-string ,err-sym))))))

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(require 'ert)
(require 'undo-fu)

;; Bind undo-fu commands to function keys for testing via `execute-kbd-macro'.
(defconst undo-fu-test-key-undo [f5]
  "Key binding used for `undo-fu-only-undo' in tests.")
(defconst undo-fu-test-key-redo [f6]
  "Key binding used for `undo-fu-only-redo' in tests.")
(defconst undo-fu-test-key-disable-checkpoint [f7]
  "Key binding used for `undo-fu-disable-checkpoint' in tests.")
(defconst undo-fu-test-key-redo-all [f8]
  "Key binding used for `undo-fu-only-redo-all' in tests.")

(global-set-key undo-fu-test-key-undo 'undo-fu-only-undo)
(global-set-key undo-fu-test-key-redo 'undo-fu-only-redo)
(global-set-key undo-fu-test-key-disable-checkpoint 'undo-fu-disable-checkpoint)
(global-set-key undo-fu-test-key-redo-all 'undo-fu-only-redo-all)

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (let ((minibuffer-message-timeout 0))
         (execute-kbd-macro keys)))))

(defun undo-fu-test-count-undo-steps ()
  "Count the number of unconstrained undo steps back to the initial state.
This accounts for both `buffer-undo-list' entries and `undo-equiv-table'
entries that represent undo/redo transitions trimmed by `undo-fu-only-redo'.

The count is: direct groups in `buffer-undo-list' plus two for each
`undo-equiv-table' entry whose key is a superlist of the current
`buffer-undo-list' (representing a consumed undo+redo pair)."
  (let ((list buffer-undo-list)
        (groups 0)
        (in-group nil)
        ;; Collect all tails of buffer-undo-list for fast lookup.
        (tails (make-hash-table :test 'eq)))
    ;; Count groups and collect tails.
    (while (consp list)
      (puthash list t tails)
      (cond
       ((null (car list))
        (setq in-group nil))
       ((null in-group)
        (setq in-group t)
        (setq groups (1+ groups))))
      (setq list (cdr list)))
    ;; Count equiv-table entries NOT in buffer-undo-list tails
    ;; but whose key is a superlist (shares a tail with buffer-undo-list).
    ;; Each such entry represents an undo+redo pair (2 transitions).
    (let ((outside 0))
      (maphash
       (lambda (key _val)
         (unless (gethash key tails)
           (let ((k (cdr key))
                 (found nil))
             (while (and (consp k) (not found))
               (when (gethash k tails)
                 (setq found t))
               (setq k (cdr k)))
             (when found
               (setq outside (1+ outside))))))
       undo-equiv-table)
      (+ groups (* 2 outside)))))

(defun buffer-reset-text (initial-buffer-text)
  "Use INITIAL-BUFFER-TEXT to initialize the buffer with text."
  (buffer-disable-undo)
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text))
  (buffer-enable-undo))

(defmacro with-undo-fu-test (initial-buffer-text &rest body)
  "Run BODY in a temporary buffer with INITIAL-BUFFER-TEXT.
Messages are captured and not displayed."
  (declare (indent 1))
  `(with-undo-fu-test-message-capture
     (let ((buf (generate-new-buffer "untitled")))
       (switch-to-buffer buf)
       (buffer-reset-text ,initial-buffer-text)
       (prog1 (progn
                ,@body)
         (kill-buffer buf)))))

;; Suppress bell during tests.
(setq ring-bell-function #'ignore)

(defun undo-fu_tests-run-all ()
  "Run all undo-fu tests."
  (ert-run-tests-batch))

;; ---------------------------------------------------------------------------
;; Tests

(ert-deftest undo-fu-test-undo-single ()
  "Undo a single action.

Verifies: inserting text and undoing restores the original empty buffer."
  (with-undo-fu-test ""
    ;; Insert text.
    (simulate-input
      "hello")
    (should (equal "hello" (buffer-string)))
    ;; Undo the insertion.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))))

(ert-deftest undo-fu-test-undo-redo-single ()
  "Undo and redo a single action.

Verifies: undoing then redoing restores the inserted text."
  (with-undo-fu-test ""
    ;; Insert text.
    (simulate-input
      "hello")
    (should (equal "hello" (buffer-string)))
    ;; Undo the insertion.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    ;; Redo the insertion.
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "hello" (buffer-string)))))

(ert-deftest undo-fu-test-redo-past-last-action-fails ()
  "Redo past the last action fails.

Verifies: undo, redo, then redo again signals an error
because there is nothing left to redo."
  (with-undo-fu-test ""
    ;; Insert text.
    (simulate-input
      "hello")
    (should (equal "hello" (buffer-string)))
    ;; Undo the insertion.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    ;; Redo the insertion (should succeed).
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "hello" (buffer-string)))
    ;; Redo again (should fail - nothing left to redo).
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo)
      'user-error
      "Redo without undo step (C-g to ignore)")))

(ert-deftest undo-fu-test-unconstrained-step-count ()
  "Verify the unconstrained undo step count through an undo/redo cycle.

Write 3 words, undo 3 times, redo 3 times.
Each undo adds a step; each redo removes one (with `undo-fu-trim' enabled,
the consumed `undo-equiv-table' entry is removed so the count decreases)."
  (with-undo-fu-test ""
    ;; Write 3 words as separate actions.
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should (equal 3 (undo-fu-test-count-undo-steps)))

    ;; Undo 3 times: each undo adds a step.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))
    (should (equal 4 (undo-fu-test-count-undo-steps)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (should (equal 5 (undo-fu-test-count-undo-steps)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (should (equal 6 (undo-fu-test-count-undo-steps)))

    ;; Redo 3 times: each redo removes an undo step (trimmed).
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa" (buffer-string)))
    (should (equal 5 (undo-fu-test-count-undo-steps)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))
    (should (equal 4 (undo-fu-test-count-undo-steps)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should (equal 3 (undo-fu-test-count-undo-steps)))))

(ert-deftest undo-fu-test-undo-redo-cancels-out ()
  "Undo N then redo N should cancel out in the step count.

After undoing and redoing the same number of steps, the buffer content
is restored and the unconstrained step count should be unchanged.
With `undo-fu-trim' enabled, consumed `undo-equiv-table' entries are
removed during redo, so undo+redo pairs cancel out cleanly."
  (with-undo-fu-test ""
    ;; Write 3 words as separate actions.
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-undo-steps)))
      ;; Undo 3 times, redo 3 times.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      (simulate-input
        undo-fu-test-key-redo)
      (simulate-input
        undo-fu-test-key-redo)
      ;; Buffer content is restored.
      (should (equal text-before (buffer-string)))
      ;; Step count should be unchanged.
      (should (equal count-before (undo-fu-test-count-undo-steps))))))

(ert-deftest undo-fu-test-redo-undo-cancels-out ()
  "Redo N then undo N should cancel out in the step count.

Starting from a partially undone state, redoing then undoing the same
number of steps should restore both buffer content and step count.
With `undo-fu-trim' enabled, consumed `undo-equiv-table' entries are
removed during redo, so redo+undo pairs cancel out cleanly."
  (with-undo-fu-test ""
    ;; Write 3 words, then undo 3 to create redo-able state.
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-undo-steps)))
      ;; Redo 2 times, undo 2 times.
      (simulate-input
        undo-fu-test-key-redo)
      (simulate-input
        undo-fu-test-key-redo)
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      ;; Buffer content is restored.
      (should (equal text-before (buffer-string)))
      ;; Step count should be unchanged.
      (should (equal count-before (undo-fu-test-count-undo-steps))))))

(ert-deftest undo-fu-test-partial-undo-redo-cancels-out ()
  "Partial undo+redo should cancel out in the step count.

Write 3 words, undo only 1, redo only 1.  The buffer content and
unconstrained step count should be unchanged, verifying that trim
works for partial cycles (not just exhaustive ones)."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-undo-steps)))
      ;; Undo 1 time, redo 1 time.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      ;; Buffer content is restored.
      (should (equal text-before (buffer-string)))
      ;; Step count should be unchanged.
      (should (equal count-before (undo-fu-test-count-undo-steps))))))

(ert-deftest undo-fu-test-repeated-undo-redo-cycles-cancel-out ()
  "Repeated full undo/redo cycles should not drift the step count.

Write 3 words, then repeat (undo 3, redo 3) three times.  The step
count should remain the same after each cycle, verifying that trim
is stable and idempotent over multiple rounds."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-undo-steps)))
      (dotimes (_ 3)
        ;; Undo 3 times, redo 3 times.
        (simulate-input
          undo-fu-test-key-undo)
        (simulate-input
          undo-fu-test-key-undo)
        (simulate-input
          undo-fu-test-key-undo)
        (simulate-input
          undo-fu-test-key-redo)
        (simulate-input
          undo-fu-test-key-redo)
        (simulate-input
          undo-fu-test-key-redo)
        ;; Buffer content is restored.
        (should (equal text-before (buffer-string)))
        ;; Step count should be unchanged after each cycle.
        (should (equal count-before (undo-fu-test-count-undo-steps)))))))

(ert-deftest undo-fu-test-interleaved-edit-undo-redo-cancels-out ()
  "Undo/redo cycles interleaved with edits should each cancel out.

Write 2 words, undo 1 + redo 1 (cancel), write a 3rd word,
undo 1 + redo 1 (cancel).  The step count should be correct after
each cycle, verifying that trim works when new edits are interspersed."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (let ((text-phase1 (buffer-string))
          (count-phase1 (undo-fu-test-count-undo-steps)))
      ;; First cancel cycle.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal text-phase1 (buffer-string)))
      (should (equal count-phase1 (undo-fu-test-count-undo-steps))))
    ;; New edit.
    (simulate-input
      " ccc")
    (let ((text-phase2 (buffer-string))
          (count-phase2 (undo-fu-test-count-undo-steps)))
      ;; Second cancel cycle.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal text-phase2 (buffer-string)))
      (should (equal count-phase2 (undo-fu-test-count-undo-steps))))))

(ert-deftest undo-fu-test-nop-undo-disabled ()
  "Undo and redo should error when undo is disabled.

With `buffer-undo-list' set to t (undo disabled), both
`undo-fu-only-undo' and `undo-fu-only-redo' should signal
an error indicating undo has been disabled."
  (with-undo-fu-test ""
    (buffer-disable-undo)
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-undo)
      'user-error
      "Undo has been disabled!")
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo)
      'user-error
      "Undo has been disabled!")))

(ert-deftest undo-fu-test-nop-empty-buffer ()
  "Undo and redo should fail in a fresh empty buffer.

With no edit history, undo should fail (no undo information)
and redo should error (no undo step to redo)."
  (with-undo-fu-test ""
    ;; Undo fails (error caught internally, buffer unchanged).
    (setq undo-fu-test--captured-messages nil)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (should (member "No further undo information" undo-fu-test--captured-messages))
    ;; Redo signals an error (no undo step to redo).
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo)
      'user-error
      "Redo without undo step (C-g to ignore)")))

(ert-deftest undo-fu-test-nop-clear-all ()
  "Undo should fail after clearing all undo data.

Write text, call `undo-fu-clear-all', then verify that undo
fails because no undo information remains."
  (with-undo-fu-test ""
    (simulate-input
      "hello")
    (should (equal "hello" (buffer-string)))
    (undo-fu-clear-all)
    ;; Undo fails (error caught internally, buffer unchanged).
    (setq undo-fu-test--captured-messages nil)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))
    (should (member "No further undo information" undo-fu-test--captured-messages))))

(ert-deftest undo-fu-test-unconstrained-undo ()
  "Unconstrained undo traverses the full history including undo branches.

Add A B C, undo twice, add (Hello World), undo, add X Y Z,
backspace all.  Unconstrained undo should reach the branched
state \"A (Hello World)\" that constrained undo would skip."
  (with-undo-fu-test ""
    ;; Add A B C as separate actions.
    (simulate-input
      "A ")
    (simulate-input
      "B ")
    (simulate-input
      "C")
    (should (equal "A B C" (buffer-string)))
    ;; Undo twice: remove C and B.
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "A " (buffer-string)))
    ;; Add (Hello World) on a new branch.
    (simulate-input
      "(Hello World)")
    (should (equal "A (Hello World)" (buffer-string)))
    ;; Undo back to "A ".
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "A " (buffer-string)))
    ;; Add X Y Z on yet another branch.
    (simulate-input
      "X Y Z")
    (should (equal "A X Y Z" (buffer-string)))
    ;; Backspace all.
    (simulate-input
      (make-string 7 ?\177))
    (should (equal "" (buffer-string)))
    ;; Disable checkpoint and perform the first unconstrained undo in a single
    ;; key-macro so that `last-command' is `undo-fu-disable-checkpoint' when the
    ;; undo runs (`execute-kbd-macro' resets `last-command' on return).
    (execute-kbd-macro (vconcat undo-fu-test-key-disable-checkpoint undo-fu-test-key-undo))
    (should (equal "A X Y Z" (buffer-string)))
    ;; Subsequent unconstrained undos (checkpoint already disabled).
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "A " (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "A (Hello World)" (buffer-string)))))

(ert-deftest undo-fu-test-redo-all ()
  "Redo-all restores the buffer to the state before any undos.

Write 3 words, undo all 3, then `undo-fu-only-redo-all' should
restore the full buffer in a single command."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Undo all 3.
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    ;; Redo all in one command.
    (simulate-input
      undo-fu-test-key-redo-all)
    (should (equal "aaa bbb ccc" (buffer-string)))))

(ert-deftest undo-fu-test-edit-after-undo-breaks-redo ()
  "A new edit after undo should prevent redo.

Write text, undo, write different text.  Redo should error because
the new edit started a new branch, invalidating the redo chain."
  (with-undo-fu-test ""
    (simulate-input
      "hello")
    (should (equal "hello" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    ;; New edit breaks the redo chain.
    (simulate-input
      "world")
    (should (equal "world" (buffer-string)))
    ;; Redo should fail.
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo)
      'user-error
      "Redo without undo step (C-g to ignore)")))

(ert-deftest undo-fu-test-multi-step-undo ()
  "Undo multiple steps at once with a numeric prefix argument.

Write 3 words, undo all 3 at once via C-u 3.  Verifies the ARG
parameter for `undo-fu-only-undo'."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Undo 3 steps at once via C-u 3.
    (execute-kbd-macro (vconcat [?\C-u ?3] undo-fu-test-key-undo))
    (should (equal "" (buffer-string)))))

(ert-deftest undo-fu-test-multi-step-redo ()
  "Redo multiple steps at once with a numeric prefix argument.

Write 3 words, undo 3 times (one at a time to create 3 redo groups),
then redo all 3 at once via C-u 3.  Verifies the ARG parameter
for `undo-fu-only-redo'."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Undo one at a time (creates separate redo groups).
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    ;; Redo 3 steps at once via C-u 3.
    (execute-kbd-macro (vconcat [?\C-u ?3] undo-fu-test-key-redo))
    (should (equal "aaa bbb ccc" (buffer-string)))))

(ert-deftest undo-fu-test-trim-disabled-step-count-grows ()
  "With `undo-fu-trim' disabled, redo should increase the step count.

When `undo-fu-trim' is nil, `undo-equiv-table' entries are preserved
during redo, so each redo adds a step rather than canceling one out."
  (with-undo-fu-test ""
    (let ((undo-fu-trim nil))
      (simulate-input
        "aaa")
      (simulate-input
        " bbb")
      (simulate-input
        " ccc")
      (should (equal 3 (undo-fu-test-count-undo-steps)))
      ;; Undo 3 times: each adds a step (same as with trim).
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (should (equal 6 (undo-fu-test-count-undo-steps)))
      ;; Redo 3 times: each ALSO adds a step (no trim).
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 7 (undo-fu-test-count-undo-steps)))
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 8 (undo-fu-test-count-undo-steps)))
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 9 (undo-fu-test-count-undo-steps))))))

(ert-deftest undo-fu-test-undo-stops-at-initial-text ()
  "Undo should not go past the initial buffer text.

Start with pre-existing text, insert more, then undo repeatedly.
The buffer should return to the initial text and undo should fail
when there is nothing left to undo."
  (with-undo-fu-test "hello "
    (goto-char (point-max))
    (simulate-input
      "world")
    (should (equal "hello world" (buffer-string)))
    ;; Undo the insertion.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello " (buffer-string)))
    ;; Further undo should fail (initial text can't be undone).
    (setq undo-fu-test--captured-messages nil)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello " (buffer-string)))
    (should (member "No further undo information" undo-fu-test--captured-messages))))

(ert-deftest undo-fu-test-redo-all-partial ()
  "Redo-all after partial undo should restore the full buffer.

Write 3 words, undo only 2, then redo-all should redo exactly
those 2 and stop at the original state."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Undo only 2 of 3.
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    ;; Redo all should restore to "aaa bbb ccc".
    (simulate-input
      undo-fu-test-key-redo-all)
    (should (equal "aaa bbb ccc" (buffer-string)))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; undo-fu_tests.el ends here
