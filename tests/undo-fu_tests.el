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
(defconst undo-fu-test-key-keyboard-quit [f10]
  "Key binding used for `undo-fu-test--keyboard-quit' in tests.")

(defun undo-fu-test--keyboard-quit ()
  "Stand in for `keyboard-quit' in keyboard macros.

Running `keyboard-quit' from a keyboard macro signals `quit', aborting the macro,
so only set `this-command', seen as `last-command' by the next command in the macro."
  (interactive)
  (setq this-command 'keyboard-quit))

(global-set-key undo-fu-test-key-undo 'undo-fu-only-undo)
(global-set-key undo-fu-test-key-redo 'undo-fu-only-redo)
(global-set-key undo-fu-test-key-disable-checkpoint 'undo-fu-disable-checkpoint)
(global-set-key undo-fu-test-key-redo-all 'undo-fu-only-redo-all)
(global-set-key undo-fu-test-key-keyboard-quit 'undo-fu-test--keyboard-quit)

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (let ((minibuffer-message-timeout 0))
         (execute-kbd-macro keys)))))

(defun undo-fu-test-count-history-entries ()
  "Count undo history entries, including those left by undo+redo pairs.

The count is: groups in `buffer-undo-list' plus two for each
`undo-equiv-table' entry whose key is a superlist of the current
`buffer-undo-list' (left by an undo+redo pair).

Note that these entries can't be reached from `buffer-undo-list',
undo/redo don't use them and they may be removed by garbage collection.
This measures the entries `undo-fu-trim' removes, not the steps undo can reach."
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

(defmacro with-undo-fu-test-without-gc (initial-buffer-text &rest body)
  "Run `with-undo-fu-test' with INITIAL-BUFFER-TEXT & BODY, without garbage collection.

Use for tests calling `undo-fu-test-count-history-entries',
as garbage collection removes the entries it counts."
  (declare (indent 1))
  ;; With `undo-fu-trim' disabled the count would be too low,
  ;; when enabled it would hide entries trimming failed to remove.
  ;; Other tests run with garbage collection, as it runs for users.
  `(let ((gc-cons-threshold most-positive-fixnum))
     (with-undo-fu-test ,initial-buffer-text
       ,@body)))

(defmacro with-undo-fu-test-other-buffer (vars &rest body)
  "Run BODY with VARS, a list of symbols (BUF BUF-OTHER), bound to buffers.
BUF is the current buffer, BUF-OTHER is a new buffer,
killed after BODY runs (switching back to BUF)."
  (declare (indent 1))
  (let ((buf (nth 0 vars))
        (buf-other (nth 1 vars)))
    `(let ((,buf (current-buffer))
           (,buf-other (generate-new-buffer "untitled-other")))
       (unwind-protect
           (progn
             ,@body)
         (switch-to-buffer ,buf)
         (kill-buffer ,buf-other)))))

(defmacro with-undo-fu-test-undo-in-region (&rest body)
  "Run BODY with `undo-fu-allow-undo-in-region' enabled.
Also enable `transient-mark-mode' (off in batch mode), needed for `region-active-p'."
  (declare (indent 0))
  `(let ((undo-fu-allow-undo-in-region t)
         (transient-mark-mode t))
     ,@body))

(defun undo-fu-test--select-all ()
  "Select the whole buffer, activating the region.
As `mark-whole-buffer' does, which is only intended for interactive use."
  (push-mark (point-min) nil t)
  (goto-char (point-max)))

(defun undo-fu-test--undo-in-other-buffer-impl (buf-other key in-region words)
  "Write WORDS in BUF-OTHER & undo the last using KEY, in region when IN-REGION is non-nil.
This replaces `pending-undo-list'.  Switch back to the current buffer afterwards."
  (let ((buf (current-buffer)))
    (switch-to-buffer buf-other)
    (buffer-reset-text "")
    (dolist (word words)
      (simulate-input
        word))
    (when in-region
      (undo-fu-test--select-all))
    (simulate-input
      key)
    (should (equal (apply #'concat (butlast words)) (buffer-string)))
    (switch-to-buffer buf)))

(defun undo-fu-test--undo-in-region-in-other-buffer (buf-other &rest words)
  "Write WORDS in BUF-OTHER & undo the last in region, setting `undo-in-region'.
Use with `with-undo-fu-test-undo-in-region'.  Switch back to the current buffer afterwards."
  (undo-fu-test--undo-in-other-buffer-impl buf-other undo-fu-test-key-undo t words))

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

(ert-deftest undo-fu-test-history-entry-count ()
  "Verify the history entry count through an undo/redo cycle.

Write 3 words, undo 3 times, redo 3 times.
Each undo adds an entry; each redo removes one (with `undo-fu-trim' enabled,
the consumed `undo-equiv-table' entry is removed so the count decreases)."
  (with-undo-fu-test-without-gc ""
    ;; Write 3 words as separate actions.
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should (equal 3 (undo-fu-test-count-history-entries)))

    ;; Undo 3 times: each undo adds an entry.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))
    (should (equal 4 (undo-fu-test-count-history-entries)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (should (equal 5 (undo-fu-test-count-history-entries)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (should (equal 6 (undo-fu-test-count-history-entries)))

    ;; Redo 3 times: each redo removes an entry (trimmed).
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa" (buffer-string)))
    (should (equal 5 (undo-fu-test-count-history-entries)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))
    (should (equal 4 (undo-fu-test-count-history-entries)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should (equal 3 (undo-fu-test-count-history-entries)))))

(ert-deftest undo-fu-test-undo-redo-cancels-out ()
  "Check undo N then redo N cancels out in the history entry count.

After undoing and redoing the same number of steps, the buffer content
is restored and the history entry count should be unchanged.
With `undo-fu-trim' enabled, consumed `undo-equiv-table' entries are
removed during redo, so undo+redo pairs cancel out cleanly."
  (with-undo-fu-test-without-gc ""
    ;; Write 3 words as separate actions.
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-history-entries)))
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
      (should (equal count-before (undo-fu-test-count-history-entries))))))

(ert-deftest undo-fu-test-redo-undo-cancels-out ()
  "Check redo N then undo N cancels out in the history entry count.

Starting from a partially undone state, redoing then undoing the same
number of steps should restore both buffer content and history entry count.
With `undo-fu-trim' enabled, consumed `undo-equiv-table' entries are
removed during redo, so redo+undo pairs cancel out cleanly."
  (with-undo-fu-test-without-gc ""
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
          (count-before (undo-fu-test-count-history-entries)))
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
      (should (equal count-before (undo-fu-test-count-history-entries))))))

(ert-deftest undo-fu-test-partial-undo-redo-cancels-out ()
  "Check partial undo+redo cancels out in the history entry count.

Write 3 words, undo only 1, redo only 1.  The buffer content and
history entry count should be unchanged, verifying that trim
works for partial cycles (not just exhaustive ones)."
  (with-undo-fu-test-without-gc ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-history-entries)))
      ;; Undo 1 time, redo 1 time.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      ;; Buffer content is restored.
      (should (equal text-before (buffer-string)))
      ;; Step count should be unchanged.
      (should (equal count-before (undo-fu-test-count-history-entries))))))

(ert-deftest undo-fu-test-repeated-undo-redo-cycles-cancel-out ()
  "Check repeated full undo/redo cycles don't drift the history entry count.

Write 3 words, then repeat (undo 3, redo 3) three times.  The history
entry count should remain the same after each cycle, verifying that trim
is stable and idempotent over multiple rounds."
  (with-undo-fu-test-without-gc ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (let ((text-before (buffer-string))
          (count-before (undo-fu-test-count-history-entries)))
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
        (should (equal count-before (undo-fu-test-count-history-entries)))))))

(ert-deftest undo-fu-test-interleaved-edit-undo-redo-cancels-out ()
  "Undo/redo cycles interleaved with edits should each cancel out.

Write 2 words, undo 1 + redo 1 (cancel), write a 3rd word,
undo 1 + redo 1 (cancel).  The history entry count should be correct after
each cycle, verifying that trim works when new edits are interspersed."
  (with-undo-fu-test-without-gc ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (let ((text-phase1 (buffer-string))
          (count-phase1 (undo-fu-test-count-history-entries)))
      ;; First cancel cycle.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal text-phase1 (buffer-string)))
      (should (equal count-phase1 (undo-fu-test-count-history-entries))))
    ;; New edit.
    (simulate-input
      " ccc")
    (let ((text-phase2 (buffer-string))
          (count-phase2 (undo-fu-test-count-history-entries)))
      ;; Second cancel cycle.
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal text-phase2 (buffer-string)))
      (should (equal count-phase2 (undo-fu-test-count-history-entries))))))

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

(ert-deftest undo-fu-test-multi-step-redo-stops-at-initial-undo ()
  "Redo with a count larger than the undo steps, without undoing.

Write 3 words, undo 2, then redo 3 at once via C-u 3.  Only the 2 undo
steps should be redone, the remaining step must not undo \" ccc\"
\(which would also remove it from the undo history)."
  (with-undo-fu-test ""
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
    (should (equal "aaa" (buffer-string)))
    ;; Redo 3 steps at once via C-u 3, only 2 are available.
    (execute-kbd-macro (vconcat [?\C-u ?3] undo-fu-test-key-redo))
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; The history is intact, undo all the way back.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))))

(ert-deftest undo-fu-test-multi-step-redo-keeps-undone-history ()
  "Redo multiple steps, keeping undo from restoring previously undone changes.

Write 2 words, undo the second, write 2 more words, undo 2, then redo
both at once via C-u 2.  Undoing all the way should never restore the
word undone earlier.  Verifies `undo-fu-trim' only removes the entries
consumed by redo, keeping those `undo-only' uses to skip undone changes."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    ;; Undo " bbb", then continue editing so it's part of older history.
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      " xxx")
    (simulate-input
      " yyy")
    (should (equal "aaa xxx yyy" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    ;; Redo 2 steps at once via C-u 2.
    (execute-kbd-macro (vconcat [?\C-u ?2] undo-fu-test-key-redo))
    (should (equal "aaa xxx yyy" (buffer-string)))
    ;; Undo everything, " bbb" must not be restored.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa xxx" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))))

(ert-deftest undo-fu-test-multi-step-redo-history-entry-count ()
  "Verify redoing multiple steps at once removes an entry for each step.

Write 3 words, undo 3 times, then redo all 3 at once via C-u 3.
With `undo-fu-trim' enabled, the history entry count returns to 3
\(as when redoing one step at a time, see `undo-fu-test-history-entry-count')."
  (with-undo-fu-test-without-gc ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (should (equal 3 (undo-fu-test-count-history-entries)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (should (equal 6 (undo-fu-test-count-history-entries)))
    ;; Redo 3 steps at once via C-u 3, each step removes an entry (trimmed).
    (execute-kbd-macro (vconcat [?\C-u ?3] undo-fu-test-key-redo))
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should (equal 3 (undo-fu-test-count-history-entries)))))

(defun undo-fu-test--trim-stress-log (trim seed steps)
  "Run STEPS random actions from SEED with `undo-fu-trim' set to TRIM.
Return a log of each action with the resulting buffer text (or error),
followed by the texts reached when undoing all the way back.

Also check redo-all returns to the text of the last edit,
unless an unconstrained action ran since that edit."
  (with-undo-fu-test ""
    (let ((undo-fu-trim trim)
          (log (list))
          (word 0)
          ;; The text after the last edit, nil after an unconstrained action.
          (text-edit ""))
      (random seed)
      (dotimes (_ steps)
        (let* ((n (1+ (random 3)))
               (action (random 10))
               (keys
                (pcase action
                  ((or 0 1)
                   (setq word (1+ word))
                   (format " w%d" word))
                  ((or 2 3 4) (vconcat [?\C-u] (number-to-string n) undo-fu-test-key-undo))
                  ((or 5 6 7) (vconcat [?\C-u] (number-to-string n) undo-fu-test-key-redo))
                  (8 undo-fu-test-key-redo-all)
                  (9
                   ;; Unconstrained, in a single key-macro so `last-command' is
                   ;; `undo-fu-disable-checkpoint' when undo/redo runs.
                   (vconcat
                    undo-fu-test-key-disable-checkpoint
                    (cond
                     ((zerop (random 2))
                      undo-fu-test-key-undo)
                     (t
                      undo-fu-test-key-redo)))))))
          (push (cons
                 keys
                 (condition-case err
                     (progn
                       (execute-kbd-macro keys)
                       (buffer-string))
                   (error
                    (list 'error (error-message-string err)))))
                log)
          (pcase action
            ((or 0 1) (setq text-edit (buffer-string)))
            (8
             ;; Redo-all stops at the initial undo, neither before (keeping changes undone)
             ;; or after (undoing changes made before the undo started).
             (when text-edit
               (should (equal text-edit (buffer-string)))))
            (9 (setq text-edit nil)))))
      ;; Edit so the next undo starts a new (constrained) chain,
      ;; otherwise an unconstrained action above may still be in effect.
      (simulate-input
        " end")
      ;; Undo all the way back, recording each state.
      ;; Each action adds at most one step to undo, limit the number of undo steps
      ;; so a regression that alternates between states fails instead of hanging.
      ;;
      ;; NOTE: don't stop once undo leaves the text unchanged,
      ;; as this happens for steps which don't change the text.
      (let ((limit (+ steps 3)))
        (while (progn
                 (setq undo-fu-test--captured-messages nil)
                 (simulate-input
                   undo-fu-test-key-undo)
                 (push (buffer-string) log)
                 (setq limit (1- limit))
                 (and (not (member "No further undo information" undo-fu-test--captured-messages))
                      (> limit 0))))
        (should (> limit 0)))
      (nreverse log))))

(ert-deftest undo-fu-test-trim-stress ()
  "Check trimming doesn't change the behavior of undo/redo.

Run the same random sequence of edits, (multi-step) undo/redo, redo-all
and unconstrained undo/redo with `undo-fu-trim' enabled and disabled.
Verifies the buffer text after every action is identical, that redo-all
returns to the last edit and that (after an edit) undoing all the way back
reaches the initial (empty) state."
  (unwind-protect
      (dolist (seed '("a" "b" "c" "d" "e" "f" "g" "h"))
        (let ((log-trim (undo-fu-test--trim-stress-log t seed 300))
              (log-no-trim (undo-fu-test--trim-stress-log nil seed 300)))
          (should (equal log-no-trim log-trim))
          (should (equal "" (car (last log-trim))))))
    ;; Emacs can't restore the previous random state,
    ;; reseed so tests which run after this don't use a fixed sequence.
    (random t)))

(ert-deftest undo-fu-test-trim-disabled-history-entry-count-grows ()
  "Check redo increases the history entry count with `undo-fu-trim' disabled.

When `undo-fu-trim' is nil, `undo-equiv-table' entries are preserved
during redo, so each redo leaves an entry rather than canceling one out
\(see `undo-fu-test-count-history-entries' for what is counted)."
  (with-undo-fu-test-without-gc ""
    (let ((undo-fu-trim nil))
      (simulate-input
        "aaa")
      (simulate-input
        " bbb")
      (simulate-input
        " ccc")
      (should (equal 3 (undo-fu-test-count-history-entries)))
      ;; Undo 3 times: each adds an entry (same as with trim).
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (simulate-input
        undo-fu-test-key-undo)
      (should (equal 6 (undo-fu-test-count-history-entries)))
      ;; Redo 3 times: each ALSO adds an entry (no trim).
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 7 (undo-fu-test-count-history-entries)))
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 8 (undo-fu-test-count-history-entries)))
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal 9 (undo-fu-test-count-history-entries))))))

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

(ert-deftest undo-fu-test-multi-step-redo-partial-failure ()
  "Redo multiple steps where a later step fails, keeping the steps redone.

Edit the end then the start of the buffer, undo both, then narrow the buffer
to exclude the start.  Redo 2 steps at once via C-u 2, the first step succeeds
and the second fails, which is shown as a message instead of an error."
  (with-undo-fu-test "hello"
    (goto-char (point-max))
    (simulate-input
      "X")
    (goto-char (point-min))
    (simulate-input
      "Y")
    (should (equal "YhelloX" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))
    (narrow-to-region 2 (point-max))
    (setq undo-fu-test--captured-messages nil)
    (execute-kbd-macro (vconcat [?\C-u ?2] undo-fu-test-key-redo))
    (widen)
    (should (equal "helloX" (buffer-string)))
    (should
     (member
      "Changes to be undone are outside visible portion of buffer"
      undo-fu-test--captured-messages))))

(ert-deftest undo-fu-test-multi-step-redo-partial-failure-within-step ()
  "Redo multiple steps where a later step fails part way, then undo.

Edit the buffer, then edit the end and the start in one command, undo both,
then narrow the buffer to exclude the start.  Redo 2 steps at once via C-u 2,
the second step fails after redoing the edit at the end.
Undo reverts the partially redone step instead of finishing it."
  (with-undo-fu-test "hello"
    (goto-char (point-max))
    (simulate-input
      "X")
    (let ((map (make-sparse-keymap)))
      (define-key
       map [f9]
       (lambda ()
         (interactive)
         (goto-char (point-max))
         (insert "E")
         (goto-char (point-min))
         (insert "S")))
      (use-local-map map))
    (simulate-input
      [f9])
    (should (equal "ShelloXE" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))
    (narrow-to-region 2 (point-max))
    (execute-kbd-macro (vconcat [?\C-u ?2] undo-fu-test-key-redo))
    (widen)
    (should (equal "helloXE" (buffer-string)))
    ;; Revert the partially redone step.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "helloX" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))))

(ert-deftest undo-fu-test-redo-all-partial-failure ()
  "Redo-all where a later step fails, keeping the steps redone.

Edit the end then the start of the buffer, undo both, then narrow the buffer
to exclude the start.  Redo-all redoes the first step and raises the error
of the second, without the message which would show all steps were redone."
  (with-undo-fu-test "hello"
    (goto-char (point-max))
    (simulate-input
      "X")
    (goto-char (point-min))
    (simulate-input
      "Y")
    (should (equal "YhelloX" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))
    (narrow-to-region 2 (point-max))
    (setq undo-fu-test--captured-messages nil)
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo-all)
      'error
      "Changes to be undone are outside visible portion of buffer")
    (widen)
    (should (equal "helloX" (buffer-string)))
    (should-not (member "Redo All" undo-fu-test--captured-messages))))

(ert-deftest undo-fu-test-redo-zero-steps ()
  "Redo zero steps via C-u 0, redoing nothing.

Write a word, undo it, then redo 0 steps.  The buffer is unchanged
and the undo step can still be redone afterwards."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (setq undo-fu-test--captured-messages nil)
    (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-redo))
    (should (equal "" (buffer-string)))
    (should-not undo-fu-test--captured-messages)
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa" (buffer-string)))))

(ert-deftest undo-fu-test-redo-zero-steps-between-unconstrained-undo ()
  "Redo zero steps via C-u 0 between unconstrained undo steps.

Write 3 words, undo 1 step, then disable the checkpoint.  Redoing nothing
must not change the undo state, otherwise the next unconstrained undo
would redo \" ccc\" instead of undoing \" bbb\"."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))
    (simulate-input
      undo-fu-test-key-disable-checkpoint)
    (setq undo-fu-test--captured-messages nil)
    (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-redo))
    (should (equal "aaa bbb" (buffer-string)))
    (should-not undo-fu-test--captured-messages)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))))

(ert-deftest undo-fu-test-redo-zero-steps-after-keyboard-quit ()
  "Redo zero steps via C-u 0 after `keyboard-quit', stepping over the checkpoint.

Write 3 words, undo 2, then press `keyboard-quit' and redo nothing.
The checkpoint is stepped over as with any other count, so the following
redo steps are unconstrained, continuing past the initial undo step."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (setq undo-fu-test--captured-messages nil)
    ;; A single key-macro so `last-command' is `keyboard-quit' when the redo runs.
    (execute-kbd-macro (vconcat undo-fu-test-key-keyboard-quit [?\C-u ?0] undo-fu-test-key-redo))
    (should (equal "aaa" (buffer-string)))
    (should (member "Redo checkpoint stepped over!" undo-fu-test--captured-messages))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Unconstrained, redo continues past the initial undo step.
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))))

(ert-deftest undo-fu-test-redo-other-buffer-undo-in-region-message ()
  "Redo after undoing in region in another buffer, without showing \"Redo in region\".

Undo in one buffer, then undo in region in another buffer and switch back.
Redo never runs in region, so the global `undo-in-region' left by the other buffer
must not be shown."
  (with-undo-fu-test-undo-in-region
    (with-undo-fu-test ""
      (with-undo-fu-test-other-buffer (buf buf-other)
        (simulate-input
          "aaa")
        (simulate-input
          " bbb")
        (simulate-input
          undo-fu-test-key-undo)
        (should (equal "aaa" (buffer-string)))

        (undo-fu-test--undo-in-region-in-other-buffer buf-other "1" " 2")
        (setq undo-fu-test--captured-messages nil)
        (simulate-input
          undo-fu-test-key-redo)
        (should (equal "aaa bbb" (buffer-string)))
        (should (member "Redo" undo-fu-test--captured-messages))
        (should-not (member "Redo in region" undo-fu-test--captured-messages))))))

(ert-deftest undo-fu-test-undo-after-redo-other-buffer-undo-in-region ()
  "Undo after redoing, following an undo in region in another buffer, not in region.

Undo twice, then undo in region in another buffer and switch back.
Redo one step, then undo: redo records `undo-in-region' for the next undo,
so it must clear the value left by the other buffer, otherwise undo runs in region."
  (with-undo-fu-test-undo-in-region
    (with-undo-fu-test ""
      (with-undo-fu-test-other-buffer (buf buf-other)
        (simulate-input
          "aaa")
        (simulate-input
          " bbb")
        (simulate-input
          " ccc")
        (simulate-input
          undo-fu-test-key-undo
          undo-fu-test-key-undo)
        (should (equal "aaa" (buffer-string)))

        (undo-fu-test--undo-in-region-in-other-buffer buf-other "1" " 2")
        (simulate-input
          undo-fu-test-key-redo)
        (should (equal "aaa bbb" (buffer-string)))
        (setq undo-fu-test--captured-messages nil)
        (simulate-input
          undo-fu-test-key-undo)
        (should (equal "aaa" (buffer-string)))
        (should (member "Undo" undo-fu-test--captured-messages))
        (should-not (member "Undo in region" undo-fu-test--captured-messages))))))

(ert-deftest undo-fu-test-unconstrained-redo-after-redo ()
  "Unconstrained redo after a constrained redo, continuing the redo.

Write 3 words, undo them, then redo 1 step.  Disable the checkpoint and redo:
the second word is redone instead of undoing the first.
Redo then continues the chain, after which undo reverses it."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo
      undo-fu-test-key-undo
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa" (buffer-string)))
    (simulate-input
      undo-fu-test-key-disable-checkpoint
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb ccc" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))))

(ert-deftest undo-fu-test-unconstrained-redo-after-redo-and-cursor-motion ()
  "Unconstrained redo after a constrained redo, with the cursor moved in between.

Write 3 words, undo them, then redo 1 step.  Disable the checkpoint,
move the cursor and redo: the second word is redone instead of undoing the first.

Verifies the last redo being constrained is tracked, not taken from the checkpoint,
which is already disabled by the time redo runs."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo
      undo-fu-test-key-undo
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa" (buffer-string)))
    (simulate-input
      undo-fu-test-key-disable-checkpoint)
    ;; Move the cursor, a command which doesn't change the buffer.
    (simulate-input
      [?\C-a])
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))))

(ert-deftest undo-fu-test-unconstrained-redo-after-redo-partial-failure ()
  "Unconstrained redo after a redo of multiple steps which fails part way.

Edit the end then the start of the buffer, undo both, then narrow the buffer
to exclude the start.  Redo 2 steps at once via C-u 2, the first step succeeds
and the second fails.  Widen, disable the checkpoint and redo:
the step which failed is redone, instead of undoing the step which succeeded."
  (with-undo-fu-test "hello"
    (goto-char (point-max))
    (simulate-input
      "X")
    (goto-char (point-min))
    (simulate-input
      "Y")
    (should (equal "YhelloX" (buffer-string)))
    (simulate-input
      undo-fu-test-key-undo)
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "hello" (buffer-string)))
    (narrow-to-region 2 (point-max))
    (execute-kbd-macro (vconcat [?\C-u ?2] undo-fu-test-key-redo))
    (widen)
    (should (equal "helloX" (buffer-string)))
    (simulate-input
      undo-fu-test-key-disable-checkpoint
      undo-fu-test-key-redo)
    (should (equal "YhelloX" (buffer-string)))))

(ert-deftest undo-fu-test-undo-zero-steps ()
  "Undo zero steps via C-u 0, undoing nothing.

Write 3 words, then undo 0 steps.  The buffer is unchanged and
redo must not treat the last edit as an undo step (undoing \" ccc\"
and removing it from the undo history)."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (setq undo-fu-test--captured-messages nil)
    (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-undo))
    (should (equal "aaa bbb ccc" (buffer-string)))
    (should-not undo-fu-test--captured-messages)
    (should-error-with-message
        (simulate-input
          undo-fu-test-key-redo)
      'user-error
      "Redo without undo step (C-g to ignore)")
    (should (equal "aaa bbb ccc" (buffer-string)))
    ;; Undo is unaffected.
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))))

(ert-deftest undo-fu-test-undo-zero-steps-between-unconstrained-redo ()
  "Undo zero steps via C-u 0 between unconstrained redo steps.

Write 3 words, undo them, then redo 2 steps unconstrained.  Undoing nothing
must not change the redo state, otherwise the next unconstrained redo
would undo the last redo instead of redoing \" ccc\"."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo
      undo-fu-test-key-undo
      undo-fu-test-key-undo)
    (should (equal "" (buffer-string)))
    (simulate-input
      undo-fu-test-key-disable-checkpoint
      undo-fu-test-key-redo
      undo-fu-test-key-redo)
    (should (equal "aaa bbb" (buffer-string)))
    (setq undo-fu-test--captured-messages nil)
    (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-undo))
    (should (equal "aaa bbb" (buffer-string)))
    (should-not undo-fu-test--captured-messages)
    (simulate-input
      undo-fu-test-key-redo)
    (should (equal "aaa bbb ccc" (buffer-string)))))

(ert-deftest undo-fu-test-undo-zero-steps-after-keyboard-quit ()
  "Undo zero steps via C-u 0 after `keyboard-quit', stepping over the checkpoint.

Write 2 words, undo the second, then write a third word and undo it.
Press `keyboard-quit' and undo nothing: the checkpoint is stepped over
as with any other count, so the following undo is unconstrained,
restoring the second word instead of skipping it."
  (with-undo-fu-test ""
    (simulate-input
      "aaa")
    (simulate-input
      " bbb")
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (simulate-input
      " ccc")
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa" (buffer-string)))
    (setq undo-fu-test--captured-messages nil)
    ;; A single key-macro so `last-command' is `keyboard-quit' when the undo runs.
    (execute-kbd-macro (vconcat undo-fu-test-key-keyboard-quit [?\C-u ?0] undo-fu-test-key-undo))
    (should (equal "aaa" (buffer-string)))
    (should (member "Undo checkpoint ignored!" undo-fu-test--captured-messages))
    (simulate-input
      undo-fu-test-key-undo)
    (should (equal "aaa bbb" (buffer-string)))))

(ert-deftest undo-fu-test-undo-redo-zero-steps-deactivate-region ()
  "Undo & redo zero steps via C-u 0 with an active region, deactivating it.

Undo & redo deactivate the region before running (unless `undo-fu-allow-undo-in-region'
is set), this must also apply when nothing is undone or redone."
  ;; Off in batch mode, needed for `region-active-p'.
  (let ((transient-mark-mode t))
    (with-undo-fu-test ""
      (simulate-input
        "aaa")
      (simulate-input
        " bbb")
      (simulate-input
        undo-fu-test-key-undo)
      (should (equal "aaa" (buffer-string)))
      (set-mark (point-min))
      (should (region-active-p))
      (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-undo))
      (should (equal "aaa" (buffer-string)))
      (should-not (region-active-p))
      (set-mark (point-min))
      (should (region-active-p))
      (execute-kbd-macro (vconcat [?\C-u ?0] undo-fu-test-key-redo))
      (should (equal "aaa" (buffer-string)))
      (should-not (region-active-p))
      ;; The undo step can still be redone.
      (simulate-input
        undo-fu-test-key-redo)
      (should (equal "aaa bbb" (buffer-string))))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; undo-fu_tests.el ends here
