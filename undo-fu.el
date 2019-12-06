;;; undo-fu.el --- Undo helper with redo -*- lexical-binding: t -*-

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-undo-fu
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Undo/redo convenience wrappers to Emacs default undo commands.
;;
;; The redo operation can be accessed from a key binding
;; and stops redoing once the initial undo action is reached.
;;
;; If you want to cross the initial undo step to access
;; the full history, pressing C-g (keyboard-quit)
;; lets you continue redoing for functionality not typically
;; accessible with regular undo/redo.
;;

;;; Usage

;; ;; Bind the keys
;; (global-unset-key (kbd "C-z"))
;; (global-set-key (kbd "C-z")   'undo-fu-only-undo)
;; (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;;; Code:

;; Internal variables.

;; First undo step in the chain, don't redo past this.
(defvar-local undo-fu--checkpoint nil)
;; We have reached the checkpoint, don't redo.
(defvar-local undo-fu--checkpoint-is-blocking nil)
;; Apply undo/redo constraints to stop redo from undoing or
;; passing the initial undo checkpoint.
(defvar-local undo-fu--respect t)


(defun undo-fu--next-step (list)
  "Get the next undo step in the list.

Argument LIST compatible list `buffer-undo-list'."
  (while (car list)
    (setq list (cdr list)))
  (while (and list (null (car list)))
    (setq list (cdr list)))
  list)


(defun undo-fu-only-redo ()
  "Redo an action until the initial undo action.

wraps the `undo' function."
  (interactive)

  (let*
    ( ;; Assign for convenience.
      (was-undo (not (null (member last-command '(undo undo-fu-only-undo)))))
      (was-redo (not (null (member last-command '(undo-fu-only-redo)))))
      (was-undo-or-redo (or was-undo was-redo)))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (setq undo-fu--respect t)))

    ;; Allow crossing the boundary, if we press G-g
    ;; to allow explicitly over-stepping the boundary, in cases where it's needed.
    (when undo-fu--respect
      (when (string-equal last-command 'keyboard-quit)
        (setq undo-fu--respect nil)
        (setq undo-fu--checkpoint-is-blocking nil)
        (message "Redo end-point stepped over!")))

    (when undo-fu--respect
      (unless
        ;; Ensure the next steps is a redo action.
        (let ((list (undo-fu--next-step buffer-undo-list)))
          (and list (gethash list undo-equiv-table)))
        (user-error "Redo step not found (C-g to ignore)")))

    (when undo-fu--checkpoint-is-blocking
      (user-error "Redo end-point hit (C-g to step over it)"))

    (let*
      (
        (last-command
          (cond
            (was-undo
              ;; Break undo chain, avoid having to press C-g.
              'ignore)
            (was-redo
              ;; Checked by the undo function.
              'undo)
            (t
              ;; No change.
              last-command)))
        (success
          (condition-case err
            (progn
              (undo)
              t)
            (user-error (message "%s" (error-message-string err))))))
      (when success
        (when undo-fu--respect
          (when (eq (gethash buffer-undo-list undo-equiv-table) undo-fu--checkpoint)
            (setq undo-fu--checkpoint-is-blocking t))))))

  (setq this-command 'undo-fu-only-redo))


(defun undo-fu-only-undo ()
  "Undo the last action.

wraps the `undo-only' function."
  (interactive)

  (let*
    ( ;; Assign for convenience.
      (was-undo (not (null (member last-command '(undo undo-fu-only-undo)))))
      (was-redo (not (null (member last-command '(undo-fu-only-redo)))))
      (was-undo-or-redo (or was-undo was-redo)))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (setq undo-fu--respect t)))

    (when (or undo-fu--checkpoint-is-blocking (not was-undo-or-redo))
      (setq undo-fu--checkpoint (cdr buffer-undo-list)))

    (let*
      ;; Swap in 'undo' for our own function name.
      ;; Without this undo won't stop once the first undo step is reached.
      (
        (last-command
          (cond
            (was-undo-or-redo
              ;; Checked by the undo function.
              'undo)
            (t
              ;; No change.
              last-command)))
        (success
          (condition-case err
            (progn
              (undo-only)
              t)
            (user-error (message "%s" (error-message-string err)))
            (error (message "%s" (error-message-string err))))))
      (when success
        (when undo-fu--respect
          (setq undo-fu--checkpoint-is-blocking nil)))))
  (setq this-command 'undo-fu-only-undo))

(provide 'undo-fu)

;;; undo-fu.el ends here
