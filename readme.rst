
#######
Undo Fu
#######

Simple, stable undo with redo for emacs.

Unlike alternatives, this provides redo functionality with emacs own undo,
without loosing access to the full undo history emacs supports
allowing you to visit all previous states of the document.

The changes compared emacs undo are as follows:

- Redo will not redo past the initial undo action.
- Redo will not undo if the last action in the undo stack is not an undo.

- These constraints can be temporarily disabled by pressing C-g before undo or redo.

Note that this doesn't interfere with Emacs internal undo data,
which can be error prone.


Motivation
==========

The default emacs undo has two limitations this package aims to resolve,

- Two actions are required to initiate redo.
- It's easy to accidentally redo past the point where undo started
  making it inconvenient to restore the document to the point when undo began.


Usage
=====

This package exposes two functions:

- ``undo-fu-only-undo``
- ``undo-fu-only-redo``

You will need to make these to keys yourself.

Key binding example:

.. code-block:: elisp

   (global-unset-key (kbd "C-z"))
   (global-set-key (kbd "C-z")   'undo-fu-only-undo)
   (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

Assuming you have these key bindings set, you can do the following.

- Holding ``Ctrl-Z`` undoes all available actions.
- Holding ``Ctrl-Shift-Z`` redoes all actions until the first undo performed after an edit.
- Redoing beyond this point is prevented, as you might expect since this is how undo/redo normally works,
  this means you can conveniently hold the redo key to reach the newest state of the document.

  If you want to keep redoing past this point
  you're prompted to press ``Ctrl-G`` (``keyboard-quit``),
  then you can continue to redo using emacs default behavior
  until a new chain of undo/redo events is started.
