#######
Undo Fu
#######

Simple, stable linear undo with redo for Emacs.

Unlike alternatives, this provides redo functionality with Emacs own undo,
without losing access to the full undo history Emacs supports
allowing you to visit all previous states of the document.

The changes compared Emacs undo are as follows:

- Redo will not redo past the initial undo action.
- Redo will not undo if the last action in the undo stack is not an undo.

- These constraints can be temporarily disabled by pressing C-g before undo or redo.

Note that this doesn't interfere with Emacs internal undo data,
which can be error prone.


Motivation
==========

The default Emacs undo has two limitations this package aims to resolve,

- Two actions are required to initiate redo.
- It's easy to accidentally redo past the point where undo started
  making it inconvenient to restore the document to the point when undo began.


Usage
=====

This package exposes the following functions:

- ``undo-fu-only-undo``
- ``undo-fu-only-redo``
- ``undo-fu-only-redo-all``


Key Bindings
------------

You will need to make these to keys yourself.

Key binding example for evil-mode:

.. code-block:: elisp

   (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
   (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

Key binding example for typical ``Ctrl-Z``, ``Ctrl-Shift-Z`` keys:

.. code-block:: elisp

   (global-unset-key (kbd "C-z"))
   (global-set-key (kbd "C-z")   'undo-fu-only-undo)
   (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)


Details
-------

- Holding the undo-key undoes all available actions.
- Holding the redo-key redoes all actions until the first undo performed after an edit.
- Redoing beyond this point is prevented, as you might expect since this is how undo/redo normally works,
  this means you can conveniently hold the redo key to reach the newest state of the document.

  If you want to keep redoing past this point
  you're prompted to press ``Ctrl-G`` (``keyboard-quit``),
  then you can continue to redo using Emacs default behavior
  until a new chain of undo/redo events is started.


Customization
-------------

``undo-fu-allow-undo-in-region``
   This option exists for users who prefer to trade-off undo/redo functionality
   with the ability to limit undo to a region.

   When this boolean is ``t``, undoing with a selection
   will use undo within this region.


Limitations
===========

The feature ``undo-in-region`` is disabled by default.


Installation
============

The package is available in melpa as ``undo-fu``.

.. code-block:: elisp

   (use-package undo-fu)

Combined with key bindings, for evil-mode:

.. code-block:: elisp

   (use-package undo-fu
     :config
     (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
     (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))
