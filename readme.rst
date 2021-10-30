#######
Undo Fu
#######

Simple, stable linear undo with redo for Emacs.

This is a light weight wrapper for Emacs built-in undo system,
adding convenient undo/redo without losing access to the full undo history,
allowing you to visit all previous states of the document if you need.

The changes compared Emacs undo are as follows:

- Redo will not pass the initial undo action.
- Redo will not undo *(unlike Emacs redo which traverses previous undo/redo steps)*.

- These constraints can be temporarily disabled by pressing C-g before undo or redo.

Note that this doesn't interfere with Emacs internal undo data,
which can be error prone.

Available via `melpa <https://melpa.org/#/undo-fu>`__.


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
- ``undo-fu-disable-checkpoint`` (only needed when ``undo-fu-ignore-keyboard-quit`` is in use).


Key Bindings
------------

You will need to map these to keys yourself.

Key binding example for evil-mode:

.. code-block:: elisp

   (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
   (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

Key binding example for typical ``Ctrl-Z``, ``Ctrl-Shift-Z`` keys:

.. code-block:: elisp

   (global-unset-key (kbd "C-z"))
   (global-set-key (kbd "C-z")   'undo-fu-only-undo)
   (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)


Customization
-------------

``undo-fu-allow-undo-in-region`` (``nil``)
   This option exists for users who prefer to trade-off undo/redo functionality
   with the ability to limit undo to a region.

   When non-nil, undoing with a selection will use undo within this region.
``undo-fu-ignore-keyboard-quit`` (``nil``)
   Don't use ``Ctrl-G`` (``keyboard-quit``) for non-linear behavior,
   instead, use the ``undo-fu-disable-checkpoint`` command.

   This was added for users who prefer to explicitly activate this behavior.
   As ``keyboard-quit`` may be used for other reasons.


Details
=======

- Holding the undo-key undoes all available actions.
- Holding the redo-key redoes all actions until the first undo performed after an edit.
- Redoing beyond this point is prevented, as you might expect since this is how undo/redo normally works,
  this means you can conveniently hold the redo key to reach the newest state of the document.

  If you want to keep redoing past this point
  you're prompted to press ``Ctrl-G`` (``keyboard-quit``),
  then you can continue to redo using Emacs default behavior
  until a new chain of undo/redo events is started.


Limitations
===========

The feature ``undo-in-region`` is disabled by default.


Installation
============

The package is `available in melpa <https://melpa.org/#/undo-fu>`__ as ``undo-fu``.

.. code-block:: elisp

   (use-package undo-fu)

Combined with key bindings:

.. code-block:: elisp

   (use-package undo-fu
     :config
     (global-unset-key (kbd "C-z"))
     (global-set-key (kbd "C-z")   'undo-fu-only-undo)
     (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


Evil Mode
---------

Evil mode can be configured to use ``undo-fu`` by default.

.. code-block:: elisp

   (use-package evil
     :init
     (setq evil-undo-system 'undo-fu))


Other Packages
==============

As there are multiple packages which deal with undo, it's worth mentioning how this interacts with other packages.

`Undo Fu Session <https://gitlab.com/ideasman42/emacs-undo-fu-session>`__
   This package is intended for use with undo-fu,
   as a way to save and restore undo sessions, even after restarting Emacs.

`Undohist <https://github.com/emacsorphanage/undohist>`__
   This packages stores undo data between sessions,
   while it is compatible with undo-fu on a basic level, it doesn't store redo information
   (``undo-fu-session`` is an improved alternative).

`Undo Tree <https://www.emacswiki.org/emacs/UndoTree>`__
   This handles undo steps as a tree by re-implementing parts of Emacs undo internals.

   Undo-Fu was written to be a simpler alternative
   as Undo Tree had long standing unresolved bugs at the time of writing.
