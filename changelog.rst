
##########
Change Log
##########

- Version 0.5 (in development)
  - Protect against ``aggressive-indent-mode``.

- Version 0.4 (2020-05-22)

  - Back-port ``undo-redo`` from Emacs-28,
    replace the redo logic with this function.
  - Fix continual redo in unconstrained mode trapping the user in a state
    where neither undo or redo can be performed.
  - Undo in *unconstrained* mode no longer uses ``undo-only``,
    matching redo behavior.
  - Raise an error when using undo commands when undo has been disabled for the buffer.
    *(was failing to set the checkpoint in this case).*

- Version 0.3 (2020-03-03)

  - Support non-destructive commands between undo/redo actions without breaking the chain.
    Internally ``last-command`` is no longer used to detect changes.
  - Add ``undo-fu-ignore-keyboard-quit`` option for explicit non-linear behavior.
  - Support using ``undo-fu-only-redo`` after regular ``undo`` / ``undo-only``.

- Version 0.2 (2020-01-12)

  - Linear redo support (which wont undo).
  - Evil-Mode attribute not to repeat these undo/redo actions.
  - Fix counting bug with ``undo-fu-only-redo-all``.
  - Add ``undo-fu-allow-undo-in-region`` option.

- Version 0.1 (2019-12-14)

  Initial release.
