# save-buffer-state
An Emacs Lisp special form like `save-excursion`

From Section 3.10 of the Emacs manual:

In Emacs Lisp programs used for editing, the save-excursion function is very common.
It saves the location of point, executes the body of the function, and then restores
point to its previous position if its location was changed. Its primary purpose is
to keep the user from being surprised and disturbed by unexpected movement of point.
