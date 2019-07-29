# save-buffer-state
An Emacs Lisp special form like `save-excursion`

From Section 3.10 of the Emacs manual:

> In Emacs Lisp programs used for editing, the `save-excursion` function is very common.
> It saves the location of point, executes the body of the function, and then restores
> point to its previous position if its location was changed. Its primary purpose is
> to keep the user from being surprised and disturbed by unexpected movement of point.

In other words, the `save-excursion` function returns the position of point to its initial
position after a command performs some editing function so as not to confuse and inconvenience
the user.

So what's the point of a `save-buffer-state` function? To perform some editing function and
the completely undo it?

Well, there are other use cases for the `save-excursion` function.