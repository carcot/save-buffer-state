(defmacro save-buffer-state (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  `(let (buffer-undo-list   ;  maintain original buffer-undo-list
         (indirect-buffer (clone-indirect-buffer nil nil)))
     (unwind-protect
         (progn ,@body)
       (primitive-undo 1 buffer-undo-list)
       (kill-buffer indirect-buffer))))

(defmacro save-buffer-state-2 (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let (point
           (indirect-buffer (clone-indirect-buffer nil nil)))
       (unwind-protect
           (with-current-buffer indirect-buffer
             (catch 'done
               (atomic-change-group
                 (let ((,return-value (progn ,@body)))
                   (throw 'done ,return-value)))))
         (kill-buffer indirect-buffer)))))

(defmacro with-cloned-indirect-buffer (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let (buffer-undo-list   ;  maintain original buffer-undo-list
           (indirect-buffer (clone-indirect-buffer nil nil)))
       (let ((,return-value (progn ,@body)))
         (primitive-undo 1 buffer-undo-list)
         (kill-buffer indirect-buffer)
         ,return-value))))

(setq variable-symbol-list 1)
'(macroexpand-1 '(save-buffer-state* (insert "A")))

'(macroexpand-1
  '(save-buffer-state*
    (insert "A")
    (left-char 1)
    (looking-at-p "A")))

'(save-buffer-state*
  (insert "A")
  (left-char 1)
  (looking-at-p "A"))

(defun test (arg)
  "TODO: documentation"
  (interactive)
  (let (arg)
    ;; (setq arg "setq")
    (msg arg))
  (msg arg))

'(test "abc")

'(setq test "abc")

'(defun test2 ()
   "TODO: documentation"
   (interactive)
   (let ((test test))
     (test test)))

'(test test)
'(test2)

(defmacro protect-buffer-state (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let* ((protected-variable-symbol-list      (get-protected-variable-symbol-list body))
         (protected-let-symbol-valueform-list (make-let-symbol-valueform-list protected-variable-symbol-list))
         (body                                (filter-protect-form-from-body body))
         (indirect-buffer                     (cl-gensym "indirect-buffer")))
    `(let ,protected-let-symbol-valueform-list
       (let (buffer-undo-list    ;  maintain original buffer-undo-list
             (,indirect-buffer (clone-indirect-buffer nil nil)))
         (unwind-protect
             (progn ,@body)
           (primitive-undo 1 buffer-undo-list)
           (kill-buffer ,indirect-buffer))))))

'(macroexpand-1
  '(protect-buffer-state
     (protect kill-whole-line)
     (setq kill-whole-line nil)
     (kill-line)))

'(protect-buffer-state
   (protect kill-whole-line)
   (set kill-whole-line nil)
   (kill-line))

'(macroexpand-1
  '(protect-buffer-state
     (protect x y z)
     (insert "A")
     (left-char 1)
     (looking-at-p "A")))

'(progn (setq x 3)
        (save-buffer-state**
         (protect x y z)
         (setq x 4)
         (insert "A")
         (left-char 1)
         (looking-at-p "A")
         (msg- x))
        (msg- x))

;; (gensym "indirect-buffer")

(defun get-protected-variable-symbol-list (body)
  "TODO: documentation"
  (interactive)
  (if (eq (caar body) 'protect)
      (cdar body)))

(defun filter-protect-form-from-body (body)
  "TODO: documentation"
  (interactive)
  (if (eq (caar body) 'protect)
      (cdr body)
    body))

(defun make-let-symbol-valueform (symbol)
  "TODO: documentation"
  (interactive)
  (list symbol (list 'if (list 'boundp (list 'quote symbol))
                     symbol
                     nil)))

(defun make-let-symbol-valueform-list (symbol-list)
  "TODO: documentation"
  (interactive)
  (mapcar #'make-let-symbol-valueform symbol-list))

'(with-cloned-indirect-buffer
   (insert "A")
   (left-char 1)
   (looking-at-p "A"))

'(with-cloned-indirect-buffer
   (insert "A")
   (left-char 1)
   (looking-at-p "B"))

'(with-cloned-indirect-buffer
   (let ((kill-whole-line nil))
     (kill-line)
     (eolp)))

'(with-cloned-indirect-buffer
   (let ((kill-whole-line t))
     (kill-line)
     (eolp)))

'(let ((buffer-file-name nil))
   (clone-buffer nil t))

(defmacro with-cloned-buffer (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  (let ((return-value (make-symbol "return-value")))
    `(let ((buffer-file-name nil))
       (clone-buffer nil t)
       (let ((,return-value (progn ,@body)))
         (kill-buffer-and-window)
         ,return-value))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (insert "A")
                                 (left-char 1)
                                 (looking-at-p "A"))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (insert "A")
                                 (left-char 1)
                                 (looking-at-p "B"))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (with-cloned-buffer
                                      (let ((kill-whole-line nil))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (with-cloned-buffer
                                      (let ((kill-whole-line t))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 nil)))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (with-cloned-buffer
                                 (msg (setq god-local-mode (not god-local-mode))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (save-buffer-state
                                 (msg (setq god-local-mode (not god-local-mode))))))

'(macroexpand-1 '(save-buffer-state nil))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (save-buffer-state
                                 (msg (setq god-local-mode (not god-local-mode))))))

(global-set-key (kbd "M-1") (lambda ()
                              (interactive)
                              (msg (save-buffer-state-2
                                     (insert "A")
                                     (left-char 1)
                                     (looking-at-p "A")))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state
                                      (insert "A")
                                      (left-char 1)
                                      (looking-at-p "B")))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state-2
                                      (let ((kill-whole-line nil))
                                        (kill-line)
                                        (eolp))))))

'(global-set-key (kbd "M-1") (lambda ()
                               (interactive)
                               (msg (save-buffer-state-2
                                      (let ((kill-whole-line t))
                                        (kill-line)
                                        (eolp))))))

'(loop for x being the symbols if (boundp x) collect (symbol-name x))

;; (defun bound-symbol-list ()
;;   "TODO: documentation"
;;   (interactive)
;;   (loop for x being the symbols if (boundp x) collect x))

(defun variable-symbol-list ()
  "TODO: documentation"
  (interactive)
  (loop for x being the symbols
        if (and (boundp x) (not (keywordp x)) (not (eq x nil)))
        collect x))

(defun function-symbol-list ()
  "TODO: documentation"
  (interactive)
  (loop for x being the symbols
        if (fboundp x)
        collect x))

'(benchmark-run (variable-symbol-list))
'(benchmark-run (function-symbol-list))

'(clone-indirect-buffer nil t)
'(setq kill-whole-line nil)
'(setq kill-whole-line t)
