;; (defun save-editing-state (&rest commands)
;;   "TODO: documentation"
;;   (interactive)
;;   (save-mark-and-excursion
;;    (save-restriction
;;      (save-match-data
;;        (let ((buffer-undo-list nil)
;;              kill-ring
;;              kill-ring-yank-pointer
;;              (result (eval commands)))
;;          (undo)
;;          result)))))

;; (defmacro with-cloned-buffer-0 (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list
;;          (message-log-max nil)
;;          (inhibit-message t)
;;          ;; (temp-buffer (clone-indirect-buffer nil nil))
;;          (temp-buffer (clone-indirect-buffer nil t)))
;;      ;; (with-current-buffer temp-buffer
;;      ;;   ;; (add-to-list 'warning-suppress-types '(undo))
;;      ;;   (let ((buffer-undo-list nil)
;;      ;;         (message-log-max nil)
;;      ;;         (inhibit-message t))
;;      ;;     ,@body
;;      ;;     (undo)))
;;      (let ((buffer-undo-list nil)
;;            (message-log-max nil)
;;            (inhibit-message t))
;;        ,@body
;;        ;; (undo)
;;        (primitive-undo 1 buffer-undo-list))
;;      ;; (kill-buffer temp-buffer)
;;      (kill-buffer-and-window)))

;; (defmacro with-cloned-buffer-1 (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list
;;          (message-log-max nil)
;;          (inhibit-message t)
;;          (temp-buffer (clone-indirect-buffer nil t)))
;;      (let ((buffer-undo-list nil)
;;            (message-log-max nil)
;;            (inhibit-message t)
;;            z)
;;        (setq z (progn ,@body))
;;        (primitive-undo 1 buffer-undo-list)
;;        (kill-buffer-and-window)
;;        z)))

;; (defmacro with-cloned-buffer (&rest body)
;;   "Executes BODY just like `progn'."
;;   `(let (buffer-undo-list)
;;      (clone-indirect-buffer nil t)
;;      (let ((buffer-undo-list nil)
;;            z)
;;        (setq z (progn ,@body))
;;        (primitive-undo 1 buffer-undo-list)
;;        (kill-buffer-and-window)
;;        z)))

;; (defmacro with-cloned-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list)   ;  maintain original buffer-undo-list
;;        (clone-indirect-buffer nil t)
;;        (let (,return-value)
;;          (setq ,return-value (progn ,@body))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer-and-window)
;;          ,return-value))))

;; (defmacro with-cloned-indirect-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list)   ;  maintain original buffer-undo-list
;;        (clone-indirect-buffer nil t)
;;        (let ((,return-value (progn ,@body)))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer-and-window)
;;          ,return-value))))

;; (defmacro with-cloned-indirect-buffer (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let (buffer-undo-list      ;  maintain original buffer-undo-list
;;            (indirect-buffer (clone-indirect-buffer nil nil)))
;;        (let ((,return-value (progn ,@body)))
;;          (primitive-undo 1 buffer-undo-list)
;;          (kill-buffer indirect-buffer)
;;          ,return-value))))

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

(defmacro save-buffer-state (&rest body)
  "Executes BODY just like `progn' but maintains original buffer state."
  (declare (indent 0))
  `(let (buffer-undo-list   ;  maintain original buffer-undo-list
         (indirect-buffer (clone-indirect-buffer nil nil)))
     (unwind-protect
         (progn ,@body)
       (primitive-undo 1 buffer-undo-list)
       (kill-buffer indirect-buffer))))

'(defmacro save-buffer-state* (&rest body)
   "Executes BODY just like `progn' but maintains original buffer state."
   (declare (indent 0))
   (let ((variable-symbol-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) (variable-symbol-list)))))
     `(let ,variable-symbol-list
        (let (buffer-undo-list   ;  maintain original buffer-undo-list
              (indirect-buffer (clone-indirect-buffer nil nil)))
          (unwind-protect
              (progn ,@body)
            (primitive-undo 1 buffer-undo-list)
            (kill-buffer indirect-buffer))))))

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

;; '(defmacro save-buffer-state** (&rest body)
;;    "Executes BODY just like `progn' but maintains original buffer state."
;;    (declare (indent 0))
;;    (let ((variable-symbol-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) (variable-symbol-list))))
;;          (function-symbol-list (mapcar (lambda (x) (list 'symbol-function x)) (function-symbol-list))))
;;      `(let ,variable-symbol-list
;;         (letf ,function-symbol-list
;;           (let (buffer-undo-list ;  maintain original buffer-undo-list
;;                 (indirect-buffer (clone-indirect-buffer nil nil)))
;;             (unwind-protect
;;                 (progn ,@body)
;;               (primitive-undo 1 buffer-undo-list)
;;               (kill-buffer indirect-buffer)))))))

;; (defmacro save-buffer-state** (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((protected-variable-symbol-pairs-list (mapcar (lambda (x) (list x x)) (seq-remove (lambda (x) (or (equal x 'body) (equal x 'x))) '(protected-variable-symbol-list))))
;;         `(let ,protected-variable-symbol-pairs-list
;;            (let (buffer-undo-list    ;  maintain original buffer-undo-list
;;                  (indirect-buffer (clone-indirect-buffer nil nil)))
;;              (unwind-protect
;;                  (progn ,@body)
;;                (primitive-undo 1 buffer-undo-list)
;;                (kill-buffer indirect-buffer)))))))

;; (setq body '((protect a b c) (insert "A") (left-char 1) (looking-at-p "A")))
;; (if (eq (caar body) 'protect)
;;     (cdar body))

;; (defmacro save-buffer-state** (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let* ((protected-variable-symbol-list (if (eq (caar body) 'protect)
;;                                              (cdar body)))
;;          (body (if (eq (caar body) 'protect)
;;                    (cdr body)
;;                  body))
;;          (protected-let-symbol-valueform-list (mapcar #'make-let-symbol-valueform protected-variable-symbol-list)))
;;     `(let ,protected-let-symbol-valueform-list
;;        (let (buffer-undo-list    ;  maintain original buffer-undo-list
;;              (indirect-buffer (clone-indirect-buffer nil nil)))
;;          (unwind-protect
;;              (progn ,@body)
;;            (primitive-undo 1 buffer-undo-list)
;;            (kill-buffer indirect-buffer))))))
;;
;; (macroexpand-1
;;  '(save-buffer-state**
;;     (protect x y z)
;;     (insert "A")
;;     (left-char 1)
;;     (looking-at-p "A")))
;;
;; (progn (setq x 3)
;;        (save-buffer-state**
;;          (protect x y z)
;;          (setq x 4)
;;          (insert "A")
;;          (left-char 1)
;;          (looking-at-p "A")
;;          (msg- x))
;;        (msg- x))

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

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(catch 'done
;;        (atomic-change-group
;;          ;; (clone-indirect-buffer nil t)
;;          (let ((,return-value (progn ,@body)))
;;            ;; (kill-buffer-and-window)
;;            (throw 'done ,return-value))))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (progn
;;                             (clone-indirect-buffer nil t)
;;                             (catch 'done
;;                               (atomic-change-group
;;                                 (let ((,return-value (progn ,@body)))
;;                                   (throw 'done ,return-value)))))))
;;        (kill-buffer-and-window)
;;        ,return-value)))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (unwind-protect
;;                               (progn
;;                                 (clone-indirect-buffer nil t)
;;                                 (catch 'done
;;                                   (atomic-change-group
;;                                     (let ((,return-value (progn ,@body)))
;;                                       (throw 'done ,return-value)))))
;;                             (kill-buffer-and-window))))
;;        (msg ,return-value))))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((return-value (make-symbol "return-value")))
;;     `(let ((,return-value (unwind-protect
;;                               (catch 'done
;;                                 (clone-indirect-buffer nil t)
;;                                 (atomic-change-group
;;                                   (throw 'done (progn ,@body))))
;;                             (kill-buffer-and-window))))
;;        ,return-value)))

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   `(unwind-protect
;;        (catch 'done
;;          (clone-indirect-buffer nil t)
;;          (atomic-change-group
;;            (throw 'done (progn ,@body))))
;;      (kill-buffer-and-window)))

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

;; (defmacro save-buffer-state (&rest body)
;;   "Executes BODY just like `progn' but maintains original buffer state."
;;   (declare (indent 0))
;;   (let ((variable-symbol-list '(variable-symbol-list))
;;         (function-symbol-list '(function-symbol-list)))
;;     `(let ,variable-symbol-list
;;        (letf ,function-symbol-list
;;          (unwind-protect
;;              (catch 'done
;;                (clone-indirect-buffer nil t)
;;                (atomic-change-group
;;                  (throw 'done (progn ,@body))))
;;            (kill-buffer-and-window))))))

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

;; (defmacro with-indirect-buffer-in-foo-mode (beg end &rest body)
;;   `(with-current-buffer (clone-indirect-buffer nil nil)
;;      (narrow-to-region beg end)
;;      (foo-mode)
;;      (unwind-protect
;;          ,body
;;        (kill-buffer (current-buffer)))))

;; (defmacro with-indirect-buffer (&rest body)
;;   `(with-current-buffer (clone-indirect-buffer nil nil)
;;      ;; (narrow-to-region beg end)
;;      ;; (foo-mode)
;;      (unwind-protect
;;          @,body
;;        (kill-buffer (current-buffer)))))

;; (with-indirect-buffer
;;  (let ((kill-whole-line t))
;;    (kill-line)
;;    (eolp)))

'(clone-indirect-buffer nil t)
'(setq kill-whole-line nil)
'(setq kill-whole-line t)

;; (with-cloned-buffer
;;   (let ((kill-whole-line t))
;;     (kill-line)
;;     (eolp)))

;; (with-cloned-buffer
;;   (insert "A")
;;   (left-char 1)
;;   (looking-at-p "A"))

;; (with-cloned-buffer
;;   (insert "A")
;;   (left-char 1)
;;   (looking-at-p "B"))

;; (with-cloned-buffer
;;   (insert "A")
;;   (insert "B")
;;   (left-char 2)
;;   (looking-at-p "B"))

;; (let ((temp-buffer (clone-indirect-buffer nil nil)))
;;   (with-current-buffer temp-buffer
;;     (insert "A")))
