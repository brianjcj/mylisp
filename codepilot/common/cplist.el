(eval-when-compile
  (require 'cl)
  )

(require 'cphistory)
(require 'cp-layout)
(require 'cp-base)
(require 'cpfilter)

(defvar cplist-line-number-saved 0)
(defvar cplist-text-selected "")


(defvar cplist-mode-syntax-table nil
  "Syntax table in use in CPXREF buffers.")

(unless cplist-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?. "." table)
    (setq cplist-mode-syntax-table table)))


(defvar cplist-mode-map nil
  "keymap for cplist mode")

(unless cplist-mode-map
  (let ((map (make-sparse-keymap)))
    (setq cplist-mode-map map)))

(defun cplist-mode ()
  ""
  (interactive)
  (if (eq major-mode 'cplist-mode)
      ()
    (kill-all-local-variables)
    (setq major-mode 'cplist-mode)
    (setq mode-name "CPList")
    (use-local-map cplist-mode-map)
    (set-syntax-table cplist-mode-syntax-table)
    (setq case-fold-search t)
    (cplist-font-lock-setup)
    (setq buffer-read-only t)
    (run-mode-hooks 'cplist-mode-hook)))


(defun cplist-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cplist-font-lock-keywords t))
  )

(defface cplist-item-face
  '((default (:inherit font-lock-type-face)))
  "To highlight cplist, cptree, msym buffer."
  :group 'codepilot)

(defface cplist-head-face
  '((default (:inherit font-lock-warning-face)))
  "To highlight cplist, cptree, msym buffer."
  :group 'codepilot)


(defvar cplist-font-lock-keywords
  (list

   ;; (list "^[a-zA-Z ] \\(.+\\)$"
   ;;       '(1 'cplist-item-face)
   ;;       )

   (list "^ +\\(.+\\)$"
         '(1 'cplist-item-face)
         )
   (list "ก่ \\(.+\\)$"
         '(1 'cplist-head-face)
         )
   )
  "font-lock keywords setting for cpxref buffers.")


(defvar cplist-action-func nil)

(defun cplist-action ()
  "will be overrided by various impls."
  (interactive)
  (when cplist-action-func
    (funcall cplist-action-func)
    )
  )

(defun cplist-mouse-click (event)
  (interactive "e")

  (multiple-value-bind (ret sidebar code-win bottom-win)
      (codepilot-window-layout-wise)
    (case ret
      ((:window-layout-1))
      ((:window-layout-1&1
        :window-layout-1&2+)
       (select-window code-win))))

  (codepilot-push-marker)

  (mouse-set-point event)
  (cplist-action)
  )

(defun cplist-enter ()
  (interactive)

  (let ((win (selected-window)))
    (other-window 1)
    (codepilot-push-marker)

    ;(set-buffer cplist-buf-name)
    (select-window win)
    (cplist-action)
    ))

(defun cplist-mark-for-delete ()
  ""
  (interactive)
  (let ((case-fold-search t)
        (inhibit-read-only t)
        (buffer-unqdo-list t))
    (save-match-data
      (forward-line 0)
      (when (looking-at "^ <\\(.*\\)>")
        (forward-line))
      (when (looking-at "[A-Za-Z ] ")
        (replace-match "D " nil t)
        (forward-line)
        ))
    (set-buffer-modified-p nil)
    ))

(defun cplist-unmark ()
  ""
  (interactive)
  (let ((case-fold-search t)
        (inhibit-read-only t)
        (buffer-unqdo-list t))
    (save-match-data
      (forward-line 0)
      (when (looking-at "^ <\\(.*\\)>")
        (forward-line))
      (when (looking-at "[A-Za-Z] ")
        (replace-match "  " nil t)
        ;(forward-line)
        )
      (forward-line)
      )
    (set-buffer-modified-p nil)
    ))

(defun cplist-kill-del-mark-lines (b e &optional sec-list)
  (let (n)
    (setq n (if sec-list 0 1))
    (save-excursion
      (save-match-data
        (with-modify-in-readonly
            (goto-char b)
          (while (re-search-forward "^D \\(.+\\)$" nil t)
            (delete-region (line-beginning-position n) (line-beginning-position 2))
            ))))))


;; (defcustom cplist-side-window-default-size 38
;;   "*Size of cplist side window."
;;   :type 'integer
;;   :group 'codepilot)

(defvar cplist-side-window-size 38)
;(setq cplist-side-window-size cplist-side-window-default-size)

(defvar cplist-query-sort-type 'create)
(defvar cplist-section-sort-type 'last)

(defun cplist-del-dedicated-win ()
  (dolist (w (window-list))
    (when (window-dedicated-p w)
      (condition-case nil
          (delete-window w)
        (error
         (set-window-dedicated-p w nil))))))

(defun cplist-side-window ()
  ""
  (interactive)
  (let (win edges width)
    (if (setq win (cplist-get-list-win))
        ;; (delete-window win)
        (progn
          (setq edges (window-edges win))
          (setq width (- (nth 2 edges) (nth 0 edges)))

          (unless (= width window-min-width)
            (cplist-save-current-list-win-size))

          (run-hooks 'cplist-win-del)
          (kill-buffer cplist-buf-name)
          )

      (cplist-del-dedicated-win)

      (unless (one-window-p :nomin)
        (delete-other-windows))

      (condition-case nil
          (split-window nil cplist-side-window-size t)
        (error
         (split-window-horizontally)
         )
        )
      (other-window 1)
      (cplist-list-id-section-bufs)
      (run-hooks 'cplist-win-added)
      )))

(defun cplist-get-list-win ()
  ""
  (get-buffer-window cplist-buf-name))


(defun cplist-list-id-section-bufs ()
  ""
  (interactive)
  (let ((buf (get-buffer-create cplist-buf-name))
        (win (selected-window))
        id)
    ;; (save-window-excursion
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; Don't generate undo entries for creation of the initial contents.
            (buffer-undo-list t))

        ;;(setq buffer-read-only nil)
        (erase-buffer)

        ;; Call the hooks to fill the contents.
        (run-hooks 'cplist-fill-contents-hook)
        
        (set-buffer-modified-p nil)
        ))
    
    (multiple-value-bind (ret sidebar code-win bottom-win)
        (codepilot-window-layout-wise)
      (case ret
        ((:window-layout-1&1
          :window-layout-1&2+
          :window-layout-no-dedicated
          )
         (select-window sidebar)
         (switch-to-buffer buf)
         (setq buffer-read-only t)
         (goto-char (point-min))
         (cplist-mode) ;; brian: delete it later.
         (cpfilter-add-edit-entry-field)
         (run-hooks 'cplist-turn-on-mode-hook)
         (set-window-dedicated-p (selected-window) t)
         )))

    ))


(define-key cplist-mode-map [mouse-3] 'cplist-mouse-click)
(define-key cplist-mode-map "g" 'cplist-list-id-section-bufs)

(define-key cplist-mode-map "d" 'cplist-mark-for-delete)
(define-key cplist-mode-map "u" 'cplist-unmark)
(define-key cplist-mode-map "x" 'cplist-do-kill-on-deletion-marks)

;; (define-key cplist-mode-map "s" 'cplist-sort-query-by-create)

(define-key cplist-mode-map "\r" 'cplist-enter)

(define-key cplist-mode-map "8" 'msym-show-ref)
(define-key cplist-mode-map "D" 'msym-goto-def-nosection)
(define-key cplist-mode-map "Q" 'msym-goto-imp-nosection)

(define-key cplist-mode-map "p" 'codepilot-query-proc)
(define-key cplist-mode-map "i" 'codepilot-query-id)
(define-key cplist-mode-map "o" 'codepilot-query-sym)
(define-key cplist-mode-map "l" 'codepilot-query-string)
(define-key cplist-mode-map "c" 'codepilot-query-comment)
(define-key cplist-mode-map "t" 'codepilot-query-patch)
(define-key cplist-mode-map "m" 'codepilot-query-module)
(define-key cplist-mode-map "n" 'codepilot-query-section)

(define-key cplist-mode-map "P" 'codepilot-query-proc-dim)
(define-key cplist-mode-map "I" 'codepilot-query-id-dim)
(define-key cplist-mode-map "O" 'codepilot-query-sym-dim)

(define-key cplist-mode-map "a" 'codepilot-open-calltrack)
(define-key cplist-mode-map "`" 'cplist-minimize/restore-sidebar)


(global-set-key [(f8)] 'cplist-side-window)




(defun cplist-mode-del-window ()
  ""
  (interactive)
  (let ((win (cplist-get-list-win)))
    (when win
      (delete-window win)
        )
    ))


(defun cplist-sort-query-list-by-name ()
  ""
  (interactive)
  (let ((case-fold-search t)
        beg end
        (inhibit-read-only t)
        (buffer-unqdo-list t)
        )
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (forward-line)
        (forward-line)
        (setq beg (point))
        (setq end beg)
        (when (re-search-forward "^$" nil t)
          (setq end (point))
          )
        (unless (= beg end)
          (sort-lines nil beg end)
          )
        (setq cplist-query-sort-type 'name)
        ))
    (set-buffer-modified-p nil)
    ))

(defun cplist-sort-query-by-id-name ()
  ""
  (interactive)
  (let ((case-fold-search t)
        beg end
        (inhibit-read-only t)
        (buffer-unqdo-list t)
        )
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (forward-line)
        (forward-line)
        (setq beg (point))
        (setq end beg)
        (when (re-search-forward "^$" nil t)
          (setq end (point))
          )
        (unless (= beg end)
          (sort-fields 2 beg end)
          )
        (setq cplist-query-sort-type 'id-name)
        ))
    (set-buffer-modified-p nil)
    ))



(defun cplist-save-current-list-win-size ()
  ""
  (interactive)
  (let ((win (get-buffer-window cplist-buf-name))
        edges)
    (if win
        (progn
          (setq edges (window-edges win))
          (setq cplist-side-window-size (- (nth 2 edges) (nth 0 edges)))
          )
      (error "No IDList buffer now.")
        )
    ))



(defmacro cplist-sort-frame (section-start-regexp section-end-regexp variable mode mylist &rest body)
  (let ((tempvar (make-symbol "--cl-var--"))
        (tempbeg (make-symbol "--cl-beg--"))
        (tempend (make-symbol "--cl-end--"))
        (tempbuf (make-symbol "--cl-buf--"))
        )
    `(let ((case-fold-search t)
           ,tempbeg ,tempend ,tempvar
           (inhibit-read-only t)
           (buffer-unqdo-list t)
           ,mylist
           )
       (save-excursion
         (save-match-data
           (goto-char (point-min))
           (when (re-search-forward ,section-start-regexp nil t)
             (forward-line)
             (setq ,tempbeg (point))
             (setq ,tempend ,tempbeg)

             (when (re-search-forward ,section-end-regexp nil t)
               (setq ,tempend (point))
               )
             (unless (= ,tempbeg ,tempend)
               (delete-region ,tempbeg ,tempend))

             (dolist (,tempbuf (buffer-list))
               (when (eq (with-current-buffer ,tempbuf
                           ,(if variable
                                `(setq ,tempvar ,variable)
                              ()
                              )
                           major-mode)
                         ,mode)
                 (push
                  ,(if variable
                       `(cons ,tempvar (buffer-name ,tempbuf))
                     `(buffer-name ,tempbuf)
                     )
                  ,mylist)
                 ))
             ,@body
             (set-buffer-modified-p nil)
             ))))))



(defun cplist-minimize/restore-sidebar ()
  (interactive)
  (let ((buf (get-buffer cplist-buf-name))
        swin
        edges width)
    (if buf
      (save-selected-window
        (setq swin (get-buffer-window (get-buffer cplist-buf-name)))
        (select-window swin)
        (setq edges (window-edges swin))
        (setq width (- (nth 2 edges) (nth 0 edges)))

        (if (= width window-min-width)
            (enlarge-window-horizontally (- cplist-side-window-size width))
          (setq cplist-side-window-size width)
          (shrink-window-horizontally (- width window-min-width))))
      (cplist-side-window)
      )))


(provide 'cplist)

