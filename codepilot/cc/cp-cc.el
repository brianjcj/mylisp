(eval-when-compile
  (require 'cl)
  )

(require 'cp-layout)
(require 'cp-base)
(require 'cpfilter)
(require 'cp-hl)
(require 'mycutil)

(defgroup codepilot-cc nil
  "Codepilot for CC"
  :group 'programming
  )

(defcustom codepilot-cc-major-modes
  '(c-mode c++-mode java-mode)
  "List of major modes. The buffer in these major modes will be
showed in the codepilot sidebar."
  :group 'codepilot-cc
  :type '(repeat (symbol :tag "Major mode"))
  )

(defsubst in-codepilot-cc-major-modes? (mm)
  (memq mm codepilot-cc-major-modes)
  )


(defun cplist-do-kill-on-deletion-marks ()
  ""
  (interactive)
  (let ((case-fold-search t)
        buf-list
        s
        mark-found
        prompt)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward "^D \\(.+\\)$" nil t)
          (setq s (match-string 1))
          (setq mark-found t)
          (push s buf-list)
          (setq prompt (concat prompt s "  "))
          )
        ))
    
    (when (and mark-found
           (y-or-n-p (concat prompt "\nReally delete above buffers? ")))
      (dolist (b buf-list)
        (if (get-buffer b)
            (kill-buffer b)
          ;; else, it must be a history list
          (run-hook-with-args 'cplist-delete-history-entry b)
          ))
      ;; (cplist-list-id-section-bufs)
      (let (sec-pos)
        (save-excursion
          (save-match-data
            (cplist-kill-del-mark-lines 1 (point-max))
            )))
      )))

(defun cplist-action-cc ()
  ""
  (interactive)
  (let ((case-fold-search t)
        buf-name)
    (save-match-data
      (forward-line 0)
      (when (looking-at "^ <\\(.*\\)>")
        (forward-line))
      (cond ((looking-at "^  > ")
             (let (tag)
               (save-excursion
                 (goto-char (match-end 0))
                 (setq tag (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
                 (multiple-value-bind (ret sidebar code-win bottom-win)
                     (codepilot-window-layout-wise)
                   (when code-win
                     (select-window code-win)))
                 (find-tag tag)
                 )))
            ((looking-at "^  = ")
             (let (tag)
               (save-excursion
                 (goto-char (match-end 0))
                 (setq tag (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
                 (multiple-value-bind (ret sidebar code-win bottom-win)
                     (codepilot-window-layout-wise)
                   (when code-win
                     (select-window code-win)))
                 (gtags-goto-tag tag "")
                 )))
            ((looking-at "[A-Za-Z ] \\(.+\\)$")
             (setq buf-name (match-string 1))
             (when (get-buffer buf-name)
               (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                 (codepilot-pop-or-switch-buffer buf-name))
               (run-hooks 'cplist-action-hook)
               ))
            ((looking-at "^\\��")
             ;; fold/unfold it.
             (let (b e pos ret)
               (end-of-line)
               (setq pos (point))
               (dolist (o (overlays-at pos))
                 (cptree-delete-overlay o 'cptree)
                 (setq ret t))

               (unless ret
                 (save-excursion
                   (end-of-line)
                   (setq b (point))
                   (cond ((re-search-forward "^��" nil t)
                          (forward-line 0)
                          (setq e (1- (point))))
                         (t
                          (setq e (point-max))))
                   (cptree-hide-region b e 'cptree)
                   )))
             )))))


(setq cplist-action-func 'cplist-action-cc)


(defun cplist-fold ()
  (interactive)
  (let (pos (loo t) b e)
    (save-excursion
      (save-match-data
        (setq pos (line-end-position))

        (when (re-search-backward "^��" nil t)
          (end-of-line)
          (setq b (point))

          (cond ((re-search-forward "^��" nil t)
                 (forward-line 0)
                 (setq e (1- (point))))
                (t
                 (setq e (point-max))))
          
          (cptree-hide-region b e 'cptree)

          (when (> (window-start) b)
            (let ((current-prefix-arg 4)) (call-interactively 'recenter)))
          )))))

(defun cplist-unfold ()
  (interactive)
  (let (ret)
    (dolist (o (overlays-at (line-end-position)))
      (when (cptree-delete-overlay o 'cptree)
        (setq ret t)
        ))
    ret))

(defun cplist-toggle-folding ()
  (interactive)
  (let ((case-fold-search t))
    (unless (cplist-unfold)
      (cplist-fold)
      )))

(defun cplist-fold-all ()
  (interactive)
  (cplist-unfold-all)
  (let (b e)
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^��" nil t)
          (setq b (line-end-position))

          (cond ((re-search-forward "^��" nil t)
                 (forward-line 0)
                 (setq e (1- (point))))
                (t
                 (setq e (point-max))))

          (cptree-hide-region b e 'cptree)
          )
        (goto-char (point-min))
        (let ((current-prefix-arg 4)) (call-interactively 'recenter))
        )
      )
    ))

(defun cplist-unfold-all ()
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (cptree-delete-overlay o 'cptree)
    ))


(defun cplist-add-line-to-idlist (which-list-regexp line)
  (let ((b (get-buffer cplist-buf-name)))
    (when b
      (with-current-buffer b
        (with-modify-in-readonly
         (goto-char (point-min))
         (save-match-data
           (when (re-search-forward which-list-regexp nil t)
             (forward-line)
             (insert line)
             (backward-char)
             (remove-overlays (line-beginning-position) (line-beginning-position 2) 'tag 'cpfilter)
             )))))))

(defvar cplist-menu
  '("CPList"
    ["Mark delete" cplist-mark-for-delete t]
    ["Unmark" cplist-unmark t]
    "-"
    ["Do mark action" cplist-do-kill-on-deletion-marks t]
    "-"
    ["Fold all" cplist-fold-all t]
    ["Unfold all" cplist-unfold-all t]
    ["Toggle foldding" cplist-toggle-folding t]
    "-"
    ["Sort Query List by last access" cplist-sort-query-by-last-access
     :style toggle :selected (eq cplist-query-sort-type 'last)]
    ["Sort Query List by name" cplist-sort-query-list-by-name
     :style toggle :selected (eq cplist-query-sort-type 'name)]
    ["Sort Query List by id name" cplist-sort-query-by-id-name
     :style toggle :selected (eq cplist-query-sort-type 'id-name)]
    ["Sort Query List by create" cplist-sort-query-by-create
     :style toggle :selected (eq cplist-query-sort-type 'create)]
    "-"
    ["Save Current IDList Window width" cplist-save-current-list-win-size t]
    ))

(easy-menu-define cplist-menu-symbol
                  cplist-mode-map
                  "CPlist menu"
                  cplist-menu)

(defun cc-search-and-hl-text (text &optional backward search-type class-id)
  ""
  (let* (
         (my-loop t)
         (goto-point (point))
         (success nil)
         s-func
         s-text
         (s-type (if search-type search-type 'id))
         (case-fold-search (not (eq 'id s-type)))
         ;; text-for-lazy-hl
         c
        )

    ;; (message (format "%s, hl begin, Current Point: %d" (current-time-string) (point)))
    
    (when (= 0 (length text))
      (error "Search string is empty!")
      )
    
    (setq codepilot-current-search-text (if (eq 'id s-type) text (downcase text)))
    (setq codepilot-current-search-type search-type)

    (setq s-text codepilot-current-search-text)
    ;; (setq text-for-lazy-hl s-text)

    (cond ((eq search-type 'id)
           (if backward
               (setq s-func 're-search-backward) ;; (setq s-func 'word-search-backward)
               (setq s-func 're-search-forward)) ;; (setq s-func 'word-search-forward))
           ;; (setq text-for-lazy-hl (concat "\\_<" (regexp-quote s-text) "\\_>"))
           (setq s-text (concat "\\_<" (regexp-quote s-text) "\\_>"))
           )
          ((eq search-type 'part-id)
           (if backward
               (setq s-func 're-search-backward)
             (setq s-func 're-search-forward)
             ))
          (t
           (if backward
               (setq s-func 'search-backward)
             (setq s-func 'search-forward)
             )))
    (save-match-data
      (save-excursion
        (unless inhibit-codepilot-highlight-2
          (condition-case nil
              (run-with-idle-timer 0.0 nil 'codepilot-highlight-2 (current-buffer))
            (error
             "error in run-with-idle-timer."
             nil
             )
            ))
        (while my-loop
          (if (not (funcall s-func s-text nil t))
              (progn
                (setq my-loop nil))
            (cond ((and (eq s-type 'id)
                        (my-in-quote/comment))
                   ;; search again
                   ())

                  
                  ;; brian update later todo
                  ;; ((and (eq s-type 'comment)
                  ;;       (not (my-in-comment))
                  ;;       )
                  ;;  ;;search again
                  ;;  ())

                  
                  ((and (eq s-type 'literal)
                        (or (not (setq c (my-in-quote/comment)))
                            (eq (char-after c) ?%)))
                   ;; search again
                   )
                  (t
                   (setq success (match-string 0))
                   (setq goto-point (point))
                   (setq my-loop nil)
                   (unless inhibit-codepilot-hl-text
                     (codepilot-hl-text (match-beginning 0) (match-end 0)))
                   ))))))
    (goto-char goto-point)
    ;; (message "after goto, Point: %s" (point))
    (when success
      (save-excursion
        ;; fixme: why sometimes it raises error (o sbcproci) ? -brian
        (unless inhibit-which-func-update
          (condition-case nil
              (which-func-update)
            (error nil))
          ;; (codepilot-highlight (point-min) (point-max) text-for-lazy-hl t t)
          ))      
      (unless backward
        (backward-char)))
    ;; brian: debug
    ;; (message (format "%s, Point: %d, Current Point: %d" (current-time-string) goto-point (point)))
    success
    ))

(defun cc-set-search-and-hl-func ()
  (interactive)
  (make-local-variable 'codepilot-search-and-hl-text-func)
  (setq codepilot-search-and-hl-text-func 'cc-search-and-hl-text)
  )

(add-hook 'c-mode-hook 'cc-set-search-and-hl-func)
(add-hook 'c++-mode-hook 'cc-set-search-and-hl-func)
(add-hook 'java-mode-hook 'cc-set-search-and-hl-func)

(defun codepilot-highlight-2 (buf)
  ;; (message (format "When In hl-2: %s, Current Point: %d" (current-time-string) (point)))
  
  (let ((codepilot-mark-tag 'codepilot-highlight-2)
        s-text
        )
    (condition-case nil
        (save-excursion
          (set-buffer buf)
          (when codepilot-hl-text-overlay
            (overlay-put codepilot-hl-text-overlay 'face 'codepilot-hl-text-face)
            (overlay-put codepilot-hl-text-overlay 'priority 1001))
          (save-excursion
            (save-restriction
              (widen)
              (codepilot-unmark-all)
              (unless (or (codepilot-string-all-space? codepilot-current-search-text)
                          (< (length codepilot-current-search-text) 2)
                          )
                (save-match-data
                  (goto-char (point-min))
                  (cond ((eq codepilot-current-search-type 'id)
                         (setq s-text (concat "\\_<" (regexp-quote codepilot-current-search-text) "\\_>"))
                         (while (re-search-forward s-text nil t)
                           (codepilot-mark-region (match-beginning 0) (match-end 0))
                           ))
                        ((eq codepilot-current-search-type 'part-id)
                         (while (re-search-forward codepilot-current-search-text nil t)
                           (codepilot-mark-region (match-beginning 0) (match-end 0))
                           ))
                        (t
                         (while (search-forward codepilot-current-search-text nil t)
                           (codepilot-mark-region (match-beginning 0) (match-end 0))
                           )
                         )))
                ))))
      (error
       (message "error in codepilot-highlight-2")
       nil
       )
      ))
  ;; (message (format "When out hl-2: %s, Current Point: %d" (current-time-string) (point)))
  )

(defun codepilot-kill-buffer-action ()
  (let (m)
    (dolist (b (cdr (buffer-list)))
      (setq m (with-current-buffer b major-mode))
      (cond ((eq m 'occur-mode)
             (bury-buffer b)))
      ;;       (cond ((or (eq m 'occur-mode)
      ;;                  (eq m 'cplist-mode)
      ;;                  (eq m ')))
      ;;             (t
      ;;              (save-current-buffer
      ;;                (switch-to-buffer b))
      ;;              (return)))

      ))

  (dolist (b codepilot-buffer-to-bury)
    (when (get-buffer b)
      (bury-buffer b)))
  
  (let ((b (get-buffer cplist-buf-name)))
    (when b
      (let ((name (buffer-name))
            (m major-mode))
        (with-current-buffer b
          (with-modify-in-readonly
           (goto-char (point-min))
           (save-match-data
             (when (re-search-forward (concat "  " (regexp-quote name) "\n") nil t)
               (forward-line -1)
               (delete-region (point)
                              (progn
                                (forward-line 1)
                                (point)))
                
               ))))))))

(add-hook 'kill-buffer-hook 'codepilot-kill-buffer-action)

(define-minor-mode codepilot-ro-mode
       "Toggle codepilot mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

     When Hungry mode is enabled, the control delete key
     gobbles all preceding whitespace except the last.
     See the command \\[hungry-electric-delete]."
      ;; The initial value.
      :init-value nil
      ;; The indicator for the mode line.
      :lighter " CPRO"
      ;; The minor mode bindings.
      :keymap
      '(("," . codepilot-previous-buffer)
        ("." . codepilot-forward-buffer)
        ("f" . codepilot-search-hl-again-f)
        ("b" . codepilot-search-hl-again-b)
        ("1" . delete-other-windows)
        ("0" . delete-window)
        ("k" . kill-this-buffer)
        )
      :group 'codepilot
      (if codepilot-ro-mode
          (setq buffer-read-only t)
        (setq buffer-read-only nil))
      )

(defvar codepilot-ro-menu
  '("CPRO"
    ["Occurances" cp-pb-search-id-and-which-procs t]
    ["Blocktrace" mycutil-cp-pb-where-we-are t]
    "-"
    ["Toggle Folding" hs-toggle-hiding t]
    ["Fold C switch" fold-c-switch (or (eq major-mode 'c-mode)
                                    (eq major-mode 'c++-mode)
                                    (eq major-mode 'java-mode)
                                    )]
    ["Fold C switch branch" fold-c-switch-branch (or (eq major-mode 'c-mode)
                                                  (eq major-mode 'c++-mode)
                                                  (eq major-mode 'java-mode)
                                                  )]
    "-"
    ["<< Go back" codepilot-previous-buffer t]
    ["Go next >>" codepilot-forward-buffer t]
    "-"
    ["Search text" codepilot-search-hi-string t]
    ["Search forward" codepilot-search-hl-again-f t]
    ["Search backward" codepilot-search-hl-again-b t]
    ))

(easy-menu-define codepilot-ro-menu-symbol
                  codepilot-ro-mode-map
                  "Codepilot-ro menu"
                  codepilot-ro-menu)


(defvar codepilot-ro-enabled-globally nil)
(defun codepilot-ro-toggle-globally ()
  "Toggle CPRO minor mode for all related buffers"
  (interactive)
  (let ((flag (if codepilot-ro-enabled-globally 0 1)))
    (setq codepilot-ro-enabled-globally (not codepilot-ro-enabled-globally))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (in-codepilot-cc-major-modes? major-mode)
          (codepilot-ro-mode flag)
          )))
    (cond (codepilot-ro-enabled-globally
           (add-hook 'c-mode-hook 'codepilot-ro-mode)
           (add-hook 'c++-mode-hook 'codepilot-ro-mode)
           (add-hook 'java-mode-hook 'codepilot-ro-mode))
          (t
           (remove-hook 'c-mode-hook 'codepilot-ro-mode)
           (remove-hook 'c++-mode-hook 'codepilot-ro-mode)
           (remove-hook 'java-mode-hook 'codepilot-ro-mode)))
    ))

(defalias 'cpro 'codepilot-ro-toggle-globally)

(define-key codepilot-ro-mode-map "-" 'cp-pb-which-procs-i-in)
(define-key codepilot-ro-mode-map "[" 'cp-pb-search-id-and-which-procs)
(define-key codepilot-ro-mode-map "]" 'mycutil-cp-pb-where-we-are)
(define-key codepilot-ro-mode-map [(f6)] 'mycutil-which-block)
(define-key codepilot-ro-mode-map "/" 'codepilot-search-hi-string)
(define-key codepilot-ro-mode-map "n" 'codepilot-search-hl-again-f)
(define-key codepilot-ro-mode-map "N" 'codepilot-search-hl-again-b)
(define-key codepilot-ro-mode-map "v" 'find-tag)
(define-key codepilot-ro-mode-map "7" 'gtags-find-tag)
(define-key codepilot-ro-mode-map "8" 'gtags-find-rtag)
(define-key codepilot-ro-mode-map "9" 'gtags-find-symbol)
(define-key codepilot-ro-mode-map "u" 'gtags-find-tag)
(define-key codepilot-ro-mode-map "i" 'gtags-find-rtag)
(define-key codepilot-ro-mode-map "o" 'gtags-find-symbol)
(define-key codepilot-ro-mode-map "`" 'cplist-minimize/restore-sidebar)
(define-key codepilot-ro-mode-map "v" 'hs-toggle-hiding)

(define-key cplist-mode-map "v" 'cplist-toggle-folding)
(define-key cplist-mode-map "c" 'cplist-fold-all)
(define-key cplist-mode-map "o" 'cplist-unfold-all)


(provide 'cp-cc)
