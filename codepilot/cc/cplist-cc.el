(require 'cl)
(require 'cp-cc)
(require 'mycscope)
(require 'mygtags)
(require 'myetags)

(defun cplist-update-buffer-list ()
  ;; (message "find file hook hit")

  (let ((b (get-buffer cplist-buf-name))
        ;;(b-name (file-name-nondirectory buffer-file-name))
        (b-name (buffer-name))
        )
    
    (when (and b
               (in-codepilot-cc-major-modes? major-mode)
               b-name)
      (cplist-add-line-to-idlist "^\\ก่ Buffer List  " (concat "  " b-name "\n"))
      )))


(defun cplist-cc-fill-cplist ()
  (insert-image codepilot-image-bucket-1 "ก่")
  (insert " CScope Query List  \n")
  (if (not (eq cplist-query-sort-type 'create))
      (progn
        (dolist (b (buffer-list))
          (if (eq (with-current-buffer b  major-mode) 'cscope-list-entry-mode)
              (insert "  " (concat (buffer-name b) "\n")))
          )
        (cond ((eq cplist-query-sort-type 'name)
               (cplist-sort-query-list-by-name)
               )
              ((eq cplist-query-sort-type 'id-name)
               (cplist-sort-query-by-id-name)
               )
              )
        (insert "\n")
        )
    (insert "\n")
    (mycscope-cplist-sort-query-by-create)
    )

  (insert-image codepilot-image-bucket-1 "ก่")
  (insert " GTags List  \n")
  (insert "\n")
  (mygtags-list-sort-by-create)
  ;; (let (mm)
  ;;   (dolist (b (buffer-list))
  ;;     (setq mm (with-current-buffer b major-mode))
  ;;     (when (eq mm 'gtags-select-mode)
  ;;       (insert "  " (concat (buffer-name b) "\n"))
  ;;       )))
  ;; (insert "\n")

  (insert-image codepilot-image-bucket-1 "ก่")
  (insert " Buffer List  \n")
  (let (mm)
    (dolist (b (buffer-list))
      (setq mm (with-current-buffer b major-mode))
      (when (in-codepilot-cc-major-modes? mm)
        (insert "  " (concat (buffer-name b) "\n"))
        )))
  (insert "\n")

  (insert-image codepilot-image-bucket-1 "ก่")
  (insert " GTags History List  \n")
  ;; (dolist (i (delete-duplicates find-tag-history :from-end t :test #'string=))
  (dolist (i find-gtag-history)
    (when i
      (insert "  = " i "\n")))
  (insert "\n")

  (insert-image codepilot-image-bucket-1 "ก่")
  (insert " ETags History List  \n")
  ;; (dolist (i (delete-duplicates find-tag-history :from-end t :test #'string=))
  (dolist (i find-tag-history)
    (when i
      (insert "  > " i "\n")))
  (insert "\n")
  
  )

(defun cplist-cc-for-cplist-action ()
  (cond ((eq major-mode 'cscope-list-entry-mode)
         (let (pos)
           (setq pos (cscope-single-match?))
           (when pos
             (goto-char pos)
            (let ((cscope-jump-single-match t))
              (cscope-select-entry-other-window)))))
        ((eq major-mode 'gtags-select-mode)
         (cond ((= 1 (count-lines (point-min) (point-max)))                
                (gtags-select-it nil t) ;; do not delete
                )))))

(defun tag-delete-in-tag-history (str)
  (let (tagname ind ltail)
    (cond ((eq ?= (aref str 0))
           (setq tagname (subseq str 2))
           (setq ind (position tagname find-gtag-history :test #'string=))
           (cond ((null ind))
                 ((= ind 0)
                  (pop find-gtag-history))
                 (t
                  (setq ltail (nthcdr (1- ind) find-gtag-history))
                  (setcdr ltail (cdr (cdr ltail)))
                  ))
           )
          ((eq ?> (aref str 0))
           (setq tagname (subseq str 2))
           (setq ind (position tagname find-tag-history :test #'string=))
           (cond ((null ind))
                 ((= ind 0)
                  (pop find-tag-history))
                 (t
                  (setq ltail (nthcdr (1- ind) find-tag-history))
                  (setcdr ltail (cdr (cdr ltail)))
                  ))
           )
          )))

(defun cplist-cc-goto-next-visible-tagline ()
  (forward-line)
  (while (and (not (eobp))
              (or (looking-at "^ก่") ;; head line
                  (looking-at "^$") ;; blank line
                  (let ((ol (overlays-at (point)))) ;; in overlay. invisible.
                    (and ol
                         (some #'(lambda (o)
                                   (eq 'cpfilter (overlay-get o 'tag)))
                               ol)))))
    (forward-line)
    )
  (unless (eobp)
    t
    ))

(defun cplist-cc-tab ()
  (interactive)
  (let (pos)
    (save-excursion
      (when (cplist-cc-goto-next-visible-tagline)
        (setq pos (point))
        ))
    (cond (pos
           (goto-char pos)
           t
           )
          (t
           (save-excursion
             (goto-char (point-min))
             (forward-line) ;; skip the cpfilter line
             (when (cplist-cc-goto-next-visible-tagline)
               (setq pos (point))
               ))
           (when pos
             (goto-char pos)
             t)))))

(provide 'cplist-cc)