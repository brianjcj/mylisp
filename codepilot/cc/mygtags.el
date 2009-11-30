(require 'cl)
(require 'gtags)
(require 'cp-cc)
(require 'hideshow)

(defvar find-gtag-history nil)

(defun mygtags-push-find-gtag-history (tagname)
  (let (ind ltail)
    (when tagname
      (setq ind (position tagname find-gtag-history :test #'string=))
      (cond ((null ind)
             (setq find-gtag-history (push tagname find-gtag-history))

             ;; update [GTags History List] in idlist
             (cplist-add-line-to-idlist "^ก่ GTags History List  "
                                          (concat "  = " tagname "\n"))
             )
            ((= ind 0))
            (t
             ))
      )))


(defun gtags-goto-tag (tagname flag)
  (let (option save prefix buffer lines)
    (setq save (current-buffer))
                                        ; Use always ctags-x format.
    (setq option (concat "-x" flag))
    (cond
     ((equal flag "P")
      (setq prefix "F) "))
     ((equal flag "g")
      (setq prefix "G) "))
     ((equal flag "I")
      (setq prefix "I) "))
     ((equal flag "s")
      (setq prefix "S) "))
     ((equal flag "r")
      (setq prefix "R) "))
     ((equal flag "f")
      (setq prefix "O) "))
     (t (setq prefix "D) ")))
    ;; load tag
    ;; brian
    ;; (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
    
    (cond ((setq buffer (get-buffer (concat prefix tagname)))
           (codepilot-pop-or-switch-buffer buffer)
           (setq lines (count-lines (point-min) (point-max)))
           (cond ((= 1 lines)
                  (mygtags-push-find-gtag-history tagname)
                  (message "Searching %s ... Done" tagname)
                  ;; (gtags-select-it t)
                  (gtags-select-it nil t) ;; do not delete
                  ))
           )
          (t
           (setq buffer (generate-new-buffer (generate-new-buffer-name (concat prefix tagname))))
           (codepilot-pop-or-switch-buffer buffer)
           (set-buffer buffer)
           ;; 
           ;; If project directory is specified, 'Gtags Select Mode' print paths using
           ;; the relative path name from the project directory else absolute path name.
           ;; 
           (if gtags-rootdir
               (cd gtags-rootdir)
             (setq option (concat option "a"))) 
           (message "Searching %s ..." tagname)
           (if (not (= 0 (call-process "global" nil t nil option tagname)))
               (progn (message (buffer-substring (point-min)(1- (point-max))))
                      (gtags-pop-context))
             (goto-char (point-min))
             (setq lines (count-lines (point-min) (point-max)))
             (cond
              ((= 0 lines)
               (cond
                ((equal flag "P")
                 (message "%s: path not found" tagname))
                ((equal flag "g")
                 (message "%s: pattern not found" tagname))
                ((equal flag "I")
                 (message "%s: token not found" tagname))
                ((equal flag "s")
                 (message "%s: symbol not found" tagname))
                (t
                 (message "%s: tag not found" tagname)))
               (gtags-pop-context)
               (kill-buffer buffer)
               (set-buffer save))
              ((= 1 lines)
               (mygtags-push-find-gtag-history tagname)
               (cplist-add-line-to-idlist "^ก่ GTags List  "
                                            (concat "  " (buffer-name buffer)  "\n")
                                            )
               
               (message "Searching %s ... Done" tagname)
               (gtags-select-mode)
               
               (with-modify-in-readonly
                (mygtags-split-gtags-output))
               
               ;; serial number for sort by the create time.
               (make-local-variable 'cptree-serial-number)
               (setq cptree-serial-number cptree-serial-no-last)
               (setq cptree-serial-no-last (1+ cptree-serial-no-last))
               
               ;; (gtags-select-it t)
               (gtags-select-it nil t) ;; do not delete

               )
              (t
               (mygtags-push-find-gtag-history tagname)
               ;; (codepilot-pop-or-switch-buffer buffer)

               (cplist-add-line-to-idlist "^ก่ GTags List  "
                                            (concat "  " (buffer-name buffer)  "\n")
                                            )
                  
               (gtags-select-mode)

               (with-modify-in-readonly
                (mygtags-split-gtags-output))
               
               ;; serial number for sort by the create time.
               (make-local-variable 'cptree-serial-number)
               (setq cptree-serial-number cptree-serial-no-last)
               (setq cptree-serial-no-last (1+ cptree-serial-no-last))
               )))
           ))))

(defun gtags-select-it (delete &optional not-mark)
  (let (line file sym)
    ;; get context from current tag line
    (beginning-of-line)
    (if (not (looking-at "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]"))
        (gtags-pop-context)
      (setq sym (gtags-match-string 1))
      (setq line (string-to-number (gtags-match-string 2)))
      (setq file (gtags-match-string 3))
      ;;
      ;; Why should we load new file before killing current-buffer?
      ;;
      ;; If you kill current-buffer before loading new file, current directory
      ;; will be changed. This might cause loading error, if you use relative
      ;; path in [GTAGS SELECT MODE], because emacs's buffer has its own
      ;; current directory.
      ;; 
      (let ((prev-buffer (current-buffer)))
        ;; move to the context
        ;; (if gtags-read-only (find-file-read-only file) (find-file file))
        (let ((buf (find-file-noselect file)))
          (when buf
            (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook not-mark))
              (codepilot-pop-or-switch-buffer buf)
              (when not-mark
                (bury-buffer prev-buffer))
              )
            ))
        
        (if delete (kill-buffer prev-buffer)))
      (setq gtags-current-buffer (current-buffer))
      (goto-line line)
      (which-func-update)
      (codepilot-search-and-hl-text sym nil 'id)
      (gtags-mode 1))))

(defun gtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (let (buffer input n)
    (if (equal gtags-rootdir nil)
      (save-excursion
        (setq buffer (generate-new-buffer (generate-new-buffer-name "*rootdir*")))
        (set-buffer buffer)
        (setq n (call-process "global" nil t nil "-pr"))
        (if (= n 0)
          (setq gtags-rootdir (file-name-as-directory (buffer-substring (point-min)(1- (point-max)))))
         (setq gtags-rootdir (directory-file-name default-directory))) ;; brian
        (kill-buffer buffer)))
    (setq input (read-directory-name "Visit root directory: "
			default-directory default-directory t))
    (if (equal "" input) nil
      (if (not (file-directory-p input))
        (message "%s is not directory." input)
       (setq gtags-rootdir (directory-file-name (expand-file-name input)))  ;; remove the ending / if any.
       (setenv "GTAGSROOT" gtags-rootdir)))))


(defface mygtags-delimiter-face
    '((default (:inherit region))
      (((class color) (background light)) (:background "darkseagreen2"))
      (((class color) (background dark)) (:background "DarkSlateGrey" :foreground "white"))
      )
  "Cpfilter edit entry face."
  :group 'coldpilot
  )

(defface mygtags-delimiter-1-face
    '((default (:inherit region))
      (((class color) (background light)) (:background "LightYellow2"))
      (((class color) (background dark)) (:background "DarkGreen" :foreground "white"))
      )
  "Cpfilter edit entry face."
  :group 'coldpilot
  )0;115;0cq


(defun mygtags-split-gtags-output ()
  (let (o
        (file1 "")
        file2
        (color t)
        pos
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward " +" nil t 2)
        (setq pos (point))
        (setq file2 (buffer-substring-no-properties (point) (progn (search-forward " " nil t) (1- (point)))))

        (setq o (make-overlay pos (1- (point))))
        (overlay-put o 'face 'font-lock-warning-face)

        (cond ((string= file1 file2))
              (t
               (setq file1 file2)
               (setq o (make-overlay (line-beginning-position 0) (line-beginning-position 1)))
               (overlay-put o 'face 'underline)
               (setq color (not color))
               ))
        
        (if color
          (put-text-property (line-beginning-position) (line-beginning-position 2)
                             'face 'mygtags-delimiter-face)
          (put-text-property (line-beginning-position) (line-beginning-position 2)
                             'face 'mygtags-delimiter-1-face)
          )
        
        (forward-line))
      (setq o (make-overlay (line-beginning-position 0) (line-beginning-position 1)))
      (overlay-put o 'face 'underline)
      )))



(defun mygtags-next-file ()
  (interactive)
  (let (pos)
    (setq pos (next-property-change (point)))
    (if pos
        (goto-char pos)
      (goto-char (point-min))
      )))


(defun mygtags-list-sort-by-create ()
  ""
  (interactive)

  (cplist-sort-frame
   "ก่ GTags List  "
   "^$"
   cptree-serial-number
   'gtags-select-mode
   m-list
   (when m-list
     (setq m-list (sort m-list (lambda (a b)
                                 (>  (car a)  (car b))
                                 )))
     
     (dolist (b1 m-list)
       (insert "  " (cdr b1) "\n")
       )
     ))
  (setq cplist-query-sort-type 'create)
  )



(define-key gtags-mode-map [mouse-3] 'gtags-find-tag-by-event)
(define-key gtags-mode-map [mouse-2] (lambda (e)
                                       (interactive "e")
                                       (mouse-set-point e)
                                       (hs-toggle-hiding)
                                       ))

(define-key gtags-select-mode-map [mouse-3] 'gtags-select-tag-by-event)
(define-key gtags-select-mode-map [tab] 'mygtags-next-file)
(define-key gtags-select-mode-map "," 'codepilot-previous-buffer)
(define-key gtags-select-mode-map "." 'codepilot-forward-buffer)
(define-key gtags-select-mode-map "k" 'kill-buffer)
(define-key gtags-select-mode-map "`" 'cplist-minimize/restore-sidebar)
(define-key gtags-select-mode-map "1" 'delete-other-windows)


(easy-menu-define mygtags-menu-symbol
  ;; (list gtags-mode-map codepilot-ro-mode-map)
  gtags-mode-map
  "Menu for Gtags!"
  '("Gtags"
    ["Find tag" gtags-find-tag t ]
    ["Find rtag" gtags-find-rtag t ]
    ["Find symbol" gtags-find-symbol t ]
    "-"
    ["Visit-rootdir" gtags-visit-rootdir t]
    )
  )


(add-hook 'gtags-select-mode-hook 'codepilot-ro-mode)

(require 'desktop)
(pushnew 'find-gtag-history desktop-globals-to-save)
(pushnew 'find-tag-history desktop-globals-to-save)



(provide 'mygtags)