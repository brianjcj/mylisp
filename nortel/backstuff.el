(defun output-imenu-alist (ilist &optional prefix-str mj-mode)
  ""
  (let (pos ll ll2 ll-method item-name ind proc-name class-name aso ll3 proc-method?
            block-start
            )
    (setq ll ilist)
    ;; (when (and (null prefix-str)
    ;;            (eq mj-mode 'protel-mode)
    ;;            )
    ;;   (setq proc-method? t)
    ;;   )

    (unless prefix-str
      (setq prefix-str " "))

    (dolist (ii ll)
      (when (consp ii)
        (cond ((listp (cdr ii))
               (insert prefix-str)
               (insert-image ezimage-directory "ก๔") ;; brian test
               (insert " " (car ii) "  \n")
               (setq block-start (1- (point)))

               (setq ii (cdr ii))
               ;; (cond ((eq *sort-method* :position)
               ;;        (setq ii (sort (copy-sequence ii) #'imenu--sort-by-position))
               ;;        )
               ;;       ((eq *sort-method* :name)
               ;;        (setq ii (sort (copy-sequence ii) #'imenu--sort-by-name))))

               (output-imenu-alist ii (concat prefix-str " "))
               ;; (insert "\n")

               (put-text-property block-start (1+ block-start)
                                  'myimenu-block-end (point)
                                  )
               
               )
              (t
               (setq item-name (car ii))
               ;; (cond ((and proc-method?
               ;;             (eq ?\[ (aref item-name 0)))
               ;;        (setq ind (string-match "::" item-name))
               ;;        (setq class-name (substring item-name 0 ind))
               ;;        (setq proc-name (substring item-name (+ ind 2)))

               ;;        (cond ((setq aso (assoc class-name ll-method))
               ;;               (push (cons proc-name (cdr ii)) (cdr aso))
               ;;               )
               ;;              (t
               ;;               (push (list class-name (cons proc-name (cdr ii))) ll-method)
               ;;               )))
               ;;       (t
               ;;        (push ii ll2)))
               (push ii ll2)
               ))))
    (when  ll2

      (cond ((eq *sort-method* :position)
             (setq ll2 (sort ll2 #'imenu--sort-by-position))
             )
            ((eq *sort-method* :name)
             (setq ll2 (sort ll2 #'imenu--sort-by-name)))
            (t
             (setq ll2 (nreverse ll2))))

      (dolist (ii ll2)

        (insert prefix-str)
        (insert-image ezimage-page " ") ;; brian test
        (insert " " (car ii))

        (put-text-property (line-beginning-position) (line-end-position)
                           'myimenu-target (if (integerp (cdr ii))
                                               (cdr ii)
                                               (if (overlayp (cdr ii))
                                                   (overlay-start (cdr ii))
                                                   (marker-position (cdr ii)))))

        (insert "\n")

        )

      (insert "\n")
      )

    ;; (when ll-method
    ;;   (setq ll-method (nreverse ll-method))
    ;;   (dolist (i ll-method)
    ;;     (insert (car i) "\n")

    ;;     (setq ll3 (cdr i))
    ;;     (cond ((eq *sort-method* :position)
    ;;            (setq ll3 (sort ll3 #'imenu--sort-by-position))
    ;;            )
    ;;           ((eq *sort-method* :name)
    ;;            (setq ll3 (sort ll3 #'imenu--sort-by-name)))
    ;;           (t
    ;;            (setq ll3 (nreverse ll3))))
    ;;     (dolist (ii ll3)
    ;;       (insert "  " (car ii))
    ;;       (put-text-property (line-beginning-position) (line-end-position)
    ;;                          'myimenu-target (if (integerp (cdr ii))
    ;;                                              (cdr ii)
    ;;                                              (if (overlayp (cdr ii))
    ;;                                                  (overlay-start (cdr ii))
    ;;                                                  (marker-position (cdr ii)))))
    ;;       (insert "\n")
    ;;       ))
    ;;   (insert "\n")
    ;;   )
    
    ))


(defun protel-which-func-2 ()
  (let (name
        (alist (imenu--make-index-alist t))
        (minoffset (point-max))
        offset elem pair mark
        pos win
        mp
        )
    (setq alist (cdr alist)) ;; get rid of *rescan*
    (while alist
      (setq elem  (car-safe alist)
            alist (cdr-safe alist))
      ;; Elements of alist are either ("name" . marker), or
      ;; ("submenu" ("name" . marker) ... ).
      (unless (listp (cdr elem))
        (setq elem (list elem)))
      (while elem
        (setq pair (car elem)
              elem (cdr elem))
        (cond ((consp pair)
               (number-or-marker-p (setq mark (cdr pair)))
               (if (>= (setq offset (- (point) mark)) 0)
                   (if (< offset minoffset) ; find the closest item
                       (setq minoffset offset
                             pos (cdr pair)
                             name (car pair)))
                 ;; Entries in order, so can skip all those after point.
                 (setq elem nil)))
              (t
               )
              )))

    ;; (setq buf-name (buffer-name))
    (setq pos (- (point) minoffset))
    (if name
        (progn
;;            (setq mp (make-marker))
;;            (set-marker mp pos)
          (list name pos))
      nil)))


(defvar cplist-font-lock-keywords
  (list

   (list ")\\((I)\\)("
         '(1 font-lock-warning-face)
         )

   (list ")\\((D)\\)("
         '(1 font-lock-warning-face)
         )

   (list "^\\(\\[.*\\]\\)"
         '(1 font-lock-warning-face)
         )
   (list "^  \\[[a-z]\\{1,2\\} \\(.+\\)\\]"
         '(1 'font-lock-keyword-face)
         )

   (list "^  \\[\\(.+\\)\\]"
         '(1 'font-lock-keyword-face)
         )

   (list "^ <\\(.*\\)>"
         '(1 'font-lock-keyword-face)
         )
   (list "\\](\\(.+\\))("
         '(1 'font-lock-type-face)
         )

   (list "^[a-zA-Z ] \\(.+\\)$"
         '(1 'font-lock-type-face)
         )
   (list "ก่ \\(.+\\)$"
         '(1 'font-lock-warning-face)
         )
   )
  "font-lock keywords setting for cpxref buffers.")



(defun myimenu-which-func (&optional taglist)  
  (let (name
        (alist (imenu--make-index-alist t))
        (minoffset (point-max))
        offset elem pair mark
        pos win buf-name update?
        ooo
        )
    (setq alist (cdr alist)) ;; get rid of *rescan*
    (when alist
      (cond ((and (featurep 'semantic) semantic--buffer-cache)
             (unless taglist
               (setq taglist (semantic-find-tag-by-overlay (point))))
             (when taglist
               (setq ooo (car (last taglist)))
               (setq pos (semantic-tag-start ooo))
               (setq name (semantic-tag-name ooo)))
             )
            (t
             (while alist
               (setq elem  (car-safe alist)
                     alist (cdr-safe alist))
               ;; Elements of alist are either ("name" . marker), or
               ;; ("submenu" ("name" . marker) ... ).
               (unless (listp (cdr elem))
                 (setq elem (list elem)))
               (while elem
                 (setq pair (car elem)
                       elem (cdr elem))
                 (cond ((consp pair)
                        (cond ((listp (cdr pair)) ;; ("submenu" ("name" . marker) ... )
                               (setq elem (append (cdr pair) elem))
                               )
                              (t ;; ("name" . marker)
                               (number-or-marker-p (setq mark (cdr pair)))
                               (if (>= (setq offset (- (point) mark)) 0)
                                   (if (< offset minoffset) ; find the closest item
                                       (setq minoffset offset
                                             pos (cdr pair)
                                             name (car pair)))
                                   ;; Entries in order, so can skip all those after point.
                                   (setq elem nil))
                               )
                              )
                        
                        )
                       (t
                        )))))))

    (setq buf-name (buffer-name))
    (when (setq win (get-buffer-window "*MyIMenu*"))
      (with-current-buffer "*MyIMenu*"
        (goto-char (point-min))
        (cond ((string= buf-name myimenu-connected-buffer)
               )
              (t
               (setq update? t)
               )))
      (when update?
        (save-current-buffer
          (myimenu)))

      (when name
        ;; (setq pos (- (point) minoffset))

        (with-selected-window win
          (goto-char (point-min))
          (save-match-data
            (let ((loo t)
                  pos-in-line
                  )
              
              (while (and loo (not (eobp)))
                (cond ((and (setq pos-in-line (get-text-property (point) 'myimenu-target))
                            (= pos pos-in-line)
                            ;; (looking-at (concat "^[ ]*" (regexp-quote name) "$"))
                            )

                       ;; (myimenu-hl-text (match-beginning 0) (match-end 0))
                       (myimenu-hl-text (line-beginning-position) (line-end-position))
                       ;; (recenter (/ (window-height) 2))
                       (setq loo nil)
                       )
                      (t
                       (forward-line)
                       ))
                )
                )))))
    name))




(defun cc-which-func-2 ()

  (let (name
        (alist (imenu--make-index-alist t))
        (minoffset (point-max))
        minoffset-myi
        offset elem pair mark
        pos win buf-name update?
        myimenu-not-care?
        name-myi
        pos-myi
        )
    (setq alist (cdr alist)) ;; get rid of *rescan*
    (while alist
      (setq elem  (car-safe alist)
            alist (cdr-safe alist))
      ;; Elements of alist are either ("name" . marker), or
      ;; ("submenu" ("name" . marker) ... ).
      (unless (listp (cdr elem))
        (setq elem (list elem)))
      (setq myimenu-not-care? nil)
      (while elem
        (setq pair (car elem)
              elem (cdr elem))
        (cond ((consp pair)
               (number-or-marker-p (setq mark (cdr pair)))
               (if (>= (setq offset (- (point) mark)) 0)
                   (when (< offset minoffset) ; find the closest item
                     (setq minoffset offset
                           pos (cdr pair)
                           name (car pair))
                     (unless myimenu-not-care?
                       (setq name-myi name)
                       (setq pos-myi pos)
                       ))
                   ;; Entries in order, so can skip all those after point.
                   (setq elem nil)
                   
                   ))
              (t
               (cond ((or (string= pair "Enumeration")
                          (string= pair "Member")
                          (string= pair "Variable")
                          (string= pair "Macro")
                          )
                      ;; (setq elem nil)
                      (setq myimenu-not-care? t)
                      ))
               )
              )))

    ;; (setq buf-name (buffer-name))
    (if name-myi
        (list name-myi pos-myi)
        )
    ))


(defun myctags-get-tags-list (file-input mj-mode)
  "work in the ctags output buffer"
  (let (al tag ln kind buf aso class-name in-class
           tag-line items options m-str
           (myctags-include-parmlist (if (eq mj-mode 'c-mode) nil t))
           )
    ;; (make-local-variable myctags-cache)
    
    (cond ((eq mj-mode 'java-mode)
           (cond ((string-match "\.js$" file-input)
                  (setq myctags-cache (copy-tree myctags-js-kinds))
                  )
                 (t
                  (setq myctags-cache (copy-tree myctags-java-kinds))))       
           )
          (t
           (setq myctags-cache (copy-tree myctags-ckinds))
           ))
    
    (setq myctags-class-list nil)

    (erase-buffer)
    
    (call-process exuberant-ctags-program
                  nil t nil "-f" "-" "--format=2" "--excmd=number"
                  "--fields=ksS"
                  ;; "--fields=k"
                  "--regex-c=/^DEFUN[ \t]*\\([ \t]*\"([^\"]+)\"/\\1/z,defun/"
                  myctags-sort-option
                  file-input)
    ;; analyzed the output
    ;; e.g.;
    ;; swap	f:/boost/boost_1_39_0/boost/smart_ptr/detail/shared_ptr_nmt.hpp	168;"	f	namespace:boost	signature:(shared_ptr<T> & a, shared_ptr<T> & b)
    ;;
    ;; swap==== 0:tag
    ;; f:/boost/boost_1_39_0/boost/smart_ptr/detail/shared_ptr_nmt.hpp==== 1:filename
    ;; 168;"==== 2:line
    ;; f==== 3:kind
    ;; namespace:boost==== (optional)
    ;; signature:(shared_ptr<T> & a, shared_ptr<T> & b)==== (optional)
    ;; nil

    (goto-char (point-min))
    (while (not (eobp))
      ;; at the line begining now.

      (setq tag-line (buffer-substring-no-properties (point) (line-end-position)))
      (setq items (split-string tag-line "\t"))

      (setq tag (car items))
      (setq ln (nth 2 items))
      (setq kind (aref (nth 3 items) 0))

      (setq options (nthcdr 4 items))
      
      (setq in-class nil)

      (when options
        ;; process the options; parents and signature.
        
        (cond ((or (eq ?f kind)
                   (eq ?m kind)
                   (eq ?e kind)
                   (eq ?t kind)
                   )
               (setq m-str (car options))
               (when (and myctags-include-class
                          (string-match "\\(class\\|struct\\|enum\\):\\(.+\\)"
                                        m-str)
                          
                          )

                 (setq class-name (concat (match-string 1 m-str) " " (match-string 2 m-str)))
                 (setq in-class t)
                     
                 )
               (when (and myctags-include-parmlist
                          (setq m-str (car (last options)))
                          (string-match "signature:\\(.+\\)" m-str)
                          )
                 (setq tag (concat tag " " (match-string 1 m-str)))
                 )
               )
              ((or (eq ?c kind)
                   (eq ?s kind)
                   (eq ?n kind)
                   )
               
               (setq m-str (car options))
               (when (and myctags-include-class
                          (string-match "namespace:\\(.+\\)"
                                        m-str)
                          
                          )

                 (setq tag (concat tag " |" (match-string 1 m-str) "|"))
                 )
               )
              ))
      (cond (in-class
             (cond ((setq aso (assoc class-name myctags-class-list))
                    (push (cons tag (string-to-number ln)) (cdr aso))
                    )
                   (t
                    (push (list class-name (cons tag (string-to-number ln))) myctags-class-list)
                    ))
             )
                
            (t
             (setq al (assq kind myctags-cache))
             (when al
               (push (cons tag (string-to-number ln)) (cdr (cdr al))))))
        
      (forward-line))

    (setq myctags-class-list (nreverse myctags-class-list))
    
    ))



(defun cc-create-index-function ()
  "Create imenu index using exuberant CTags"
  (let (alist
        (file (buffer-file-name))
        pos kind pos2 tag ln al
        (m major-mode)
        file-input
        ll
        tag-lines
        )

    (setq file-input (myctags-make-temp-file-if-file-nonlocal file))
    
    (save-match-data
      (with-temp-buffer
        (setq tag-lines (myctags-get-tags-list file-input m))
        ))

    (when tag-lines
      (setq tag-lines (nreverse tag-lines))
      (setq (make-local-variable 'myctags-tag-vect) (make-vector))
      ;; get the point of the line number.
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          
          (let (ln (prev-line 1) delta)
            (dolist (line tag-lines)
              (setq ln (car line))
              (setq delta (- ln prev-line))
              
              ))))
      )
    
    (save-excursion
      (dolist (i myctags-cache)
        (when (cdr (cdr i))
          (setq ll nil)
          (push (concat (capitalize (second i))) ll) ;; brian
        
          (dolist (ii (sort (cdr (cdr i)) #'(lambda (a b) (< (cdr a) (cdr b)))))
            (my-goto-line (cdr ii))
            (setcdr ii (point-marker))
            (push (cons (car ii) (cdr ii)) ll))
          (push (nreverse ll) alist)
          ))
      (dolist (i myctags-class-list)
        (setq ll nil)
        (push (concat (car i)) ll)
        
        (dolist (ii (sort (cdr i) #'(lambda (a b) (< (cdr a) (cdr b)))))
          (my-goto-line (cdr ii))
          (setcdr ii (point-marker))
          (push (cons (car ii) (cdr ii)) ll)
          )
        (push (nreverse ll) alist)
        )
      )

    (nreverse alist)
    ))


(defun cc-which-func-2 ()  ;; =ToSync= with which-func.el

  (let (name
        pos win buf-name update?
        myimenu-not-care?
        name-myi
        pos-myi
        submenu-name
        )

    (when (and (null imenu--index-alist)
	       (null which-function-imenu-failed))
      (imenu--make-index-alist t)
      (unless imenu--index-alist
	(make-local-variable 'which-function-imenu-failed)
	(setq which-function-imenu-failed t)))
    ;; If we have an index alist, use it.
    (when (and (boundp 'imenu--index-alist) imenu--index-alist)
      (let ((alist imenu--index-alist)
            (minoffset (point-max))
            offset pair mark imstack namestack)
        ;; Elements of alist are either ("name" . marker), or
        ;; ("submenu" ("name" . marker) ... ). The list can be
        ;; arbitrarily nested.
        (while (or alist imstack)
          (if alist
              (progn
                (setq pair (car-safe alist)
                      alist (cdr-safe alist))

                (cond ((atom pair))     ; skip anything not a cons

                      ((imenu--subalist-p pair)  ;; brian: pair => ("submenu" ("name" . marker) ... )
                       (setq imstack   (cons alist imstack)
                             namestack (cons (car pair) namestack)
                             alist     (cdr pair))  ;; brian: process the submenu first.

                       (setq submenu-name (car pair))
                       (cond ((or (string= submenu-name "Variable")
                                  (string= submenu-name "Macro")
                                  )
                              ;; (setq elem nil)
                              (setq myimenu-not-care? t)
                              ))
                       )

                      ((number-or-marker-p (setq mark (cdr pair)))  ;; brian: ("name" . marker)
                       (if (>= (setq offset (- (point) mark)) 0)
                           (when (< offset minoffset) ; find the closest item
                             (setq minoffset offset
                                   pos (cdr pair)
                                   ;;name (car pair)
                                   name (concat (apply 'concat namestack) "." (car pair))
                                   
                                   ;; (funcall
                                   ;;       which-func-imenu-joiner-function
                                   ;;       (reverse (cons (car pair)
                                   ;;    		  namestack)))
                                   
                                   )
                             (unless myimenu-not-care?
                               (setq name-myi name)
                               (setq pos-myi pos)
                               )
                             )
                           ;; Entries in order, so can skip all those after point.
                           ;; NOTE: brian !!! our myctags is sorted by position in each submenu.
                           ;;       So we can skip here.
                           (setq alist nil)
                           ))))
              (setq alist (car imstack)
                    namestack (cdr namestack)
                    imstack   (cdr imstack)
                    myimenu-not-care? nil
                    )))))

    (when name-myi
      (list name-myi pos-myi name pos)
      )
    ))

