
(require 'proc-outline-base)

(defvar protel-block-keywords-regexp
  (concat "\\_<"
          (regexp-opt (list
                       "IF"
                       "THEN"
                       "ELSEIF" "ELSE"
                       "ENDIF"
                       "WHILE" "FOR" "BLOCK" "ENDBLOCK" "DO" "ENDDO"
                       "SELECT" "CASE" "OUT" "ENDSELECT" "ENDCASE"
                       "INSPECT" "ENDINSPECT"
                       )
                      t)
          "\\_>"
          "\\|}[ \\t]*:"
          "\\|:"))



(defun protel-proc-outline ()
  (interactive)
  (catch 'loop
    (let ((inhibit-push-mark t)
          b e
          ll
          buf proc
          begpt
          endpt
          ln
          (pos 1)
          tmp-pos
          marker proc-and-pos
          (f-name (buffer-file-name)))

      (setq ln (line-beginning-position))
      (save-excursion
        (save-match-data
          (cond ((which-proc)
                 (setq b (point))
                 )
                ((search-uncommented-f "BLOCK")
                 (setq b (line-beginning-position))
                 )
                (t
                 (throw 'loop nil)
                 ))
          (setq proc-and-pos (protel-which-func-2))
          (setq proc (car proc-and-pos))

          (unless (which-block-forward)
            (throw 'loop nil))
          (setq e (line-end-position))

          (goto-char b)
          (while (re-search-forward protel-block-keywords-regexp e t)
            (cond ((my-in-quote/comment)
                   ;; ignore
                   )
                  (t
                   (setq begpt (line-beginning-position))
                   (setq endpt (line-end-position))
                   (if (and (if (boundp 'jit-lock-mode) jit-lock-mode)
                            (text-property-not-all begpt endpt 'fontified t))
                       (if (fboundp 'jit-lock-fontify-now)
                           (jit-lock-fontify-now begpt endpt)))

                   (setq marker (make-marker))
                   (set-marker marker (match-beginning 0))

                   (push (list (buffer-substring begpt endpt)
                               marker
                               ;; (line-number-at-pos)
                               (line-beginning-position)
                               )
                         ll)
                   ))
            (end-of-line)
            )
          (setq ll (nreverse ll))
          (setq buf (get-buffer-create "*Proc Outline*"))

          (with-current-buffer buf
            (kill-all-local-variables)
            (font-lock-mode 0)
            (protel-mode)
            (proc-outline-mode 1)

            (setq buffer-read-only t)

            (make-local-variable 'proc-outline-buffer-file-name)
            (setq proc-outline-buffer-file-name f-name)

            (with-modify-in-readonly
             (erase-buffer)
             (when proc
               (insert "Procedure: ")
               (setq tmp-pos (point))
               (insert proc)
               (put-text-property tmp-pos (point) 'face 'codepilot-hl-text-face)
               (put-text-property (line-beginning-position) (line-end-position)
                                  'proc-outline-target (second proc-and-pos))

               (insert "\n")
               )
             (dolist (i ll)
               (insert (car i))
               (put-text-property (line-beginning-position) (line-end-position)
                                  'proc-outline-target (second i))
               (when (>= ln (third i))
                 (setq pos (line-beginning-position))
                 )
               (insert "\n")
               )
             (goto-char pos)
             ;; (myimenu-hl-text (line-beginning-position) (line-end-position))
             (mymisc-highlight-one-line-1)
             ))
          (save-selected-window
            (proc-outline-pop-to-buffer buf)
            (goto-char pos)
            ;; (fit-window-to-buffer (get-buffer-window buf) (/ (frame-height) 2))
            )
          ))
      )))


(defun protel-search-uncommented-b-for-listing (str &optional limit)
  (let (m)
    (setq m (search-uncommented-b str limit))
    (while (and (not (bobp))
                (< (current-column) 29)
                (eq ?| (char-before (+ 29 (line-beginning-position)))))
      (forward-line 0)
      (setq m (search-uncommented-b str limit)))
    m))

(defun proc-outlie-check-lable (ll search-uncommented-b-func)
  (let (ln-b pos md)
    (setq ln-b (line-beginning-position))
    (save-excursion
      (setq pos nil)
      (setq md (funcall search-uncommented-b-func "[^ \t\n]"))
      (cond ((and (string= md ":")
                  (save-excursion
                    (not (string= "}" (funcall search-uncommented-b-func "[^ \t\n]")))
                    ))
             (unless (= (line-beginning-position) ln-b)
               (setq pos (point)))
             )))
    (when pos
      (goto-char pos)
      (setq ll (proc-outlie-push-line ll))))
  ll)

(defun protel-entire-comment/blank-line? ()
  (save-excursion
    (forward-line 0)
    (or (looking-at "^[ \t]*%")
        (looking-at "^[ \t]*$")
        )))

(defun protel-entire-comment/blank-line-listing? ()
  (save-excursion
    (move-to-column 29)
    (or (looking-at "[ \t]*%")
        (looking-at "[ \t]*$")
        )))


(defun skip-chars-backward-listing (str)
  (let ()
    (skip-chars-backward str)
    (while (and (not (bobp))
                (= 29 (current-column))
                (eq ?| (char-before)))
      (forward-line 0)
      (skip-chars-backward str)
      )))

(defun proc-outline-where-we-are ()
  (interactive)
  (let ((inhibit-push-mark t)
        ms ll begpt endpt marker buf proc md md1 pos proc-and-pos proc-pos
        (f-name (buffer-file-name)) to-pos
        p2 p1 (last-line-b -1) stored-pos
        skip-back-func
        protel-entire-comment/blank-line-func?
        search-uncommented-b-func
        )
    (cond ((protel-is-listing-file?)
           (setq skip-back-func 'skip-chars-backward-listing)
           (setq protel-entire-comment/blank-line-func? 'protel-entire-comment/blank-line-listing?)
           (setq search-uncommented-b-func 'protel-search-uncommented-b-for-listing)
           )
          (t
           (setq skip-back-func 'skip-chars-backward)
           (setq protel-entire-comment/blank-line-func? 'protel-entire-comment/blank-line?)
           (setq search-uncommented-b-func 'search-uncommented-b)
           ))

    (make-local-variable 'proc-outline-buffer-file-name)
    (setq proc-outline-buffer-file-name (buffer-file-name))

    (setq proc-and-pos (protel-which-func-2))
    (setq proc (car proc-and-pos))
    (setq proc-pos (second proc-and-pos))

    (push (list (buffer-substring (line-beginning-position) (line-end-position))
                (point))
          ll)

    (setq last-line-b (line-beginning-position))

    (save-excursion
      (save-match-data
        (save-restriction
          (widen)
          (end-of-line)
          (while (setq ms (which-block))
            (setq ms (upcase ms))
            (cond ((string= ms "THEN")
                   (funcall skip-back-func "[ \t\n]")
                   (setq pos (line-beginning-position))
                   (setq md (search-uncommented-b "\\_<\\(IF\\|ELSEIF\\)\\_>" proc-pos))
                   (when md
                     (setq p2 (line-beginning-position))
                     (setq stored-pos (point))

                     (goto-char pos)
                     (while (>= (point) p2)
                       (when (not (funcall protel-entire-comment/blank-line-func?))
                         (setq ll (proc-outlie-push-line ll last-line-b))
                         )
                       (forward-line -1)
                       )
                     (goto-char stored-pos)

                     (when (string= (upcase md) "IF")
                       (setq ll (proc-outlie-check-lable ll search-uncommented-b-func)))))
                  ;; }:
                  ((char-equal ?} (aref ms 1))

                   (save-excursion
                     (setq pos (line-beginning-position))
                     (setq stored-pos (point))
                     (cond ((search-uncommented-b "{" proc-pos)
                            (setq p2 (line-beginning-position))
                            (goto-char pos)
                            (while (>= (point) p2)
                              (when (not (funcall protel-entire-comment/blank-line-func?))
                                (setq ll (proc-outlie-push-line ll last-line-b))
                                )
                              (forward-line -1))
                            (goto-char stored-pos)
                            )
                           (t
                            (setq ll (proc-outlie-push-line ll last-line-b)))))


                   (search-ifcaseselect-backward)
                   (setq stored-pos (point))
                   (setq p2 (line-beginning-position))
                   (when (search-uncommented-f "\\_<IN\\_>")
                     (backward-word)
                     (funcall skip-back-func "[ \t\n]")
                     )
                   (while (>= (point) p2)
                     (when (not (funcall protel-entire-comment/blank-line-func?))
                       (setq ll (proc-outlie-push-line ll last-line-b))
                       )
                     (forward-line -1))
                   (goto-char stored-pos)
                   )
                  ((string= ms "OUT")
                   (setq ll (proc-outlie-push-line ll last-line-b))
                   (search-ifcaseselect-backward)
                   (setq stored-pos (point))
                   (setq p2 (line-beginning-position))
                   (when (search-uncommented-f "\\_<IN\\_>")
                     (backward-word)
                     (funcall skip-back-func "[ \t\n]")
                     )
                   (while (>= (point) p2)
                     (when (not (funcall protel-entire-comment/blank-line-func?))
                       (setq ll (proc-outlie-push-line ll last-line-b))
                       )
                     (forward-line -1))
                   (goto-char stored-pos)
                   )
                  ((string= ms "DO")
                   (save-excursion
                     (setq pos nil)
                     (funcall skip-back-func "[ \t\n]")
                     (setq p1 (line-beginning-position))
                     (setq stored-pos (point))
                     (setq md (search-uncommented-b "\\_<\\(WHILE\\|FOR\\|FROM\\|OVER\\|DO\\)\\_>" proc-pos))
                     (when md
                       (setq md (upcase md))
                       (cond ((string= md "DO")
                              (goto-char stored-pos)
                              )
                             ((or (string= md "OVER")
                                  (string= md "FROM")
                                  )
                              (setq pos (point))
                              (setq md1 (search-uncommented-b "\\_<\\(FOR\\|DO\\)\\_>" proc-pos))
                              (when md1
                                (setq md1 (upcase md1))
                                (cond ((string= md1 "DO")
                                       (goto-char pos)
                                       )
                                      ((string= md1 "FOR")
                                       (setq pos (point))
                                       ))))
                             ((string= md "FOR")
                              (setq pos (point))
                              )

                             ((string= md "WHILE")
                              (setq pos (point))

                              ;; search "FOR"
                              (setq md1 (search-uncommented-b "\\_<\\(FOR\\|OVER\\|DO\\|FROM\\)\\_>" proc-pos))
                              (when md1
                                (setq md1 (upcase md1))
                                (cond ((string= md1 "DO")
                                       (goto-char pos)
                                       )
                                      ((or (string= md1 "OVER")
                                           (string= md1 "FROM")
                                           )
                                       (setq pos (point))

                                       (setq md1 (search-uncommented-b "\\_<\\(FOR\\|DO\\)\\_>" proc-pos))
                                       (when md1
                                         (setq md1 (upcase md1))
                                         (cond ((string= md1 "DO")
                                                (goto-char pos)
                                                )
                                               ((string= md1 "FOR")
                                                (setq pos (point))
                                                )))

                                       )
                                      ((string= md1 "FOR")
                                       (setq pos (point))
                                       )
                                      ))

                              (setq p2 (line-beginning-position))
                              (goto-char p1)
                              (while (> (point) p2)
                                (when (not (funcall protel-entire-comment/blank-line-func?))
                                  (setq ll (proc-outlie-push-line ll last-line-b))
                                  )
                                (forward-line -1))
                              ;; (goto-char stored-pos)
                              ))
                       ))
                   (when pos
                     (goto-char pos))
                   (setq ll (proc-outlie-push-line ll last-line-b))
                   (setq ll (proc-outlie-check-lable ll search-uncommented-b-func))
                   )
                  ((string= ms "BLOCK")
                   (setq ll (proc-outlie-push-line ll last-line-b))
                   (setq ll (proc-outlie-check-lable ll search-uncommented-b-func))
                   )
                  (t
                   (setq ll (proc-outlie-push-line ll last-line-b))
                   ))

            ))))

    (setq buf (get-buffer-create "*Block Traceback*"))

    (with-current-buffer buf
      (font-lock-mode 0)
      (protel-mode)
      (proc-outline-mode 1)

      (make-local-variable 'proc-outline-buffer-file-name)
      (setq proc-outline-buffer-file-name f-name)

      (setq buffer-read-only t)
      (with-modify-in-readonly
          (erase-buffer)
        (insert "Proc: ")
        (when proc
          (setq pos (point))
          (insert proc)
          (put-text-property pos (point)
                             'face 'codepilot-hl-text-face)
          (insert " - How did I reach the point?")
          (put-text-property (line-beginning-position) (line-end-position)
                             'proc-outline-target (second proc-and-pos))
          )

        (insert "\n")
        (dolist (i ll)
          (insert (car i))
          (put-text-property (line-beginning-position) (line-end-position)
                             'proc-outline-target (second i))
          (insert "\n")
          )
        (goto-char (point-max))
        (forward-line -1)
        (setq to-pos (point))
        ;; (myimenu-hl-text (line-beginning-position) (line-end-position))
        (mymisc-highlight-one-line-1)
        ))

    (save-selected-window
      (proc-outline-pop-to-buffer buf)
      (goto-char to-pos)
      (recenter -1)
      ;; (shrink-window-if-larger-than-buffer)
      ;; (fit-window-to-buffer (get-buffer-window buf) (/ (frame-height) 2))
      )
    ))

(defun proc-outline-update ()
  (let ((buf (current-buffer)))
    (cond ((and (eq major-mode 'protel-mode)
                (not (string= (buffer-name) "*Proc Outline*")))
           (when (get-buffer-window "*Proc Outline*")
             (protel-proc-outline)
             )))))

(defun proc-outline-auto-turn-on ()
  (interactive)
  (cancel-function-timers 'proc-outline-update)
  (run-with-idle-timer 0.5 t 'proc-outline-update)
  )

(defun proc-outline-auto-turn-off ()
  (interactive)
  (cancel-function-timers 'proc-outline-update)
  )



(defun proc-outline-which-procs-i-in ()
  (interactive)
  (let ((inhibit-which-func-update t)
        (proc "")
        (prev-pos -1)
        pnm mp ll begpt endpt p1 buf pos pt
        (txt codepilot-current-search-text)
        (s-type codepilot-current-search-type)
        (f-name (buffer-file-name))
        (b-pos (line-end-position))
        (to-pos (make-marker))
        line-pos
        (proc-count 0))

    (set-marker to-pos 1)

    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (not (string= "" codepilot-current-search-text))
          (while (codepilot-search-hl-again-f-2)
            (setq pnm (protel-which-func-2))
            (cond (pnm
                   (setq proc (car pnm))
                   (setq mp (second pnm))
                   )
                  (t
                   (setq proc "--Global---")
                   (setq mp nil)
                   ))
            (setq begpt (line-beginning-position))
            (setq endpt (line-end-position))
            (if (and (if (boundp 'jit-lock-mode) jit-lock-mode)
                     (text-property-not-all begpt endpt 'fontified t))
                (if (fboundp 'jit-lock-fontify-now)
                    (jit-lock-fontify-now begpt endpt)))
            (push (list proc mp (buffer-substring begpt endpt) (point))
                  ll)
            (forward-line)
            ))))

    (setq buf (get-buffer-create "*Which Procs*"))

    (when ll
      (with-current-buffer buf
        (cond
         ((and proc-outline-buffer-file-name
               proc-outline-buffer-search-text
               proc-outline-buffer-search-type
               (string= proc-outline-buffer-file-name f-name)
               (string= proc-outline-buffer-search-text txt)
               (string= proc-outline-buffer-search-type s-type)
               )
          ;; Highlight Corresponding line.
          (set-marker to-pos (proc-outline-highlight-line-cordingly b-pos))
          )
         (t
          (font-lock-mode 0)
          (protel-mode)
          (proc-outline-mode 1)

          (message "Generat Which Procs buffer")
          (make-local-variable 'proc-outline-buffer-file-name)
          (setq proc-outline-buffer-file-name f-name)

          (make-local-variable 'proc-outline-buffer-search-text)
          (setq proc-outline-buffer-search-text txt)

          (make-local-variable 'proc-outline-buffer-search-type)
          (setq proc-outline-buffer-search-type s-type)

          (setq buffer-read-only t)
          (with-modify-in-readonly
           (erase-buffer)

           (insert "Procs contain: ")
           (setq pos (point))
           (insert txt)
           (put-text-property pos (point) 'face 'codepilot-hl-text-face)

           (insert "\tType: " (prin1-to-string s-type) "\n\n")
           (dolist (i (nreverse ll))
             (setq mp (second i))

             (cond (mp
                    (unless (= prev-pos mp)
                      (insert "[" (car i) "]:")
                      (put-text-property (line-beginning-position) (line-end-position)
                                         'proc-outline-target mp)
                      (put-text-property (line-beginning-position) (line-end-position)
                                         'face 'font-lock-function-name-face)
                      (when (>= b-pos mp)
                        (set-marker to-pos (line-beginning-position)))
                      (insert "\n")
                      (setq prev-pos mp)
                      (setq proc-count (1+ proc-count))
                      ))
                   (t
                    (insert "[" (car i) "]:")
                    (put-text-property (line-beginning-position) (line-end-position)
                                       'face 'font-lock-function-name-face)
                    (insert "\n")
                    ))

             (insert "   |" (third i))

             (setq line-pos (fourth i))
             (when (>= b-pos line-pos)
               (set-marker to-pos (line-beginning-position))
               )
             (put-text-property (line-beginning-position) (line-end-position)
                                'proc-outline-target line-pos)

             (insert "\n")
             )
           (goto-char (point-min))
           (end-of-line)
           (insert "\tProc count: " (int-to-string proc-count))

           (goto-char to-pos)
           ;; (myimenu-hl-text (line-beginning-position) (line-end-position))
           (mymisc-highlight-one-line-1)
           (when (/= 0 (length txt))
             (my-highlight-3 txt s-type buf))
           )))))

    (save-selected-window
      (proc-outline-pop-to-buffer buf)
      (goto-char to-pos)
      ;; (recenter -1)
      ;; (fit-window-to-buffer (get-buffer-window buf) (/ (frame-height) 2))
      )
    ;; (set-window-point (get-buffer-window buf) to-pos)
    ))



(provide 'proc-outline)