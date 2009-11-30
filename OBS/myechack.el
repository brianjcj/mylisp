;; 2008-4-15
;; Brian Jiang

;; When the emacs frame was maximized and is currently minimized into
;; the TaskBar, if opening a file from emacsclient, the frame then is
;; restored down to non maximized status. This is not the behavior
;; that most of us expect. In this case, we want the emacs frame keep
;; in the maximized status when it is restore from minimized.

;; So? Hack Emacs a little to let it behave as we expect....

;; The best solution will be to change the C codes. e.g., procedure
;; x_make_frame_visible. When the frame is currently minimized, use
;; SW_RESTORE instead of SW_SHOWNORMAL to show the window.


;; (defun select-frame-set-input-focus (frame)
;;   "Select FRAME, raise it, and set input focus, if possible."
;;     (select-frame frame)

;;     ;; Brian: change 1/1
;;     ;; (raise-frame frame)
;;     (cond ((eq (cdr (assq 'visibility (frame-parameters))) t)
;;            (raise-frame frame))
;;           (t
;;            ;; SC_RESTORE
;;            (w32-send-sys-command 61728)
;;            ))
;;     ;; Brian: end of change 1/1
;;     (debug)
;;     ;; Ensure, if possible, that frame gets input focus.
;;     (cond ((memq window-system '(x mac))
;; 	   (x-focus-frame frame))
;; 	  ((eq window-system 'w32)
;; 	   (w32-focus-frame frame)))
;;     (cond (focus-follows-mouse
;; 	   (set-mouse-position (selected-frame) (1- (frame-width)) 0))))


(defadvice raise-frame (around raise-frame (&optional frame))
  (cond ((eq (cdr (assq 'visibility (frame-parameters frame))) t)
         ad-do-it)
        (t
         ;; SC_RESTORE
         (w32-send-sys-command 61728)
         )))

(ad-activate 'raise-frame)

(require 'server)



;; overide the function server-visit-files in server.el
(cond
 ((= 22 emacs-major-version)
  (defun server-visit-files (files client &optional nowait)
    "Find FILES and return the list CLIENT with the buffers nconc'd.
FILES is an alist whose elements are (FILENAME LINENUMBER COLUMNNUMBER).
NOWAIT non-nil means this client is not waiting for the results,
so don't mark these buffers specially, just visit them normally."
    ;; Bind last-nonmenu-event to force use of keyboard, not mouse, for queries.
    (let ((last-nonmenu-event t) client-record)
      ;; Restore the current buffer afterward, but not using save-excursion,
      ;; because we don't want to save point in this buffer
      ;; if it happens to be one of those specified by the server.
      (save-current-buffer
        (dolist (file files)
          ;; If there is an existing buffer modified or the file is
          ;; modified, revert it.  If there is an existing buffer with
          ;; deleted file, offer to write it.
          (let* ( ;; (minibuffer-auto-raise (or server-raise-frame  ;; brian
                 ;; minibuffer-auto-raise))                        ;; brian
                 (filen (car file))
                 (obuf (get-file-buffer filen)))
          
            (add-to-history 'file-name-history filen)
            (if (and obuf (set-buffer obuf))
                (progn
                  (cond ((file-exists-p filen)
                         (when (not (verify-visited-file-modtime obuf))
                           ;; brian, hack...
                           (when (and server-raise-frame
                                      (not (eq (cdr (assq 'visibility (frame-parameters))) t)))
                             (w32-send-sys-command 61728)
                             (sit-for 0.1))
                           (let ((minibuffer-auto-raise (or server-raise-frame minibuffer-auto-raise)))
                             (revert-buffer t nil))))
                        (t
                         ;; brian, hack...
                         (when (and server-raise-frame
                                    (not (eq (cdr (assq 'visibility (frame-parameters))) t)))
                           (w32-send-sys-command 61728)
                           (sit-for 0.1))
                         (let ((minibuffer-auto-raise (or server-raise-frame minibuffer-auto-raise)))
                           (when (y-or-n-p
                                  (concat "File no longer exists: "
                                          filen
                                          ", write buffer to file? "))
                             (write-file filen)))))
                  (setq server-existing-buffer t)
                  (server-goto-line-column file))
              (set-buffer (find-file-noselect filen))
              (server-goto-line-column file)
              (run-hooks 'server-visit-hook)))
          (unless nowait
            ;; When the buffer is killed, inform the clients.
            (add-hook 'kill-buffer-hook 'server-kill-buffer nil t)
            (push (car client) server-buffer-clients))
          (push (current-buffer) client-record)))
      (nconc client client-record))))
 ((= 23 emacs-major-version)
  ;; 2008-4-16 CVS
  (defun server-visit-files (files proc &optional nowait)
    "Find FILES and return a list of buffers created.
FILES is an alist whose elements are (FILENAME . FILEPOS)
where FILEPOS can be nil or a pair (LINENUMBER . COLUMNNUMBER).
PROC is the client that requested this operation.
NOWAIT non-nil means this client is not waiting for the results,
so don't mark these buffers specially, just visit them normally."
    ;; Bind last-nonmenu-event to force use of keyboard, not mouse, for queries.
    (let ((last-nonmenu-event t) client-record)
      ;; Restore the current buffer afterward, but not using save-excursion,
      ;; because we don't want to save point in this buffer
      ;; if it happens to be one of those specified by the server.
      (save-current-buffer
        (dolist (file files)
          ;; If there is an existing buffer modified or the file is
          ;; modified, revert it.  If there is an existing buffer with
          ;; deleted file, offer to write it.
          (let* ( ;; (minibuffer-auto-raise (or server-raise-frame      ;; brian
                 ;;                           minibuffer-auto-raise))  ;; brian
                 (filen (car file))
                 (obuf (get-file-buffer filen)))
            (add-to-history 'file-name-history filen)
            (if (null obuf)
                (set-buffer (find-file-noselect filen))
              (set-buffer obuf)
              (cond ((file-exists-p filen)
                     (when (not (verify-visited-file-modtime obuf))
                       ;; brian
                       (when (and server-raise-frame
                                  (not (eq (cdr (assq 'visibility (frame-parameters))) t)))
                         (w32-send-sys-command 61728)
                         (sit-for 0.1))
                       ;; brian
                       (let ((minibuffer-auto-raise (or server-raise-frame minibuffer-auto-raise)))
                         (revert-buffer t nil))))
                    (t
                     ;; brian
                     (when (and server-raise-frame
                                (not (eq (cdr (assq 'visibility (frame-parameters))) t)))
                       (w32-send-sys-command 61728)
                       (sit-for 0.1))
                     ;; brian
                     (let ((minibuffer-auto-raise (or server-raise-frame minibuffer-auto-raise)))
                       (when (y-or-n-p
                              (concat "File no longer exists: " filen
                                      ", write buffer to file? "))
                         (write-file filen)))))
              (unless server-buffer-clients
                (setq server-existing-buffer t)))
            (server-goto-line-column (cdr file))
            (run-hooks 'server-visit-hook))
          (unless nowait
            ;; When the buffer is killed, inform the clients.
            (add-hook 'kill-buffer-hook 'server-kill-buffer nil t)
            (push proc server-buffer-clients))
          (push (current-buffer) client-record)))
      (unless nowait
        (process-put proc 'buffers
                     (nconc (process-get proc 'buffers) client-record)))
      client-record))
  ))


