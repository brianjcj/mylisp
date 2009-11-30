;;; pls.el --- 

;; Authors: Brian Jiang
;; Maintainer: Brian Jiang <brianjiang AT gdnt DOT com DOT cn>
;; Keywords: language protel

;; Keywords: language protel

;; $Revision: 0.1+ $  2007.08.11
;;
;;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some codes are copied from the official protel mode and refined.
;;

(require 'easymenu)
(require 'comint)

(provide 'pls)

(defvar emacs-dir nil)
(setq emacs-dir (file-name-directory (directory-file-name exec-directory)))


(defvar plink-program "plink")
;; (defvar plink-program (concat emacs-dir "mybin/plink"))

(defun unix-account-set (symbol value)
  (set-default symbol value)
  (setq ange-ftp-default-account value)
  (setq ange-ftp-default-user value))

(defgroup pls nil
  "Configure for PLS."
  :group 'programming)


(defcustom unix-account "brianjcj"
  "Unix Account."
  :type 'string
  :set 'unix-account-set
  :group 'pls)

(defun unix-pwd-set (symbol value)
  (let (v)
    (cond ((string= value (make-string (length value) ?*))
           (setq v unix-pwd)
           )
          (t
           (setq v value)
           ))
    (set-default symbol v)
    (setq ange-ftp-default-password v))
  )

(defun unix-pwd-get (symbol)
  (make-string (length (default-value 'unix-pwd)) ?*)
  )

(defcustom unix-pwd "zaq12WSX"
  "Unix Password."
  :type 'string
  :get 'unix-pwd-get
  :set 'unix-pwd-set
  :group 'pls)

(defcustom unix-host "202.38.33.94"
  "Unix Host."
  :type 'string
  :group 'pls)

(defcustom tempdir "/home/brianjcj/blank/tempdir"
  "Tempdir for PLS. Make sure it exists before starting PLS."
  :type 'string
  :set-after '(unix-account)
  :group 'pls)

(defcustom permdir "/home/brianjcj/blank/permdir"
  "Permdir for PLS. Make sure it exists before starting PLS."
  :type 'string
  :set-after '(unix-account)
  :group 'pls)


(defvar pls-local-source-dir-default "d:/pls-source")
(defun pls-local-source-dir-set (symbol value)
  (cond ((and (boundp 'codepilot-listing-dir)
          (string= (file-name-as-directory value) (file-name-as-directory codepilot-listing-dir)))
         (error "*ERROR* The codepilot listing dir cannot be set to the same as PLS local source dir.")
         )
        ((string-match " " value)
         (error "The dir path cannot contains spaces since WIN FTP program doesn't support it.")
         )
        (t
         (set-default symbol value)
         (unless (file-directory-p pls-local-source-dir)
           (make-directory pls-local-source-dir :parents)
           )
         )))

;; (concat emacs-dir "pls-source")
(defcustom pls-local-source-dir pls-local-source-dir-default
  "Local source dir to put the file from PLS/FTP."
  :type 'directory
  :set 'pls-local-source-dir-set
  :group 'pls)


;;  '(ange-ftp-default-account "brianjcj")
;;  '(ange-ftp-default-password "sdsds")
;;  '(ange-ftp-default-user "brianjcj")


(defcustom unix-prompt-regexp "^[0-9]*> \\|^/.*>"
  "The regular expression for the unix problem. Don't touch it unless it
doesn't work for you."
  :type 'string
  :group 'pls)



(defvar pls-buffer "*pls*"
  "*This variable gives the name of the active PLS buffer. See also
`pls-supported-libraries'.")

(defvar pls-prompt-regexp "PLS(.*).*> \\|PLS(.*): \\|[0-9]*> "
  "*This variable gives the pattern to match for PLS prompt. A single
trailing space is recommended so that C-c C-a will bring the cursor to
the true beginning of the command.")

(defvar pls-pure-prompt-regexp "^PLS(dmspl): ")
;; (defvar unix-prompt-regexp "^[0-9]*> ")

(defvar pls-section-regexp "[A-Za-z][A-Za-z0-9_]\\{2,9\\}")
(defvar pls-se-regexp (concat "^\\( \\{14\\}\\|        SECT: \\)\\(" pls-section-regexp "\\) +\\(GI\\|IM\\) "))
(defvar pls-issue-regexp "[A-Za-z]\\{2\\}[0-9]\\{2\\}")
(defvar pls-update-regexp "\\([A-Za-z][A-Za-z0-9]*\\.[0-9]+\\)")
(defvar pls-issue-line-regexp (concat "^     [\\* ] \\(" pls-issue-regexp "\\) " ))
(defvar pls-module-line-regexp (concat "^   \\*MODULE: \\(" pls-section-regexp "\\) "))
(defvar pls-mod-ver-st-regexp
      (concat "^        \\(" pls-section-regexp "\\) +\\("
              pls-issue-regexp "\\)  \\(CLOSED\\|OPEN\\|OBSOL\\)"))
(defvar pls-sdelta-regexp (concat "^            ->.*\\<\\(" pls-section-regexp "\\)\\.sdelta"))


(defvar pls-mode-map nil
  "keymap for pls mode")

(defvar pls-mode-syntax-table nil
  "Syntax table in use in PLS buffers.")

(let ((table (make-syntax-table)))
  (modify-syntax-entry ?_ "w" table)
  (modify-syntax-entry ?$ "w" table)
  (modify-syntax-entry ?. "." table)
  (setq pls-mode-syntax-table table))



(defvar pls-wait-for-cmd nil)


(defvar pls-error nil)


(defun pls-sync-tempdir (dummy)
  ""
  (let ((case-fold-search t)
        dir)
    (save-excursion
      (save-match-data
        (goto-char comint-last-input-end)
        (if (re-search-forward "^tempdir \\(.*\\)" nil t)
            (progn
              (setq dir (match-string 1))
              (unless (re-search-forward "\\*\\*\\* ERROR \\*\\*\\*" nil t)
                (setq tempdir dir)
                (message (concat "tempdir " dir))
                )))
        
        ))
    ))

(defun pls-sync-permdir (dummy)
  ""
  (interactive)
  (let ((case-fold-search t)
        dir)
    (save-excursion
      (save-match-data
        (goto-char comint-last-input-end)
        (if (re-search-forward "^permdir \\(.*\\)" nil t)
            (progn
              (setq dir (match-string 1))
              (unless (re-search-forward "\\*\\*\\* ERROR \\*\\*\\*" nil t)
                (setq permdir dir)
                (message (concat "permdir " dir))
                )))
        ))))

(defun pls-catch-setting-commands (str)
  ""
  (let ((case-fold-search t)
        dir)
    (cond ((string-match "^[ /t/f]*tempdir[ /t/f]+" str)
           (message "tempdir command input!")
           (pls-push-handler-on-stack 'pls-sync-tempdir nil)
           )
          ((string-match "^[ /t/f]*permdir[ /t/f]+" str)
           (message "permdir command input!")
           (pls-push-handler-on-stack 'pls-sync-permdir nil)
           )
          (t
           nil)
          )
    )
  t ;; this is to cause the next handler to run
  )

(defun pls-unix-comint-output-filter (str)
  "Check if a command has finished"
  (let ((case-fold-search t)
        ;dir
        max-pos
        pos
        )
    (save-match-data
      (save-excursion
        (setq max-pos (point-max))
        (goto-char max-pos)
        (forward-line 0)
        (setq pos (point))
        (when (< pos comint-last-input-end)
          (setq pos comint-last-input-end)
          (goto-char pos)
          )
        
        (when (re-search-forward unix-prompt-regexp nil t)
          ;; done!
          (pls-command-done)))
      )))

(defun pls-comint-output-filter (str)
  "Check if a command has finished"
  (let ((case-fold-search t)
        ;dir
        max-pos
        pos
        )
    (save-match-data
      (save-excursion
        (setq max-pos (point-max))
        (goto-char max-pos)
        (forward-line 0)
        (setq pos (point))
        (when (< pos comint-last-input-end)
          (setq pos comint-last-input-end)
          (goto-char pos)
          )
        
        (when (re-search-forward pls-pure-prompt-regexp nil t)
          ;; check if error occurs.
          (goto-char comint-last-input-end)
          (if (re-search-forward "Syntax error:\\|^\\*\\*\\* ERROR \\*\\*\\*" nil t)
              (progn
                (setq pls-error t)
                ))
          ;; done!
          (pls-command-done)))
      )))

(defun pls-term-comint-output-filter (str)
  "If encounting Kill is Ctrl-U, send a blank command to let it go."
  (let ((case-fold-search t)
        )
    (save-match-data
      (save-excursion
        (goto-char comint-last-input-end)
        (when (re-search-forward "^Kill is Ctrl-U" nil t)
          (comint-send-string (pls-proc) "\n")
          (with-current-buffer pls-buffer
            (remove-hook 'comint-output-filter-functions 'pls-term-comint-output-filter t)))))))

(defun pls-get-permdir ()
  ""
  (interactive)
  (let ((case-fold-search t))
    (save-excursion
      (save-match-data
        (pls-send-wait "permdir")
        (goto-char comint-last-input-end)
        (if (re-search-forward "^permdir$" nil t)
            (progn
              (forward-line)
              (when (looking-at "^\\(.*\\)$")
                (setq permdir (match-string 1))
                (message (concat "permdir " permdir))
                )))))))

(defun pls-get-tempdir ()
  ""
  (interactive)
  (let ((case-fold-search t))
    (save-excursion
      (save-match-data
        (pls-send-wait "tempdir")
        (goto-char comint-last-input-end)
        (if (re-search-forward "^tempdir$" nil t)
            (progn
              (forward-line)
              (looking-at "^\\(.*\\)$")
              (setq tempdir (match-string 1))
              (message (concat "tempdir " tempdir))
              ))))))

(defun pls-proc-check (buf)
  (and (get-buffer buf)
       (comint-check-proc buf))
  )

(defun pls-active ()
  (and (get-buffer pls-buffer)
       (comint-check-proc pls-buffer)))


(defun pls-mode ()
  "Put the current buffer into PLS mode"
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq major-mode 'pls-mode)
  (setq mode-name "PLS")
  (use-local-map pls-mode-map)
  (setq comint-prompt-regexp pls-prompt-regexp)
  (setq comint-scroll-to-bottom-on-output 'all)
  (setq case-fold-search t)
  (set-syntax-table pls-mode-syntax-table)
  (add-hook 'comint-output-filter-functions 'pls-comint-output-filter t t)
  (add-hook 'comint-input-filter-functions 'pls-catch-setting-commands t t)
  (pls-font-lock-setup)
  (run-hooks 'pls-mode-hook))

;; Do not maintain this function any more. 
;; ;; start pls and wait until finished.
;; (defun pls-start-wait ()
;;   ""
;;   (interactive)
  
;;   (unless (file-directory-p pls-local-source-dir)
;;     (make-directory pls-local-source-dir :parents)
;;     )

;;   (if (pls-proc-check pls-buffer)
;;       (switch-to-buffer pls-buffer)

;;     ;; start it.
;;     (make-comint "pls" "plink" nil (concat unix-account "@" unix-host) "-pw" unix-pwd)
;;     (set-buffer pls-buffer)

;;     ;; reset the command
;;     (setq next-pls-handlers nil)
;;     (setq next-pls-handlers-args nil)
    
;;     (pls-mode)
;;     (pls-send-wait "    \n")
;;     (pls-send-wait "    \n")
;;     (pls-send-wait "banner PLS")
;;     (pls-send-wait "pwd")
;;     ;;       (pls-send-wait (concat "mkdir " permdir))
;;     ;;       (pls-send-wait (concat "mkdir " tempdir))
;;     (pls-send-wait "setenv PAGER cat")
;;     (pls-send-wait (concat "pls dmspl permdir=" permdir " tempdir=" tempdir))
;;     (pls-get-permdir)
;;     (pls-get-tempdir)
;;     (switch-to-buffer pls-buffer)
;;     ))

(defun pls-proc()
  ""
  (get-buffer-process pls-buffer)
  )


;; (defun pls-send-string (string)
;;   "Evaluate STRING in inferior Pls process."
;;   (interactive "sCommand: ")
;;   (comint-send-string (pls-proc) string)
;;   (unless (string-match "\n\\'" string)
;;     ;; Make sure the text is properly LF-terminated.
;;     (comint-send-string (pls-proc) "\n"))
;;   (when (string-match "\n[ \t].*\n?\\'" string)
;;     ;; If the string contains a final indented line, add a second newline so
;;     ;; as to make sure we terminate the multiline instruction.
;;     (comint-send-string (pls-proc) "\n")))


(defun pls-string-input (string)
  ""
  (interactive "sCommand: ")
  (set-buffer pls-buffer)
  (goto-char (point-max))
  (comint-kill-input)
  (insert string)
  ;; use comint-send-string which can update comint-last-input-end!!!
  (setq pls-wait-for-cmd t)
  (comint-send-input)
  )

(defun pls-send-wait (string &optional time)
  ""
  (let ((proc (pls-proc)))
    (with-current-buffer (process-buffer proc)
      (setq pls-error nil)
      (pls-string-input string)
      (while (progn
               (accept-process-output proc (if time time 0.1))
               pls-wait-for-cmd
               ))
      pls-error
      )))

(defun pls-tempdir-path ()
  ""
  (concat "/" unix-account "@" unix-host ":" tempdir )
  ;(concat "/" unix-host ":" tempdir )
  )

(defun pls-copy-file-to-local (ftp-filename local-filename &optional ok-if-already-exists)
  ""
  (let ((ok nil))
    (catch 'ftp-error
      (let ((ange-ftp-waiting-flag t)
            )
        (copy-file ftp-filename local-filename ok-if-already-exists)
        (setq ok t)
        ))
    (unless ok
      (when (file-readable-p local-filename)
        (delete-file local-filename)
        (message "Error encounter. Unreliable local copy deleted. Please check the PLS connection status.")))
    ok
    ))

(defun pls-re-retrieve-and-open-section-noselect (section)
  ""
  (setq section (downcase section))
  (let (buf
        (ftp-filename (concat (file-name-as-directory (pls-tempdir-path)) section ))
        (local-filename (concat (file-name-as-directory pls-local-source-dir) section))
        copy-ok
        )
    
    ;; brian: for lazy highlight
    (setq isearch-lazy-highlight-last-string nil)
        
    (if (pls-send-wait (concat "gets " section " file " section))
        (error "Command failed!")
             
        (condition-case err
            (setq copy-ok (pls-copy-file-to-local ftp-filename local-filename :ok-if-already-exists))
          (error          ;error
           ;; Display the usual message for this error.
           (message "%s" (error-message-string err))
             
           ;; Most of time it failed due to ftp connection time out.
           ;; Try one more time.
           (message "Try one more time.")
           (setq copy-ok (pls-copy-file-to-local ftp-filename local-filename :ok-if-already-exists))
           ))
        (when copy-ok
          (setq buf (find-file-noselect local-filename))))
        
    buf))

(defun pls-open-section-noselect (section)
  ""
  (setq section (downcase section))
  (let (buf
        (ftp-filename (concat (file-name-as-directory (pls-tempdir-path)) section ))
        (local-filename (concat (file-name-as-directory pls-local-source-dir) section))
        copy-ok
        )
    (setq buf (get-file-buffer local-filename))
    (unless buf

      ;; brian: for lazy highlight
      (setq isearch-lazy-highlight-last-string nil)
      
      (if (file-readable-p local-filename)
          (progn
            ; open it locally
            (setq buf (find-file-noselect local-filename))
            (message (concat "File already exited in " pls-local-source-dir ". Just open it."))
            )
        (if (pls-send-wait (concat "gets " section " file " section))
            (error "Command failed!")
             
          (condition-case err
              (setq copy-ok (pls-copy-file-to-local ftp-filename local-filename))
            (error ;error
             ;; Display the usual message for this error.
             (message "%s" (error-message-string err))
             
             ;; Most of time it failed due to ftp connection time out.
             ;; Try one more time.
             (message "Try one more time.")
             (setq copy-ok (pls-copy-file-to-local ftp-filename local-filename))
             ))
          (when copy-ok
            (setq buf (find-file-noselect local-filename))
            )
          )
        ))
    buf
    ))

(defun pls-open-section-noselect-1 (section)
  ""
  (setq section (downcase section))
  (let (buf
        (filename (concat (file-name-as-directory (pls-tempdir-path)) section )))
    (setq buf (get-file-buffer filename))
    (unless buf
      (if (pls-send-wait (concat "gets " section " file " section))
          (error "Command failed!")
        (setq buf (find-file-noselect filename))
      ))
    buf
    ))


(defun pls-open-section (section &optional pop)
  ""
  (interactive "sSection: ")
  (setq section (downcase section))
  (let ((buf (pls-open-section-noselect-1 section)))
    (if pop
        (pop-to-buffer buf)
      (switch-to-buffer buf)
      )))

(defun pls-get-section-issue (section)
  ""
  (interactive)
  (let ((case-fold-search t)
        issue)
      (save-match-data
        (save-excursion
          (if (pls-send-wait (concat "l c " section) 0.01)
              (error "Command failed!")
            (set-buffer pls-buffer)
            (save-excursion
                                        ;(goto-char comint-last-input-end)
              (goto-char (point-max))
              (forward-line -1)
              (when (looking-at "^NIL element list")
                (error "Section not in context or context is not established in PLS.")
                )
              (forward-word)
            (setq issue (current-word))
            ))))
      issue
      ))

(defun pls-open-section-from-context-noselect (section)
  ""
  (interactive "sSection: ")

  (let (issue)
    (setq section (downcase section))
    (setq issue (pls-get-section-issue section))
    (pls-open-section-noselect (concat section "." issue))
  ))

;; Async PLS command

(defvar next-pls-handlers nil
  "This variable holds the handlers to be invoked once the PLS prompt is
back in order to complete a function requested by the user. The handler of
the outermost called function is at bottom of the stack and the handler of
the innermost nested called function is at the top of the stack. When
the pls-command-done is invoked, the stack is poped and the handler
of the innermost called function is executed first. Refer to pls-command-
done for details.")

(defvar next-pls-handlers-args nil
  "This variable holds the arguments of the corresponding handler.")

(defvar last-pls-command nil
  "This variable holds the last command string sent to the active PLS
buffer, i.e., `pls-buffer'.")


(defun pls-push-handler-on-stack (handler args)
  (push handler next-pls-handlers)
  (push args next-pls-handlers-args)
  )

(defun pls-pop-handler-from-stack ()
  (let (handler args)
    (setq handler (pop next-pls-handlers))
    (setq args (pop next-pls-handlers-args))
    (list handler args)
  ))

(defun pls-append-handler-on-stack (handler args)
  (if next-pls-handlers
      (progn
        (nconc next-pls-handlers (list handler))
        (nconc next-pls-handlers-args (list args)))
    (push handler next-pls-handlers)
    (push args next-pls-handlers-args)
    ))


(defun pls-command-done ()
  "Call the command done handlers to continue executing the function
requested by the user. The handler on top of stack is popped and executed
first as illustrated in the figure below (Fa calls Fb; Fb calls Fc;...
Ha, Hb,... are handlers associated with Fa, Fb,...):

   Function calls (nested)   Resulted stack
   -----------------------   --------------

   Fa --> Fb --> Fc                Hc   <----- Top of stack
                                   Hb
                                   Ha

Each handler must return a non-nil value in order to execute the next one
on the handler stack. If nil is returned, this function is exited and the
handler stack remains intact. This allows the current handler to be
replaced by a new one(s) without affecting other handlers (previously pushed
onto the stack) waiting to be executed. The last handler, if known, should
reset the next-pls-handlers and next-pls-handlers-args stacks.

See also the variable inter-pls-handlers-message."
  (let* ((last-cmd (and (not next-pls-handlers) last-pls-command))
         (continue t)
         (prev-buf (current-buffer))
         next-cmd next-cmd-args curr-buf)
    ;(setq waiting-for-pls nil)
    (setq pls-wait-for-cmd nil)
    (setq last-pls-command nil)
    (if next-pls-handlers
        (progn
          (setq next-cmd (pop next-pls-handlers))
          (setq next-cmd-args (pop next-pls-handlers-args))
          (condition-case nil
              (progn
                ;; this while loop is protected, i.e., the clean up forms are
                ;; evaluated if an error occurs during execution of handlers
                (while (and next-cmd continue)
                  (setq continue (funcall next-cmd next-cmd-args))
                  (if continue
                      (progn
                        (setq next-cmd-args (pop next-pls-handlers-args))
                        (setq next-cmd (pop next-pls-handlers))
                        ))
                  )
                ;; Since this function is now running inside the context of
                ;; comint-output-filter-functions, the current buffer set in
                ;; the last handler is not truly in effect, the reason is that
                ;; comint-output-filter-functions restores the previous buffer
                ;; stored in the local variable obuf. The kludge below is to
                ;; override obuf to contain the last active buffer.
;;                 (setq curr-buf (current-buffer))
;;                 (if (not (eq prev-buf curr-buf))
;;                     (setq obuf curr-buf))
                )
            ;; an error was signaled by one of the handlers,
            ;; clean up and exit
            (error (progn
                     ;(setq starting-pls-supported-lib nil)
                     (setq next-pls-handlers nil)
                     (setq next-pls-handlers-args nil)))
            )
        ))
    (if last-cmd
        ;; the last command sent to PLS buffer has terminated.
        (message (concat last-cmd " done."))
      )
  ))

(defun pls-remove-unix-hook (dummy)
  ""
  (with-current-buffer pls-buffer
    (remove-hook 'comint-output-filter-functions 'pls-unix-comint-output-filter t)))

(defun pls-remove-term-hook (dummy)
  ""
  (with-current-buffer pls-buffer
    (remove-hook 'comint-output-filter-functions 'pls-term-comint-output-filter t)))

(defun pls-switch-to-buffer (buffer)
  (switch-to-buffer buffer))

(defun pls-start-async ()
  ""
  (interactive)
  (unless (file-directory-p pls-local-source-dir)
    (make-directory pls-local-source-dir :parents)
    )

  (if (pls-proc-check pls-buffer)
      (pls-switch-to-buffer pls-buffer)

    ;; start it.

    ;; brian: change for Emacs-22.2, for double prompts issue.
    ;; (make-comint "pls" "plink" nil (concat unix-account "@" unix-host) "-ssh" "-pw" unix-pwd)
    (make-comint "pls" "cmd")
    (set-buffer pls-buffer)
    
    (pls-string-input (concat plink-program " " unix-account "@" unix-host " -ssh -pw " unix-pwd))
    (pls-mode)


    (setq next-pls-handlers nil)
    (setq next-pls-handlers-args nil)

    ;; mount unix hook for execute unix command
    (with-current-buffer pls-buffer
      (add-hook 'comint-output-filter-functions 'pls-unix-comint-output-filter t t))

    ;; deal with the term.
    (with-current-buffer pls-buffer
      (add-hook 'comint-output-filter-functions 'pls-term-comint-output-filter t t))
    
    (pls-append-handler-on-stack 'pls-string-input "   ")
    (pls-append-handler-on-stack 'pls-string-input "banner PLS")
    (pls-append-handler-on-stack 'pls-string-input "pwd")
    ;;       (pls-append-handler-on-stack 'pls-string-input (concat "mkdir " permdir))
    ;;       (pls-append-handler-on-stack 'pls-string-input (concat "mkdir " tempdir))
    (pls-append-handler-on-stack 'pls-string-input "setenv PAGER cat")

    ;; unmount unix hook
    (pls-append-handler-on-stack 'pls-remove-unix-hook nil)

    ;; unmount term hook if it still there.
    (pls-append-handler-on-stack 'pls-remove-term-hook nil)
    
    (pls-append-handler-on-stack 'pls-string-input (concat "pls dmspl permdir=" permdir " tempdir=" tempdir))
    (pls-append-handler-on-stack 'pls-string-input (concat "permdir " permdir))
    (pls-append-handler-on-stack 'pls-string-input (concat "tempdir " tempdir))
    (pls-switch-to-buffer pls-buffer)
    ))

(defalias 'pls 'pls-start-async)

(defvar pls-font-lock-keywords
  (list
   (list (concat "\\(\\<" pls-section-regexp ".sdelta\\>\\)")
         '(1 font-lock-warning-face))
   
   (list (concat "\\(" pls-prompt-regexp "\\)[ \t]*\\(.*\\)$")
         '(1 font-lock-keyword-face)
         '(2 font-lock-function-name-face)
         )
   (list (concat "\\(\\<" pls-issue-regexp "\\>\\)")
         '(1 font-lock-keyword-face)
         )
   (list (concat "\\(\\<" pls-update-regexp "\\>\\)")
         '(1 font-lock-keyword-face)
   )
   )
  "font-lock keywords setting for PLS buffers.")

(defun pls-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pls-font-lock-keywords t))
  )


(defun pls-do-what ()
  ""
  (interactive)
  (let ((section "")
        (issue "")
        (module "")
	(update "")
        (pos (point))
        (quit nil)
        (cur-wd "")
        )
    (save-match-data
      (save-excursion
        ;; check update
        (unless (looking-at "\\<")
          (backward-word)
          (if (looking-at "[0-9]")
              (backward-word)
              )
          (when (looking-at (concat "\\<" pls-update-regexp "\\>"))
            (setq update (match-string 1))
            (pls-string-input (concat "l " update))
            (setq quit t)
            )))
      (unless quit
        (save-excursion
          (forward-line 0)
          (cond ((looking-at pls-se-regexp)
                 (setq section (match-string 2))
                 (when (re-search-backward pls-issue-line-regexp nil t)
                   (setq issue (match-string 1))
                   (pls-open-section (concat section "." issue))
                   )
                 )
                ((looking-at pls-issue-line-regexp)
                 (setq issue (match-string 1))
                 (goto-char pos)
                 (unless (looking-at "\\<")
                   (backward-word)
                   )
                 (cond ((looking-at pls-issue-regexp)
                        ;; Point in issue text, l module.issue se
                        ;; Find the module name
                        (when (re-search-backward pls-module-line-regexp nil t)
                          (setq module (match-string 1))
                          (pls-string-input (concat "l " module "." issue " se"))
                          )
                        )
                       (t
                        (if (looking-at "[0-9]")
                            (backward-word)
                            )
                        (when (looking-at (concat "\\<" pls-update-regexp "\\>"))
                          (setq update (match-string 1))
                          (pls-string-input (concat "l " update))
                          ))))
                ((looking-at pls-mod-ver-st-regexp)
                 (setq module (match-string 1))
                 (setq issue (match-string 2))
                 (goto-char pos)
                 (setq cur-wd (current-word))
                 (cond ((string= module cur-wd)
                        (pls-string-input (concat "l " module " i"))
                        )
                       ((string= issue cur-wd)
                        (pls-string-input (concat "l " module "." issue " se"))
                        )
                       (t
                        (pls-string-input (concat "x sd_2lxxx m " module "." issue))
                        )))
                ((looking-at pls-module-line-regexp)
                 (setq module (match-string 1))
                 (pls-string-input (concat "l " module " i"))
                 )
                ((looking-at pls-sdelta-regexp)
                 (setq section (match-string 1))
                 (pls-find-file (concat (file-name-as-directory (pls-tempdir-path)) section ".sdelta"))
                 (toggle-truncate-lines 1)
                 )
                (t (message "Sorry...."))
                ))))))

(defun pls-find-file (filename)
  ""
  (find-file filename)
  )

(defun pls-do-what-mouse (event)
  ""
  (interactive "e")
  (mouse-set-point event)
  (pls-do-what)
  )

(let ((map (copy-keymap comint-mode-map)))
    (setq pls-mode-map map))

(define-key  pls-mode-map [mouse-3] 'pls-do-what-mouse)

(defun pls-goto-me()
  ""
  (interactive)
  (pls-switch-to-buffer pls-buffer))

(defun pls-sdelta ()
  ""
  (interactive)
  (let ((module "")
        (issue "")
        )
    (save-match-data
      (save-excursion
          (forward-line 0)
          (when (looking-at pls-mod-ver-st-regexp)
            (setq module (match-string 1))
            (setq issue (match-string 2))
            (pls-string-input (concat "x sd_2lxxx m " module "." issue))
            )))))

(defun pls-dired()
  ""
  (interactive)
  (dired (pls-tempdir-path)))


(defun pls-permdir-path()
  ""
  (concat "/" unix-account "@" unix-host ":" permdir ))

(defun pls-dired-perm()
  ""
  (interactive)
  (dired (pls-permdir-path)))


(defvar pls-menu nil
  "PLS Menu")

(setq pls-menu
      '("PLS"
        :active (pls-active)
        ["Go to PLS" pls-goto-me (pls-active)]
        "-"
        ["Open Tempdir" pls-dired t]
        ["Open Permdir" pls-dired-perm t]
        "-"
        ["PLS Command" pls-string-input t]
        ["Open Section" pls-open-section t]
        "-"
        )
      )

;; (easy-menu-define protel-menu-symbol
;;                   pls-mode-map
;;                   "PLS menu"
;;                   pls-menu)

(easy-menu-define protel-menu-symbol
  (current-global-map)
  "PLS menu"
  pls-menu)


(defun pls-change-tempdir ()
  ""
  (interactive)
  (message "Not implemented.")
  )

(defun pls-change-permdir ()
  ""
  (interactive)
  (message "Not implemented.")
  )


(defun pls-output (&optional delete)
  "Return the output of the last PLS command.
The shell output is discarded if delete is non-nil."
  (if (get-buffer pls-buffer)
      (let ((case-fold-search t)
            beg end)
        (save-excursion
          (set-buffer pls-buffer)
          (goto-char comint-last-input-end)
          (forward-line)
          (setq beg (point))
          (setq end
                (save-excursion
                  (goto-char (point-max))
                  (re-search-backward pls-pure-prompt-regexp)
                  (backward-char)
                  (point)))
          (prog1
              (buffer-substring-no-properties beg end)
            (if delete (comint-delete-output)))))
    nil))