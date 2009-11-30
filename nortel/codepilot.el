;;; codepilot.el ---

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
(require 'pls)
(require 'protel)
(require 'remember)
(require 'outline)
(require 'ange-ftp)
(require 'mymiscfunc)
(require 'myfilter)

(require 'codepilot-base)

(provide 'codepilot)


(defalias 'protel-search-and-hl-text 'codepilot-search-and-hl-text)
(defalias 'protel-search-hl-again-f 'codepilot-search-hl-again-f)
(defalias 'protel-search-hl-again-b 'codepilot-search-hl-again-b)

;; (defcustom codepilot-default-context "ppc mtx00015"
;;   ""
;;   :type 'string
;;   :group 'codepilot)


(defvar codepilot-default-context "ppc mtx00014")

(defgroup codepilot-simple nil
  "Configure for codepilot java client. Only the frequently change options are provide
here. If you need to configure the other options, please go to the .codepilot dir to
edit the XML dirrectly."
  :group 'programming
  )

(defcustom codepilot-dot-dir (concat (file-name-as-directory (getenv "USERPROFILE")) ".codepilot/")
  "The Dir to hold CodePilot XML files. Don't touch it unless it is set incorrectly
by the system."
  :type 'directory
  :group 'codepilot)

(defvar PasswordSettings-fmt
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<PASSWORD_VALUE><PASSWORD>%s</PASSWORD></PASSWORD_VALUE>")

(defvar UserNameSettings-fmt
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<USERNAME_VALUE><USERNAME>%s</USERNAME></USERNAME_VALUE>")

(defvar ListingDirSettings-fmt
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<LISTINGDIR_VALUE><LISTINGDIR>%s</LISTINGDIR></LISTINGDIR_VALUE>")

(defvar ProductSettings-fmt
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<PRODUCT_VALUE><PRODUCT>%s</PRODUCT></PRODUCT_VALUE>")

(defvar PlatformSettings-fmt
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<PLATFORM_VALUE><PLATFORM>%s</PLATFORM></PLATFORM_VALUE>")

(defun codepilot-cnt-number-set (symbol value)
  (set-default symbol value)
  (message "\nGenerate codePilot.UserNameSettings xml file....")
  ;; (message UserNameSettings-fmt value)
  (with-temp-file  (concat (file-name-as-directory codepilot-dot-dir) "codePilot.UserNameSettings")
    (insert (format UserNameSettings-fmt value))))

(defun codepilot-cnt-password-set (symbol value)
  (let (v)
    (cond ((string= value (make-string (length value) ?*))
           (setq v cnt-password)
           )
          (t
           (setq v value)
           ))
    (set-default symbol v)
    (message "\nGenerate codePilot.PasswordSettings xml file....")
    (with-temp-file  (concat (file-name-as-directory codepilot-dot-dir) "codePilot.PasswordSettings")
      (insert (format PasswordSettings-fmt v)))))

(defun codepilot-cnt-password-get (symbol)
  (make-string (length (default-value 'cnt-password)) ?*))

(defun codepilot-listing-dir-set (symbol value)
  (cond ((string= (file-name-as-directory value) (file-name-as-directory pls-local-source-dir))
         (error "*ERROR* The codepilot listing dir cannot be set to the same as PLS local source dir.")
         )
        (t
         (set-default symbol value)
         (message "\nGenerate codePilot.ListingDirSettings xml file....")
         ;; (message ListingDirSettings-fmt value)
         (with-temp-file  (concat (file-name-as-directory codepilot-dot-dir) "codePilot.ListingDirSettings")
           (insert (format ListingDirSettings-fmt value))))))

(defun codepilot-default-product-set (symbol value)
  (set-default symbol value)
  (when (boundp 'codepilot-default-platform)
      (setq codepilot-default-context (concat codepilot-default-platform " " codepilot-default-product)))
  (message "\nGenerate codePilot.ProductSettings xml file....")
  (with-temp-file  (concat (file-name-as-directory codepilot-dot-dir) "codePilot.ProductSettings")
    (insert (format ProductSettings-fmt value))))

(defun codepilot-default-platform-set (symbol value)
  (set-default symbol value)
  (when (boundp 'codepilot-default-product)
      (setq codepilot-default-context (concat codepilot-default-platform " " codepilot-default-product)))
  (message "\nGenerate codePilot.PlatformSettings xml file....")
  (with-temp-file  (concat (file-name-as-directory codepilot-dot-dir) "codePilot.PlatformSettings")
    (insert (format PlatformSettings-fmt value))))

(defcustom cnt-number "cnt30448"
  "Norpass account."
  :type 'string
  :set 'codepilot-cnt-number-set
  :set-after '(codepilot-dot-dir)
  :group 'codepilot-simple)

(defcustom cnt-password "yourpasswd..."
  "Norpass passowrd."
  :type 'string
  :set 'codepilot-cnt-password-set
  :get 'codepilot-cnt-password-get
  :set-after '(codepilot-dot-dir)
  :group 'codepilot-simple)

(defcustom codepilot-listing-dir (concat emacs-dir "listing")
  "The Dir to put the listing file from CodePilot server."
  :type 'directory
  :set 'codepilot-listing-dir-set
  :set-after '(codepilot-dot-dir)
  :group 'codepilot-simple)

(defcustom codepilot-default-product "mtx00014"
  "The default context for CodePilot"
  :type 'string
  :set 'codepilot-default-product-set
  :set-after '(codepilot-dot-dir)
  :group 'codepilot-simple
  )

(defcustom codepilot-default-platform "ppc"
  "The default context for CodePilot"
  :type 'string
  :set 'codepilot-default-platform-set
  :set-after '(codepilot-dot-dir)
  :group 'codepilot-simple
  )

(defcustom codepilot-cli "C"
  "CodePilot CLI program"
  :type '(choice
          (const :tag "C" "C")
          (const :tag "JAVA" "JAVA"))
  :group 'codepilot-simple
  )


(define-key  protel-mode-map [(f10)] 'more-smarter-fold-unfold)
(define-key  protel-mode-map [(f11)] 'narrow-proc)
(define-key  protel-mode-map [(f6)] 'which-block)
(define-key  protel-mode-map [(shift f6)] 'smart-search-backward)
(define-key  protel-mode-map [(shift f7)] 'fold-ifcaseselect-block)
(define-key  protel-mode-map [(f7)] 'fold-ifcaseselect-branches)
(define-key  protel-mode-map [(shift f12)] 'fold-proc)
(define-key  protel-mode-map [(f12)] 'which-proc)

;; (define-key  protel-mode-map [left-fringe  mouse-1] 'protel-mouse-fold-unfold)
(define-key  protel-mode-map [mouse-2] 'protel-mouse-fold-unfold)
(define-key  protel-mode-map [(shift mouse-3)] 'mouse-save-then-kill)

(add-hook 'protel-mode-hook 'protel-turn-on-imenu t)
(add-hook 'protel-mode-hook 'imenu-add-menubar-index t)


(defvar codepilot-dir "D:/ftp/CodePilot")

(defvar cptree-node-keyword)
(setq cptree-node-keyword (regexp-opt (list
                                         "Query"
                                         "Identifier"
                                         "Procedure"
                                         "Module"
                                         "Version"
                                         "Patch ID"
                                         "PLS"
                                         "Update"
                                         )
                                        nil
                                        ))

(defvar cptree-node-regexp)
(setq cptree-node-regexp (concat "^\\( *\\)\\(" cptree-node-keyword "\\): "))

(defvar cptree-leaf-regexp (concat "^\\( +\\)\\(Section\\):  \\(" pls-section-regexp "\\)"))

(defvar cptree-lineno-regexp "\\(<[0-9]+>\\)")

(defvar cptree-nl-regexp)
(setq cptree-nl-regexp (concat "^\\( *\\)\\(" cptree-node-keyword "\\|Section\\): "))

(defvar codepilot-buffer "*CodePilot*"
  "")

(defvar codepilot-prompt-regexp "^CP (.*)> "
  "*This variable gives the pattern to match for CodePilot prompt. A single
trailing space is recommended so that C-c C-a will bring the cursor to
the true beginning of the command.")


(defvar codepilot-mode-map nil
  "keymap for codepilot mode")

(defvar codepilot-mode-syntax-table nil
  "Syntax table in use in CODEPILOT buffers.")

(unless codepilot-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?. "." table)
    (setq codepilot-mode-syntax-table table)))



(defvar codepilot-wait-for-cmd nil)

(defvar codepilot-error nil)

(defvar codepilot-context-dir)
(defvar codepilot-context "")

(defun codepilot-context-to-pls-context-str (str)
  (let (pls-context-str cp-plat cp-stream pls-plat pls-stream s1 s2)
    (when (and str
               (string-match "\\([^-]+\\)-\\([^-]*\\)" str)
               )
      (setq cp-plat (match-string 1 str))
      (setq cp-stream (match-string 2 str))
      (cond ((string= cp-plat "isn")
             (setq pls-plat "mc68020")
             (when (string-match "^\\(ciu\\|cau\\|rmu\\|acc\\)[0-9]\\([0-9]\\{2\\}\\)" cp-stream)
               (setq pls-context-str (concat "mtx" (match-string 2 cp-stream) " " pls-plat)))
             )
            ((string= cp-plat "ppc")
             (setq pls-plat "powerpc")
             (setq s1 (downcase (substring cp-stream 0 3)))
             (cond ((string= s1 "mtx")
                    (cond ((string= cp-stream "mtx00160")
                           (setq pls-context-str
                                 (concat "mtxx16" " " pls-plat)))
                          (t
                           (setq pls-context-str
                                 (concat "mtx" (substring cp-stream 6)

                                         " " pls-plat))))
                    ))
             )
            ((string= cp-plat "m88k")
             (setq pls-plat "mc88100")
             (setq s1 (downcase (substring cp-stream 0 3)))
             (cond ((string= s1 "mtx")
                    (setq pls-context-str (concat (replace-regexp-in-string "0" "" cp-stream) " " pls-plat))
                    ))
             )))
    pls-context-str))

(defun codepilot-sync-pls-context-with-codepilot (context-str)
  (save-excursion
    (let (pls-context-str)
      (setq pls-context-str (codepilot-context-to-pls-context-str context-str))
      (when pls-context-str
        (cond (pls-wait-for-cmd
               (pls-append-handler-on-stack 'pls-string-input (concat "establish " pls-context-str))
               )
              (t
               (pls-string-input (concat "establish " pls-context-str))
               ))))))

(defcustom codepilot-auto-sync-pls-context-with-codepilot nil
  "Whether to sync pls context with codepilot automatically."
  :type 'boolean
  :group 'codepilot-convenience
  )

(defun codepilot-find-and-sync-context (dummy)
  ""
  (let ((case-fold-search t)
        msg)
    (save-excursion
      (save-match-data
        (goto-char comint-last-input-end)
        (if (re-search-forward "\\*\\*\\* CONTEXT \\*\\*\\* - Context set" nil t)
            (progn
              (goto-char comint-last-input-end)
              (if (re-search-forward "^Context: \\(.*\\)" nil t)
                  (progn
                    (setq codepilot-context (match-string 1))
                    (setq msg (concat "Context set: [" codepilot-context "]" ))
                    (condition-case nil
                        (when (and codepilot-auto-sync-pls-context-with-codepilot
                                   (pls-active)
                                   )
                          (codepilot-sync-pls-context-with-codepilot codepilot-context)
                          )
                      (error nil)
                      )
                    )
                  (setq msg (concat msg "\n*** ERROR *** - Cannot sync the context with codepilot."))
                  )

              (if (re-search-forward "^ListingDir: \\[\\(.*\\)\\]" nil t)
                  (progn
                    (setq codepilot-context-dir (file-name-as-directory (match-string 1)))
                    (let (win buf)
                      (when (setq buf (get-buffer "*XREF*"))
                        (when (setq win (get-buffer-window buf))
                          (delete-window win))
                        (kill-buffer buf)))
                    (setq msg (concat msg "\nListing dir set: [" codepilot-context-dir "]" ))
                    )
                  (setq msg (concat msg "\n*** ERROR *** - Cannot sync the listing dir with codepilot."))
                  )
              (when msg
                (message msg))
              ))))))

(defun codepilot-sync-listingdir (dummy)
  ""
  (let ((case-fold-search t))
    (save-excursion
      (save-match-data
        (goto-char comint-last-input-end)
        (if (re-search-forward "\\*\\*\\* DIR \\*\\*\\*" nil t)
            (progn
              (goto-char comint-last-input-end)

              (if (re-search-forward "^ListingDir: \\[\\(.*\\)\\]" nil t)
                  (progn
                    (setq codepilot-context-dir (file-name-as-directory (match-string 1)))
                    (kill-buffer "*XREF*")
                    (message (concat "Listing dir set: [" codepilot-context-dir "]" ))
                    )
                (message "*** ERROR *** - Cannot sync the listing dir with codepilot.")
                )))
        ))))


(defun codepilot-catch-setting-commands (str)
  ""
  (let ((case-fold-search t))
    (cond ((string-match "^[ /t/f]*/co[ /t/f]+" str)
           (message "/co command input!")
           (codepilot-push-handler-on-stack 'codepilot-find-and-sync-context nil)
           )
          ((string-match "^[ /t/f]*/dir[ /t/f]+" str)
           (message "/dir command input!")
           (codepilot-push-handler-on-stack 'codepilot-sync-listingdir nil)
           )
          (t
           nil)
          )
    )
  t ;; this is to cause the next handler to run
  )


(defun codepilot-comint-output-filter (str)
  "Check if a command has finished"
  (let ((case-fold-search t)
        max-pos
        pos
        )
    (save-match-data
      (save-excursion

        ;; check done.
        (setq max-pos (point-max))
        (goto-char max-pos)
        (forward-line 0)
        (setq pos (point))
        (when (< pos comint-last-input-end)
          (setq pos comint-last-input-end)
          (goto-char pos)
          )

        (when (re-search-forward codepilot-prompt-regexp nil t)

          ;; check error
          (goto-char comint-last-input-end)
          (when (re-search-forward "\\*\\*\\* ERROR \\*\\*\\*" nil t)
              (setq codepilot-error t)
              )

          (codepilot-command-done)
              )))))

(defun codepilot-proc-check (buf)
  (and (get-buffer buf)
       (comint-check-proc buf))
  )

(defun codepilot-mode ()
  "Put the current buffer into CODEPILOT mode"
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq major-mode 'codepilot-mode)
  (setq mode-name "CodePilot")
  (use-local-map codepilot-mode-map)
  (setq comint-prompt-regexp codepilot-prompt-regexp)
  (setq comint-scroll-to-bottom-on-output 'all)
  (setq case-fold-search t)
  (set-syntax-table codepilot-mode-syntax-table)
  (add-hook 'comint-output-filter-functions 'codepilot-comint-output-filter t t)
  (add-hook 'comint-input-filter-functions 'codepilot-catch-setting-commands t t)
  (codepilot-font-lock-setup)
  (run-mode-hooks 'codepilot-mode-hook))

;; (defcustom codepilot-default-context "ppc mtx00015"
;;   ""
;;   :type 'string
;;   :group 'codepilot)


;; start codepilot and wait until finished.
(defun codepilot-start ()
  ""
  (interactive)

  (unless (file-directory-p pls-local-source-dir)
    (make-directory pls-local-source-dir)
    )

  (if (codepilot-proc-check codepilot-buffer)
      (codepilot-switch-to-buffer codepilot-buffer)

    ;; start it.
    ;; (setenv "CLASSPATH" ".;D:/ftp/CodePilot")
    ;; (make-comint "CodePilot" "C:/j2sdk1.4.2_15/bin/java" nil "com.nortelnetworks.codepilot.codepilotCLI.CodePilotLogin")



    (cond ((string= "C" codepilot-cli)
           (make-comint "CodePilot" (concat codepilot-dir "/codepilot.exe"))
           )
          (t
           ;; java
           (make-comint "CodePilot" "java" nil "-jar" (concat codepilot-dir "/codepilot.jar"))
           ))

    (set-buffer codepilot-buffer)

    (setq next-codepilot-handlers nil)
    (setq next-codepilot-handlers-args nil)

    (codepilot-mode)

    (codepilot-append-handler-on-stack 'codepilot-string-input "\n")
    (codepilot-append-handler-on-stack 'codepilot-string-input (concat "/co " codepilot-default-context))
    (codepilot-append-handler-on-stack 'codepilot-add-context-menu nil)
    (codepilot-switch-to-buffer codepilot-buffer)
    ))

(defun codepilot-proc()
  ""
  (get-buffer-process codepilot-buffer)
  )



(defun codepilot-string-input (string)
  ""
  (interactive "sCommand: ")
  (set-buffer codepilot-buffer)
  (goto-char (point-max))
  (comint-kill-input)
  (insert string)
  ;; use comint-send-string which can update comint-last-input-end!!!
  (setq codepilot-wait-for-cmd t)
  (comint-send-input)
  nil ;; this is not to run the next command until this command done.
  )

(defun codepilot-send-wait (string)
  ""
  (let ((proc (codepilot-proc))
        (prg "Waiting"))
    (with-current-buffer (process-buffer proc)
      (setq codepilot-error nil)
      (codepilot-string-input string)
      (while (progn
               (message (setq prg (concat prg "......")))
               (accept-process-output proc 0.5)
               codepilot-wait-for-cmd
               ))
      (message "Done.")
      codepilot-error
      )))


(defvar next-codepilot-handlers nil
  "This variable holds the handlers to be invoked once the CODEPILOT prompt is
back in order to complete a function requested by the user. The handler of
the outermost called function is at bottom of the stack and the handler of
the innermost nested called function is at the top of the stack. When
the codepilot-command-done is invoked, the stack is poped and the handler
of the innermost called function is executed first. Refer to codepilot-command-
done for details.")

(defvar next-codepilot-handlers-args nil
  "This variable holds the arguments of the corresponding handler.")

(defvar last-codepilot-command nil
  "This variable holds the last command string sent to the active CODEPILOT
buffer, i.e., `codepilot-buffer'.")


(defun codepilot-push-handler-on-stack (handler args)
  (push handler next-codepilot-handlers)
  (push args next-codepilot-handlers-args)
  )

(defun codepilot-pop-handler-from-stack ()
  (let (handler args)
    (setq handler (pop next-codepilot-handlers))
    (setq args (pop next-codepilot-handlers-args))
    (list handler args)
  ))

(defun codepilot-append-handler-on-stack (handler args)
  (if next-codepilot-handlers
      (progn
        (nconc next-codepilot-handlers (list handler))
        (nconc next-codepilot-handlers-args (list args)))
    (push handler next-codepilot-handlers)
    (push args next-codepilot-handlers-args)
    ))


(defun codepilot-command-done ()
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
reset the next-codepilot-handlers and next-codepilot-handlers-args stacks.

See also the variable inter-codepilot-handlers-message."
  (let* ((last-cmd (and (not next-codepilot-handlers) last-codepilot-command))
         (continue t)
         (prev-buf (current-buffer))
         next-cmd next-cmd-args curr-buf)
    (setq codepilot-wait-for-cmd nil)
    (setq last-codepilot-command nil)
    (if next-codepilot-handlers
        (progn
          (setq next-cmd (pop next-codepilot-handlers))
          (setq next-cmd-args (pop next-codepilot-handlers-args))
          (condition-case nil
              (progn
                ;; this while loop is protected, i.e., the clean up forms are
                ;; evaluated if an error occurs during execution of handlers
                (while (and next-cmd continue)
                  (setq continue (funcall next-cmd next-cmd-args))
                  (if continue
                      (progn
                        (setq next-cmd-args (pop next-codepilot-handlers-args))
                        (setq next-cmd (pop next-codepilot-handlers))
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
                     ;(setq starting-codepilot-supported-lib nil)
                     (setq next-codepilot-handlers nil)
                     (setq next-codepilot-handlers-args nil)))
            )
        ))
    (if last-cmd
        ;; the last command sent to CODEPILOT buffer has terminated.
        (message (concat last-cmd " done."))
      )
  ))


(defalias 'codepilot 'codepilot-start)

(defvar codepilot-font-lock-keywords
  (list

   (list (concat "\\(" codepilot-prompt-regexp "\\)[ \t]*\\(.*\\)$")
         '(1 'codepilot-purple-face)
         '(2 font-lock-function-name-face)
         )

   (list cptree-node-regexp
         '(2 'codepilot-purple-face)
         )
   (list "\\(\\[.*\\]\\)"
         '(1 font-lock-warning-face)
         )
   (list "\\(([DIM]+)\\)"
         '(1 font-lock-warning-face)
         )

   (list (concat "\\(\\(    \\)+\\)\\(" cptree-node-keyword "\\|Section\\): " )
         '(1 font-lock-label-face)
         )

   (list cptree-leaf-regexp
         '(2 font-lock-warning-face)
         '(3 'codepilot-purple-face)
         )
   (list cptree-lineno-regexp
         '(1 font-lgock-type-face)
         )
   (list (regexp-quote "*** ERROR ****")
         '(1 font-lock-warning-face)
         )
   )
  "font-lock keywords setting for CodePilot buffers.")

(defun codepilot-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(codepilot-font-lock-keywords t))
  )


(unless codepilot-mode-map
  (let ((map ;;(copy-keymap comint-mode-map)
         (make-sparse-keymap)
          ))
    (setq codepilot-mode-map map)
    (set-keymap-parent codepilot-mode-map comint-mode-map)))


(defun codepilot-goto-me()
  ""
  (interactive)
  (switch-to-buffer codepilot-buffer)
  )

(defun codepilot-make-context-menu ()
  ""
  (interactive)
  (let ((case-fold-search t)
        menu
        submenu
        plat
        stream
        cmd
        beg
        end)
    (save-excursion
      (save-match-data
        (set-buffer codepilot-buffer)
        (goto-char comint-last-input-end)
        (when (re-search-backward "^Version OK" nil t)
          (forward-line)
          (setq beg (point))
          (when (re-search-forward "^Context OK" nil t)
            (setq end (point))
            (narrow-to-region beg end)
            (setq menu (list "Context"))
            (goto-char (point-min))
            (while (not (eobp))
              (cond ((looking-at "^\\w")
                     (end-of-line)
                     (setq plat (current-word))
                     (when submenu
                         (push (nreverse submenu) menu))
                     (setq submenu (list plat))
                     )
                    (t
                     (setq stream (current-word))
                     (setq cmd (concat "/co " plat " " stream))
                     (when submenu
                       (push (vector
                              stream
                              `(lambda ()
                                (interactive)
                                (codepilot-string-input ,cmd))
                              t)
                             submenu))
                     ))
              (forward-line)
              )
            (widen)
            (nreverse menu)
            ))))))

(defvar codepilot-context-menu nil)

(defun codepilot-add-context-menu (dummy)
  ""
  (let ((menu (codepilot-make-context-menu)))
    (setq codepilot-context-menu menu)
    (easy-menu-add-item codepilot-welcome-symbol nil menu "Relogin CodePilot")))

(defun codepilot-help ()
  ""
  (interactive)
  (codepilot-string-input "/help")
  (goto-char (point-min))
  )

(defvar codepilot-menu
  '("CodePilotConsole"
    ["Help" codepilot-help t])
  "CodePilotConsole Menu")

(easy-menu-define codepilot-console-menu-symbol
                  codepilot-mode-map
                  "CodePilotConsole menu"
                  codepilot-menu)


(defmacro with-modify-in-readonly (&rest body)
  `(let ((inhibit-read-only t)
         (buffer-unqdo-list t))
     ,@body
     (set-buffer-modified-p nil)
     ))

(defun codepilot-output (&optional delete)
  "Return the output of the last CODEPILOT command.
The shell output is discarded if delete is non-nil."
  (if (get-buffer codepilot-buffer)
      (let ((case-fold-search t)
            beg end)
        (save-excursion
          (set-buffer codepilot-buffer)
          (setq beg comint-last-input-end)
          (setq end
                (save-excursion
                  (goto-char (point-max))
                  (re-search-backward codepilot-prompt-regexp)
                  (backward-char)
                  (point)))
          (prog1
              (buffer-substring-no-properties beg end)
            (if delete (comint-delete-output)))))
    nil))

(defun codepilot-get-patch-info-from-pls ()
  (let ((buf (current-buffer))
        ll issue update patch output)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (move-to-column 10)
        (setq patch (substring (current-word) 0 5))

        (when (pls-active)
          (if (pls-send-wait (concat "l " patch " i") 0.1)
              (error "Command failed!")
            (set-buffer pls-buffer)
            (save-excursion
              (goto-char comint-last-input-end)
              (forward-line 3)
              (while (re-search-forward pls-issue-line-regexp nil t)
                (move-to-column 7)
                (setq issue (current-word))
                (move-to-column 57)
                (setq update (buffer-substring-no-properties
                              (point)
                              (progn (end-of-line) (skip-chars-backward "[ \n]")
                                     (point))))
                (push (cons update issue) ll))
              (when ll
                (let ((inhibit-read-only t)
                      (buffer-undo-list t))
                  (set-buffer buf) ;; back to the patch info buffer.
                  (goto-char (point-max))
                  (insert "PLS: \n")
                  (loop for (upd . iss) in (reverse ll) do
                        (if (pls-send-wait (concat "l " upd) 0.1)
                            (error "Command failed!")
                          (setq output (pls-output t))
                          (insert "Update: " iss output "\n")
                          ))
                  (set-buffer-modified-p nil)
                  )))))))))

(defun codepilot-send-and-output-1 (string buf-name &optional action)
  ""
  (codepilot-send-wait string)

  (let (
        (buf (get-buffer-create buf-name))
        (output (codepilot-output t))
        )

    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; Don't generate undo entries for creation of the initial contents.
            (buffer-undo-list t))
        (erase-buffer)
        (insert output)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        ))

    (codepilot-pop-or-switch-buffer buf)
    (cptree-mode)
    (cond ((eq action 'patch-info)
           (codepilot-get-patch-info-from-pls)
           (goto-char (point-min))
           )
          (t
           ;; (> (line-number-at-pos) (window-height))
           (goto-char (point-min))
           (if (and (re-search-forward "\\]  Matches: \\([0-9]+\\)" nil t)
                    (> (string-to-number (match-string 1)) 1))
               (cptree-fold-sublevel 1)
             (cptree-fold-sublevel 2)
             )
           (goto-char (point-min))
           (forward-line 2)
           ))

;;     (when (re-search-forward "^[ ]+Section:  " nil t)
;;       (cptree-unfold-branch)
;;       (when codepilot-i-like-listing
;;         (backward-word)))

    (let ((b (get-buffer cplist-buf-name)))
      (when b
        (with-current-buffer b
          (with-modify-in-readonly
              (goto-char (point-min))
            (forward-line)
            (forward-line)
           (insert "  " buf-name "\n")
           (backward-char)
           (remove-overlays (line-beginning-position) (line-beginning-position 2) 'tag 'myfilter)

           ))))
    ))


(defun codepilot-send-and-output (command &optional action)
  ""
  (interactive "sCommand: ")
  (let ((buf (concat "[" (downcase command) "](" codepilot-context ")"))
        pos)
    (if (get-buffer buf)
        (codepilot-pop-or-switch-buffer buf)
      (codepilot-send-and-output-1 command buf action))

    (when action
      (setq pos (point))
      (goto-char (point-min))
      (if (and (re-search-forward "\\]  Matches: \\([0-9]+\\)" nil t)
               (> (string-to-number (match-string 1)) 1))
          (progn
            (goto-char pos)
            (message "More than one result found!"))

        (cond ((eq action 'goto-def)
               (when (re-search-forward "    Section:  .+ (D)" nil t)
                 (cptree-unfold-branch)
                 (when codepilot-i-like-listing
                   (forward-line 0)
                   (skip-chars-forward " ")
                   )
                 (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                   (cptree-do-what))
                 (bury-buffer buf)
                 )
               )
              ((eq action 'goto-imp)
               (when (re-search-forward "    Section:  .+ (I)" nil t)
                 (cptree-unfold-branch)
                 (when codepilot-i-like-listing
                   (forward-line 0)
                   (skip-chars-forward " ")
                   )
                 (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                   (cptree-do-what))
                 (bury-buffer buf)
                 )
               )
              (t
               (goto-char pos))
              )

        )
      ))
  )

(defun codepilot-open-buf-and-go (buf &optional lineno slineno id search-type class-id)
  ""
  (codepilot-pop-or-switch-buffer buf)
  ;; (protel-mode)
  (when lineno
    (goto-line lineno)
    )
  (when slineno
    (let ((pos nil))
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (concat "^ *" (int-to-string slineno) " ") nil t)
            (setq pos (point))
            )))
      (when pos
        (goto-char pos)
        )))
  (when id
    (let ((pos nil))
      (save-excursion
        (unless (or lineno slineno)
          (goto-char (point-min)))
        (when (protel-search-and-hl-text id nil search-type class-id)
          (setq pos (point))
          ))
      (when pos
        (goto-char pos)
        )))
  (codepilot-update-section-list)
  )

(defun codepilot-open-section (section &optional lineno slineno id search-type class-id)
  ""
  (interactive "sSection: ")

  (let ((buf (get-buffer (upcase section)))
        file1 done)

    (setq file1 (concat codepilot-context-dir (upcase section)))

    (cond (buf
           ;; check the file name
           (cond ((string= file1 (with-current-buffer buf (buffer-file-name)))
                  (message "Section already open.")
                  (codepilot-open-buf-and-go buf lineno slineno id search-type)
                  (setq done t)
                  )
                 (t
                  (message "Re-open the buffer.")
                  (kill-buffer buf)))))

    (unless done
      ;; brian: for lazy highlight
      (setq isearch-lazy-highlight-last-string nil)

      (if (file-readable-p file1)
          (progn
            (message "File already exists in context dir. Open it from context dir.")
            (setq buf (find-file-noselect file1))
            (codepilot-open-buf-and-go buf lineno slineno id search-type)
            )

        (if (codepilot-send-wait (concat "cl " section))
            (error "*** ERROR ***"))
        (let ((case-fold-search t)
              )
          (setq buf nil)
          (save-excursion
            (save-match-data
              (set-buffer codepilot-buffer)
              (goto-char comint-last-input-end)
              (when (re-search-forward "^Listing file: \\[\\(.*\\)\\]" nil t)
                (setq buf (find-file-noselect (match-string 1)))
                )))
          (if buf
              (codepilot-open-buf-and-go buf lineno slineno id search-type))
          )))))

(defvar inhibit-calltrak-warning-msg-box nil)

(defun codepilot-open-calltrack-1 (buf)
  (let ((case-fold-search t)
        lineno
        proc
        file-name
        msg
        ok
        buf2
        )
    (save-excursion
      (save-match-data
        (set-buffer buf)
        (goto-char (point-min))
        (when (re-search-forward "^Proc: \\(.+\\)$" nil t)
          (setq proc (match-string 1))
          (when (re-search-forward "^Line: \\([0-9]+\\)$" nil t)
            (setq lineno (string-to-number (match-string 1)))
            (when (re-search-forward "^Listing file: \\[\\(.*\\)\\]" nil t)
              (setq file-name (match-string 1))

              (goto-char (point-min))
              (when (re-search-forward (concat "^"(regexp-quote "*** Warning *** ")) nil t)
                (let (beg end)
                  (forward-line 0)
                  (setq beg (point))
                  (forward-line 2)
                  (backward-char)
                  (setq end (point))
                                        ;(message (replace-regexp-in-string "\n" " " (buffer-substring beg end)))
                  (setq msg (buffer-substring beg end))
                  ))
              (setq ok t)
              )))))

    (when ok
      (setq buf2 (get-buffer (file-name-nondirectory file-name)))

      (when (and buf2
                 (not (string= (file-name-as-directory file-name)
                               (file-name-as-directory (with-current-buffer buf2 (buffer-file-name))))))
        (kill-buffer buf2)
        (message "Reopen buffer.")
        )

      (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
        (codepilot-pop-or-switch-buffer (find-file-noselect file-name)))
      (bury-buffer buf)
      ;; (protel-mode)
      (goto-char (point-min))
      (if lineno
          (goto-line (1+ lineno)))
      (if proc
          (protel-search-and-hl-text proc nil 'id))

      (codepilot-update-section-list)
      (when msg
        (message msg)
        (unless inhibit-calltrak-warning-msg-box
          (message-box (replace-regexp-in-string "\n" ".  " msg))))
      )))

(defun codepilot-open-calltrack (calltrak)
  ""
  (interactive "sCalltrak or SWER string : ")

  (when (string-match "^[ \t]*[0-9A-Z]\\{8\\} \\(.+\\..+:.+\\)\\+#[0-9]+" calltrak)
    (setq calltrak (match-string 1 calltrak))
    )
  (when (string-match ":" calltrak)
    (setq calltrak (replace-match " " t t calltrak))
    )
  (when (string-match "^.*=" calltrak)
    (setq calltrak (setq calltrak (replace-match "" t t calltrak)))
    )
  (when (string-match "\\+.*$" calltrak)
    (setq calltrak (setq calltrak (replace-match "" t t calltrak)))
    )

  (when (string-match "\n" calltrak)
    (setq calltrak (setq calltrak (replace-match "" t t calltrak)))
    )

  (setq calltrak (downcase calltrak))

  ;;   (if (codepilot-send-wait (concat "ct " calltrak))
  ;;       (error "*** ERROR ***"))
  (let ((case-fold-search t)
        (buf (concat "[ct " calltrak "](" codepilot-context ")"))
        (command (concat "ct " calltrak))
        )

    (if (get-buffer buf)
        () ;;(codepilot-pop-or-switch-buffer buf)
        (codepilot-send-and-output-1 command buf 'calltrak))

    (codepilot-open-calltrack-1 buf)

    ))



;; (defun codepilot-get-context-dir ()
;;   ""
;;   (interactive)
;;   (let ((case-fold-search t))
;;     (save-excursion
;;       (save-match-data
;;         (codepilot-send-wait "/info")
;;         (set-buffer codepilot-buffer)
;;         (goto-char comint-last-input-end)
;;         (if (re-search-forward "^ListingDir: \\[\\(.*\\)\\]" nil t)
;;             (progn
;;               (setq codepilot-context-dir (match-string 1))
;;               (message (concat "Context set: [" codepilot-context-dir "]" ))
;;               )
;;           (message "*** ERROR *** - Cannot get the context dir with codepilot.")
;;           )))))

(defun codepilot-open-context-dir ()
  ""
  (interactive)
  (dired codepilot-context-dir)
    )

(defun codepilot-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "^[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+$" s) (setq s (replace-match "" t t s)))
  s)


(defmacro def-query-proc (proc-name arg doc prompt command option &optional action)
  `(defun ,proc-name (,arg)
     ,doc
     ;(def-query-interactive ,prompt)
     (interactive
      (list
       (let ((cur (current-word)))
         (read-string
          (concat ,prompt (if cur (concat " (default " cur ")") "") ": ")
         nil nil cur))))
     (codepilot-send-and-output (concat ,command " " (codepilot-trim ,arg) ,(if (null option) "" (concat " " option))) ,action)
     ))

;(print (macroexpand-all '(def-query-proc codepilot-query-proc symid "Query Proc" "Procedure" "p" nil)))

(def-query-proc codepilot-query-proc symid "Query Proc" "Procedure" "p" nil)
(def-query-proc codepilot-query-id symid  "Query Identifier" "Identifier" "i" nil)
(def-query-proc codepilot-query-sym symid  "Query Symbol" "Procedure or Identifer" "o" nil)
(def-query-proc codepilot-query-string symid  "Query String" "String" "l" nil)
(def-query-proc codepilot-query-comment symid  "Query Comments" "Comments" "c" nil)
(def-query-proc codepilot-query-module symid  "Query Module" "Module" "m" nil)
(def-query-proc codepilot-query-section symid  "Query Section" "Section" "e" nil)
(def-query-proc codepilot-query-patch symid  "Query Patch" "Patch" "t" nil)


(def-query-proc codepilot-query-proc-dim symid "Query proc dim" "Procedure(DIM)" "p" "-d")
(def-query-proc codepilot-query-id-dim symid  "Query Identifier dim" "Identifier(DIM)" "i" "-d")
(def-query-proc codepilot-query-sym-dim symid  "Query Symbol dim" "Procedure or Identifer(DIM)" "o" "-d")

(def-query-proc codepilot-goto-def symid  "Goto Def" "CodePilot Goto Def" "o" "-d" 'goto-def)
(def-query-proc codepilot-goto-imp symid  "Goto Imp" "GodePilot Goto Imp" "o" "-d" 'goto-imp)

(def-query-proc codepilot-query-patch-info symid "Query Patch Info" "Patch Info" "v" nil 'patch-info)


;; ==============CP Tree========================
`
(require 'easymenu)


(defvar cptree-mode-map nil
  "keymap for cptree mode")

(defvar cptree-mode-syntax-table nil
  "Syntax table in use in CPTREE buffers.")


(unless cptree-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?. "." table)
    (setq cptree-mode-syntax-table table)))


(defun cptree-mode ()
  "Put the current buffer into CPTREE mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'cptree-mode)
  (setq mode-name "CPTree")
  (use-local-map cptree-mode-map)
  (setq case-fold-search t)
  (set-syntax-table cptree-mode-syntax-table)
  (cptree-font-lock-setup)

  ;serial number for sort by the create time.
  (make-local-variable 'cptree-serial-number)
  (setq cptree-serial-number cptree-serial-no-last)
  (setq cptree-serial-no-last (1+ cptree-serial-no-last))

  (run-mode-hooks 'cptree-mode-hook))

;(defvar pls-section-regexp "[A-Za-z][A-Za-z0-9_]\\{4,9\\}")

(defvar cptree-font-lock-keywords
  (list

   (list cptree-node-regexp
         '(2 'codepilot-purple-face)
         )
   (list "\\(\\[.*\\]\\)"
         '(1 font-lock-warning-face)
         )
   (list "\\(([DIM]+)\\)"
         '(1 font-lock-warning-face)
         )

   (list (concat "\\(\\(    \\)+\\)\\(" cptree-node-keyword "\\|Section\\): " )
         '(1 font-lock-label-face)
         )

   (list cptree-leaf-regexp
         '(2 font-lock-warning-face)
         '(3 'codepilot-purple-face)
         )

   (list cptree-lineno-regexp
         '(1 'codepilot-forest-green-face)
         )
   )
  "font-lock keywords setting for Cptree buffers.")

(defun cptree-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cptree-font-lock-keywords t))
  )


(unless cptree-mode-map
  (let ((map (make-sparse-keymap)))
    (setq cptree-mode-map map)))

(defvar cptree-menu
  '("CPTree"
    ["Fold all" cptree-fold-all t]
    ["Unfold all" cptree-unfold-all t]
    "-"
    ["Unfold branch" cptree-unfold-branch t]
                                        ;        ["Fold other branches" cptree-fold-other-branch t]
    "-"
    ["Fold sublevel" cptree-fold-sublevel t]
    ["Fold level between" cptree-fold-level-between t]
    "-"
    ["Up level" cptree-up-level t]
    "-"
    ["Next section" cptree-next-section t]
    ))

(easy-menu-define cptree-menu-symbol
                  cptree-mode-map
                  "Cptree menu"
                  cptree-menu)


(defun cptree-unfold-branch ()
  ""
  (interactive)
  (save-excursion
    (save-match-data
      ;(end-of-line)
      (forward-line 0)
      (cptree-ov-delete)
      )))


(defun cptree-fold-1 (level)
  ""
  (let ((loop t)
        from to curlevel)
    (end-of-line)
    (setq from (point))
    (setq to from)
    (while loop
      (if (eobp)
          (progn
            (setq loop nil)
            (setq to (point))
            )
        (forward-line)
        (if (looking-at cptree-node-regexp)
            (progn
              (setq curlevel (length (match-string 1)))
              (when (<= curlevel level)
                (setq loop nil)
                (backward-char)
                (setq to (point)))
              ))))
    (unless (= from to)
      (cptree-hide-region from to 'cptree)))
  )

(defun cptree-get-patch-info ()
  (let (patch)
    (save-excursion
      (move-to-column 15)
      (setq patch (current-word)))
    (codepilot-query-patch-info patch)))

(defun cptree-unfold/fold-node ()
  ""
  (interactive)
  ()
  (let (pos ret level to str)
    (save-excursion
      (save-match-data
        (setq pos (point))
        (forward-line 0)
        (when (looking-at cptree-node-regexp)
          (setq level (length (match-string 1)))
          (setq str (match-string 2))
          (cond ((and (>= pos (save-excursion (move-to-column 15) (point)))
                      (string= "Patch ID" (match-string 2)))
                 (cptree-get-patch-info))
                (t
                 (end-of-line)
                 (setq pos (point))
                 (dolist (o (overlays-at pos))
                   (cptree-delete-overlay o 'cptree)
                   (setq ret t))
                 (cond
                  (ret
                   ;; (forward-line)
                   ;; (skip-chars-forward " ")
                   (cond ((and (or (string= str "Module")
                                   (string= str "Version"))
                               (re-search-forward "^[ ]+Section:  " nil t))
                          (cptree-unfold-branch)
                          (when codepilot-i-like-listing
                            (backward-word))
                          (setq to (point))
                          )
                         (t
                          (forward-line)
                          (skip-chars-forward " ")
                          (setq to (point))
                          )))
                  (t
                   ;; if no overlays at all, fold it
                   (setq ret (cptree-fold-1 level))
                   )))))))
    (when to
      (goto-char to))
    ret))

(defun cptree-unfold-all()
  ""
  (interactive)
  (save-excursion
    (dolist (o (overlays-in (point-min) (point-max)))
      (cptree-delete-overlay o 'cptree)
      )))

(defun cptree-toggle-fold-1 ()
  (interactive)
  (or (cptree-unfold/fold-node)
      (cptree-do-what)
      (cptree-do-check-listing)
      (codepilot-ct-from-trap/swer/debug))
  )

(defun cptree-mouse-toggle-fold (event)
  (interactive "e")
  (mouse-set-point event)
  (cptree-toggle-fold-1))

(defun cptree-up-level ()
  ""
  (interactive)
  (let ((case-fold-search t)
        (pos (point))
        cl
        pl
        )
    (save-excursion
      (save-match-data
        (forward-line 0)
        (if (looking-at cptree-nl-regexp)
            (progn
              (setq cl (length (match-string 1)))
              (setq pl (* 4 (1- (/ cl 4))))
              (if (and (>= pl 0)
                   (re-search-backward (concat "^ \\{" (int-to-string pl) "\\}\\(" cptree-node-keyword "\\): ")))
                  (setq pos (point))
                )))))
    (goto-char pos)
    (skip-chars-forward " ")
    ))

(defun cptree-fold-other-branch ()
  ""
  (interactive)
  (cptree-fold-all)
  (cptree-unfold-branch)
  )

(defun cptree-do-check-listing ()
  ""
  (interactive)
  (let ((case-fold-search t))
    (save-excursion
      (save-match-data
        (forward-line 0)
        (if (looking-at "^Listing file: \\[\\(.*\\)\\]")
            (progn
              (find-file (match-string 1))
              ;; (protel-mode)
              )
          (if (looking-at "^Xref file:    \\[\\(.*\\)\\]")
              (progn
                (find-file (match-string 1))
                (cpxref-mode))
              ))))))

(defun cptree-next-section ()
  (interactive)
  (let (to pos)
    (save-excursion
      (save-match-data
        (forward-line 0)
        (cond
         ((looking-at "^[ ]+Section:  ")
          (forward-line)
          (cond ((looking-at "^[ ]+Section:  ")
                 (setq to (match-end 0)))
                ((progn
                   (setq pos (point))
                   (and (re-search-forward "^[ ]+Module:  " nil t)
                        (re-search-forward "^[ ]+Section:  " nil t)))
                 (cptree-unfold-branch)
                 (setq to (point))
                 (goto-char pos)
                 (save-excursion
                     (save-match-data
                       (when (re-search-backward "^\\([ ]+\\)Module:  " nil t)
                         (cptree-fold-1 (length (match-string 1))))))
                 )))
         ((looking-at "^[ ]+Module:  ")
          (when (re-search-forward "^[ ]+Section:  " nil t)
            (cptree-unfold-branch)
            (setq to (point))))
         (t
          (forward-line)
          (when (and (re-search-forward "^[ ]+Module:  " nil t)
                     (re-search-forward "^[ ]+Section:  " nil t))
            (cptree-unfold-branch)
            (setq to (point))
            )))
        ))
    (when to
      (goto-char to)
      (when codepilot-i-like-listing
        (backward-word))
      )))

(define-key cptree-mode-map [tab] 'cptree-next-section)

(define-key cptree-mode-map [mouse-3] 'cptree-mouse-toggle-fold)
(define-key cptree-mode-map "1" 'delete-other-windows)
(define-key cptree-mode-map "g" 'cplist-list-id-section-bufs)

(define-key cptree-mode-map "p" 'codepilot-query-proc)
(define-key cptree-mode-map "i" 'codepilot-query-id)
(define-key cptree-mode-map "o" 'codepilot-query-sym)
(define-key cptree-mode-map "l" 'codepilot-query-string)
(define-key cptree-mode-map "c" 'codepilot-query-comment)
(define-key cptree-mode-map "t" 'codepilot-query-patch)
(define-key cptree-mode-map "m" 'codepilot-query-module)
(define-key cptree-mode-map "n" 'codepilot-query-section)

(define-key cptree-mode-map "P" 'codepilot-query-proc-dim)
(define-key cptree-mode-map "I" 'codepilot-query-id-dim)
(define-key cptree-mode-map "O" 'codepilot-query-sym-dim)
(define-key cptree-mode-map [return] 'cptree-toggle-fold-1)

(define-key cptree-mode-map "v" 'cptree-fold-all)
(define-key cptree-mode-map "u" 'cptree-unfold-all)

(define-key cptree-mode-map "e" 'cptree-up-level)
(define-key cptree-mode-map "z" 'cptree-unfold-branch)

(define-key cptree-mode-map "a" 'codepilot-open-calltrack)


(defun cptree-fold-sublevel (ll)
  ""
  (interactive
   (list (read-number "Level: " 2)))
  (let ((lll (* 4 ll))
        from to level curlevel)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at cptree-node-regexp)
          (setq level (length (match-string 1)))
          (when (>= level lll)
              (save-excursion
                (cptree-fold-1 level))
              ))
        (forward-line)
        )))))

(defun cptree-fold-all()
  ""
  (interactive)
  (cptree-unfold-all)
  (cond ((save-excursion
           (goto-char (point-min))
           (looking-at "Patch ID: "))
         (cptree-fold-sublevel 0))
        (t
         (cptree-fold-sublevel 1)
         ))
  (recenter -3))


(defun cptree-fold-level-between (ul dl)
  ""
  (interactive "nUpLevel: \nnDownLevel: ")
  (let ((ull (* 4 ul))
        (dll (* 4 dl))
        from to level curlevel)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at cptree-node-regexp)
          (setq level (length (match-string 1)))
          (when (and (>= level ull)
                     (<= level dll))
              (save-excursion
                (cptree-fold-1 level))
              ))
        (forward-line)
        )))))

(defvar pls-issue-regexp "[A-Za-z]\\{2\\}[0-9]\\{2\\}")

(defvar cptree-version-regexp (concat "^\\( +\\)Version:  \\(" pls-issue-regexp "\\)"))



;; Brian: still cannot find a good line number mode. So turn it off by default.
(defcustom codepilot-turn-on-linum nil
  "Turn on the line number"
  :type 'boolean
  :group 'codepilot
  )

(defcustom codepilot-i-like-listing t
  "Set to true if you prefer to listing over original file.
Some PPCMM behaviors will depend on this for your convenient."
  :type 'boolean
  :group 'codepilot-convenience)

(defun codepilot-update-section-list ()
  (let ((b (get-buffer cplist-buf-name)))
    (when b
      (let ((name (buffer-name))
            (m major-mode)
            (text codepilot-current-search-text)
            pos)
        (with-current-buffer b
          (with-modify-in-readonly
           (let ((case-fold-search nil))
             (goto-char (point-min))
             (save-match-data
               (when (re-search-forward "^\\[Section List\\]")
                 (setq pos (point))
                 (when (re-search-forward (concat "  " (regexp-quote name) "\n") nil t)
                   (forward-line -2)
                   (delete-region (point)
                                  (progn
                                    (forward-line 2)
                                    (point))))
                 (goto-char pos)
                 (forward-line)
                 (insert " <" text ">\n" "  " name "\n")
                 (remove-overlays (line-beginning-position -2) (line-beginning-position) 'tag 'myfilter)


                 )))))))))

(defun cptree-open-pls-section (section &optional id proc search-type class-id)
  ""
  (let (buf
        (sym (if id id proc))
        ss pos
        )
    (setq buf (pls-open-section-noselect section))
    (when buf
      (codepilot-pop-or-switch-buffer buf)
      ;; (protel-mode)

      (goto-char (point-min))
      (when sym
        (if class-id
            (setq ss (concat "[" class-id "]::" sym ))
            (setq ss sym)
            )
        (when (setq pos (protel-get-proc-pos-in-current-buf ss))
          ;;(message "OK......imenu index.....")
          (goto-char pos))
        )

      (cond (id
                                        ;(goto-char (point-min))
             (protel-search-and-hl-text id nil search-type)
             )
            (proc
                                        ;(goto-char (point-min))
             (protel-search-and-hl-text proc nil search-type)
             )
            (t
             t)
            )
      ;;       (when codepilot-turn-on-linum
      ;;         (linum-mode 1)
      ;;         )

      (codepilot-update-section-list)
      )))

(defun cptree-do-what ()
  ""
  (interactive)
  (let* (section
         version
         level
         plevel
         whatnode
         (case-fold-search t)
         (lineno -1)
         s-list
         my-id
         class-id
         (opos (point))
         (cur (current-word))
         (pls (not (string= cur "Section")))
         (buf-name (buffer-name))
         buf-context
         command
         )

    (save-match-data
      (cond
        ((= 1 (line-number-at-pos))
         (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
           (codepilot-query-sym (current-word)))
         )
        (t
         (unless (string-match "^\\[\\(.*\\)\\](\\(.+\\))" buf-name)
           (error "Wrong buffer name. Weird!")
           )

         (setq command (match-string 1 buf-name))
         (setq buf-context (match-string 2 buf-name))
         (unless (string= buf-context codepilot-context)
           (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
             (codepilot-send-and-output command))
           (bury-buffer buf-name)
           (error "Error: Current context is: %s, but you try to query in context %s!
Automatically query it in the new context again....done!"
                  codepilot-context
                  buf-context))

         (forward-line 0)
         (cond
           ((looking-at cptree-leaf-regexp)
            (setq section (match-string 3))
            (when (re-search-forward "<\\([0-9]+\\)>" nil t)
              (setq lineno (string-to-number (match-string 1))))
            ;; get the version
            (cond
              ((re-search-backward cptree-version-regexp nil t)
               (setq version (match-string 2))

               ;; skip the module level. we don't need the info.
               (setq level (length (match-string 1)))
               (setq plevel (* (- (/ level 4) 2) 4))
               (when (re-search-backward (concat "^[ ]\\{" (int-to-string plevel) "\\}\\(" cptree-node-keyword "\\): ") nil t)
                 (setq whatnode (match-string 1))
                 (cond ((string= whatnode "Identifier")
                        (forward-word 2)
                        (setq my-id (current-word))

                        (setq s-list (split-string my-id "\\$_\\$"))
                        (when (> (length s-list) 1)
                          (setq my-id (nth 0 s-list))
                          ;; (setq class-id (nth 1 s-list))
                          )

                        (goto-char opos)
                        (if pls
                            (cptree-open-pls-section (concat section "." version) my-id nil 'id)
                            (codepilot-open-section (concat section "." version) lineno nil my-id 'id)
                            )
                        )
                       ((string= whatnode "Procedure")
                        (forward-word 2)
                        (setq my-id (current-word))

                        (setq s-list (split-string my-id "\\$_\\$"))
                        (when (> (length s-list) 1)
                          (setq my-id (nth 0 s-list))
                          (setq class-id (nth 1 s-list))
                          )
                        (goto-char opos)
                        (if pls
                            (cptree-open-pls-section (concat section "." version) nil my-id 'id class-id)
                            (codepilot-open-section (concat section "." version) lineno nil my-id 'id class-id)
                            )
                        )
                       ((string= whatnode "Patch ID")
                        (goto-char opos)
                        (if pls
                            (cptree-open-pls-section (concat section "." version))
                            (codepilot-open-section (concat section "." version) lineno)
                            ))
                       ((string= whatnode "Query")

                        (if (looking-at "^Query: \\(STRING\\|COMMENT\\) - \\[\\(.*\\)\\] ")
                            (let (query-type text)
                              (setq query-type (match-string 1))
                              (setq text (match-string 2))
                              (cond ((string= query-type "STRING")
                                     (setq text (downcase text))
                                     (setq text (replace-regexp-in-string "\\*" "" text))
                                     (goto-char opos)
                                     (if pls
                                         (cptree-open-pls-section (concat section "." version) text nil 'literal)
                                         (codepilot-open-section (concat section "." version) nil nil text 'literal)
                                         ))
                                    ((string= query-type "COMMENT")
                                     (setq text (downcase text))
                                     (setq text (replace-regexp-in-string "\\*" "" text))
                                     (goto-char opos)
                                     (if pls
                                         (cptree-open-pls-section (concat section "." version) text nil 'comment)
                                         (codepilot-open-section (concat section "." version) nil nil text 'comment)
                                         ))
                                    (t
                                     (message "???? It shall not happen.")))
                              )
                            (goto-char opos)
                            (if pls
                                (cptree-open-pls-section (concat section "." version))
                                (codepilot-open-section (concat section "." version) lineno)
                                )))
                       (t
                        (goto-char opos)
                        (if pls
                            (cptree-open-pls-section (concat section "." version )))
                        ))))
              (t (goto-char opos) nil)))
           (t (goto-char opos) nil)))))))


(defun protel-find-sym-in-xref ()
  (interactive)
  (protel-find-sym-in-xref-1 (current-word)))

(defvar protel-xref-connected-listing-file nil)

(defface codepilot-xref-default-face '((((class color)) (:height 0.9)))
  "Face for xref buffer"
  :group 'codepilot
  )

(defvar codepilot-xref-default-overlay nil)

(defun codepilot-set-xref-default-face ()
  (when codepilot-xref-default-overlay
    (delete-overlay codepilot-xref-default-overlay))
  (setq codepilot-xref-default-overlay (make-overlay 1 (point-max)))
  (overlay-put codepilot-xref-default-overlay 'face 'codepilot-xref-default-face)
  ;; (overlay-put codepilot-xref-default-overlay 'priority 1)
  )

(defun protel-find-sym-in-xref-1 (sym)
  ""
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Symbol(XREF)" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))

  (unless (string= (buffer-name) (upcase (buffer-name)))
    (protel-open-listing)
    (set-buffer (upcase (buffer-name))))

  (let* ((case-fold-search t)
         (buf-name (buffer-name))
         sym-new
         (xref-buf-name (concat buf-name ".XREF"))
         buf line-str search-text win
         (f-name (buffer-file-name))
         (old-pos (point))
         )

    (unless (looking-at "\\_<")
      (re-search-backward "\\_<"))

    (save-excursion
      (when (re-search-backward "^ *\\([0-9]+\\) " nil t)
        (setq line-str (match-string 1))))

    ;;     (unless (setq buf (get-buffer (upcase xref-buf-name)))
    ;;       (setq buf (find-file-noselect xref-buf-name)))

    (setq buf (get-buffer-create "*XREF*"))

    (setq win (selected-window))
    (multiple-value-bind (ret sidebar code-win bottom-win num)
        (window-layout-wise)
      (case ret
        ((:window-layout-1)
         (split-window-vertically)
         (other-window 1)
         (switch-to-buffer buf))
        ((:window-layout-1&1)
         (select-window code-win)
         (split-window-vertically)
         (other-window 1)
         (switch-to-buffer buf))
        ((:window-layout-1&2+
          :window-layout-2
          :window-layout-3+)
         (select-window bottom-win)

         (unless (or (eq major-mode 'occur-mode)
                     (eq major-mode 'cpxref-mode)
                     (let ((s (buffer-name)))
                       (and (eq ?* (aref s 0))
                            (eq ?* (aref s (1- (length s))))))
                     ;; (eq major-mode 'help-mode)
                     )
           (when (<= num 2)
             (condition-case nil
                 (progn
                   (split-window-vertically)
                   (other-window 1)
                   (setq bottom-win (selected-window)))
               (error
                ;; for window too small error!
                nil))))
         (switch-to-buffer buf))
        (otherwise
         (pop-to-buffer buf)
         )))

    (unless (and protel-xref-connected-listing-file
                 (string= protel-xref-connected-listing-file f-name))
      (message "Load %s." xref-buf-name)
      (with-modify-in-readonly
       (erase-buffer)
       (insert-file-contents xref-buf-name)
       (cpxref-mode)
       ;; (put-text-property 1 (point-max) 'face 'codepilot-xref-default-face)
       (codepilot-set-xref-default-face)
       (make-local-variable 'protel-xref-connected-listing-file)
       (setq protel-xref-connected-listing-file f-name)
       ))

    (fit-window-to-buffer (get-buffer-window
                           buf)
                          (/ (frame-height) 5)
                          (/ (frame-height) 5))

    (goto-char (point-min))
    (setq buffer-read-only t)

    (when (> (length sym) 27)
      (setq sym-new (substring sym 0 27)))
    (if sym-new
        (setq search-text (concat "^\\(" (regexp-quote sym) "\\|" (regexp-quote sym-new) "\\)\\(?:\\_>\\|\\$\\w*\\_>\\)"))
      (setq search-text (concat "^\\(" (regexp-quote sym) "\\)\\(?:\\_>\\|\\$\\w*\\_>\\)")))
    (protel-search-and-hl-text search-text nil 'part-id)
    (when (re-search-forward (concat "\\_<" line-str "\\_>") nil t)
      (protel-search-and-hl-text search-text t 'part-id)
      (myimenu-hl-text (line-beginning-position) (line-end-position))
      )
    (select-window win)
    (my-highlight (point-min) (point-max) (concat "\\_<" sym "\\_>") t t)
    (goto-char old-pos)
    ))

(defun protel-xref-goto-line ()
  ""
  (interactive)
  (myimenu-hl-text (line-beginning-position) (line-end-position))
  (let ((case-fold-search t)
        full-id
        (id "")
        (cur (current-word)))
    (when (string-match "^[0-9]+$" cur)
      (save-excursion
        (save-match-data
          (when (re-search-backward "^\\w" nil t)
            (setq id (current-word))
            (setq full-id id)

            (cond ((eq (aref id 0) ?$)
                   (setq id (concat "$" (nth 0 (split-string id "\\$" t))))
                   )
                  (t
                   (setq id (nth 0 (split-string id "\\$")))
                   )
                  )

            (setq id (downcase id))
            (protel-search-and-hl-text full-id nil 'id)
            (forward-line 0)
            )))
      (protel-xref-goto (string-to-number cur) id)
      )))

(defun protel-xref-goto (number &optional id)
  ""

  (interactive
   (list (read-number "Line Number: " (string-to-number (current-word)))))
  (let* ((case-fold-search t)
         (sline (concat "^ *" (int-to-string number)))
         (buf (find-file-noselect protel-xref-connected-listing-file))
         ;; (xref-buf-name (buffer-name))
         ;; (buf-name (substring xref-buf-name 0 (string-match "\\.XREF" xref-buf-name)))
         )

    (multiple-value-bind (ret sidebar code-win bottom-win)
        (window-layout-wise)
      (case ret
        ((:window-layout-1
          :window-layout-1&1)
         (split-window-vertically)
         (other-window 1)
         (fit-window-to-buffer (get-buffer-window
                                (current-buffer))
                               (/ (frame-height) 5)
                               (/ (frame-height) 5))
         )))

    (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
      (codepilot-pop-or-switch-buffer buf :xref-to-listing))
    (push-mark (point) t)
    ;; (protel-mode)
    (goto-char (point-min))
    (re-search-forward sline nil t)
    (when id
      (setq id (regexp-quote id))
      (if (re-search-forward (concat "\\_<" id) nil t)
          (progn
            (setq id (current-word))
            (forward-line 0)
            (protel-search-and-hl-text id nil 'id))))))

(defun pop-cptree-buf ()
  ""
  (interactive)
  (catch 'loop
    (dolist (buf (buffer-list))
      (if (eq (with-current-buffer buf major-mode) 'cptree-mode)
          (progn
            (codepilot-pop-or-switch-buffer buf)
            (throw 'loop t)
            )))))

(defun pop-protel-view-buf ()
  ""
  (interactive)
  (catch 'loop
    (dolist (buf (buffer-list))
      (if (eq (with-current-buffer buf major-mode) 'protel-mode)
          (progn
            (codepilot-pop-or-switch-buffer buf)
            (throw 'loop t)
            )))))



;; =============== CP XREF ========================
(defvar cpxref-mode-map nil
  "keymap for cpxref mode")

(defvar cpxref-mode-syntax-table nil
  "Syntax table in use in CPXREF buffers.")

(unless cpxref-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?. "." table)
    (setq cpxref-mode-syntax-table table)))

(unless cpxref-mode-map
  (let ((map (make-sparse-keymap)))
    (setq cpxref-mode-map map)))

(define-key cpxref-mode-map "p" 'codepilot-query-proc)
(define-key cpxref-mode-map "i" 'codepilot-query-id)
(define-key cpxref-mode-map "o" 'codepilot-query-sym)
(define-key cpxref-mode-map "l" 'codepilot-query-string)
(define-key cpxref-mode-map "c" 'codepilot-query-comment)
(define-key cpxref-mode-map "t" 'codepilot-query-patch)
(define-key cpxref-mode-map "m" 'codepilot-query-module)
(define-key cpxref-mode-map "n" 'codepilot-query-section)


(define-key cpxref-mode-map "P" 'codepilot-query-proc-dim)
(define-key cpxref-mode-map "I" 'codepilot-query-id-dim)
(define-key cpxref-mode-map "O" 'codepilot-query-sym-dim)

(define-key cpxref-mode-map "1" 'delete-other-windows)
(define-key cpxref-mode-map "g" 'cplist-list-id-section-bufs)

(define-key cpxref-mode-map "a" 'codepilot-open-calltrack)


(defun cpxref-mode ()
  "Put the current buffer into cpxref mode"
  (interactive)
  (unless (eq major-mode 'cpxref-mode)

    (kill-all-local-variables)
    (setq major-mode 'cpxref-mode)
    (setq mode-name "CP-XREF")
    (use-local-map cpxref-mode-map)
    (setq case-fold-search t)
    (set-syntax-table cpxref-mode-syntax-table)
    (cpxref-font-lock-setup)
    (setq buffer-read-only t)
    (run-mode-hooks 'cpxref-mode-hook)
    ))

(defun cpxref-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cpxref-font-lock-keywords t))
  )

(defvar cpxref-font-lock-keywords
  (list

   (list " \\([0-9]+\\)[ \n]"
         '(1 'codepilot-purple-face)
         )
   )
  "font-lock keywords setting for cpxref buffers.")

(defun cpxref-goto-line-mouse (event)
  ""
  (interactive "e")
  (mouse-set-point event)
  (protel-xref-goto-line))

(define-key cpxref-mode-map [mouse-3] 'cpxref-goto-line-mouse)

(define-key cpxref-mode-map "s" 'codepilot-search-hi-string)
(define-key cpxref-mode-map "S" 'codepilot-search-hi-1)
(define-key cpxref-mode-map "f" 'codepilot-search-hl-again-f)
(define-key cpxref-mode-map "b" 'codepilot-search-hl-again-b)



;; ==================== Protel View Only Mode =======================

(require 'protel)

(defvar protel-view-mode-map nil
  "keymap for protel-view mode")

;; ;; (let ((map (copy-keymap protel-mode-map)))
;; ;;     (setq protel-view-mode-map map))

(unless protel-view-mode-map
  (let ((map (make-sparse-keymap)))
    (setq protel-view-mode-map map)
    ;; (set-keymap-parent protel-view-mode-map protel-mode-map)
    ))

(define-minor-mode protel-view-mode
  ""
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " CPView"
  ;; The minor mode bindings.
  :group 'codepilot
  (if protel-view-mode
      (setq buffer-read-only t)
    (setq buffer-read-only nil))
  )


(add-hook 'protel-mode-hook (lambda ()
                              (protel-view-mode 1)
                              (toggle-truncate-lines 1)
                              ))


;(define-key protel-view-mode-map [mouse-3] 'protel-view-goto-line-mouse)
(define-key protel-view-mode-map "p" 'codepilot-query-proc)
(define-key protel-view-mode-map "i" 'codepilot-query-id)
(define-key protel-view-mode-map "o" 'codepilot-query-sym)
(define-key protel-view-mode-map "l" 'codepilot-query-string)
(define-key protel-view-mode-map "c" 'codepilot-query-comment)
(define-key protel-view-mode-map "t" 'codepilot-query-patch)
(define-key protel-view-mode-map "m" 'codepilot-query-module)
(define-key protel-view-mode-map "n" 'codepilot-query-section)


(define-key protel-view-mode-map "P" 'codepilot-query-proc-dim)
(define-key protel-view-mode-map "I" 'codepilot-query-id-dim)
(define-key protel-view-mode-map "O" 'codepilot-query-sym-dim)

(define-key protel-view-mode-map "x" 'protel-find-sym-in-xref)
(define-key protel-view-mode-map "X" 'protel-find-sym-in-xref-1)
(define-key protel-view-mode-map "e" 'protel-open-pls-section-from-listing)
(define-key protel-view-mode-map "w" 'protel-open-listing)


(define-key protel-view-mode-map "s" 'codepilot-search-hi-string)
(define-key protel-view-mode-map "S" 'codepilot-search-hi-1)
(define-key protel-view-mode-map "/" 'codepilot-search-hi)
(define-key protel-view-mode-map "f" 'codepilot-search-hl-again-f)
(define-key protel-view-mode-map "b" 'codepilot-search-hl-again-b)

(define-key protel-view-mode-map "a" 'codepilot-open-calltrack)

(define-key protel-view-mode-map "1" 'delete-other-windows)

(define-key protel-view-mode-map "g" 'cplist-list-id-section-bufs)

;(define-key protel-view-mode-map [mouse-3] 'codepilot-query-sym-under-mouse)
(define-key protel-view-mode-map [mouse-2] 'protel-mouse-fold-unfold)

(define-key protel-view-mode-map "8" 'msym-show-ref)
(define-key protel-view-mode-map [return] 'msym-show-ref)
(define-key protel-view-mode-map "5" 'msym-show-ref-from-this-section)
(define-key protel-view-mode-map "d" 'msym-goto-def)
(define-key protel-view-mode-map "D" 'msym-goto-def-nosection)
(define-key protel-view-mode-map "Q" 'msym-goto-imp-nosection)
(define-key protel-view-mode-map "q" 'msym-goto-imp)


(defun codepilot-query-sym-under-mouse (e)
  ""
  (interactive "e")
  (mouse-set-point e)
  (codepilot-query-sym (current-word))
  )


(defun codepilot-active ()
  (and (get-buffer codepilot-buffer)
       (comint-check-proc codepilot-buffer)))

;; (defun out-of-codepilot ()
;;   ""
;;   (interactive)
;;   (let ((mj (with-current-buffer (current-buffer) major-mode)))
;;     (not (eq major-mode 'protel-mode))
;;     ))

;; (defvar protel-view-global-menu nil
;;   "protel-view global Menu")

;; (setq protel-view-global-menu
;;       '("Code Pilot"
;;         :active (and (out-of-codepilot) (codepilot-active))
;;         ["Calltrak/Traceback" codepilot-open-calltrack t]
;;         "-"
;;         ["Go to CodePilot Console" codepilot-goto-me (codepilot-active)]
;;         ["Input CodePilot Command" codepilot-send-and-output t]
;;         "-"
;;         ["Procedure" codepilot-query-proc t]
;;         ["Procedure (DIM)" codepilot-query-proc-dim t]
;;         ["Identifier" codepilot-query-id t]
;;         ["Identifier (DIM)" codepilot-query-id-dim t]
;;         ["Combined" codepilot-query-sym t]
;;         ["Combined (DIM)" codepilot-query-sym-dim t]
;;         "-"
;;         ["Comment" codepilot-query-comment t]
;;         ["Literal String" codepilot-query-string t]
;;         "-"
;;         ("Package/Patch"
;;          ["Module Name" codepilot-query-module t]
;;          ["Section Name" codepilot-query-section t]
;;          ["Patch" codepilot-query-patch t]
;;          )
;;         "-"
;;         )
;;       )

;; (easy-menu-define protel-menu-symbol
;;   (current-global-map)
;;   "protel-view global menu"
;;   protel-view-global-menu)


(defvar protel-view-menu-common
  '(:active (codepilot-active)
    ["Calltrak/Traceback" codepilot-open-calltrack t]
    "-"
    ["Go to CodePilot Console" codepilot-goto-me (codepilot-active)]
    ["Input CodePilot Command" codepilot-send-and-output t]
    "-"
    ["Goto Definition" codepilot-goto-def t]
    ["Goto Implementation" codepilot-goto-imp t]
    "-"
    ["Procedure" codepilot-query-proc t]
    ["Procedure (DIM)" codepilot-query-proc-dim t]
    ["Identifier" codepilot-query-id t]
    ["Identifier (DIM)" codepilot-query-id-dim t]
    ["Combined" codepilot-query-sym t]
    ["Combined (DIM)" codepilot-query-sym-dim t]
    "-"
    ["Comment" codepilot-query-comment t]
    ["Literal String" codepilot-query-string t]
    "-"
    ["Module Name" codepilot-query-module t]
    ["Section Name" codepilot-query-section t]
    ["Patch" codepilot-query-patch t]
    "-"
    ["Open PLS section" protel-open-pls-section-from-listing t]
    ["Open CP section" protel-open-listing t]
    "-"
    ["XREF" protel-find-sym-in-xref t]
    ["XREF-1" protel-find-sym-in-xref-1 t]
    "-"
    ["Re-retrieve file from PLS/CodePilot" codepilot-re-retrieve-pls/codepilot-section t]
    ))

(defvar protel-view-menu
  (cons "CodePilot" protel-view-menu-common)
  )

(easy-menu-define protel-view-menu-symbol
  protel-view-mode-map
  "protel-view menu"
  protel-view-menu)


(defvar codepilot-welcome-buffer "CodePilotWelcome")
(defun codepilot-welcome ()
  ""
  (interactive)
  (let ((win (selected-window))
        (listwin (cplist-get-list-win))
        )
    (if (eq win listwin)
        (other-window 1)
      ))

  (if (get-buffer codepilot-welcome-buffer)
      (codepilot-pop-or-switch-buffer codepilot-welcome-buffer)
    (let ((buf (get-buffer-create codepilot-welcome-buffer))
          (inhibit-read-only t)
          (buffer-unqdo-list t))
      (codepilot-pop-or-switch-buffer buf)
      (set-buffer buf)
;;       (insert "%% Welcome to Emacs CodePilot!\n")
;;       (insert "%% Start the CodePilot first if you have not.\n")
;;       (insert "%% And then go back to this page and you will find\n")
;;       (insert "%% available commands in CodePilot Menu.\n\n")
      (insert "%% Welcome to PPCMM!
%% Protel, PLS, CodePilot, Mentor and MSym All in ONE.
%%
%% For the detail, please refer to http://202.38.44.80/Emacs PPCMM.html
%%
%% Use the menu items in Menu PPCMM to start codepilot and PLS.
%%
%% If it is your first time to use PPCMM. Configure it using PPCMM ->
%% Configurations menu.
%% If you just need to use CodePilot, you only need to set your CNT account
%% and password there.
%%
%% When you focus on a Protel file or this welcome buffer, the following
%% menu will show up:
%% - CodePilot
%% - MSym
%% - Navigate
%% - Protel
%% Refer to them for the available commands.
%%

%% Have a try!
%% If you have already configured CodePilot, click following indentifier
%% using your mouse right button [mouse-3]. Normally PPCMM use the right
%% button to jump/fold/unfold. Try to click the right button :->
DCL answer_msg

%% If you have set it up for PLS/MSym, put the cursor on the 'answer_msg'
%% and press Enter key.

%% If you are new to Emacs, here is a very useful command, to terminate
%% a command:
%%  Ctrl-g
%% It is just like C-c in most program.

%% If you want to go back here, click PPCMM->Welcome Page.

")
      (set-buffer-modified-p nil)
      (protel-mode)
      ;; (setq buffer-read-only nil)
      )))

(defun codepilot-ide ()
  ""
  (interactive)

  (codepilot-welcome)

  (unless (cplist-get-list-win)
    (cplist-side-window)
    ))

(defun codepilot-open-remember-data-file ()
  ""
  (interactive)
  (if (find-buffer-visiting remember-data-file)
      (codepilot-switch-to-buffer (get-file-buffer remember-data-file))
    (codepilot-switch-to-buffer (find-file-noselect remember-data-file)))
  (cpnote-mode)
  (goto-char (point-max)))

(defun ppcmm-customize (g)
  (interactive)
  (customize-group g)
  (delete-other-windows)
  )

(defun codepilot-relogin ()
  (interactive)
  (codepilot-send-wait "/rl")
  (message "Done!")
  )

(require 'callplot)
(require 'mycalltrak)
(defvar protel-view-welcome-menu
  '("PPCMM"
    ["Start CodePilot" codepilot t]
    ["Relogin CodePilot" codepilot-relogin :active (codepilot-active)]
    "-"
    ["Start PLS" pls t]
    "-"
    ["Start MSym" msym t]
    "-"
    ["Welcome Page" codepilot-welcome t]
    ;;         ["Pop up CPTree buffer" pop-cptree-buf t]
    ;;         ["Pop up Protel View buffer" pop-protel-view-buf t]
    "-"
    ["PPCMM IDE" codepilot-ide t]
    "-"
    ["Note File" codepilot-open-remember-data-file t]
    "-"
    ["Toggle Sidebar" cplist-side-window t]
    ["Minimize/Restore Sidebar" cplist-minimize/restore-sidebar :help "Hotkey: `"]
    ["Update Sidebar" cplist-list-id-section-bufs :help "Hotkey: g"]
    "-"
    ["Toggle MyImenu" myimenu-toggle-myimenu-win :help "Toggle the window to show the proc and type list."]
    "-"
    ["Save IDList" codepilot-save-query-list :help "Save the IDList into a file."]
    ["Restore IDList" codepilot-restore-query-list-from-file :help "Restore the IDList from a file."]
    "-"
    ("Misc Commands"
     ["Clean history buffer list" codepilot-clear-marker-ring t]
     "-"
     ["Add Note to Note file" re :help "Hotkey in Protel View Mode: r"]
     "-"
     ["Draw Callplot" draw-plot :help "Draw callplot based on current callplot script or ASUTrack log file."]
     "-"
     ["Turn on Mycalltrak mode" mycalltrak-mode :help "Turn on mycalltrak mode for current file."]
     "-"
     ["Del all Occur buffer" del-occur-bufs t]
     ["Pop up Occur buffer" pop-occur-buf t]
     "-"
     ["Mark words" mark-words t]
     ["Mark string" mark-string t]
     ["Mark regexp" mark-regexp t]
     ["Mark line" mymark-mark-line t]
     ["Unmark line" mymark-unmark-line t]
     ["Unmark all" unmark-all t]
     )
    "-"
    ("UE Style Bookmark"
     ["Toggle" bm-toggle t]
     ["Next" bm-next t]
     ["Previous" bm-previous t]
     ["Annotate a bookmark" bm-bookmark-annotate t]
     ["Show" bm-show t]
     ["Remove all" bm-remove-all t]
     )
    "-"
    ("Configurations"
     ["Configure CodePilot" (ppcmm-customize 'codepilot-simple) t]
     "-"
     ["Configure PLS/MSym" (ppcmm-customize 'pls) t]
     "-"
     ["Options" (ppcmm-customize 'codepilot-convenience) t]
     )
    ))

(require 'mycpgmenu)

(easy-menu-define codepilot-welcome-symbol
  (current-global-map)
  "codepilot welcome menu"
  protel-view-welcome-menu)




;; ======================

(defun protel-open-pls-section-from-listing ()
  ""
  (interactive)
  (let ((s (buffer-name)))
    (cond
     ((not (eq major-mode 'protel-mode)))
     ((string= s (downcase s)))
     (t (let ((case-fold-search t)
              (stext codepilot-current-search-text)
              (stype codepilot-current-search-type)
              buf
              (line 0)
              (cur-col (current-column)))
          (save-match-data
            (save-excursion
              (when (re-search-backward "^ *\\([0-9]+\\) " nil t)
                (setq line (string-to-number (match-string 1)))
                ))
            (setq buf (pls-open-section-noselect (downcase (buffer-name))))
            (when buf
              (codepilot-pop-or-switch-buffer (pls-open-section-noselect (downcase (buffer-name))))
              ;; (protel-mode)
              (when t        ; (string= codepilot-current-search-text "")
                (setq codepilot-current-search-text stext)
                (setq codepilot-current-search-type stype)
                (unless codepilot-hl-text-overlay
                  (setq codepilot-hl-text-overlay (make-overlay 1 1)) ; to be moved
                  (overlay-put codepilot-hl-text-overlay 'face 'codepilot-hl-text-face)
                  (overlay-put codepilot-hl-text-overlay 'priority 1001)
                  )
                (run-with-idle-timer 0.25 nil 'my-highlight-2 (current-buffer))
                )

              (codepilot-update-section-list)

              (unless (= 0 line)
                (goto-line line)
                )

              (move-to-column (- cur-col 30))

              ;;           (when codepilot-turn-on-linum
              ;;             (linum-mode 1)
              ;;             )
              )))))))

(defun protel-open-listing ()
  ""
  (interactive)
  (let ((case-fold-search t)
        (stext codepilot-current-search-text)
        (stype codepilot-current-search-type)
        cur-col
        (s (buffer-name)))

    (cond
     ((not (eq major-mode 'protel-mode)))
     ((string= s (upcase s)))
     (t (save-match-data
          (widen)
          (setq cur-col (current-column))
          (codepilot-open-section (buffer-name) nil (line-number-at-pos))
          (move-to-column (+ 30 cur-col))
          (when t            ; (string= codepilot-current-search-text "")
            (setq codepilot-current-search-text stext)
            (setq codepilot-current-search-type stype)
            (unless codepilot-hl-text-overlay
              (setq codepilot-hl-text-overlay (make-overlay 1 1)) ; to be moved
              (overlay-put codepilot-hl-text-overlay 'face 'codepilot-hl-text-face)
              (overlay-put codepilot-hl-text-overlay 'priority 1001))
            (run-with-idle-timer 0.25 nil 'my-highlight-2 (current-buffer))
            )
          (codepilot-update-section-list)
          )))))



(defcustom codepilot-del-xref-window-when-not-using nil
  "Delete *XREF* window when not using"
  :type 'boolean
  :group 'codepilot-convenience)

(defun codepilot-remove-xref-if-proper (buf bottom-win)
  (when codepilot-del-xref-window-when-not-using
    (let ((bottom-buf-name (buffer-name (window-buffer bottom-win)))
          code-mode bottom-mode)
      (setq bottom-mode (with-current-buffer bottom-buf-name major-mode))
      (when (eq bottom-mode 'cpxref-mode)
        (cond ((string= (buffer-file-name (get-buffer buf)) (with-current-buffer bottom-buf-name
                                                              protel-xref-connected-listing-file)))
              (t
               ;; (kill-buffer bottom-buf-name)
               (delete-window bottom-win)
               ))))))



(require 'cphistory)

(define-key cptree-mode-map "," 'codepilot-previous-buffer)
(define-key cptree-mode-map "." 'codepilot-forward-buffer)

(define-key cptree-mode-map "k" 'kill-buffer)


(define-key protel-view-mode-map "," 'codepilot-previous-buffer)
(define-key protel-view-mode-map "." 'codepilot-forward-buffer)

(define-key protel-view-mode-map "k" 'kill-this-buffer)





; =====================MSym=========================

(require 'easymenu)
(require 'comint)

(require 'pls)

(defvar msym-buffer "*msym*"
  "*This variable gives the name of the active MSYM buffer. See also
`msym-supported-libraries'.")


(defvar msym-wait-for-cmd nil)

(defvar msym-error nil)


(defun msym-string-input (string)
  ""
  (interactive "sCommand: ")
  (set-buffer msym-buffer)
  (goto-char (point-max))
  (comint-kill-input)
  (insert string)
  ;; use comint-send-string which can update comint-last-input-end!!!
  (setq msym-wait-for-cmd t)
  (comint-send-input)
  )

(defun msym-send-wait (string time)
  ""
  (let ((proc (msym-proc)))
    (with-current-buffer (process-buffer proc)
      (setq msym-error nil)
      (msym-string-input string)
      (while (progn
               (accept-process-output proc time)
               msym-wait-for-cmd
               ))
      msym-error
      )))


;; Async MSYM command

(defvar next-msym-handlers nil
  "This variable holds the handlers to be invoked once the MSYM prompt is
back in order to complete a function requested by the user. The handler of
the outermost called function is at bottom of the stack and the handler of
the innermost nested called function is at the top of the stack. When
the msym-command-done is invoked, the stack is poped and the handler
of the innermost called function is executed first. Refer to msym-command-
done for details.")

(defvar next-msym-handlers-args nil
  "This variable holds the arguments of the corresponding handler.")

(defvar last-msym-command nil
  "This variable holds the last command string sent to the active MSYM
buffer, i.e., `msym-buffer'.")


(defun msym-push-handler-on-stack (handler args)
  (push handler next-msym-handlers)
  (push args next-msym-handlers-args)
  )

(defun msym-pop-handler-from-stack ()
  (let (handler args)
    (setq handler (pop next-msym-handlers))
    (setq args (pop next-msym-handlers-args))
    (list handler args)
  ))

(defun msym-append-handler-on-stack (handler args)
  (if next-msym-handlers
      (progn
        (nconc next-msym-handlers (list handler))
        (nconc next-msym-handlers-args (list args)))
    (push handler next-msym-handlers)
    (push args next-msym-handlers-args)
    ))


(defun msym-command-done ()
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
reset the next-msym-handlers and next-msym-handlers-args stacks.

See also the variable inter-msym-handlers-message."
  (let* ((last-cmd (and (not next-msym-handlers) last-msym-command))
         (continue t)
         (prev-buf (current-buffer))
         next-cmd next-cmd-args curr-buf)
    ;(setq waiting-for-msym nil)
    (setq msym-wait-for-cmd nil)
    (setq last-msym-command nil)
    (if next-msym-handlers
        (progn
          (setq next-cmd (pop next-msym-handlers))
          (setq next-cmd-args (pop next-msym-handlers-args))
          (condition-case nil
              (progn
                ;; this while loop is protected, i.e., the clean up forms are
                ;; evaluated if an error occurs during execution of handlers
                (while (and next-cmd continue)
                  (setq continue (funcall next-cmd next-cmd-args))
                  (if continue
                      (progn
                        (setq next-cmd-args (pop next-msym-handlers-args))
                        (setq next-cmd (pop next-msym-handlers))
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
                     ;(setq starting-msym-supported-lib nil)
                     (setq next-msym-handlers nil)
                     (setq next-msym-handlers-args nil)))
            )
        ))
    (if last-cmd
        ;; the last command sent to MSYM buffer has terminated.
        (message (concat last-cmd " done."))
      )
  ))


(defun msym-unix-comint-output-filter (str)
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
          (msym-command-done)))
      )))


(defun msym-remove-unix-hook (dummy)
  ""
  (with-current-buffer msym-buffer
    (remove-hook 'comint-output-filter-functions 'msym-unix-comint-output-filter t))
  )

(defun msym-term-comint-output-filter (str)
  "If encounting Kill is Ctrl-U, send a blank command to let it go."
  (let ((case-fold-search t)
        )
    (save-match-data
      (save-excursion
        (goto-char comint-last-input-end)
        (when (re-search-forward "^Kill is Ctrl-U" nil t)
          (comint-send-string (msym-proc) "\n")
          (with-current-buffer msym-buffer
            (remove-hook 'comint-output-filter-functions 'msym-term-comint-output-filter t)))))))


(defun msym-remove-term-hook (dummy)
  ""
  (with-current-buffer msym-buffer
    (remove-hook 'comint-output-filter-functions 'msym-term-comint-output-filter t)))


(defun msym-start-async ()
  ""
  (interactive)

  (if (msym-proc-check msym-buffer)
      (codepilot-switch-to-buffer msym-buffer)

    ;; start it.

    ;; brian: change for emacs-22.2, for double prompts issue.
    ;; (make-comint "msym" "plink" nil (concat unix-account "@" unix-host) "-ssh" "-pw" unix-pwd)
    (make-comint "msym" "cmd")
    (set-buffer msym-buffer)
    (msym-string-input (concat plink-program " " unix-account "@" unix-host " -ssh -pw " unix-pwd))
    (msym-mode)

    (setq next-msym-handlers nil)
    (setq next-msym-handlers-args nil)

    ;; mount unix hook for execute unix command
    (with-current-buffer msym-buffer
      (add-hook 'comint-output-filter-functions 'msym-unix-comint-output-filter t t))

    ;; deal with the term.
    (with-current-buffer msym-buffer
      (add-hook 'comint-output-filter-functions 'msym-term-comint-output-filter t t))

    (msym-append-handler-on-stack 'msym-string-input "   ")
    (msym-append-handler-on-stack 'msym-string-input "banner MSYM")
    (msym-append-handler-on-stack 'msym-string-input "pwd")
    (msym-append-handler-on-stack 'msym-string-input "setenv PAGER cat")


    ;; unmount unix hook
    (msym-append-handler-on-stack 'msym-remove-unix-hook nil)

    ;; unmount term hook if it still there.
    (msym-append-handler-on-stack 'msym-remove-term-hook nil)


    (msym-append-handler-on-stack 'msym-string-input "msym -i" )
    (msym-append-handler-on-stack 'msym-string-input "context ?" )
    (msym-append-handler-on-stack 'msym-string-input "mset edit off")
    (msym-append-handler-on-stack 'msym-string-input "mset expand off")

    (codepilot-switch-to-buffer msym-buffer)
    ))

(defalias 'msym 'msym-start-async)


(defun msym-proc()
  ""
  (get-buffer-process msym-buffer)
  )

(defun msym-proc-check (buf)
  (and (get-buffer buf)
       (comint-check-proc buf))
  )

(defun msym-comint-output-filter (str)
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

        (when (re-search-forward msym-pure-prompt-regexp nil t)

          (goto-char comint-last-input-end)
          (if (re-search-forward "\\[msym\\] " nil t) ;I am familiar about msym. may be changed later, brian
              (progn
                (setq msym-error t)
                (message "MSym: *** ERROR ***")
              ))

          (msym-command-done)))
      )))

(defvar msym-prompt-regexp "^MSym> \\|^[0-9]*>"
  "*This variable gives the pattern to match for MSYM prompt. A single
trailing space is recommended so that C-c C-a will bring the cursor to
the true beginning of the command.")


(defvar msym-pure-prompt-regexp "^MSym> ")


(defvar msym-command-mode-syntax-table nil
  "Syntax table in use in MSym command buffer.")

(unless msym-command-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (setq msym-command-mode-syntax-table table)))


(defun msym-mode ()
  "Put the current buffer into MSYM mode"
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq major-mode 'msym-mode)
  (setq mode-name "MSYM")
  ;(use-local-map msym-mode-map)
  (setq comint-prompt-regexp msym-prompt-regexp)
  (setq comint-scroll-to-bottom-on-output 'all)
  (setq case-fold-search t)
  (set-syntax-table msym-command-mode-syntax-table)
  (add-hook 'comint-output-filter-functions 'msym-comint-output-filter t t)
  ;(add-hook 'comint-input-filter-functions 'msym-catch-setting-commands t t)
  ;(msym-font-lock-setup)
  (run-mode-hooks 'msym-mode-hook))

(defvar msym-prompt-regexp-1 "^MSym> ")


(defun msym-output (&optional delete)
  "Return the output of the last MSYM command.
The shell output is discarded if delete is non-nil."
  (if (get-buffer msym-buffer)
      (let ((case-fold-search t)
            beg end)
        (save-excursion
          (set-buffer msym-buffer)
          (setq beg comint-last-input-end)
          (setq end
                (save-excursion
                  (goto-char (point-max))
                  (re-search-backward msym-prompt-regexp-1)
                  (backward-char)
                  (point)))
          (prog1
              (buffer-substring-no-properties beg end)
            (if delete (comint-delete-output)))))
      nil))

;; (defun msym-send-and-output-1 (string buf-name)
;;   ""
;;   (msym-send-wait string 0.1)

;;   (let ((buf (get-buffer-create buf-name))
;;         (output (msym-output t)))

;;     (with-current-buffer buf
;;       ;; (cp-list-mode)
;;       (let ((inhibit-read-only t)
;;          ;; Don't generate undo entries for creation of the initial contents.
;;          (buffer-undo-list t))
;;      (erase-buffer)
;;         (insert output)
;;         ;; (display-buffer buf)
;;         (setq buffer-read-only t)
;;         (set-buffer-modified-p nil)
;;         ))

;;     (pop-to-buffer buf)
;;     (msym-output-mode)
;;     (goto-char (point-min))

;;     (let ((b (get-buffer cplist-buf-name)))
;;       (when b
;;         (with-current-buffer b
;;           (with-modify-in-readonly
;;            (goto-char (point-min)) ;; wrong here!!
;;            (forward-line)
;;            (insert "  " buf-name "\n")

;;            ))))
;;     ))


(defvar msym-context "")

;; (defun msym-send-and-output (command)
;;   ""
;;   (interactive "sCommand: ")
;;   (let ((buf (concat "[" (downcase command) "](" msym-context ")")))
;;     (if (get-buffer buf)
;;         (pop-to-buffer buf)
;;       (msym-send-and-output-1 command buf))
;;   ))


(defvar msym-mode-map nil
  "Keymap used in MSym mode.")


(unless msym-mode-map
  (let ((map (make-sparse-keymap)))

    ;; Key stroke
    (define-key map "\C-c=" 'msym-show-context)
    (define-key map "\C-c+" 'msym-set-context)

    (define-key map "\C-c\C-o" 'protel-open-module-goto-symbol)
    (define-key map "\C-c\C-i" 'msym-intersect)
    (define-key map "\C-c\C-c" 'msym-collapse-sections)

    (setq msym-mode-map map)))

;; MSym menu (using "easymenu")

(defun msym-goto-me()
  ""
  (interactive)
  (switch-to-buffer msym-buffer)
  )

(defun msym-active ()
  (and (get-buffer msym-buffer)
       (comint-check-proc msym-buffer)))


(defvar msym-menu
  '("MSym1"
    ;; ["Open Section" protel-open-module-goto-symbol (get-buffer pls-buffer)]
    ;; ["Show current Context" msym-show-context (get-buffer msym-buffer)]
    ;; ["Set Context" msym-set-context (get-buffer msym-buffer)]
    ["Intersect" msym-intersect t]
    "-"
    ("Show only"
     ["All" (msym-collapse-sections "all") t]
     "----"
     ("Class"
      ["address" (msym-collapse-sections "address") t]
      ["bind" (msym-collapse-sections "bind") t]
      ["bind_as" (msym-collapse-sections "bind_as") t]
      ["cast" (msym-collapse-sections "cast") t]
      ["classdesc" (msym-collapse-sections "classdesc") t]
      ["data" (msym-collapse-sections "data") t]
      ["data_definition_type"
       (msym-collapse-sections "data_definition_type") t]
      ["definition" (msym-collapse-sections "definition") t]
      ["execute" (msym-collapse-sections "execute") t]
      ["execution" (msym-collapse-sections "execution") t]
      ["handle" (msym-collapse-sections "handle") t]
      ["implementation" (msym-collapse-sections "implementation") t]
      ["miscellaneous_data"
       (msym-collapse-sections "miscellaneous_data") t]
      ["miscellaneous_type"
       (msym-collapse-sections "miscellaneous_type") t]
      ["raise" (msym-collapse-sections "raise") t]
      ["read" (msym-collapse-sections "read") t]
      ["refinement" (msym-collapse-sections "refinement") t]
      ["refinement_type" (msym-collapse-sections "refinement_type") t]
      ["type" (msym-collapse-sections "type") t]
      ["type_definition_type"
       (msym-collapse-sections "typedefinition_type") t]
      ["typedesc_type" (msym-collapse-sections "typedesc_type") t]
      ["unknown" (msym-collapse-sections "unknown") t]
      ["write" (msym-collapse-sections "write") t]
      )
     ("Attribute"
      ["direct" (msym-collapse-sections "direct") t]
      ["evaluation" (msym-collapse-sections "evaluation") t]
      ["implied" (msym-collapse-sections "implied") t]
      ["indirect" (msym-collapse-sections "indirect") t]
      )
     ["User-defined" msym-collapse-sections t]
     )
    "-"
    ["Show definition" (progn (msym-collapse-sections "all")(msym-collapse-sections "definition")) t]
    ["Show implementation" (progn (msym-collapse-sections "all")(msym-collapse-sections "implementation")) t]
    "-"
    ["Show write" (progn (msym-collapse-sections "all")(msym-collapse-sections "write")) t]
    "-"
    ["Show All" (msym-collapse-sections "all") t]
    "-"
    ["Mark Line" msym-mark-line t]
    ["Unmark Line" msym-unmark-line t]
    "-"
    ["Goto MSym buffer" msym-goto-me (msym-active)]
    ;; "----"
    ;; ["MSym mode Help" (describe-function 'msym-mode) t]
    ;; ["MSym Man Page" (man "msym") t]
    ))

(easy-menu-define msym-menu-symbol
                  msym-mode-map
                  "MSym menu"
                  msym-menu)

;;;
;;; MSym Intersect
;;; --------------
(defun msym-buf-p (&optional buf)
  "Return the associated ID if this is an MSym buffer, nil otherwise."
  (if (null buf) (setq buf (current-buffer)))
  (let ((case-fold-search t)
        (buf-name (buffer-name (get-buffer buf)))
        )
    (if (string-match "^\\[\\(.+\\)\\]" buf-name)
        (match-string 1 buf-name)
      (if (string-match "msym\\." buf-name)
          (substring buf-name 5)
        nil))
    ))

(defun msym-get-msym-output-buffer-list ()
  ""
  (interactive)
  (let (b-list)
    (dolist (buf (buffer-list))
      (if (eq (with-current-buffer buf major-mode) 'msym-output-mode)
          (push buf b-list)
          ))
    (nreverse b-list)
    ))

(defun msym-intersect (buf1 buf2)
  "Intersect 2 msym buffers."
  ;(interactive "bBuffer 1: \nbBuffer 2: ")
  (interactive
   (let* ((b-list (msym-get-msym-output-buffer-list))
          (ba (car b-list))
          (bb (cadr b-list))
         )
     (list (read-buffer "Buffer A to intersect: " ba t)
           (read-buffer "Buffer B to intersect: " bb t)
           )))

  (let* ((id1 (msym-buf-p buf1))
         (id2 (msym-buf-p buf2))
         (out-buf (concat "msym." id1 "." id2))
         (buf1-size (save-excursion (set-buffer buf1) (buffer-size)))
         (buf2-size (save-excursion (set-buffer buf2) (buffer-size)))
         (inhibit-read-only t)
         section)
    (if (null id1) (error "%s is not an msym buffer." buf1))
    (if (null id2) (error "%s is not an msym buffer." buf2))

    ;; The swapping below is done for efficiency reason
    (if (< buf1-size buf2-size)
        ()  ; no need to swap
      (let (temp)
        (setq temp buf1)
        (setq buf1 buf2)
        (setq buf2 temp)))

    (get-buffer-create out-buf)
    (pop-to-buffer out-buf)
    (set-buffer out-buf)
    (erase-buffer)
    (msym-output-mode)
    (insert (concat (upcase id1) "." (upcase id2) " intersection"))
    (insert "\n \n")
    (save-excursion
      (set-buffer buf2)
      (goto-char (point-min))
      (forward-line 2)
      (set-buffer buf1)
      (goto-char (point-min))
      (forward-line 2)
      (catch 'loop
        (while (not (eobp))
          (skip-chars-forward " \t\n") ; skip blank lines
          (setq section (current-word))
          (unless section
            (throw 'loop t)
            )
          (set-buffer buf2)
          (goto-char (point-min))
          (if (re-search-forward (concat "^" section "\\b") nil t)
              (progn
                (set-buffer out-buf)
                (insert section)
                (newline)))
        (set-buffer buf1)
        (forward-line 1)
        )))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
;;     (message (substitute-command-keys
;; "Position on a section and hit \\[protel-open-module-goto-symbol] to open it."))
  ))


(defun msym-output-mode ()
  "This mode is to view the MSym output; the MSym process is run in the
*MSym* command buffer.  To get help on MSym command, click on MSym man page
in this mode or issue a help command in the *MSym* command buffer.

Below is a summary of features provided by MSym mode.

- The symbols, sections, reference classes/attributes, etc, in the
  cross-reference list are fontified/colored differently.

- Section can be opened and the symbol is automatically located.

- Show/set the MSym context.

- Get the intersection of 2 cross-reference lists.

- Collapse sections with different reference classes/attributes.

To get help on each individual function below, do a help on that function,
i.e., M-x help-for-help then select f and enter the function name.

\\{msym-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (use-local-map msym-mode-map)
  (setq major-mode 'msym-output-mode)
  (set-syntax-table protel-mode-syntax-table)
  (make-local-variable 'outline-regexp)
  (setq selective-display t)
  (setq selective-display-ellipses nil)
  (setq case-fold-search t)
  (setq mode-name (concat "MSym [" msym-selected-ref-classes "]"))
  ;(setq mode-name "MSym Output")
  (show-all)
  (msym-mode-font-lock-setup)
  (setq buffer-read-only t)

   ;serial number for sort by the create time.
  (make-local-variable 'cptree-serial-numberxb)
  (setq cptree-serial-number cptree-serial-no-last)
  (setq cptree-serial-no-last (1+ cptree-serial-no-last))

  (run-mode-hooks 'msym-output-mode-hook))


(defvar msym-ref-classes-regexp
  (concat "\\<\\("
          ;; address bind bind_as cast classdesc
          "address\\|bind\\(\\|_as\\)\\|c\\(ast\\|lassdesc\\)\\|"
          ;; data data_definition_type definition
          "data\\(\\|_definition_type\\)\\|definition\\|"
          ;; execute execution handle implementation
          "execut\\(e\\|ion\\)\\|handle\\|implementation\\|"
          ;; miscellaneous_data miscellaneous_type
          "miscellaneous_\\(data\\|type\\)\\|"
          ;; raise read refinement refinement_type
          "r\\(aise\\|e\\(ad\\|finement\\(\\|_type\\)\\)\\)\\|"
          ;; type type_definition_type typedesc_type
          "type\\(\\|_definition_type\\|desc_type\\)\\|"
          ;; unknown write
          "unknown\\|write"
          "\\)\\>")
  "Regular expression of reference classes.")

(defvar msym-ref-attributes-regexp
  (concat "\\<\\("
          ;; direct evaluation implied indirect
          "direct\\|evaluation\\|i\\(mplied\\|ndirect\\)"
          "\\)\\>")
  "Regular expression of reference attributes.")


(defvar msym-mode-font-lock-keywords
  (list

   (list
    "\\_<\\(definition\\|implementation\\|write\\)\\_>[:/]"
    '(1 font-lock-warning-face))

   (list
    (concat "^\\<\\([A-Z0-9_$]+\\)\\> defined in \\<\\([A-Z0-9_]+\\)\\>"
            "\\( and implemented in \\<\\([A-Z0-9_]+\\)\\>\\)?")
    '(1 'codepilot-purple-face nil t)
    '(2 font-lock-variable-name-face nil t)
    '(4 font-lock-variable-name-face nil t))
   (list
    "^\\<\\([A-Z0-9_$.]+\\)\\> intersection"
    '(1 'codepilot-purple-face nil t))
   (list
    "^[ \t]*\\<\\([a-z0-9_]+\\)\\>"
    '(1 font-lock-comment-face nil t))
   (list
    (concat "\\(" msym-ref-classes-regexp "\\)")
    '(1 'codepilot-forest-green-face))
   (list
    (concat "\\(" msym-ref-attributes-regexp "\\)")
    '(1 font-lock-reference-face))

   (list
    (concat "\\_<\\(intersection\\)\\_>")
    '(1 font-lock-warning-face))

   )
  "font-lock keywords setting for MSym mode buffers.")

(defun msym-mode-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(msym-mode-font-lock-keywords t))
  )


(defvar msym-selected-ref-classes ""
  "This variable holds the selected classes and/or attributes to display
in the current buffer of MSym references. This variable is made local
in each MSym mode buffer.")

(make-variable-buffer-local 'msym-selected-ref-classes)


(defun msym-collapse-sections (&optional type)
  "Collapse all other sections except the one given in CLASS."
  (interactive "sWhich section type to show? (e.g., all, read, write,...) ")
  (if (or (string= type "") (string= type "all") (null type))
      (progn
        (show-all)
        (setq msym-selected-ref-classes ""))
    (let ((start (save-excursion (goto-char (point-min))
                                 (forward-line 1)
                                 (point)))
          (end (point-max))
          (type (downcase type)))
      (setq outline-regexp (concat "^\\([ \t]*[a-zA-Z_0-9]+[ \t]+[a-z/_: \t]*\\<"
                                   type "\\>\\| $\\)"))
      ;; (print outline-regexp)
      (hide-region-body start end)
      (setq msym-selected-ref-classes
            (if (string= msym-selected-ref-classes "")
                type
              (concat msym-selected-ref-classes "," type)))
      ))
  (setq mode-name (concat "MSym [" msym-selected-ref-classes "]"))
  (force-mode-line-update)
  )


(defun msym-read-symbol (prompt)
  ""
  (let ((cur (current-word))
        sym)
    (read-string
     (concat prompt (if cur (concat " (default " cur ")") "") ": ")
     nil nil cur))
  )

(defun msym-find-definition-section (sym &optional from-section class)
  ""
  (interactive)
  (let ((case-fold-search t)
        command
        buf
        section
        sym-new
        sym-with-class
        )
    (setq command (concat "fdef " sym " " from-section))
    (if (msym-send-wait command 0.1)
        ;;Error find....
        (message "Failed to find the definition!")

      (save-match-data
        (save-excursion
          (set-buffer msym-buffer)
          (goto-char comint-last-input-end)
          (forward-line)

;;           ;; brian for 22.2
;;           (while (looking-at "^$")
;;             (forward-line))

          (setq sym-new (current-word))
          (forward-word 2)
          (setq section (current-word))
          (forward-line)
          (unless (looking-at msym-prompt-regexp-1)
            ;; More than One result found!
            (cond (class
                   (setq sym-with-class (concat sym "$_$" class "$_$"))
                   (unless (re-search-forward (concat "^"
                                                      sym-with-class
                                                      " ")
                                            nil t)
                     (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                       (msym-create-id-buf (concat "[" (downcase sym-with-class) "]"
                                                   (if from-section
                                                       (concat "<" from-section ">")
                                                       ""
                                                       )
                                                   "(" msym-context ")"
                                                   )))
                     (error "Cannot find the exact definition!"))
                   )
                  (t
                   ;; More than One result found!
                   (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                     (msym-create-id-buf (concat "[" (downcase sym) "]"
                                                 (if from-section
                                                     (concat "<" from-section ">")
                                                     ""
                                                     )
                                                 "(" msym-context ")"
                                                 )))
                   ;;(error "More than one definition. Use Show References instead!")
                   (error "More than one definition.")
                   ))))))
    (values section sym-new)
    ))

(defun msym-goto-def-1 (sym &optional section class)
  ""
  (let ((case-fold-search t)
        buf)

    (multiple-value-bind (def-section sym-new)
        (msym-find-definition-section sym section class)
      (when def-section
        (when class (setq sym-new (concat sym-new "$_$" class "$_$")))
        (msym-create-id-buf (concat "[" (downcase sym-new) "]"
                                    "(" (downcase def-section) ")"
                                    "(D)"
                                    "(" msym-context ")"
                                    ))
        (setq buf (current-buffer))
        (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
          (msym-open-pls-section-from-context def-section sym-new 'def 1))
        (bury-buffer buf)


        ))))

(defvar protel-imenu-method-im-regexp "^\\[\\(.+\\)\\]::\\(.+\\)$")

(defun msym-goto-def (sym &optional class)
  ""
  (interactive
   (list
    (msym-read-symbol "MSym Goto Def")
    ))

  (unless class
    ;; try to get the class
    (let ((str (which-function)))
      (when str
        (let (id cls)
          (when (string-match protel-imenu-method-im-regexp str)
            (setq id (match-string 2 str))
            (setq cls (match-string 1 str))
            (when (string= id (downcase sym))
              (setq class cls))
            )))))

  (msym-goto-def-1 (codepilot-trim sym)
                   ;;(file-name-sans-extension (buffer-name))
                   (car (split-string (buffer-name) "\\."))
                   class
                   ))

(defun msym-goto-def-nosection (sym)
  ""
  (interactive
   (list
    (msym-read-symbol "Goto Def (w/o section suplied)")
    ))

  (msym-goto-def-1 (codepilot-trim sym))
  )

(defun msym-goto-imp-1 (sym &optional section class)
  ""
  (let ((case-fold-search t)
        buf)

    (multiple-value-bind (def-section sym-new ok)
        (msym-find-definition-section sym section class)
      (when def-section
        ;; (when class (setq sym-new (concat sym-new "$_$" class "$_$")))
        (msym-ref sym-new def-section "implementation" class)
        (when (= 1 (msym-count-ref-line))
          (goto-char (point-min))
          (forward-line 2)
          (setq buf (current-buffer))
          (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
            (msym-ref-open-section-and-go-1))
          (bury-buffer buf)
          )))))


(defun msym-goto-imp-nosection (sym)
  ""
  (interactive
   (list
    (msym-read-symbol "MSym Goto Imp (w/o section suplied)")
    ))

  (msym-goto-imp-1 (codepilot-trim sym))
  )


(defun msym-goto-imp (sym &optional class)
  ""
  (interactive
   (list
    (msym-read-symbol "MSym Goto Imp")
    ))

  (unless class
    ;; try to get the class
    (let ((str (which-function))
          (case-fold-search t)
          id cls cw)
      (when str
        ;; analyze the str
        (cond
          ((string-match protel-imenu-method-im-regexp str)
          ;;                (setq id (match-string 2 str))
          ;;                (setq cls (match-string 1 str))
          ;;                ;; Self.method?
          ;;                (save-excursion
          ;;                  (when (re-search-backward "\\_<" nil t)
          ;;                    (backward-char 5)
          ;;                    (when (looking-at "self\\.")
          ;;                      ;; (setq class cls)
          ;;                      )))
           )
          (t
           ;; not in method block
           ;; is it a method defined?
           (save-excursion
             (unless (looking-at "\\_>")
               (forward-word))

             (forward-word-out-of-comment)
             (setq cw (current-word))
             (when (and cw
                        (string= (downcase cw) "method"))
               (setq class str)))
           )))))

  (msym-goto-imp-1 (codepilot-trim sym) (file-name-sans-extension (buffer-name)) class)
  )

(defun msym-count-ref-line ()
  ""
  (interactive)
  (let ((case-fold-search t)
        beg end)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (setq beg (point))
      (setq end (point-max))
      (1- (count-lines beg end))
      )
    ))


(defun msym-show-ref-from-this-section (sym)
  "Get a cross-reference list for symbol under cursor using MSym. The definition
section is automatically determined based on the section where this ID is
referenced which is the one in the current buffer."
  (interactive
   (list
    (msym-read-symbol "Show Refs from this section")
    ))
  (let (section)
    (setq section (file-name-sans-extension (buffer-name)))
    (msym-show-ref-1 (codepilot-trim sym) section)
  ))

(defun msym-show-ref (sym)
  "Get a cross-reference list for symbol under cursor using MSym. The definition
section is automatically determined based on the section where this ID is
referenced which is the one in the current buffer."
  (interactive
   (list
    (msym-read-symbol "Show Refs")
    ))
  (msym-show-ref-1 (codepilot-trim sym))
  )

(defun msym-show-ref-1 (sym &optional section)
  "Get a cross-reference list for symbol under cursor using MSym. The definition
section is automatically determined based on the section where this ID is
referenced which is the one in the current buffer."
  (interactive
   (list
    (msym-read-symbol "Show Refs")
    ))

  (let ((case-fold-search t)
        buf
        command
        module
        id-buf-name
        one-result-p)

    (setq command (concat "fdef " sym " " section))
    (if (msym-send-wait command 0.1)
        ;;Error find....
        (message "Failed to find the definition!")

        (if (not section)
            (progn
              (save-excursion
                (set-buffer msym-buffer)
                (save-excursion
                  (goto-char comint-last-input-end)
                  (forward-line)

                  (setq sym (current-word))
                  (forward-word 2)
                  (setq module (current-word))
                  (forward-line)
                  (setq one-result-p (looking-at msym-prompt-regexp-1)))
                )
              ;; go head to process the fdef output.

              (cond (one-result-p
                     (setq id-buf-name (concat "[" (downcase sym) "](" (downcase module) ")(" msym-context ")"))
                     (setq buf (get-buffer id-buf-name))
                     (cond (buf
                            ;; see how many line it has.
                            ;; ##brian
                            (codepilot-pop-or-switch-buffer buf)
                            (set-buffer buf)
                            (when (= (count-lines (point-min) (point-max)) 1)
                              (message "Only result found!")
                              (goto-char (point-min))
                              (msym-ref-go-1))
                            )
                           (t
                            ;; go "xref"
                            (msym-find-ref sym module)

                            )))
                    (t
                     ;; More than One result found!
                     (msym-create-id-buf-and-go (concat "[" (downcase sym) "]" "(" msym-context ")"))
                     )))

            (set-buffer msym-buffer)
            (save-excursion
              (goto-char comint-last-input-end)
              (forward-line)
              (forward-word 2)
              (setq module (current-word))
              (forward-line)
              (setq one-result-p (looking-at msym-prompt-regexp-1))
              )
            (if one-result-p
                (progn
                  ;; go "xref"
                  (msym-find-ref sym module)
                  )
                ;; More than One result found!
                ;; (msym-create-id-buf-and-go (concat "[-temp-]" id-buf-name))
                (msym-create-id-buf-and-go (concat "[" (downcase sym) "]"
                                                   (concat "<" (downcase section) ">")
                                                   "(" msym-context ")"
                                                   ))
                )
            ))))

(defun msym-find-ref (sym module)
  ""
  (interactive)
  (msym-ref sym module "all")
  (when (= 1 (msym-count-ref-line))
    (goto-char (point-min))
    (forward-line 2)
    (msym-ref-open-section-and-go-1)
    ))

(defun msym-ref (sym module &optional search-type-str obj-class)
  ""
  (interactive)
  (let (command
        (cls (if search-type-str search-type-str "all"))
        sym-with-class try-again
        )

    (setq sym-with-class sym)
    (when obj-class
      (setq sym-with-class (concat sym "$_$" obj-class "$_$")))

    (setq command (concat "ref " sym-with-class ":" module " all " cls))
    (if (msym-send-wait command 0.1)
        (progn
          ;;Error find....
          (message (concat "Failed to find xref!(class:" cls ")"))

          (setq try-again t)
          (when obj-class
            (message "Try with class")
            (setq command (concat "ref " sym ":" module " all " cls))
            (if (msym-send-wait command 0.1)
                (progn
                  (message (concat "Failed to find xref (not obj class) !(class:" cls ")"))
                  )
                (msym-show-refs-output sym module "all")
                (setq try-again nil)
                ))
          (when try-again
            (unless (string= cls "all")
              (message (concat "Try class all instead " search-type-str "!"))
              (setq command (concat "ref " sym ":" module " all " "all"))
              (if (msym-send-wait command 0.1)
                  (message "Failed to find xref!(class: all)")
                  (msym-show-refs-output sym module "all")
                  )
              )))
        (msym-show-refs-output sym-with-class module cls)
        )))


(defun msym-create-id-buf (buf-name)
  ""
  (let (buf
        (inhibit-read-only t)
        ;; Don't generate undo entries for creation of the initial contents.
        (buffer-undo-list t)
        b e
        )

    (cond ((setq buf (get-buffer buf-name))
           (codepilot-pop-or-switch-buffer buf)
           )
          (t
           (setq buf (get-buffer-create buf-name))
           (codepilot-pop-or-switch-buffer buf)
           (set-buffer buf)
           (erase-buffer)
           (insert (msym-output t))
           (goto-char (point-min))
           (setq b (point))
           (forward-line)

           ;;     ;; brian for 22.2
           ;;     (while (looking-at "^$")
           ;;       (forward-line))

           (setq e (point))

           (delete-region b e)
           ;;(kill-line 1)
           (set-buffer-modified-p nil)
           (msym-id-mode)

           (let ((b (get-buffer cplist-buf-name)))
             (when b
               (with-current-buffer b
                 (with-modify-in-readonly
                     (goto-char (point-min))
                   (save-match-data
                     (when (re-search-forward "^\\[MSym ID List\\]")
                       (forward-line)
                       (insert "  " buf-name "\n")
                       (backward-char)
                       (remove-overlays (line-beginning-position) (line-beginning-position 2) 'tag 'myfilter)
                       ))))))))
    ))

(defun msym-create-id-buf-and-go (buf-name)
  ""
  (msym-create-id-buf buf-name)
  (if (> (count-lines (point-min) (point-max)) 1)
      ()
    (msym-id-mode)
      (message "Only result found!")
      (goto-char (point-min))
      (msym-ref-go-1)
      )
  )


(defun msym-show-refs-output (symbol module &optional search-type-str)
  "Put the ref output into its own buffer"
  (let ((case-fold-search t)
        (inhibit-read-only t)
        (buf (if (and search-type-str (string= search-type-str "implementation"))
                 (concat "[" (downcase symbol) "](" (downcase module) ")(I)(" msym-context ")")
               (concat "[" (downcase symbol) "](" (downcase module) ")(" msym-context ")")
               ))
        b1
        section
        b e)
    (setq b1 (get-buffer buf))

    (if b1 ;; brian
        (codepilot-pop-or-switch-buffer buf)

      (get-buffer-create buf)
      (codepilot-pop-or-switch-buffer buf)
      (set-buffer buf)
      (erase-buffer)
      (insert (msym-output t))

      (goto-char (point-min))

      (setq b (point))
      (forward-line)

      ;;       ;; brian for 22.2
      ;;       (while (looking-at "^$")
      ;;         (forward-line))

      (setq e (point))

      (delete-region b e)
      ;;(kill-line 1)
      (if (re-search-forward "\\<implementation\\>" nil 'move)
          (setq section (buffer-substring (progn (back-to-indentation) (point))
                                          (progn (forward-word 1) (point))))
        (setq section nil))

      (goto-char (point-min))
      (insert (concat (upcase symbol) " defined in " (upcase module)))
      (if section
          (insert " and implemented in " (upcase section)))
      (insert "\n \n")
                                        ;    (goto-char (point-min))
                                        ;    (setq tab-width 10)
                                        ;    (fixup-output)
      (goto-char (point-min))
      (forward-line 2)
      (back-to-indentation)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      ;;     (message (substitute-command-keys
      ;; "Position on a section and hit \\[protel-open-module-goto-symbol] to open it."))
      (msym-output-mode)

      (let ((b (get-buffer cplist-buf-name)))
        (when b
          (with-current-buffer b
            (with-modify-in-readonly
             (goto-char (point-min))
             (save-match-data
               (when (re-search-forward "^\\[MSym List\\]")
                 (forward-line)
                 (insert "  " buf "\n")
                 (backward-char)
                 (remove-overlays (line-beginning-position) (line-beginning-position 2) 'tag 'myfilter)
                 ))
             ))))
      )))

(defun msym-ref-open-section-and-go-1 ()
  (interactive)
  (let(section
       sym
       buf
       (pos (point))
       (point-wd (current-word))
       end
       search-type
       issue-point
       )

    (save-excursion
      (save-match-data
        (forward-line 0)
        (if (= (point) (point-min))
            (progn
              (setq sym (current-word))

              (cond ((string= point-wd "implemented")
                     (end-of-line)
                     (setq issue-point (point))
                     (setq section (current-word))
                     (setq search-type 'im))
                    (t
                     (forward-word 4)
                     (setq section (current-word))
                     (setq search-type 'def)
                     (setq issue-point 1)
                     )))
          (setq section (current-word))
          (setq issue-point (line-beginning-position))
          (if section
              (progn
                (goto-char (point-min))
                (setq sym (current-word))
                (let ((inhibit-read-only t)
                      (buffer-undo-list t))
                  (goto-char pos)
                  (end-of-line)
                  (setq end (point))
                  (forward-line 0)
                  (forward-word)
                  (cond ((save-excursion
                           (search-forward "implementation:" end t))
                         (setq search-type 'im))
                        ((save-excursion
                           (search-forward "definition:" end t))
                         (setq search-type 'def)))
                  (goto-char pos)
                  (end-of-line)
                  (unless (string= (current-word) "Browsed")
                    (insert "\tBrowsed")
                    (set-buffer-modified-p nil)
                    )))
            (error "Failed to get the section name.")
            ))))
    (msym-open-pls-section-from-context section sym search-type issue-point)
    ))

(defun msym-open-pls-section-from-context (section &optional sym search-type issue-point)
  ""
  (let (buf
        s-list
        class-id
        proc
        pos
        issue
        )

    (cond ((and issue-point
                (setq issue (get-text-property issue-point 'issue))
                )
           )
          (t
           (setq issue (pls-get-section-issue section))
           (when issue-point
             (with-modify-in-readonly
                 (put-text-property issue-point (1+ issue-point) 'issue issue)))
           ))

    (setq buf (pls-open-section-noselect (concat section "." issue)))

    (codepilot-pop-or-switch-buffer buf)
    (set-buffer buf)
    ;; (protel-mode)
    (goto-char (point-min))

    (when sym
      (setq s-list (split-string sym "\\$_\\$"))
      (when (> (length s-list) 1)
        (setq sym (nth 0 s-list))
        (setq class-id (nth 1 s-list))
        )

      (setq proc sym)
      (cond ((eq search-type 'im)
             (when class-id
               (setq proc (concat "[" class-id "]::" sym ))
               )
             (when (setq pos (protel-get-proc-pos-in-current-buf proc))
               (goto-char pos)))
            ((eq search-type 'def)
             (cond (class-id
                    (when (setq pos (protel-get-proc-pos-in-current-buf class-id))
                      (goto-char pos)))
                   (t
                    (when (setq pos (protel-get-proc-pos-in-current-buf sym))
                      (goto-char pos)))))
            (t
             (when (setq pos (protel-get-proc-pos-in-current-buf sym))
               (goto-char pos))))

      (protel-search-and-hl-text sym nil 'id class-id))

    ;;         (if codepilot-turn-on-linum
    ;;             (linum-mode 1))

    (codepilot-update-section-list)
    ))

(defun msym-open-pls-section-from-context-i (section &optional sym)
  ""
  (interactive
   (list
    (msym-read-symbol "Section name (no issue needed)")
    ))
  (msym-open-pls-section-from-context (codepilot-trim section))
  )


(defun msym-ref-open-section-and-go (event)
  (interactive "e")
  (mouse-set-point event)
  (msym-ref-open-section-and-go-1)
  )

(define-key msym-mode-map [mouse-3] 'msym-ref-open-section-and-go)
(define-key msym-mode-map [return] 'msym-ref-open-section-and-go-1)
(define-key msym-mode-map "k" 'kill-buffer)
(define-key msym-mode-map "g" 'cplist-list-id-section-bufs)
(define-key msym-mode-map "m" 'msym-mark-line)
(define-key msym-mode-map "u" 'msym-unmark-line)
(define-key msym-mode-map [tab] 'next-line)



(defun start-msym-with-pls ()
  "Start MSym process when PLS is started."
  (condition-case nil
      (progn
        (add-hook 'comint-input-filter-functions
                  'msym-catch-establish-command
                  t t)
        (msym-start-async))
    (error
     (message "Unable to start MSym" )
     (remove-hook 'comint-input-filter-functions
                  'msym-catch-establish-command
                  t))
        )
  )

(add-hook 'pls-mode-hook 'start-msym-with-pls)

(defun msym-catch-establish-command (str)
  "This function is executed before the command is sent to PLS process.  The
command string is passed to this function as STR."
  (if (string-match "establish " str)
      ;; The context is being established, push a handler on stack
      ;; to re-adjust the MSym context as well
      (pls-push-handler-on-stack 'msym-find-and-set-pls-context
                                 nil))
  )

(defvar msym-context "")

(defun msym-find-and-set-pls-context (args)
  "Find the PLS context that was set by the user."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^Establishing context for " nil t)
        (let (context)
          (goto-char (match-end 0))
          (setq context (current-word))
          (re-search-forward "^[ \t]+target ")
          (setq context (concat context "_" (current-word)))
          (setq msym-context context)
          (message "Sync context with MSym.")
          (msym-string-input (concat "context " context))
          )
      ))
  t)  ; this is to cause the next PLS handler to run



(defvar msym-id-mode-map nil
  "keymap for pls mode")

(defvar msym-id-mode-syntax-table nil
  "Syntax table in use in PLS buffers.")

(unless msym-id-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?$ "w" table)
    (modify-syntax-entry ?. "." table)
    (setq msym-id-mode-syntax-table table)))

(defvar msym-id-mode-font-lock-keywords
  (list

   (list
    "^\\(\\w+\\)\\_>"
    '(1 'codepilot-purple-face))

   (list
    "\\_<\\(Browsed\\)$"
    '(1 'codepilot-forest-green-face))
   )
  "font-lock keywords setting for MSym mode buffers.")

(defun msym-id-mode-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(msym-id-mode-font-lock-keywords t))
  )


(defun msym-id-mode ()
  "Put the current buffer into MSYM-ID mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'msym-id-mode)
  (setq mode-name "MSYM-ID")
  (use-local-map msym-id-mode-map)
  (setq case-fold-search t)
  (set-syntax-table msym-id-mode-syntax-table)
  (setq buffer-read-only t)
  (msym-id-mode-font-lock-setup)

  ;; serial number for sort by the create time.
  (make-local-variable 'cptree-serial-number)
  (setq cptree-serial-number cptree-serial-no-last)
  (setq cptree-serial-no-last (1+ cptree-serial-no-last))

  (run-mode-hooks 'msym-id-mode-hook))

(unless msym-id-mode-map
  (let ((map (make-sparse-keymap)))
    (setq msym-id-mode-map map)))

(define-key msym-id-mode-map [mouse-3] 'msym-ref-go)
(define-key msym-id-mode-map [return] 'msym-ref-go-1)
(define-key msym-id-mode-map "g" 'cplist-list-id-section-bufs)
(define-key msym-id-mode-map "k" 'kill-buffer)
(define-key msym-id-mode-map "m" 'msym-mark-line)
(define-key msym-id-mode-map "u" 'msym-unmark-line)



(defun msym-ref-go (e)
  ""
  (interactive "e")
  (mouse-set-point e)
  (msym-ref-go-1)
  )

(defun msym-ref-go-1 ()
  ""
  (interactive)
  (let ((case-fold-search t)
        (inhibit-read-only t)
        (buffer-undo-list t)
        sym
        module)
    (save-excursion
      (end-of-line)
      (unless (string= (current-word) "Browsed")
        (insert "\tBrowsed")
        (set-buffer-modified-p nil)
        )
      (forward-line 0)
      (setq sym (current-word))
      (forward-word 2)
      (setq module (current-word))
      )
;;     (when kill
;;       (kill-buffer (current-buffer)))
    (msym-find-ref sym module)
    ))

(defvar msym-id-d-entry-regexp "^\\[\\(.+\\)\\](.+)(D)(.+)$")
(defun msym-id-act-for-cplist-action ()
  ""
  (interactive)
  (if (eq major-mode 'msym-id-mode)
      (let ((case-fold-search t)
            def-section sym)
        (cond ((= (count-lines (point-min) (point-max)) 1)
               (save-excursion
                 (message "Only result found!")
                 (goto-char (point-min))
                 (setq sym (current-word))
                 (forward-word 2)
                 (setq def-section (current-word)))
               (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t)
                     (buf (current-buffer))
                     )
                 (msym-open-pls-section-from-context def-section sym 'def 1)
                 (bury-buffer buf)
                 ))
              (t
               (let ((buf-name (buffer-name)))
                 (when (string-match msym-id-d-entry-regexp buf-name)
                   (setq sym (match-string 1 buf-name))
                   (when (string-match "\\$_\\$" sym)
                     (save-excursion
                       (goto-char (point-min))
                       (when (re-search-forward (concat "^" (regexp-quote sym) " "))
                         (forward-line 0)
                         (forward-word 2)
                         (setq def-section (current-word))
                         (msym-open-pls-section-from-context def-section sym 'def (line-beginning-position))
                         ))))))))

    (when (eq major-mode 'msym-output-mode)
      (when (= 1 (msym-count-ref-line))
        (goto-char (point-min))
        (forward-line 2)
        (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t)
              (buf (current-buffer))
              )
          (msym-ref-open-section-and-go-1)
          (bury-buffer buf)
          )
        ))))

(add-hook 'cplist-action-hook 'msym-id-act-for-cplist-action)

(defun msym-mark-line ()
  ""
  (interactive)
  (let (beg end ov)

    (save-excursion
      (forward-line 0)
      (setq beg (point))
      (forward-line)
      (setq end (point))
      (setq ov (make-overlay beg end))
      (overlay-put ov 'face 'codepilot-hl-text-face)
      (overlay-put ov 'tag 'msym)
      )))

(defun msym-unmark-line ()
  ""
  (interactive)
  (dolist (ov (overlays-at (point)))
    (when (eq (overlay-get ov 'tag) 'msym)
      (delete-overlay ov)
      )))

(defvar msym-who-use-mouse 'codepilot)


(defvar protel-view-msym-menu
  '("MSym"
    :active (msym-active)
    ["Show References" msym-show-ref t]
    "-"
    ["Goto Definition" msym-goto-def t]
    ["Goto Implementation" msym-goto-imp t]
    "-"
    ["Show References from me" msym-show-ref-from-this-section t]
    "-"
    ["Goto Definition Globally" msym-goto-def-nosection t]
    ["Goto Implementation Globally" msym-goto-imp-nosection t]
    "-"
    ["Open Section from Context" msym-open-pls-section-from-context-i t]
    "-"
    ["Give me the mouse" msym-give-me-the-mouse
     :style toggle :selected (eq msym-who-use-mouse 'msym)]
    ))

(easy-menu-define protel-view-msym-menu-symbol
  protel-view-mode-map
  "protel-view menu"
  protel-view-msym-menu)


(defun msym-query-sym-under-mouse (e)
  ""
  (interactive "e")
  (mouse-set-point e)

  (if (eq msym-who-use-mouse 'msym)
      (msym-show-ref (current-word))
    (codepilot-query-sym (current-word))
    ))


(defun msym-give-me-the-mouse ()
  ""
  (interactive)
  (cond ((eq msym-who-use-mouse 'codepilot)
         ;(define-key protel-view-mode-map [mouse-3] 'msym-query-sym-under-mouse)
         (setq msym-who-use-mouse 'msym)
         )
        ((eq msym-who-use-mouse 'msym)
         ;(define-key protel-view-mode-map [mouse-3] 'codepilot-query-sym-under-mouse)
         (setq msym-who-use-mouse 'codepilot)
         )))


(defun protel-get-proc-pos-in-current-buf (proc-s)
  ""
  (interactive)

  ;; (find-index-in-imenu-list (downcase proc-s) imenu--index-alist)
  (find-index-in-imenu-list (downcase proc-s) (imenu--make-index-alist t))
  )


(defun find-index-in-imenu-list (sym ilist)
  ""
  (let (pos
        )
    (catch 'loop
      (dolist (ii ilist)

        (when (consp ii)
          (if (listp (cdr ii))
              (progn
                (setq pos (find-index-in-imenu-list sym ii))
                (if pos
                    (throw 'loop t)
                    ))
              (when (string= sym (car ii))
                (setq pos (cdr ii))
                (throw 'loop t))
              )
          )))
    pos
    ))

;; override the same proc defined in pls.el
(defun pls-open-section (section &optional pop)
  ""
  (interactive "sSection: ")
  (setq section (downcase section))
  (let ((buf (pls-open-section-noselect-1 section)))
    (if pop
        (pop-to-buffer buf)
      (switch-to-buffer buf)
      )
    ;; (protel-view)
    ))

(defun pls-find-file (filename)
  ""
  (find-file filename)
  ;; (protel-view)
  )




(define-key protel-view-mode-map [mouse-3] 'msym-query-sym-under-mouse)


(define-key msym-mode-map "," 'codepilot-previous-buffer)
(define-key msym-mode-map "." 'codepilot-forward-buffer)

(define-key msym-id-mode-map "," 'codepilot-previous-buffer)
(define-key msym-id-mode-map "." 'codepilot-forward-buffer)

;; (define-key protel-view-mode-map "0" 'msym-open-pls-section-from-context-i)
;; (define-key cplist-mode-map "0" 'msym-open-pls-section-from-context-i)

(define-key protel-view-mode-map "h" 'codepilot-goto-imp)
(define-key cptree-mode-map "h" 'codepilot-goto-imp)
(define-key cpxref-mode-map "h" 'codepilot-goto-imp)
(define-key cplist-mode-map "h" 'codepilot-goto-imp)

(define-key protel-view-mode-map "y" 'codepilot-goto-def)
(define-key cptree-mode-map "y" 'codepilot-goto-def)
(define-key cpxref-mode-map "y" 'codepilot-goto-def)
(define-key cplist-mode-map "y" 'codepilot-goto-def)


(defun define-key-for-keymaps (key def keymap-list)
  (mapc (lambda (keymap)
          (define-key keymap key def))
        keymap-list))

;; cplist-mode-map
;; cptree-mode-map
;; cpxref-mode-map
;; msym-id-mode-map
;; msym-mode-map
;; protel-view-mode-map

(define-key msym-id-mode-map "p" 'codepilot-query-proc)
(define-key msym-id-mode-map "i" 'codepilot-query-id)
(define-key msym-id-mode-map "o" 'codepilot-query-sym)
(define-key msym-id-mode-map "l" 'codepilot-query-string)
(define-key msym-id-mode-map "c" 'codepilot-query-comment)
(define-key msym-id-mode-map "t" 'codepilot-query-patch)
(define-key msym-id-mode-map "m" 'codepilot-query-module)
(define-key msym-id-mode-map "n" 'codepilot-query-section)
(define-key msym-id-mode-map "P" 'codepilot-query-proc-dim)
(define-key msym-id-mode-map "I" 'codepilot-query-id-dim)
(define-key msym-id-mode-map "O" 'codepilot-query-sym-dim)

(define-key msym-mode-map "p" 'codepilot-query-proc)
(define-key msym-mode-map "i" 'codepilot-query-id)
(define-key msym-mode-map "o" 'codepilot-query-sym)
(define-key msym-mode-map "l" 'codepilot-query-string)
(define-key msym-mode-map "c" 'codepilot-query-comment)
(define-key msym-mode-map "t" 'codepilot-query-patch)
(define-key msym-mode-map "m" 'codepilot-query-module)
(define-key msym-mode-map "n" 'codepilot-query-section)
(define-key msym-mode-map "P" 'codepilot-query-proc-dim)
(define-key msym-mode-map "I" 'codepilot-query-id-dim)
(define-key msym-mode-map "O" 'codepilot-query-sym-dim)
(define-key msym-mode-map "8" 'msym-show-ref)
(define-key msym-mode-map "D" 'msym-goto-def-nosection)
(define-key msym-mode-map "Q" 'msym-goto-imp-nosection)


(define-key cptree-mode-map "8" 'msym-show-ref)
(define-key cptree-mode-map "5" 'msym-show-ref-from-this-section)
(define-key cptree-mode-map "D" 'msym-goto-def-nosection)
(define-key cptree-mode-map "Q" 'msym-goto-imp-nosection)
(define-key cptree-mode-map "q" 'msym-goto-imp)

(define-key cpxref-mode-map "8" 'msym-show-ref)
(define-key cpxref-mode-map "5" 'msym-show-ref-from-this-section)
(define-key cpxref-mode-map "d" 'msym-goto-def)
(define-key cpxref-mode-map "D" 'msym-goto-def-nosection)
(define-key cpxref-mode-map "Q" 'msym-goto-imp-nosection)
(define-key cpxref-mode-map "q" 'msym-goto-imp)

(add-to-list 'auto-mode-alist '("\\.[a-z][a-z][0-9][0-9]$" . protel-mode))
(add-to-list 'auto-mode-alist '("\\.fix$" . protel-mode))
(add-to-list 'auto-mode-alist '("\\.sdelta$" . protel-mode))
(add-to-list 'auto-mode-alist '("\\.protdms$" . protel-mode))

;; window layout enhancements:
(require 'mywinlayout)


(require 'myhighlight)


;; Hack on occur!
(require 'myoccurhack)

;; modified hook.
(require 'mytabbarhack)


(defun del-bufs-of-major-mode (maj-mode)
  (dolist (buf (buffer-list))
    (when (eq (with-current-buffer buf major-mode) maj-mode)
      (dolist (win (get-buffer-window-list buf))
        (delete-window win))
      (kill-buffer buf))
    ))

(defun del-occur-bufs ()
  (interactive)
  (del-bufs-of-major-mode 'occur-mode))

(defun del-xref-bufs ()
  (interactive)
  (del-bufs-of-major-mode 'cpxref-mode))

(defun del-inact-xref-bufs ()
  (interactive)
  (dolist (buf (buffer-list))
    (when (eq (with-current-buffer buf major-mode) 'cpxref-mode)
      (unless (get-buffer-window buf t)
        (kill-buffer buf))
      )))

(defun pls-switch-to-buffer (buffer)
  (codepilot-switch-to-buffer buffer))



(require 'myremember)

(define-key  cptree-mode-map "r" 're)
(define-key  protel-view-mode-map "r" 're)

(define-key  cptree-mode-map "R" 're-r)
(define-key  protel-view-mode-map "R" 're-r)


(define-key cpxref-mode-map "k" 'kill-this-buffer)

;; suppress the pop-up buffer when ftp error
(defun ange-ftp-error (host user msg)
  (save-excursion  ;; Prevent pop-to-buffer from changing current buffer.
    (let (;;(cur (selected-window))
          buf
          (pop-up-windows t))
      ;;(pop-to-buffer
      (setq buf (get-buffer-create
                 (ange-ftp-ftp-process-buffer host user)))
       ;;)
      (set-buffer buf)
      (goto-char (point-max))
      ;;(select-window cur)
      )
    (signal 'ftp-error (list (format "FTP Error: %s" msg)))))

(define-key protel-mode-map [(shift f11)] 'widen)
(define-key protel-mode-map [(control f11)] 'widen)


(defvar codepilot-all-keymaps (list cptree-mode-map
                                    cpxref-mode-map
                                    protel-view-mode-map
                                    cplist-mode-map
                                    msym-mode-map
                                    msym-id-mode-map))

(define-key-for-keymaps "`" 'cplist-minimize/restore-sidebar codepilot-all-keymaps)
(define-key-for-keymaps "0" 'delete-window codepilot-all-keymaps)
(define-key-for-keymaps "j" 'pop-to-mark-command codepilot-all-keymaps)
(define-key-for-keymaps "9" 'msym-open-pls-section-from-context-i codepilot-all-keymaps)

(define-key cplist-mode-map "0" 'cplist-side-window)

(global-set-key  [(meta f8)] 'cplist-minimize/restore-sidebar)

;; (define-key help-mode-map "0" 'delete-window)
(define-key help-mode-map "k" 'delete-window)
;; (define-key compilation-mode-map "k" 'delete-window)


;; ====query list: save and restore=====
(defcustom cplist-file-dir "c:/"
  "The Dir to put the IDList saved file."
  :type 'directory
  :group 'codepilot-convenience)

(defun write-buffer-list% (ll &optional s)
  (let (str line-count)
    (dolist (b ll)
      (with-current-buffer (car b)
        (setq line-count (count-lines (point-min) (point-max)))
        (setq str (buffer-string)))
      (insert (buffer-name (car b)) "\n")
      (insert (int-to-string line-count) "\n")
      (insert str s)
      (unless (eq ?\n (char-before))
        (insert "\n")
        )
      )))

(defun codepilot-save-query-list (filename)
  (interactive
   (list
    (read-file-name "Write CPList file: "
                    (if cplist-file-dir cplist-file-dir nil)
                    "cplistfile" nil nil)))

  (when filename
    (when (file-exists-p filename)
      (unless (y-or-n-p (concat "Override " (file-name-nondirectory filename)
                                "?                      "))
        (error "Abort..."))))

  (let (cptree-list msym-id-list msym-output-list count str line-count)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (cond
         ((eq major-mode 'cptree-mode)
          (push (cons buf cptree-serial-number) cptree-list))
         ((eq major-mode 'msym-id-mode)
          (push (cons buf cptree-serial-number) msym-id-list))
         ((eq major-mode 'msym-output-mode)
          (push (cons buf cptree-serial-number) msym-output-list)))))
    (setq cptree-list (sort cptree-list #'(lambda (a b) (< (cdr a) (cdr b)))))
    (setq msym-id-list (sort msym-id-list #'(lambda (a b) (< (cdr a) (cdr b)))))
    (setq msym-output-list (sort msym-output-list #'(lambda (a b) (< (cdr a) (cdr b)))))
    (with-temp-file filename
      ;;  (set-buffer standard-output)
      (insert "[Query List]\n" (int-to-string (length cptree-list)) "\n")
      (write-buffer-list% cptree-list "")
      (insert "[MSym ID List]\n" (int-to-string (length msym-id-list)) "\n")
      (write-buffer-list% msym-id-list "\n")
      (insert "[MSym List]\n" (int-to-string (length msym-output-list)) "\n")
      (write-buffer-list% msym-output-list "")
      )
    (values cptree-list msym-id-list msym-output-list)))

(defun read-a-buf-entry-to-a-buf (mj-func)
  (let (buf-name line from to buf content)
    "Assume at the entry beginning pos"
    (setq buf-name (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
    (forward-line)
    (setq line (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
    (forward-line)
    (setq from (point))
    (forward-line (1- (string-to-number line)))
    (end-of-line)
    (setq to (point))
    (setq content (buffer-substring-no-properties from to))
    (unless (get-buffer buf-name)
      (setq buf (get-buffer-create buf-name))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (insert content)
          (funcall mj-func)
          (setq buffer-read-only t)
          (set-buffer-modified-p nil))))
    buf))

(defvar cplist-heading-regexp "^\\[\\(Query List\\)\\|\\(MSym ID List\\)\\|\\(MSym List\\)\\]")

(defun codepilot-restore-query-list-from-file (filename)
  (interactive
   (list
    (read-file-name "Read CPList file: "
                    (if cplist-file-dir cplist-file-dir nil)
                    "cplistfile" nil nil)))

  (let (buf count mj-func)
    (with-temp-buffer
      (insert-file-contents filename)
      (while (re-search-forward cplist-heading-regexp nil t)
        (forward-line)
        (setq count
              (string-to-number
               (buffer-substring-no-properties (point) (progn (end-of-line) (point)))))
        (cond ((match-string 1)
               (setq mj-func
                     #'(lambda ()
                         (cptree-mode)
                         (save-excursion
                           (insert "\n")
                           (if t ; (> (line-number-at-pos) (window-height))
                               (progn
                                 (goto-char (point-min))
                                 (if (and (re-search-forward "\\]  Matches: \\([0-9]+\\)" nil t)
                                          (> (string-to-number (match-string 1)) 1))
                                     (cptree-fold-sublevel 1)
                                   (cptree-fold-sublevel 2)
                                   ))))
                         (goto-char (point-min))
                         (forward-line 2)
                         )))
              ((match-string 2)
               (setq mj-func #'(lambda ()
                                 (goto-char (point-min))
                                 (msym-id-mode)
                                 )))
              ((match-string 3)
               (setq mj-func #'(lambda ()
                                 (insert "\n")
                                 (goto-char (point-min))
                                 (msym-output-mode)))))

        (forward-line)
        (dotimes (i count)
          (cond ((not (eobp))
                 (read-a-buf-entry-to-a-buf mj-func)
                 (forward-line)
                 )
                (t (return))))))
    (cplist-list-id-section-bufs)))

(require 'bm)
(defun kill-this-buffer ()              ; for the menu bar
  "Kill the current buffer."
  (interactive)
  (let ((del t))
    (unless (equal (bm-lists) '(nil))
      (if (y-or-n-p "You have bookmark on this buffer. Do you really want to close it? ")
          (setq del t)
          (setq del nil)
          ))

    (when del
      (let ((buf (current-buffer)))

        (cond ((eq major-mode 'cpxref-mode)
               (delete-window)
               ;; (del-xref-bufs)
               )
              (t
               ;; (del-xref-bufs)
               (kill-buffer (current-buffer))
               ))))))


(pushnew "*XREF*" codepilot-buffer-to-bury)

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
          (when (or (eq m 'cptree-mode)
                    (eq m 'msym-id-mode)
                    (eq m 'msym-output-mode)
                    (eq m 'protel-mode))
            (with-modify-in-readonly
             (goto-char (point-min))
             (save-match-data
               (when (re-search-forward (concat "  " (regexp-quote name) "\n") nil t)
                 (cond ((eq m 'protel-mode)
                        (forward-line -2)
                        (delete-region (point)
                                       (progn
                                         (forward-line 2)
                                         (point))))
                       (t
                        (forward-line -1)
                        (delete-region (point)
                                       (progn
                                         (forward-line 1)
                                         (point)))
                        ))
                 )))))))))

(add-hook 'kill-buffer-hook 'codepilot-kill-buffer-action)

;; TRAP/SWER/Debug traceback go
;; "^[ \t]*\\(?:At \\)?[0-9A-Z]\\{8\\}[ =]\\(.+\\..+:.+\\+#[0-9]+\\)"

(defun codepilot-ct-from-trap/swer/debug ()
  (save-excursion
    (forward-line 0)
    (when (looking-at "^[ \t]*\\(?:At \\)?[0-9A-Z]\\{8\\}[ =]\\(.+\\..+:.+\\+#[0-9]+\\)")
      (codepilot-open-calltrack (match-string 1)))))

(defun codepilot-ct-from-trap/swer/debug-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (codepilot-ct-from-trap/swer/debug))

(global-set-key [mouse-3] 'codepilot-ct-from-trap/swer/debug-mouse)


(defun toggle-truncate-lines (&optional arg)
  "Toggle whether to fold or truncate long lines for the current buffer.
With prefix argument ARG, truncate long lines if ARG is positive,
otherwise don't truncate them.  Note that in side-by-side
windows, truncation is always enabled."
  (interactive "P")
  (setq truncate-lines
        (if (null arg)
            (not truncate-lines)
          (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (unless truncate-lines
    (let ((buffer (current-buffer)))
      (walk-windows (lambda (window)
                      (if (eq buffer (window-buffer window))
                          (set-window-hscroll window 0)))
                    nil t)))
  )


(define-key-for-keymaps "s" 'codepilot-search-hi-string codepilot-all-keymaps)
(define-key-for-keymaps "f" 'codepilot-search-hl-again-f codepilot-all-keymaps)
(define-key-for-keymaps "b" 'codepilot-search-hl-again-b codepilot-all-keymaps)

(require 'myimenu)


(require 'proc-outline)
(define-key protel-view-mode-map "]" 'proc-outline-where-we-are)
(define-key protel-view-mode-map "\\" 'protel-proc-outline)
(define-key protel-view-mode-map "-" 'proc-outline-which-procs-i-in)
(define-key protel-view-mode-map "z" 'proc-outline-search-id-and-which-procs)
(define-key protel-view-mode-map ";" 'proc-outline-search-id-and-which-procs)
(define-key protel-view-mode-map "[" 'proc-outline-search-id-and-which-procs)
(define-key protel-view-mode-map "3" 'proc-outline-blocktrace-and-procs-layout)
(define-key protel-view-mode-map "2" 'proc-outline-blocktrace-and-procs-layout)


;; deal with desktop
(require 'desktop)
(pushnew 'cplist-side-window-size desktop-globals-to-save)
(pushnew 'myimenu-show-with-cplist desktop-globals-to-save)
(pushnew 'myimenu-win-height desktop-globals-to-save)

;; (pushnew 'codepilot-current-search-text desktop-locals-to-save)
;; (pushnew 'codepilot-current-search-type desktop-locals-to-save)

;; (setq desktop-modes-not-to-save (delete 'protel-view-mode desktop-modes-not-to-save))



(defun codepilot-re-retrieve-pls/codepilot-section ()
  (interactive)
  (let (buf
        buf-name
        (s-text codepilot-current-search-text)
        (s-type codepilot-current-search-type)
        (pos (point))
        f-name
        )
    (when (eq major-mode 'protel-mode)
      (setq buf-name (buffer-name))
      (setq f-name (buffer-file-name))

      (cond ((yes-or-no-p (concat "Do you really want to delete file " f-name " and retrieve it again?"))
             (kill-buffer buf-name)
             (condition-case nil
                 (progn
                   (delete-file f-name)
                   (delete-file (concat f-name ".XREF"))
                   (let (win buf)
                     (when (setq buf (get-buffer "*XREF*"))
                       (when (setq win (get-buffer-window buf))
                         (delete-window win))
                       (kill-buffer buf)))
                   )
               (error nil)))
            (t
             (message "Abort!")))

      (cond ((string= (upcase buf-name) buf-name)
             ;; listing file
             ;; (codepilot-open-section (buffer-name) nil (line-number-at-pos))
             (if (codepilot-send-wait (concat "cl " (downcase buf-name)))
                 (error "*** ERROR ***"))
             (let ((case-fold-search t)
                   )
               (setq buf nil)
               (save-excursion
                 (save-match-data
                   (set-buffer codepilot-buffer)
                   (goto-char comint-last-input-end)
                   (when (re-search-forward "^Listing file: \\[\\(.*\\)\\]" nil t)
                     (setq buf (find-file-noselect (match-string 1)))
                     )))))
            (t
             (setq buf (pls-re-retrieve-and-open-section-noselect buf-name))
             ))
      (when buf
        (codepilot-pop-or-switch-buffer buf)
        (setq codepilot-current-search-text s-text)
        (setq codepilot-current-search-type s-type)
        (goto-char pos)
        (run-with-idle-timer 0.25 nil 'my-highlight-2 (current-buffer))
        ))))

(defun cptree-act-for-cplist-action ()
  (let ((buf-name (buffer-name))
        buf-context
        command
        )

    (when (eq major-mode 'cptree-mode)
      (unless (string-match "^\\[\\(.*\\)\\](\\(.+\\))" buf-name)
        (error "Wrong buffer name. Weird!")
        )

      (setq command (match-string 1 buf-name))
      (setq buf-context (match-string 2 buf-name))
      (unless (string= buf-context codepilot-context)
        (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
          (codepilot-send-and-output command))
        (bury-buffer buf-name)
        )
      )))

(add-hook 'cplist-action-hook 'cptree-act-for-cplist-action)

(defun codepilot-when-kill-emacs ()
  (let (win edges width)
    (when (setq win (cplist-get-list-win))

          (setq edges (window-edges win))
          (setq width (- (nth 2 edges) (nth 0 edges)))

          (unless (= width window-min-width)
            (cplist-save-current-list-win-size))

          (run-hooks 'cplist-win-del))))

(add-hook 'kill-emacs-hook 'codepilot-when-kill-emacs)

(defun codepilot-when-kill-emacs-query ()
  (when (y-or-n-p "Do you want to save the Query History (IDList) before quitting? ")
    (call-interactively 'codepilot-save-query-list)
    )
  t
  )


(add-hook 'kill-emacs-query-functions 'codepilot-when-kill-emacs-query)

(setq frame-title-format (list "%b - " `(:eval codepilot-context)))


(defun protel-find-sym-in-xref-mouse (e)
  (interactive "e")
  ;; (mouse-set-point e)
  (when (protel-is-listing-file?)
    (protel-find-sym-in-xref)))

(defun codepilot-pop-up-xref-when-mouse-click-set (symbol value)
  (set-default symbol value)
  (cond (value
         (define-key protel-view-mode-map [mouse-1] 'protel-find-sym-in-xref-mouse)
         )
        (t
         (define-key protel-view-mode-map [mouse-1] 'mouse-set-point))))

(defcustom codepilot-pop-up-xref-when-mouse-click nil
  "Whether pop up the XREF buffer automatically when the mouse click
in the listing file."
  :type 'boolean
  :set 'codepilot-pop-up-xref-when-mouse-click-set
  :group 'codepilot-convenience
  )

;; (define-key protel-view-mode-map [mouse-1] 'protel-find-sym-in-xref-mouse)
;; (define-key protel-view-mode-map [mouse-1] 'mouse-set-point)

(require 'mytoolbar)


(define-key protel-view-mode-map "=" 'myimenu-toggle)

(define-key-for-keymaps "3" 'proc-outline-blocktrace-and-procs-layout codepilot-all-keymaps)



(defun protel-search-and-hl-text-default (text &optional backward search-type class-id)
  ""
  (let ((case-fold-search t)
        (my-loop t)
        (goto-point (point))
        (success nil)
        s-func
        s-text
        (s-type (if search-type search-type 'id))
        ;; text-for-lazy-hl
        c
        )

    ;; brian debug
    ;; (message (format "%s, Point: %d, orig text: %s" (current-time-string) (point) codepilot-current-search-text))

    (when (= 0 (length text))
      (error "Search string is empty!")
      )

    (setq codepilot-current-search-text (downcase text))
    (setq codepilot-current-search-type search-type)

    (setq s-text codepilot-current-search-text)
    ;; (setq text-for-lazy-hl s-text)

    (cond ((eq search-type 'id)
           (if backward
               (setq s-func 'word-search-backward)
             (setq s-func 'word-search-forward))
           ;; (setq text-for-lazy-hl (concat "\\_<" (regexp-quote s-text) "\\_>"))
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
        (unless inhibit-my-highlight-2
          (run-with-idle-timer 0.0 nil 'my-highlight-2 (current-buffer))
          )
        (while my-loop
          (if (not (funcall s-func s-text nil t))
              (progn
                (setq my-loop nil))
            (cond ((and (eq s-type 'id)
                        (my-in-quote/comment))
                   ;; search again
                   ())
                  ((and (eq s-type 'comment)
                        (not (my-in-comment)))
                   ;;search again
                   ())
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
          ;; (my-highlight (point-min) (point-max) text-for-lazy-hl t t)
          ))
      (unless backward
        (backward-char)))
    ;; brian: debug
    ;; (message (format "%s, Point: %d, Current Point: %d" (current-time-string) goto-point (point)))
    success
    ))

(add-hook 'protel-mode-hook (lambda ()
                              (interactive)
                              (make-local-variable 'codepilot-search-and-hl-text-func)
                              (setq codepilot-search-and-hl-text-func 'protel-search-and-hl-text-default)
                              ))



(defun cplist-sort-query-by-create ()
  ""
  (interactive)

  (cplist-sort-frame
   "^\\[Query List\\]"
   "^$"
   cptree-serial-number
   'cptree-mode
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

(defun cplist-sort-query-by-last-access ()
  ""
  (interactive)

  (cplist-sort-frame
   "^\\[Query List\\]"
   "^$"
   nil
   'cptree-mode
   m-list
   (when m-list
     (dolist (b1 (nreverse m-list))
       (insert "  " b1 "\n")
       )
     ))
  (setq cplist-query-sort-type 'last)
  )


(defun cplist-sort-msym-by-create ()
  ""
  (interactive)

  (cplist-sort-frame
   "^\\[MSym List\\]"
   "^$"
   cptree-serial-number
   'msym-output-mode
   m-list
   (when m-list
     (setq m-list (sort m-list (lambda (a b)
                                 (>  (car a)  (car b))
                                 )))

     (dolist (b1 m-list)
       (insert "  " (cdr b1) "\n")
       )
     ))
  (setq cplist-msym-sort-type 'create)
  )

(defun cplist-sort-msym-id-by-create ()
  ""
  (interactive)

  (cplist-sort-frame
   "^\\[MSym ID List\\]"
   "^$"
   cptree-serial-number
   'msym-id-mode
   m-list
   (when m-list
     (setq m-list (sort m-list (lambda (a b)
                                 (>  (car a)  (car b))
                                 )))

     (dolist (b1 m-list)
       (insert "  " (cdr b1) "\n")
       )
     ))
  (setq cplist-msym-id-sort-type 'create)
  )

(defun cplist-sort-msym-list-by-name ()
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
        (when (re-search-forward "^\\[MSym List\\]" nil t)
          (forward-line)
          (setq beg (point))
          (setq end beg)
          (when (re-search-forward "^$" nil t)
            (setq end (point))
            )
          (unless (= beg end)
            (sort-lines nil beg end)
            )
          (setq cplist-msym-sort-type 'name)
          ))
      )
    (set-buffer-modified-p nil)
    ))

(defun cplist-sort-msym-id-list-by-name ()
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
        (when (re-search-forward "^\\[MSym ID List\\]" nil t)
          (forward-line)
          (setq beg (point))
          (setq end beg)
          (when (re-search-forward "^$" nil t)
            (setq end (point))
            )
          (unless (= beg end)
            (sort-lines nil beg end)
            )
          (setq cplist-msym-id-sort-type 'name)
          ))
      )
    (set-buffer-modified-p nil)
    ))


(defun cplist-sort-msym-last ()
  ""
  (interactive)
  (cplist-sort-frame
   "^\\[MSym List\\]"
   "^$"
   nil
   'msym-output-mode
   m-list
   (when m-list
     (dolist (b1 (nreverse m-list))
       (insert "  " b1 "\n")
       )))

    (setq cplist-msym-sort-type 'last)
  )

(defun cplist-sort-msym-id-last ()
  ""
  (interactive)
  (cplist-sort-frame
   "^\\[MSym ID List\\]"
   "^$"
   nil
   'msym-id-mode
   m-list
   (when m-list
     (dolist (b1 (nreverse m-list))
       (insert "  " b1 "\n")
       )))

    (setq cplist-msym-id-sort-type 'last)
  )


(defun cplist-sort-section-id ()
  ""
  (interactive)
  (cplist-sort-frame
   "^\\[Section List\\]"
   "^$"
   codepilot-current-search-text
   'protel-mode
   m-list
   (when m-list
     (setq m-list (sort m-list (lambda (a b)
                                 (string< (upcase (car a)) (upcase (car b)))
                                 )))
     (dolist (b1 m-list)
       (insert (concat " <" (car b1) ">\n"))
       (insert "  " (cdr b1) "\n")
       )))
  (setq cplist-section-sort-type 'id)
  )

(defun cplist-sort-section-name ()
  ""
  (interactive)
  (cplist-sort-frame
   "^\\[Section List\\]"
   "^$"
   codepilot-current-search-text
   'protel-mode
   m-list
   (when m-list
     (setq m-list (sort m-list (lambda (a b)
                                 (string< (upcase (cdr a)) (upcase (cdr b)))
                                 )))
     (dolist (b1 m-list)
       (insert (concat " <" (car b1) ">\n"))
       (insert "  " (cdr b1) "\n")
       )))

    (setq cplist-section-sort-type 'name)
  )

(defun cplist-sort-section-last ()
  ""
  (interactive)
  (cplist-sort-frame
   "^\\[Section List\\]"
   "^$"
   codepilot-current-search-text
   'protel-mode
   m-list
   (when m-list
     (dolist (b1 (nreverse m-list))
       (insert (concat " <" (car b1) ">\n"))
       (insert "  " (cdr b1) "\n")
       )))

    (setq cplist-section-sort-type 'last)
  )


(defvar cplist-menu
  '("CPList"
    ["Mark delete" cplist-mark-for-delete t]
    ["Unmark" cplist-unmark t]
    "-"
    ["Do mark action" cplist-do-kill-on-deletion-marks t]
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
    ["Sort MSym ID List by create" cplist-sort-msym-id-by-create
     :style toggle :selected (eq cplist-msym-id-sort-type 'create)]
    ["Sort MSym ID List by name" cplist-sort-msym-id-list-by-name
     :style toggle :selected (eq cplist-msym-id-sort-type 'name)]

    ["Sort MSym ID List by last" cplist-sort-msym-id-last
     :style toggle :selected (eq cplist-msym-id-sort-type 'last)]


    "-"
    ["Sort MSym List by create" cplist-sort-msym-by-create
     :style toggle :selected (eq cplist-msym-sort-type 'create)]
    ["Sort MSym List by name" cplist-sort-msym-list-by-name
     :style toggle :selected (eq cplist-msym-sort-type 'name)]

    ["Sort MSym List by last" cplist-sort-msym-last
     :style toggle :selected (eq cplist-msym-sort-type 'last)]

    "-"
    ["Sort sections by last access" cplist-sort-section-last
     :style toggle :selected (eq cplist-section-sort-type 'last)]
    ["Sort sections by search id" cplist-sort-section-id
     :style toggle :selected (eq cplist-section-sort-type 'id)]
    ["Sort sections by name" cplist-sort-section-name
     :style toggle :selected (eq cplist-section-sort-type 'name)]
    "-"
    ["Save Current IDList Window width" cplist-save-current-list-win-size t]
    ))

(easy-menu-define cplist-menu-symbol
                  cplist-mode-map
                  "CPlist menu"
                  cplist-menu)




(defun cplist-action ()
  ""
  (interactive)
  (let ((case-fold-search t)
        buf-name
        cur-buf
        )
    (save-match-data
      (forward-line 0)
      (when (looking-at "^ <\\(.*\\)>")
        (forward-line))
      (cond ((looking-at "[A-Za-Z ] \\(.+\\)$")
             (setq buf-name (match-string 1))
             (cond ((string-match "^\\[ct " buf-name)
                    (set-buffer buf-name)
                    (run-hooks 'cplist-action-hook)
                    (setq cur-buf (current-buffer))
                    (let ((inhibit-calltrak-warning-msg-box t))
                      (codepilot-open-calltrack-1 (buffer-name)))
                    (bury-buffer cur-buf)
                    )
                   (t
                    (when (get-buffer buf-name)
                      (let ((inhibit-codepilot-pre-pop-or-switch-buffer-hook t))
                        (codepilot-pop-or-switch-buffer buf-name))
                      (run-hooks 'cplist-action-hook)
                      ))))
            ((looking-at "^\\[")
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
                   (cond ((re-search-forward "^\\[" nil t)
                          (forward-line 0)
                          (setq e (1- (point))))
                         (t
                          (setq e (point-max))))
                   (cptree-hide-region b e 'cptree)
                   )))
             )))))


(defun cplist-list-id-section-bufs ()
  ""
  (interactive)
  (let ((buf (get-buffer-create cplist-buf-name))
        (win (selected-window))
        id)
                                        ;(save-window-excursion
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; Don't generate undo entries for creation of the initial contents.
            (buffer-undo-list t))

                                        ;(setq buffer-read-only nil)
        (erase-buffer)

        (insert "[Query List]\n")
        (if (not (eq cplist-query-sort-type 'create))
            (progn
              (dolist (b (buffer-list))
                (if (eq (with-current-buffer b  major-mode) 'cptree-mode)
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
            (cplist-sort-query-by-create)
            )

        (insert "[MSym ID List]\n")
        (when (eq cplist-msym-sort-type 'name)
          (dolist (b (buffer-list))
            (if (eq (with-current-buffer b  major-mode) 'msym-id-mode)
                (insert "  " (concat (buffer-name b) "\n")))
            ))

        (insert "\n")

        (cond ((eq cplist-msym-id-sort-type 'create)
               (cplist-sort-msym-id-by-create)
               )
              ((eq cplist-msym-id-sort-type 'name)
               (cplist-sort-msym-id-list-by-name)
               )
              ((eq cplist-msym-id-sort-type 'last)
               (cplist-sort-msym-id-last)
               )
              (t
               ()))


        (insert "[MSym List]\n")
        (when (eq cplist-msym-sort-type 'name)
          (dolist (b (buffer-list))
            (if (eq (with-current-buffer b  major-mode) 'msym-output-mode)
                (insert "  " (concat (buffer-name b) "\n")))
            ))

        (insert "\n")

        (cond ((eq cplist-msym-sort-type 'create)
               (cplist-sort-msym-by-create)
               )
              ((eq cplist-msym-sort-type 'name)
               (cplist-sort-msym-list-by-name)
               )
              ((eq cplist-msym-sort-type 'last)
               (cplist-sort-msym-last)
               )
              (t
               ()))

        (insert "[Section List]\n")
        (insert "\n")

        (cond ((eq cplist-section-sort-type 'last)
               (cplist-sort-section-last)
               )
              ((eq cplist-section-sort-type 'id)
               (cplist-sort-section-id)
               )
              ((eq cplist-section-sort-type 'name)
               (cplist-sort-section-name)
               ))

        (set-buffer-modified-p nil)
        ))

    (multiple-value-bind (ret sidebar code-win bottom-win)
        (window-layout-wise)
      (case ret
        ((:window-layout-1&1
          :window-layout-1&2+)
         (select-window sidebar)
         (set-window-dedicated-p (selected-window) nil)
         (switch-to-buffer buf)
         (setq buffer-read-only t)
         (goto-char (point-min))
         (cplist-mode)
         (myfilter-add-edit-entry-field)
         (set-window-dedicated-p (selected-window) t)
         )))

    ;; (select-window win)
    )
  ;; (del-inact-xref-bufs)
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
            (kill-buffer b))
        )
      ;;(cplist-list-id-section-bufs)
      (let (sec-pos)
        (save-excursion
          (save-match-data
            (goto-char 1)
            (when (re-search-forward "^\\[Section List\\]$" nil t)
              (setq sec-pos (point))
              (cplist-kill-del-mark-lines sec-pos (point-max) :sec-list)
              (cplist-kill-del-mark-lines 1 sec-pos)
              ))))
      )))


