;;; codepilot.el --- 

;; Authors: Brian Jiang
;; Maintainer: Brian Jiang <brianjiang AT gdnt DOT com DOT cn>
;; Keywords: language protel

;; Keywords: language protel

;; $Revision: 0.1+ $  2008.01.11
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



(defvar ct-pg-line-dot-regex "^[ 0-9-]\\{7\\}[0-9-]   \\(\\(?:\\.  \\)+\\)")
(defvar ct-pg-line-regex        "^\\(?:[ 0-9-]\\{7\\}[0-9-]   \\|  Proc     \\)\\(\\(?:\\.  \\)*\\)")
(defvar ct-pg-start-regex "^\\(    proc   P\\| Event     P\\)")
(defvar ct-pg-level-0-regex     "^\\(?:[ 0-9-]\\{7\\}[0-9-]   \\|  Proc     \\)[0-9A-Z]\\{8\\} ")
(defvar ct-pg-level-regex-fmt   "^\\(?:[ 0-9-]\\{7\\}[0-9-]   \\|  Proc     \\)\\(\\(?:\\.  \\)\\{%d\\}\\)[0-9A-Z]")
(defvar ct-pg-level-regex-fmt-2 "^\\(?:[ 0-9-]\\{7\\}[0-9-]   \\|  Proc     \\)\\(\\(?:\\.  \\)\\{%d,%d\\}\\)[0-9A-Z]")

(defun ct-pg-current-level (&optional noerror)
  (save-excursion
    (save-match-data
      (forward-line 0)
      (ct-pg-current-level-1)
      )))

(defun ct-pg-current-level-1 (&optional noerror)
  "Assume the point is in the beginning of line."
  (let (n)
    (cond ((looking-at ct-pg-line-regex)
           (setq n (/ (length (match-string 1)) 3))
           (cond ((> n 0)
                  n)
                 (t (cond ((save-excursion
                             (forward-char 11)
                             (looking-at "[0-9A-Z]\\{8\\}"))
                           n)
                          (t -2)))))
          ((looking-at ct-pg-start-regex)
           -1)
          (t
           ;; (message "point: %d" (point))
           ;; (debug)
           ;; not pg line
           -2))))

(defvar ct-pg-address-start-colummn 11)
(defun ct-pg-go-up-level (&optional same-level)
  ""
  (interactive)
  (let (cl pos)
    (save-excursion
      (save-match-data
        (setq cl (ct-pg-current-level))
        (forward-line 0)
        (cond ((= cl 0)
               (re-search-backward ct-pg-start-regex nil t)
               (setq pos (point)))
              ((= cl -1))
              ((re-search-backward
                (format ct-pg-level-regex-fmt (if same-level cl (1- cl))) nil t)
               (setq pos (point))
               ))))
    (when pos
      (push-mark (point) t)
      (goto-char pos)
      (move-to-column ct-pg-address-start-colummn)
      (skip-chars-forward ". \t")
      )))


(defun mycalltrak-show-line-at-point ()
  (let (from to)
    (dolist (o (overlays-at (point)))
      (when (eq (overlay-get o 'invisible) 'mycalltrak-hide)
        (forward-line 0)
        (backward-char)
        (setq from (point))
        (forward-line)
        (end-of-line)
        (forward-char)
        (setq to (point))
        (remove-overlays from to 'invisible 'mycalltrak-hide)
        ))))

(defun ct-pg-go-previous-same-level ()
  (interactive)
  (let (re-search cl pos end-l)
    (save-excursion
      (save-match-data
        (setq cl (ct-pg-current-level))
        (forward-line 0)
        (cond ((= cl 0)
               (re-search-backward (concat ct-pg-level-0-regex "\\|" ct-pg-start-regex) nil t)
               (setq pos (point))
               (mycalltrak-show-line-at-point))
              ((= cl -1)
               (when (re-search-backward ct-pg-start-regex nil t)
                 (setq pos (point))
                 (mycalltrak-show-line-at-point)))
              ((re-search-backward (format ct-pg-level-regex-fmt-2 0 cl) nil t)
               (print (format ct-pg-level-regex-fmt-2 0 cl))
               (setq end-l (/ (length (match-string 1)) 3))
               (if (> cl end-l)
                   (message "Not same level. Go to the up level"))
               (setq pos (point))
               (mycalltrak-show-line-at-point)
               ))))
    (when pos
      (push-mark (point) t)
      (goto-char pos)
      (move-to-column ct-pg-address-start-colummn)
      (skip-chars-forward ". \t"))))

(defun ct-pg-go-next-same-level ()
  (interactive)
  (let (re-search cl pos end-l)
    (save-excursion
      (save-match-data
        (setq cl (ct-pg-current-level))
        (end-of-line)
        (cond ((= cl 0)
               (if (re-search-forward (concat "\\(" ct-pg-level-0-regex
                                              "\\)\\|\\(" ct-pg-start-regex
                                              "\\)") nil t)
                   (cond ((match-string 1)
                          (setq pos (point)))
                         ((match-string 2)
                          (setq pos (point))
                          (forward-line 0)
;;                           (when (re-search-backward ct-pg-line-regex nil t)
;;                             (forward-line)
;;                             (setq pos (1+ (point))))
                          ))
                   (goto-char (point-max))
                   (setq pos (point-max))
;;                    (when (re-search-backward ct-pg-line-regex nil t)
;;                      (forward-line)
;;                      (setq pos (1+ (point))))
                   ))
              ((= cl -1)
               (cond ((re-search-forward ct-pg-start-regex nil t)
                      (setq pos (point)))
                     (t (setq pos (point-max)))))
              ((re-search-forward
                (concat (format ct-pg-level-regex-fmt-2  0 cl)
                        "\\|" ct-pg-start-regex) nil t)
               (cond ((setq end-l (match-string 1))
                      (setq end-l (/ (length end-l) 3))
                      (if (> cl end-l)
                          (message "No same level any more."))
                      (setq pos (point)))
                     (t
                      (setq pos (point))
                      (forward-line 0)
;;                       (when (re-search-backward ct-pg-line-regex nil t)
;;                         (forward-line)
;;                         (setq pos (1+ (point))))
                      )))
              (t
               (goto-char (point-max))
               (setq pos (point))
;;                (when (re-search-backward ct-pg-line-regex nil t)
;;                  (forward-line)
;;                  (setq pos (1+ (point))))
               ))))
    (when pos
      (push-mark (point) t)
      (goto-char (1- pos)))))


(defvar ct--overlay-keymap nil "keymap for folding overlay")

(unless ct--overlay-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ct-ov-delete)
    (setq ct--overlay-keymap map)
    ))

(defun ct-ov-delete ()
  ""
  (interactive)
  (dolist (o (overlays-at (point)))
    (ct-delete-overlay o 'ct))
  t)

(defun ct-hide-region (from to)
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((new-overlay (make-overlay from to)))
    ;;(overlay-put new-overlay 'invisible nil)
    (overlay-put new-overlay 'ct-tag 'ct)
    (overlay-put new-overlay 'face 'codepilot-folding-overlay)
    (overlay-put new-overlay 'display
		 (propertize
		  (format "%s<%d lines>%s"
			   my-hide-region-before-string
			  (1- (count-lines (overlay-start new-overlay)
					   (overlay-end new-overlay)))
			  my-hide-region-after-string
			  )))
    (overlay-put new-overlay 'priority (- 0 from))
    (overlay-put new-overlay 'keymap ct--overlay-keymap)
    (overlay-put new-overlay 'pointer 'hand)
    ))

(defun ct-delete-overlay(o prop)
  (if (eq (overlay-get o 'ct-tag) prop)
      (progn
	(delete-overlay o) t)
    nil))


(defun ct-unfold-all()
  ""
  (interactive)
  (save-excursion
    (dolist (o (overlays-in (point-min) (point-max)))
      (ct-delete-overlay o 'ct)
      )))

(defun ct-pg-fold-subtree-1 ()
  (let (from to)
    (setq from (progn (end-of-line) (point)))
    (ct-pg-go-next-same-level)
    (forward-line 0)
    (backward-char)
    (setq to (point))
    (ct-hide-region from to)
    ))

(defun ct-pg-fold-subtree ()
  (interactive)
  (save-excursion
    (ct-pg-fold-subtree-1)
    ))

(defun ct-pg-fold-subtree-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (save-excursion
    (ct-pg-fold-subtree-1)
    ))

(defun ct-pg-fold/unfold-subtree ()
  (interactive)
  (let (unfold)
    (save-excursion
      (end-of-line)
      (dolist (o (overlays-at (point)))
        (when (eq (overlay-get o 'ct-tag) 'ct)
          (setq unfold t)
          (return)))
      
      (cond (unfold
             (ct-ov-delete))
            (t
             (ct-pg-fold-subtree-1))))))

(defun ct-pg-fold/unfold-subtree-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (ct-pg-fold/unfold-subtree))

(defun ct-pg-fold-all-subtree ()
  (interactive)
  (ct-unfold-all)
  (let (last-level cur-level pos-stack from to delta)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (when (re-search-forward ct-pg-start-regex nil t)
          (catch 'loop1
            (while t
              (when (eobp)
                (setq to (point))
                (while (setq from (pop pos-stack))
                  (ct-hide-region from to))
                (throw 'loop1 t))
              (setq last-level -1)
              (forward-line)
              
              (catch 'loop2
                (while (not (eobp))
                  (setq cur-level (ct-pg-current-level-1))
                  (cond ((> cur-level last-level)
                         (save-excursion
                           (forward-line 0)
                           (backward-char)
                           (push (point) pos-stack))
                         (setq last-level cur-level))
                        ((= cur-level -1)
                         (save-excursion
                           (forward-line 0)
                           (backward-char)
                           (setq to (point)))
                         (while (setq from (pop pos-stack))
                           (ct-hide-region from to))
                         (throw 'loop2 :proc-start))
                        ((= cur-level -2))
                        (t
                         ;; (<= cur-level last-level)
                         (setq delta (- last-level cur-level))
                         (save-excursion
                           (forward-line 0)
                           (backward-char)
                           (setq to (point)))
                         (dotimes (i delta)
                           (cond ((setq from (pop pos-stack))
                                  (ct-hide-region from to))
                                 (t
                                  (message "parsing error %d" (point))
                                  ;; (error "error when parsing the tree.")
                                  )))
                         (setq last-level cur-level)))
                  (forward-line)
                  )))))))))


(defun ct-pg-fold-subtree-under-point ()
  (interactive)
  (let ((level (ct-pg-current-level)))
    (save-excursion (ct-pg-unfold-subtree-1))
    (save-excursion (ct-pg-fold-subtree-under-level-1 level))))


(defun ct-pg-unfold-region (from to)
  (dolist (o (overlays-in from to))
    (ct-delete-overlay o 'ct)))

(defun ct-pg-unfold-subtree-1 ()
  (let (from to)
    (setq from (progn (end-of-line) (point)))
    (ct-pg-go-next-same-level)
    (forward-line 0)
    (backward-char)
    (setq to (point))
    (ct-pg-unfold-region from to)
    ))

(defun ct-pg-unfold-subtree ()
  (save-excursion
    (ct-pg-unfold-subtree-1)))

(defun ct-pg-fold-subtree-under-level-1 (level)
  "Assume: point in the line of the pg of the level."
  (let (last-level cur-level pos-stack from to delta)
    (catch 'loop1
      (setq last-level level)
      (forward-line)
      (catch 'loop2
        (while (not (eobp))
          (setq cur-level (ct-pg-current-level-1))
          (cond ((> cur-level last-level)
                 (save-excursion
                   (forward-line 0)
                   (backward-char)
                   (push (point) pos-stack))
                 (setq last-level cur-level))
                ((= cur-level -1)
                 (save-excursion
                   (forward-line 0)
                   (backward-char)
                   (setq to (point)))
                 (while (setq from (pop pos-stack))
                   (ct-hide-region from to))
                 (throw 'loop2 :proc-start))
                ((= cur-level -2))
                ((= cur-level level)
                 ;; (<= cur-level last-level)
                 (setq delta (- last-level cur-level))
                 (save-excursion
                   (forward-line 0)
                   (backward-char)
                   (setq to (point)))
                 (while (setq from (pop pos-stack))
                   (ct-hide-region from to))
                 (throw 'loop1 :done))
                (t
                 (setq delta (- last-level cur-level))
                 (save-excursion
                   (forward-line 0)
                   (backward-char)
                   (setq to (point)))
                 (dotimes (i delta)
                   (cond ((setq from (pop pos-stack))
                          (ct-hide-region from to))
                         (t
                          (message "parsing error %d" (point))
                          ;; (error "error when parsing the tree.")
                          )))
                 (cond ((<= cur-level level)
                        (throw 'loop1 :done))
                       (t (setq last-level cur-level)))))
          (forward-line)
          ))
            
      (when (eobp)
        (setq to (point))
        (while (setq from (pop pos-stack))
          (ct-hide-region from to))))))

(defvar mycalltrak-font-lock-keywords
  (list
   (list "^    proc   Procedure\\| Event     Procedure\\|^INCOMING\\|^OUTGOING"
         '(0 font-lock-warning-face)))
  "font-lock keywords setting for Mycalltrak buffers.")

(defun mycalltrak-font-lock-setup ()
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mycalltrak-font-lock-keywords t)))


(defvar mycalltrak-mode-map nil
  "keymap for cplist mode")

(unless mycalltrak-mode-map
  (let ((map (make-sparse-keymap)))
    (setq mycalltrak-mode-map map)))


(defun mycalltrak-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mycalltrak-mode)
  (setq mode-name "My Calltrak")
  (use-local-map mycalltrak-mode-map)
  (setq case-fold-search t)
  (set-syntax-table protel-mode-syntax-table)
  (mycalltrak-font-lock-setup)
  (run-mode-hooks 'codepilot-mode-hook))

(defvar mycalltrak-menu
    '("MyCalltrak"
      ["Fold/unfold substree" ct-pg-fold/unfold-subtree t]
      ["Goto uplevel" ct-pg-go-up-level t]
      ["Same level previous" ct-pg-go-previous-same-level t]
      ["Same level next" ct-pg-go-next-same-level t]
      ["Pop to mark" pop-to-mark-command t]
      "-"
      ["Fold all" ct-pg-fold-all-subtree t]
      ["Unfold all" ct-unfold-all t]
      ["Fold all level in substree" ct-pg-fold-subtree-under-point t]
      ["Unfold all in substree" ct-pg-unfold-subtree t]
      "-"
      ["Extend Proc Name" mycalltrak-procexp t]
      "-"
      ["Lookup Proc in PLS/MSym" mycalltrak-lookup-in-pls t]
      ["Lookup Proc in CodePilot" mycalltrak-lookup-in-codepilot t]
      "-"
      ["Mark line" mycalltrak-mark-line t]
      ["Mark region" mycalltrak-mark-region t]
      ["Unmark" mycalltrak-unmark-line t]
      "-"
      ["Hide region" mycalltrak-hide-region t]
      ["Show region" mycalltrak-show-region t]
      "-"
      ["Hide lines contain word" mycalltrak-hide-line-contain-word t]
      ["Hide lines of NOT Care modeules" mycalltrak-hide-lines-match-predefined-modules t]
      "-"
      ["Del lines contain word" mycalltrak-del-line-contain-word t]
      ["Del lines of NOT Care modeules" mycalltrak-del-lines-match-predefined-modules t]
      "-"
      ["Show hided lines" mycalltrak-show-all]
      ))

(easy-menu-define mycalltrak--menu-symbol
                  mycalltrak-mode-map
                  "MyCalltrak Menu"
                  mycalltrak-menu)

(defun mycalltrak-procexp ()
  (interactive)
  (require 'pls)
  (let ((local-file-name (buffer-file-name))
        unix-dir unix-file-name temp-file-name unix-file-name-nodir)
    
    (when local-file-name
      (setq temp-file-name (make-temp-file (buffer-name)))
      (copy-file local-file-name temp-file-name t)
      (setq unix-dir (concat "/" unix-account "@" unix-host ":~/"))
      (setq unix-file-name-nodir (file-name-nondirectory temp-file-name))
      (setq unix-file-name (concat unix-dir (file-name-nondirectory temp-file-name)))
      (cond ((file-exists-p unix-file-name)
             (when (y-or-n-p (concat "File"  "already existed! Do you want to override it?"))
               (copy-file temp-file-name unix-file-name t)))
            (t
             (copy-file temp-file-name unix-file-name)))
      (pls-send-wait (concat "x procexp " unix-file-name-nodir))
      ;; ignore the error.
      ;; check the output
      (save-excursion
        (set-buffer pls-buffer)
        (goto-char comint-last-input-end)
        (when (re-search-forward "ERROR: The compiler version cannot be determined" nil t)
          (error "Please establish the context in PLS first!")))
      (copy-file unix-file-name temp-file-name t)
      (find-file temp-file-name)
      (mycalltrak-mode)
      )))

(defvar ct-mod-sec-proc-regexp "\\([A-Za-z0-9$_]+\\) (\\([A-Za-z0-9$_]+\\)) \\([A-Za-z0-9$_]+\\)")
(defvar ct-mod-proc-regexp "\\([A-Za-z0-9$_]+\\) \\([A-Za-z0-9$_]+\\)")

(defun mycalltrak-get-mod-and-proc ()
  (let (level mod sec proc)
    (save-match-data
      (save-excursion
        (forward-line 0)
        (setq level (ct-pg-current-level-1))
        (when (>= level 0)
          (move-to-column 11)
          (skip-chars-forward " \\.")
          (forward-char 9)
          ;; let us go
          (cond ((looking-at ct-mod-sec-proc-regexp)
                 (setq mod (match-string 1))
                 (setq sec (match-string 2))
                 (setq proc (match-string 3))
                 (values :exp mod proc sec))
                ((looking-at ct-mod-proc-regexp)
                 (setq mod (match-string 1))
                 (setq proc (match-string 2))
                 (values :org mod proc))))))))

(defun mycalltrak-lookup-in-codepilot ()
  (interactive)
  (require 'codepilot)
  (multiple-value-bind (type mod proc)
      (mycalltrak-get-mod-and-proc)
    (when (eq :exp type)
      (when (string-match "\\$_\\$$" proc)
        (setq proc (replace-match "" t t proc))))
    
    (codepilot-open-calltrack (concat mod " " proc))
    ))

(defun mycalltrak-lookup-in-pls ()
  (interactive)
  (require 'codepilot)
  (multiple-value-bind (type mod proc sec)
      (mycalltrak-get-mod-and-proc)
    (cond ((eq :org type)
           (setq proc (concat proc "*"))
           (msym-goto-imp-1 proc mod))
          (t
           (msym-open-pls-section-from-context sec proc 'im)
           ))))

(defun mycalltrak-lookup-in-pls-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (mycalltrak-lookup-in-pls))


(defun mycalltrak-make-overlay-of-line ()
  (let (beg end)
    (forward-line 0)
    (setq beg (point))
    (end-of-line)
    (setq end (point))
    (make-overlay beg end)
    ))

(defun mycalltrak-del-overlay-at-point (tag)
  (dolist (ov (overlays-at (point)))
    (when (eq (overlay-get ov 'tag) tag)
      (delete-overlay ov)
      )))

(defun mycalltrak-mark-line ()
  ""
  (interactive)
  (save-excursion
    (let ((ov (mycalltrak-make-overlay-of-line)))
      (overlay-put ov 'face 'codepilot-hl-text-face)
      (overlay-put ov 'tag 'mycalltrak-mark)
      )))

(defun mycalltrak-unmark-line ()
  ""
  (interactive)
  (mycalltrak-del-overlay-at-point 'mycalltrak-mark))

(defun mycalltrak-hide-line-match-regexp (regexp)
  (save-match-data
    (save-excursion
      (let (beg end o)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (forward-line 0)
          (backward-char)
          (setq beg (point))
          (forward-line)
          (end-of-line)
          (setq end (point))
          ;; (outline-flag-region beg end t)
          (setq o (make-overlay beg end))
          (overlay-put o 'invisible 'mycalltrak-hide)
          )))))

(defun mycalltrak-del-line-match-regexp (regexp)
  (save-match-data
    (save-excursion
      (let (o)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (delete-region (progn (forward-line 0) (point))
                         (progn (forward-line) (point)))
          )))))

(defun mycalltrak-hide-line-contain-word (wd)
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Word" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))
  (mycalltrak-hide-line-match-regexp (concat "\\_<" wd "\\_>")))

(defun mycalltrak-del-line-contain-word (wd)
  (interactive
   (list
    (let ((cur (current-word)))
      (read-string
       (concat "Word" (if cur (concat " (default " cur ")") "") ": ")
       nil nil cur))))
  (mycalltrak-del-line-match-regexp (concat "\\_<" wd "\\_>")))

(defvar mycalltrak-module-list-not-care nil)
(setq mycalltrak-module-list-not-care (list "CPRTTRUI" "CPTRATAB" "SYSPROCS"))
(defvar mycalltrak-module-list-not-care-regexp nil)
(setq mycalltrak-module-list-not-care-regexp
      (concat "\\_<" (regexp-opt mycalltrak-module-list-not-care t) "\\_>"))

(defun mycalltrak-hide-lines-match-predefined-modules ()
  (interactive)
  (mycalltrak-hide-line-match-regexp mycalltrak-module-list-not-care-regexp))

(defun mycalltrak-del-lines-match-predefined-modules ()
  (interactive)
  (mycalltrak-del-line-match-regexp mycalltrak-module-list-not-care-regexp))


(defun mycalltrak-show-all ()
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get o 'invisible) 'mycalltrak-hide)
      (delete-overlay o))))


(defun mycalltrak-mark-region ()
  (interactive)
  (when (mark-active-p)
    (let (o from to)
      (setq from (mark))
      (setq to (point))
      (if (> from to)
	  (setq from (prog1 to (setq to from))))
      (setq o (make-overlay from to))
      (overlay-put o 'face 'codepilot-hl-text-face)
      (overlay-put o 'tag 'mycalltrak-mark)
      )))

(defun mycalltrak-hide-region ()
  (interactive)
  (when (mark-active-p)
    (let (o from to)
      (setq from (mark))
      (setq to (point))
      (if (> from to)
	  (setq from (prog1 to (setq to from))))
      (setq o (make-overlay from to))
      (overlay-put o 'invisible 'mycalltrak-hide)
      )))

(defun mycalltrak-show-region ()
  (interactive)
  (when (mark-active-p)
    (let (from to)
      (setq from (mark))
      (setq to (point))
      (if (> from to)
	  (setq from (prog1 to (setq to from))))
      (dolist (o (overlays-in from to))
        (when (eq (overlay-get o 'invisible) 'mycalltrak-hide)
          (delete-overlay o)))
      )))

(defun mycalltrak-click-on-pg-line (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((l (ct-pg-current-level-1))
        (pos (point)))
    (cond ((>= 0 l)
           (save-excursion
             (forward-line 0)
             (re-search-forward "[0-9A-Za-z]\\{8\\}" (save-excursion (end-of-line) (point)) t)
             (cond ((< (point) pos)
                    (mycalltrak-lookup-in-pls))
                   (t
                    (ct-pg-fold/unfold-subtree)))))
          ((= l -1)
           (ct-pg-fold/unfold-subtree)
           ))))

(add-to-list 'auto-mode-alist '("calltrak" . mycalltrak-mode))
(add-to-list 'auto-mode-alist '("calltrack" . mycalltrak-mode))

;; (define-key mycalltrak-mode-map [mouse-2] 'ct-pg-fold/unfold-subtree-mouse)
(define-key mycalltrak-mode-map [(f10)] 'ct-pg-fold/unfold-subtree)
(define-key mycalltrak-mode-map [(f6)] 'ct-pg-go-up-level)
(define-key mycalltrak-mode-map [(shift f7)] 'ct-pg-go-previous-same-level)
(define-key mycalltrak-mode-map [(f7)] 'ct-pg-go-next-same-level)
(define-key mycalltrak-mode-map [(f12)] 'ct-pg-fold-all-subtree)
(define-key mycalltrak-mode-map [(shift f12)] 'ct-unfold-all)

(define-key mycalltrak-mode-map [(control ?t)] 'mycalltrak-lookup-in-pls)
(define-key mycalltrak-mode-map [(meta ?t)] 'mycalltrak-lookup-in-codepilot)
;; (define-key mycalltrak-mode-map [mouse-3] 'mycalltrak-lookup-in-pls-mouse)
(define-key mycalltrak-mode-map [mouse-3] 'mycalltrak-click-on-pg-line)
(define-key mycalltrak-mode-map [(f9)] 'mycalltrak-lookup-in-pls)



(provide 'mycalltrak)