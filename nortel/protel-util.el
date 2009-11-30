;;; protel-util.el --- Utilities to navigate PROTEL code block.

;; Authors: Brian Jiang
;; Maintainer: Brian Jiang <brianjiang AT gdnt DOT com DOT cn>
;; Keywords: language protel

;; $Revision: 0.1+ $  2004.01
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

;; Purpose:
;; Write to navigate PROTEL code block
;; because Outline functions don't work well.
;; I can not endure it any more >:|
;;
;; History:
;; Will add folding functions if I have time :)
;;
;; 2004.01.16
;; Rewrite regular expresses. Use \\< and \\> instead of \\W
;; Some folding functions are introduced!
;;
;; 2004.01.17
;; Use Overlay for folding.
;; Smart key to move over overlay
;;
;; Until now, all basic functions are provided.
;; Will do: Make it more comfortable
;; Smart searching and folding: unified search/fold
;;
;; smart search done!!!
;;
;; will do: smart folding,
;; folding comment? No need. Can be done by hide-line.el
;;
;; smart folding done!!!
;;
;;
;; 2006-12-29
;; Add new functions:
;; (defun narrow-proc ()
;; (defun fold-all-procs ()
;;
;; 2007-1-2
;; Add new function:
;; narrow-block
;;
;; 2007-7-17
;; Updated.
;;
;; Supported block keywords:
;;   IF ELSEIF ELSE ENDIF
;;   DO ENDDO
;;   BLOCK ENBLOCK
;;   CASE/SELECT {...}: ENDCASE/ENDSELECT
;;
;; ==================================

;; Installation:
;; byte-compile-file and add following into .emacs file
;; (require 'protel-util)
;; Note: Please turn off outline minor mode for folding functions.
;;
;; Customization:
;; (setq my-hide-region-before-string "++@@<-----------^_^---------------------")
;; (setq my-hide-region-after-string  "---------------------@:P----------->@@++")
;;
;; Documentation and where to get protel.el
;; N/A :(
;;; Code:


;;(require 'protel)

;;(require 'hide-region)

(provide 'protel-util)

;;{{{ Navigate funcation
(defun skip-ifblock-backward()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward "\\_<\\(IF\\|ENDIF\\)\\_>" nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment))
		   ())
		  ((string-equal (upcase (match-string 0)) "ENDIF")
		   (if (not (skip-ifblock-backward)) (setq my-loop nil)))
		  (t
		   ;;(string-equal (upcase (match-string 0)) "IF")
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))

(defun skip-block-backward(expected skip)
  ""
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	(string-to-search (concat "\\_<\\(" expected "\\|" skip "\\)\\_>"))
	)
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward string-to-search nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment))
		   ())
		  ((string-equal (upcase (match-string 0)) skip)
		   (if (not (skip-block-backward expected skip)) (setq my-loop nil)))
		  (t
		   ;;(string-equal (upcase (match-string 0)) expected)
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))


(defun skip-ifblock-forward()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward "\\_<\\(IF\\|ENDIF\\)\\_>" nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "IF")
		   (if (not (skip-ifblock-forward)) (setq my-loop nil)))
		  (t
		   ;;(string-equal (upcase (match-string 0)) "ENDIF")
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))

(defun skip-block-forward(expected skip)
  ""
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	(string-to-search (concat "\\_<\\(" expected "\\|" skip "\\)\\_>"))
	)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward string-to-search nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) skip)
		   (if (not (skip-block-forward expected skip)) (setq my-loop nil)))
		  (t
		   ;;(string-equal (upcase (match-string 0)) expected)
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))

;;=================================
(defun search-ifbranch-backward-i()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward "\\_<\\(IF\\|ENDIF\\|ELSEIF\\|ELSE\\)\\_>" nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "ENDIF")
		   (if (not (skip-ifblock-backward)) (setq my-loop nil)))
		  (t
		   ;;(or (string-equal (upcase (match-string 0)) "IF")
		   ;;(string-equal (upcase (match-string 0)) "ELSEIF")
		   ;;(string-equal (upcase (match-string 0)) "ELSE"))
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))


(defun search-ifbranch-forward-i()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward "\\_<\\(IF\\|ENDIF\\|ELSEIF\\|ELSE\\)\\_>" nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "IF")
		   (if (not (skip-ifblock-forward)) (setq my-loop nil)))
		  (t

		   ;;(or (string-equal (upcase (match-string 0)) "ENDIF")
		   ;;(string-equal (upcase (match-string 0)) "ELSEIF")
		   ;;(string-equal (upcase (match-string 0)) "ELSE"))
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))
;;==================================
(defun search-ifbranch-backward()
  ""
  (interactive)
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(backward-char 1)
	(if (not (setq success (search-ifbranch-backward-i)))
	    (message "search failed")
	  (setq goto-point (point))
	  ;;(setq success t)
	  )
	  ))
    (goto-char goto-point)
    success))


(defun search-ifbranch-forward()
  ""
  (interactive)
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(forward-char 1)
	(if (not (setq success (search-ifbranch-forward-i)))
	    (message "search failed")
	  (backward-char 1)
	  (setq goto-point (point))
	  ;;(setq success t)
	  )
	  ))
    (goto-char goto-point)
    success))



;;=====================================

(defun my-block-command-wrap-b(expected skip)
  ""
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(backward-char 1)
	(if (not (setq success (skip-block-backward expected skip)))
	    (message "search failed")
	  (setq goto-point (point))
	  ;(setq success t)
	  )
	))
    (goto-char goto-point)
    success))

(defun my-block-command-wrap-f(expected skip)
  ""
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(forward-char 1)
	(if (not (setq success (skip-block-forward expected skip)))
	    (message "search failed")
	  (backward-char 1)
	  (setq goto-point (point))
	  ;(setq success t)
	  )
	))
    (goto-char goto-point)
    success))

;;======================================

(defun search-do-for-enddo-b()
  ""
  (interactive)
  (my-block-command-wrap-b "DO" "ENDDO")
  )

(defun search-enddo-for-do-f()
  ""
  (interactive)
  (my-block-command-wrap-f "ENDDO" "DO")
  )


;==========================
(defun search-block-for-endblock-b()
  ""
  (interactive)
  (my-block-command-wrap-b "BLOCK" "ENDBLOCK")
  )


(defun search-endblock-for-block-f()
  ""
  (interactive)
  (my-block-command-wrap-f "ENDBLOCK" "BLOCK")
  )


;;==========================
;; case select
;; The regular exp can be comptimized if needed
;;
(defvar case-select-string (concat
                            "\\_<"
                            ;; \\(CASE\\|ENDCASE\\|SELECT\\|ENDSELECT\\|OUT\\)

                            (regexp-opt
                             (list "CASE" "ENDCASE" "SELECT" "ENDSELECT" "OUT"
                                   "INSPECT" "ENDINSPECT")
                             :paren)
                            "\\_>\\|.}[ \\t]*:"))


(defun search-case-select-branch-backward-i()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward case-select-string nil t))
	      (setq my-loop nil)
	    ;; (forward-char)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "ENDCASE")
		   (if (not (skip-block-backward "CASE" "ENDCASE")) (setq my-loop nil)))
		  ((string-equal (upcase (match-string 0)) "ENDSELECT")
		   (if (not (skip-block-backward "SELECT" "ENDSELECT")) (setq my-loop nil)))
                  ((string-equal (upcase (match-string 0)) "ENDINSPECT")
		   (if (not (skip-block-backward "INSPECT" "ENDINSPECT")) (setq my-loop nil)))
		  (t
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))


(defun search-case-select-branch-forward-i()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward case-select-string nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "CASE")
		   (if (not (skip-block-forward "ENDCASE" "CASE")) (setq my-loop nil)))
		  ((string-equal (upcase (match-string 0)) "SELECT")
		   (if (not (skip-block-forward "ENDSELECT" "SELECT")) (setq my-loop nil)))
                  ((string-equal (upcase (match-string 0)) "INSPECT")
		   (if (not (skip-block-forward "ENDINSPECT" "INSPECT")) (setq my-loop nil)))
		  (t

		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))

;;==================================
(defun search-case-select-branch-backward()
  ""
  (interactive)
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(backward-char 1)
	(if (not (setq success (search-case-select-branch-backward-i)))
	    (message "search failed")
	  (setq goto-point (point))
	  ;;(setq success t)
	  )
	  ))
    (goto-char goto-point)
    success))


(defun search-case-select-branch-forward()
  ""
  (interactive)
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(forward-char 1)
	(if (not (setq success (search-case-select-branch-forward-i)))
	    (message "search failed")
	  (backward-char 1)
	  (setq goto-point (point))
	  ;;(setq success t)
	  )
	  ))
    (goto-char goto-point)
    success))


;;====================================
;;}}}

;;{{{ for folding function
(defcustom my-hide-region-before-string "........"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type '(string)
  :group 'protel)

(defcustom my-hide-region-after-string "........"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay"
  :type '(string)
  :group 'protel)


;;{{{ ifbranch folding
(defun search-thenbranch-backward-i()
  ""
  (interactive)
  (let ((case-fold-search t)
	(my-loop t)
	(success nil)
	)
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward "\\_<\\(ENDIF\\|THEN\\|ELSE\\)\\_>" nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  ((string-equal (upcase (match-string 0)) "ENDIF")
		   (if (not (skip-ifblock-backward)) (setq my-loop nil)))
		  (t
		   ;;(or (string-equal (upcase (match-string 0)) "IF")
		   ;;(string-equal (upcase (match-string 0)) "ELSEIF")
		   ;;(string-equal (upcase (match-string 0)) "ELSE"))
		   (setq my-loop nil)
		   (setq success (match-string 0)))
		  )))
	success))


(defun search-thenbranch-backward()
  ""
  (interactive)
  (let ((goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(backward-char 1)
	(if (not (setq success (search-thenbranch-backward-i)))
	    (message "search failed")
	  (setq goto-point (point))
	  ;;(setq success t)
	  )
	  ))
    (goto-char goto-point)
    success))

;;}}}

;;{{{ common folding function

(defface codepilot-folding-overlay
  '((default :inherit region :box (:line-width 3 :color "DarkSeaGreen1" :style released-button))
    (((class color)) :foreground "black" :background "DarkSeaGreen2"))
  "*Font used by folding overlay."
  :group 'protel)

(defun protel-ov-delete ()
  ""
  (interactive)
  (dolist (o (overlays-at (point)))
    (my-delete-overlay o 'Protel-util))
  t
  )

(defvar protel--overlay-keymap nil "keymap for folding overlay")

(let ((map (make-sparse-keymap)))
  (define-key map [mouse-1] 'protel-ov-delete)
  (setq protel--overlay-keymap map)
  )

(defun my-hide-region-hide (from to)
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((new-overlay (make-overlay from to)))
    ;;(overlay-put new-overlay 'invisible nil)
    (overlay-put new-overlay 'protel-tag 'Protel-util)
    ;;(overlay-put new-overlay 'before-string my-hide-region-before-string)
;;     (overlay-put new-overlay 'before-string
;;                    (format "%s<%d lines>"
;; 			   my-hide-region-before-string
;;                            (1- (count-lines (overlay-start new-overlay)
;;                                         (overlay-end new-overlay)))))
;;     (overlay-put new-overlay 'after-string my-hide-region-after-string)
    (overlay-put new-overlay 'face 'codepilot-folding-overlay)
    (overlay-put new-overlay 'display
		 (propertize
		  (format "%s<%d lines>%s"
			  my-hide-region-before-string
			  (1- (count-lines (overlay-start new-overlay)
					   (overlay-end new-overlay)))
			  my-hide-region-after-string
			  )))
    (overlay-put new-overlay 'priority to)
    (overlay-put new-overlay 'keymap protel--overlay-keymap)
    (overlay-put new-overlay 'pointer 'hand)
    )
  (run-hooks 'protel-util-hide-region-hook)
  )

(defun my-delete-overlay(o prop)
  ""
  (if (eq (overlay-get o 'protel-tag) prop)
      (progn
	(delete-overlay o)
	t)
    nil
    ))

(defvar inhibit-push-mark nil)

;; block-type:
;; 0 case/select folding
;; 1 do folding
;; 2 block folding
;; 3 if branch folding
;; 4 smart
;; 5 if/case/select block

(defun common-fold-block(block-type fold)
  ""
  ;;(interactive)
  (let ((success nil)
	(str-match nil)
	search-f
	search-b
	from to
	(my-fold nil)
        (inhibit-push-mark t))

    (cond ((= block-type 0)
	   (setq search-b 'search-case-select-branch-backward)
	   (setq search-f 'search-case-select-branch-forward)
	   )
	  ((= block-type 1)
	   (setq search-b 'search-do-for-enddo-b)
	   (setq search-f 'search-enddo-for-do-f)
	   )
	  ((= block-type 2)
	   (setq search-b 'search-block-for-endblock-b)
	   (setq search-f 'search-endblock-for-block-f)
	   )
	  ((= block-type 3)
	   (setq search-b 'search-thenbranch-backward)
	   (setq search-f 'search-ifbranch-forward)
	   )
	  ((= block-type 4)
	   (setq search-b 'which-block)
	   (setq search-f 'which-block-forward)
	  )
	  ((= block-type 5)
	   (setq search-b 'search-ifcaseselect-backward)
	   (setq search-f 'search-endifcaseselect-forward)
	   )
	  )
    (save-match-data
      (save-excursion
;;	(backward-char 1)
	(if (setq str-match (funcall search-f))
	    (progn
	      (if (string-match ":" str-match)
		  (save-match-data
		    (save-excursion
		       ;;(search-backward "{" nil t)
		       (search-uncommented-b "{")
		       (forward-line 0)
		       (backward-char 1)
		       (setq to (point))

		      ))
		(save-excursion
		  (forward-line 0)
		  (backward-char 1)
		  (setq to (point)))
;; 		(forward-line 1)
;; 		(forward-word 1)
;; 		(backward-char 1)
		)
	      (if (funcall search-b)
		  (progn
		    (end-of-line)
		    (setq from (point))

		    (cond ((eq fold 'fold)
			   (setq my-fold t))
			  ((eq fold 'unfold)
			   (setq my-fold nil))
			  ((eq fold 'more-smarter)
			   (if (overlays-at from)
			       (setq my-fold nil)
			     (setq my-fold t))
			   ))
		    (if my-fold
			(if (> to from)
			    (progn
			      (my-hide-region-hide from to)
			      (setq success t))
			  )
		      (dolist (o (overlays-at from))
			(my-delete-overlay o 'Protel-util))
		      (setq success t))

		    ))
	      ))))
    success))

;;}}}

;;{{{ if branch folding
(defun fold-ifbranch()
  ""
  (interactive)
  (common-fold-block 3 'fold)
  )

(defun unfold-ifbranch()
  ""
  (interactive)
  (common-fold-block 3 'unfold)
  )

;;}}}

;;{{{ case/select folding
(defun fold-case-branch()
  ""
  (interactive)
  (common-fold-block 0 'fold) ;;0: block-type: case/select; t:fold
)

(defun unfold-case-branch()
  ""
  (interactive)
  (common-fold-block 0 'unfold) ;; unflod
  )

;;}}}

;;{{{ DO/BLOCK folding
(defun fold-do()
  ""
  (interactive)
  (common-fold-block 1 'fold)
  )

(defun unfold-do()
  ""
  (interactive)
  (common-fold-block 1 'unfold)
  )

(defun fold-block()
  ""
  (interactive)
  (common-fold-block 2 'fold)
  )

(defun unfold-block()
  ""
  (interactive)
  (common-fold-block 2 'unfold)
  )


(defun unfold-all()
  ""
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (dolist (o (overlays-in (point-min) (point-max)))
        (my-delete-overlay o 'Protel-util)
      ))))

(defun unfold-region()
  ""
  (interactive)
  (save-excursion
    (let (from to)
      (setq from (mark))
      (setq to (point))
      (if (> from to)
;; 	  (progn
;; 	    (setq from to)
;; 	    (setq to (mark)))
	  ;; exchange from to
	  (setq from (prog1 to (setq to from)))
	)
      (dolist (o (overlays-in from to))
	(my-delete-overlay o 'Protel-util)
	))))


(defun fold-region()
  ""
  (interactive)
  (save-excursion
    (let (from to)
      (setq from (mark))
      (setq to (point))
      (if (> from to)
;; 	  (progn
;; 	    (setq from to)
;; 	    (setq to (mark)))
	  ;; exchange from to
	  (setq from (prog1 to (setq to from)))
	)
      (goto-char to)
      (skip-chars-backward " \n\t\f")
      (setq to (point))
      (goto-char from)
      (skip-chars-forward " \n\t\f")
      (setq from (point))
      (my-hide-region-hide from to)
      )))

;;}}}

;;{{{ Move over overlay cleverly

;; (defun my-hide-region-move-forward-over-overlay ()
;;   "Move \"forward\" until we get out of the overlay"
;;   (interactive)
;;   (let (
;; 	(point-t (point))
;; 	(success nil)
;; 	)
;;     (save-excursion
;;       (dolist (o (overlays-at (point)))
;; 	(if (eq (overlay-get o 'protel-tag) 'Protel-util)
;; 	    (if (> (overlay-end o) point-t)
;; 		(progn
;; 		  (setq success t)
;; 		  (setq point-t (overlay-end o)))))
;; 	))
;;     (if success
;; 	(progn
;; 	  (goto-char point-t)
;; 	  (forward-char 1))
;;       )))

;; (defun my-hide-region-move-backward-over-overlay ()
;;   "Move \"backward\" until we get out of the overlay"
;;   (interactive)
;;   (let (
;; 	(point-t (point))
;; 	(success nil)
;; 	)
;;     (save-excursion
;;       (dolist (o (overlays-at (point)))
;; 	(if (eq (overlay-get o 'protel-tag) 'Protel-util)
;; 	    (if (< (overlay-start o) point-t)
;; 		(progn
;; 		  (setq success t)
;; 		  (setq point-t (overlay-start o)))))
;; 	))
;;     (if success
;; 	(progn
;; 	  (goto-char point-t)
;; 	  (backward-char 1))
;;       )))


;; (defun my-hide-region-next-line (arg)
;;   "Replacement for `next-line', trying to skip over hidden regions"
;;   (interactive "p")
;;   (next-line arg)
;;   (my-hide-region-move-forward-over-overlay)
;;   )

;; (defun my-hide-region-forward-char ()
;;   "Replacement for `forward-char', trying to skip over hidden regions"
;;   (interactive)
;;   (forward-char 1)
;;   (my-hide-region-move-forward-over-overlay)
;;   )

;; (defun my-hide-region-previous-line (arg)
;;   "Replacement for `previous-line', trying to skip over hidden regions"
;;   (interactive "p")
;;   (previous-line arg)
;;   (my-hide-region-move-backward-over-overlay)
;;   )


;; (defun my-hide-region-backward-char ()
;;   "Replacement for `backward-char', trying to skip over hidden regions"
;;   (interactive)
;;   (backward-char 1)
;;   (my-hide-region-move-backward-over-overlay)
;;   )


;; (defun my-hide-region-setup-keybindings ()
;;   "Setup local example keybindings for `my-hide-region'"
;;   (interactive)
;;   ;;(global-set-key [down] 'my-hide-region-next-line)
;;   (global-set-key [down] 'next-line)
;;   (global-set-key [up] 'previous-line)
;;   (global-set-key [right] 'my-hide-region-forward-char)
;;   (global-set-key [left] 'my-hide-region-backward-char)
;;   )

;; (defun my-hide-region-remove-keybindings ()
;;   "Remove local example keybindings for `hide-region'"
;;   (interactive)
;;   (global-set-key [down] 'next-line)
;;   (global-set-key [up] 'previous-line)
;;   (global-set-key [right] 'forward-char)
;;   (global-set-key [left] 'backward-char)
;;   )

;; (my-hide-region-setup-keybindings)



;;}}}


;;{{{ smart function
(defun get-current-word-and-goto-wordend()
  ""
  (interactive)
  (backward-char 1)
  (forward-word 1)
  (let (limit
	word
	(case-fold-search t)
	)
    (setq word (current-word t))
    (if word
	(progn
	  (end-of-line)
	  (setq limit (point))
	  (forward-line 0)
	  (word-search-forward word limit t)
	  (message word)
	  (backward-char 1)
	  word
	  ))))

(defun smart-search-common(backward)
  ""
  ;;(interactive)
  (or
   (case-mid-branch-search backward)
   (let ((old-point (point))
	 (case-fold-search t)
	 (success nil)
	 block-keyword)
     (setq block-keyword (get-current-word-and-goto-wordend))
     (if block-keyword
	 (progn
	   (setq block-keyword (upcase block-keyword))
	   (cond ((or (string-equal block-keyword "ELSEIF")
		      (string-equal block-keyword "ELSE"))
		  (if backward
		      (progn
			(backward-word 1)
			(setq success (search-ifbranch-backward))
			)
		    (setq success (search-ifbranch-forward)))
		  )

		 ((string-equal block-keyword "IF")
		  (setq success (search-ifbranch-forward)))

		 ((string-equal block-keyword "ENDIF")
		  (backward-word 1)
		  (setq success (search-ifbranch-backward)))

		 ((string-equal block-keyword "BLOCK")
		  (setq success (search-endblock-for-block-f)))

		 ((string-equal block-keyword "ENDBLOCK")
		  (backward-word 1)
		  (setq success (search-block-for-endblock-b)))

		 ((or (string-equal block-keyword "CASE")
		      (string-equal block-keyword "SELECT")
                      (string-equal block-keyword "INSPECT")
                      )
		  (setq success (search-case-select-branch-forward)))

		 ((or (string-equal block-keyword "ENDCASE")
		      (string-equal block-keyword "ENDSELECT")
                      (string-equal block-keyword "ENDINSPECT")
                      )
		  (backward-word 1)
		  (setq success (search-case-select-branch-backward)))

		 ((string-equal block-keyword "OUT")
		  (if backward
		      (progn
			(backward-word 1)
			(setq success (search-case-select-branch-backward)))
		    (setq success (search-case-select-branch-forward))))

		 ((string-equal block-keyword "DO")
		  (setq success (search-enddo-for-do-f)))

		 ((string-equal block-keyword "ENDDO")
		  (setq success (search-do-for-enddo-b)))

		 (t
		  (case-mid-branch-search backward) ;; not needed
		  ;;may {....}:
		  )))
       ;;if false
       ;;search .}:
       (case-mid-branch-search backward) ;; not needed.
       );;if keyword
     success)))

(defun case-mid-branch-search(backward)
  ""
  (let (endpoint
	(case-fold-search t)
	(goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(end-of-line)
	(setq endpoint (point))
	(forward-line 0)
	(if (re-search-forward ".}[ \\t]*:" endpoint t)
	    (if backward
		(progn
		  (backward-word 1)
		  (setq success (search-case-select-branch-backward))
		  (setq goto-point (point))
		  )
	      (setq success (search-case-select-branch-forward))
	      (setq goto-point (point))))
      ))
    (goto-char goto-point)
    success
    ))

(defun smart-search()
  ""
  (interactive)
  (smart-search-common nil)
  )


(defun smart-search-backward()
  ""
  (interactive)
  (smart-search-common t)
  )


;;===============================
;; can used regexp-opt to optimize it later.
(defvar whole-block-re nil)

(setq whole-block-re (concat "\\_<"
                             (regexp-opt (list "ELSE" "THEN" "ENDIF" "BLOCK" "ENDBLOCK"
                                               "DO" "ENDDO" "CASE"
                                               "ENDCASE" "SELECT" "ENDSELECT"
                                               "INSPECT" "ENDINSPECT"
                                               "STRUCT" "ENDSTRUCT" "OVLY" "ENDOVLY" "OUT"
                                               "AREA" "ENDAREA" "CLASS REFINES" "ENDCLASS"
                                               ) t)
                             "\\_>"
                             "\\|.}[ \\t]*:"
                             ))


(defvar whole-block-re-f nil)

(setq whole-block-re-f (concat "\\_<"
                               (regexp-opt (list "IF" "ELSE" "ELSEIF" "ENDIF" "BLOCK" "ENDBLOCK"
                                                 "DO" "ENDDO" "CASE"
                                                 "ENDCASE" "SELECT" "ENDSELECT"
                                                 "INSPECT" "ENDINSPECT"
                                                 "STRUCT" "ENDSTRUCT" "OVLY" "ENDOVLY" "OUT"
                                                 "AREA" "ENDAREA" "CLASS REFINES" "ENDCLASS"
                                                 ) t)
                               "\\_>"
                               "\\|.}[ \\t]*:"
                               ))


(defun which-block()
  ""
  (interactive)
  (let ( match-s
	 (case-fold-search t)
	(my-loop t)
	(goto-point (point))
	(success nil))
    (save-match-data
      (save-excursion
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward whole-block-re nil t))
	      (progn
		(setq my-loop nil))
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  (t
		   (setq match-s (upcase (match-string 0)))
		   (cond ((string-equal match-s "ENDIF")
			  (if (not (skip-ifblock-backward))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDBLOCK")
			  (if (not (skip-block-backward "BLOCK" "ENDBLOCK"))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDDO")
			  (if (not (skip-block-backward "DO" "ENDDO"))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDCASE")
			  (if (not (skip-block-backward "CASE" "ENDCASE"))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDSELECT")
			  (if (not (skip-block-backward "SELECT" "ENDSELECT"))
			      (setq my-loop nil)))

                         ((string-equal match-s "ENDINSPECT")
			  (if (not (skip-block-backward "INSPECT" "ENDINSPECT"))
			      (setq my-loop nil)))
                         
			 ((string-equal match-s "ENDSTRUCT")
			  (if (not (skip-block-backward "STRUCT" "ENDSTRUCT"))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDOVLY")
			  (if (not (skip-block-backward "OVLY" "ENDOVLY"))
			      (setq my-loop nil)))

                         ((string-equal match-s "ENDAREA")
			  (if (not (skip-block-backward "AREA" "ENDAREA"))
			      (setq my-loop nil)))
                         
                         ((string-equal match-s "ENDCLASS")
			  (if (not (skip-block-backward "CLASS REFINES" "ENDCLASS"))
			      (setq my-loop nil)))

			 (t
			  ;; not endblock
			  (setq my-loop nil)
			  (setq goto-point (point))
			  (setq success (match-string 0)))

			 )))))))
    (if success
        (unless inhibit-push-mark
          (push-mark (point) t)))
    (goto-char goto-point)
    success))

(defun which-block-forward()
  ""
  (interactive)
  (let ( match-s
	 (case-fold-search t)
	 (my-loop t)
	 (goto-point (point))
	 (success nil))
    (save-match-data
      (save-excursion
	(forward-char 1)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward whole-block-re-f nil t))
	      (progn
		(setq my-loop nil))
	    (cond  ((or (my-in-comment) (my-in-quote/comment) )
		    ())
		   (t
		    (setq match-s (upcase (match-string 0)))
		    (cond ((string-equal match-s "IF")
			   (if (not (skip-ifblock-forward))
			       (setq my-loop nil)))

			  ((string-equal match-s "BLOCK")
			   (if (not (skip-block-forward "ENDBLOCK" "BLOCK"))
			       (setq my-loop nil)))

			  ((string-equal match-s "DO")
			   (if (not (skip-block-forward "ENDDO" "DO"))
			       (setq my-loop nil)))

			  ((string-equal match-s "CASE")
			   (if (not (skip-block-forward "ENDCASE" "CASE"))
			       (setq my-loop nil)))

			  ((string-equal match-s "SELECT")
			   (if (not (skip-block-forward "ENDSELECT" "SELECT"))
			       (setq my-loop nil)))

                          ((string-equal match-s "INSPECT")
			   (if (not (skip-block-forward "ENDINSPECT" "INSPECT"))
			       (setq my-loop nil)))

			  ((string-equal match-s "STRUCT")
			   (if (not (skip-block-forward "ENDSTRUCT" "STRUCT"))
			       (setq my-loop nil)))
                          
			  ((string-equal match-s "OVLY")
			   (if (not (skip-block-forward "ENDOVLY" "OVLY"))
			       (setq my-loop nil)))

                          ((string-equal match-s "AREA")
			   (if (not (skip-block-forward "ENDAREA" "AREA"))
			       (setq my-loop nil)))

                          ((string-equal match-s "CLASS REFINES")
			   (if (not (skip-block-forward "ENDCLASS" "CLASS REFINES"))
			       (setq my-loop nil)))

			  (t
			   ;; not start-block
			   (setq my-loop nil)
			   (setq goto-point (point))
			   (setq success (match-string 0)))

			  )))))))
    (when success
      (unless inhibit-push-mark
        (push-mark (point) t))
      (goto-char (1- goto-point)))
    
    success))


(defun which-proc()
  ""
  (interactive)
  (let ((my-loop t)
	(goto-point (point))
	(success nil)
	)
    (save-match-data
      (save-excursion
	(while my-loop
	  (if (skip-block-backward "BLOCK" "ENDBLOCK")
	      (progn
		(setq success t)
		(setq goto-point (point)))
	    (setq my-loop nil))
	    )))
    (if success
        (unless inhibit-push-mark
          (push-mark (point) t)))
    (goto-char goto-point)
    success))

(defun smart-fold()
  ""
  (interactive)
  (common-fold-block 4 'fold)
  ;;(recenter)
  )

(defun smart-unfold()
  ""
  (interactive)
  (let (
	;;(goto-point (point))
	(success nil)
	)
    (save-match-data
      (save-excursion

	(end-of-line)
	(if (setq success (common-fold-block 4 'unfold))
	    (progn
	      ;;(setq success t)
	      ;;(setq goto-point (point))
	      ))

	;;(goto-char goto-point)
	))
    success))

(defun more-smarter-fold-unfold ()
  ""
  (interactive)
  (save-excursion
    (end-of-line)
    (common-fold-block 4 'more-smarter))

  )

(defun fold-proc()
  ""
  (interactive)
  (save-match-data
    (save-excursion
      (if (which-proc)
	  (fold-block)
      )))
  (recenter)
  )

(defun unfold-proc()
  ""
  (interactive)
  (save-match-data
    (save-excursion
      (if (which-proc)
	  (unfold-block)
      )))
;;  (smart-unfold)
  )


;; fold whole if/case/select block (not just a branch)
(defvar ifcaseselect-re nil)
(setq ifcaseselect-re (concat "\\_<"
                              (regexp-opt (list
                                           "IF" "ENDIF" "CASE" "ENDCASE" "SELECT" "ENDSELECT"
                                           "INSPECT" "ENDINSPECT")
                                          :paren)
			     "\\_>"
			     ))

(defun search-uncommented-f(to-search)
  ""
  (let (
	 (case-fold-search t)
	 (my-loop t)
	 (goto-point (point))
	 (success nil))
    (save-match-data
      (save-excursion
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward to-search nil t))
	      (setq my-loop nil)
	    (if (or (my-in-comment) (my-in-quote/comment) )
		() ;; search again
	      (setq success (match-string 0))
	      (setq goto-point (1- (point)))
	      (setq my-loop nil)
	    ))
	  )))
    (goto-char goto-point)
    success))


(defun search-uncommented-b(to-search &optional limit)
  ""
  (let (
	 (case-fold-search t)
	 (my-loop t)
	 (goto-point (point))
	 (success nil))
    (save-match-data
      (save-excursion
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward to-search limit t))
	      (setq my-loop nil)
	    (if (or (my-in-comment) (my-in-quote/comment) )
		() ;; search again
	      (setq success (match-string 0))
	      (setq goto-point (point))
	      (setq my-loop nil)
	    ))
	  )))
    (goto-char goto-point)
    success))



(defun search-ifcaseselect-backward()
  ""
  (interactive)
  (let ( match-s
	 (case-fold-search t)
	 (my-loop t)
	 (goto-point (point))
	 (success nil))
    (save-match-data
      (save-excursion
	(while (and (not (bobp)) my-loop)
	  (if (not (re-search-backward ifcaseselect-re nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  (t
		   (setq match-s (upcase (match-string 0)))
		   (cond ((string-equal match-s "ENDIF")
			  (if (not (skip-ifblock-backward))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDCASE")
			  (if (not (skip-block-backward "CASE" "ENDCASE"))
			      (setq my-loop nil)))

			 ((string-equal match-s "ENDSELECT")
			  (if (not (skip-block-backward "SELECT" "ENDSELECT"))
			      (setq my-loop nil)))

                         ((string-equal match-s "ENDINSPECT")
			  (if (not (skip-block-backward "INSPECT" "ENDINSPECT"))
			      (setq my-loop nil)))

			 ((string-equal match-s "IF")
			  (if (not (search-uncommented-f "\\_<THEN\\_>"))
			      (setq my-loop nil)
			    (setq my-loop nil)
			    (setq goto-point (point))
			    (setq success (match-string 0)))
			  )

			 (t
			  ;; not endblock
			  (setq my-loop nil)
			  (setq goto-point (point))
			  (setq success (match-string 0)))

			 )))))))
    (goto-char goto-point)
    success))


(defun search-endifcaseselect-forward()
  ""
  (interactive)
  (let ( match-s
	 (case-fold-search t)
	 (my-loop t)
	 (goto-point (point))
	 (success nil))
    (save-match-data
      (save-excursion
	(forward-char 1)
	(while (and (not (eobp)) my-loop)
	  (if (not (re-search-forward ifcaseselect-re nil t))
	      (setq my-loop nil)
	    (cond ((or (my-in-comment) (my-in-quote/comment) )
		   ())
		  (t
		   (setq match-s (upcase (match-string 0)))


		   (cond ((string-equal match-s "IF")
			  (if (not (skip-ifblock-forward))
			      (setq my-loop nil)))

			 ((string-equal match-s "CASE")
			  (if (not (skip-block-forward "ENDCASE" "CASE"))
			      (setq my-loop nil)))

			 ((string-equal match-s "SELECT")
			  (if (not (skip-block-forward "ENDSELECT" "SELECT"))
			      (setq my-loop nil)))

                         ((string-equal match-s "INSPECT")
			  (if (not (skip-block-forward "ENDINSPECT" "INSPECT"))
			      (setq my-loop nil)))
                         
			 (t
			  ;; not start-block
			  (setq my-loop nil)
			  (setq goto-point (1- (point)))
			  (setq success (match-string 0)))

			 )))))))
    (goto-char goto-point)
    success))



(defun fold-ifcaseselect-block()
  ""
  (interactive)
  (common-fold-block 5 'fold)
  ;;(recenter)
  )

(setq caseselect-branch-regexp  ".}[ \\t]*:")


(defun fold-ifcaseselect-branches ()
  ""
  (interactive)
  (let (pos
        block-key
        b-key
        (loop t)
        prev-branch-pos
        cur-pos
        to
        (case-fold-search t)
        )
    (save-excursion
      (save-match-data
        (end-of-line)
        (setq block-key (search-ifcaseselect-backward))
        (when block-key
          (setq block-key (upcase block-key))
          (cond ((string= block-key "IF")
                 (end-of-line)
                 (setq prev-branch-pos (point))
                 (while loop
                   (setq b-key (search-ifbranch-forward-i))
                   (if (not b-key)
                       (setq loop nil)
                     (if (string= b-key "ENDIF")
                         (progn
                           ;;fold block
                           (forward-line 0)
                           (backward-char)
                           (setq to (point))
                           (my-hide-region-hide prev-branch-pos to)
                           (setq loop nil)
                           )
                       (setq cur-pos (point))
                       (forward-line 0)
                       (backward-char)
                       (setq to (point))
                       (my-hide-region-hide prev-branch-pos to)
                       (goto-char cur-pos)
                       (end-of-line)
                       (setq prev-branch-pos (point))
                       )
                     )))
                (t
                 ;; select or case
                 (forward-word)
                 (while loop
                   (setq b-key (search-case-select-branch-forward-i))
                   (if (not b-key)
                       (setq loop nil)
                     (setq b-key (upcase b-key))
                     (if (or (string= b-key "ENDSELECT")
                             (string= b-key "ENDCASE")
                             (string= b-key "ENDINSPECT")
                             )
                         (progn
                           (when prev-branch-pos
                             ;;fold block
                             (forward-line 0)
                             (backward-char)
                             (setq to (point))
                             (my-hide-region-hide prev-branch-pos to)
                             )
                           (setq loop nil)
                           )
                       (when prev-branch-pos
                         (setq cur-pos (point))


                         (if (string-match ":" b-key)
                             (save-match-data
                               (save-excursion
                                 (search-uncommented-b "{")
                                 (forward-line 0)
                                 (backward-char 1)
                                 (setq to (point))

                                 ))
                             (save-excursion
                               (forward-line 0)
                               (backward-char 1)
                               (setq to (point)))
                             )
                         
                         (my-hide-region-hide prev-branch-pos to)
                         (goto-char cur-pos)
                         )
                       (setq prev-branch-pos (line-end-position))
                       )
                     )))))))))


;;}}}

;;;{{{ Misc tool


;;;}}}


(defun identifier-under-point ()
  (let (beg end symbol)
    (save-excursion
      (if (not (looking-at "\\_<"))
          (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (setq end (point)))
    (setq symbol (buffer-substring-no-properties beg end))))


;; (defun my-in-quote-1 ()
;;   "Returns t if point is in quote, nil otherwise."
;;   (let ((case-fold-search t)
;;         (limit (point))
;;         (count 0))
;;     (save-match-data
;;       (save-excursion
;;         (forward-line 0)
;;         (while (search-forward "'" limit 'move)
;;           (setq count (1+ count)))
;;         (if (= (% count 2) 0) nil t)))
;;   ))

(defun my-in-quote/comment ()
  "Returns none nil if point is in quote or comment, nil otherwise."
;;   (if (get-text-property (point) 'fontified)
;;       (eq (get-text-property (point) 'face) font-lock-string-face)
;;     ;; Not fontified. Determine it by text.
;;     (my-in-quote-1)
;;     )
  (save-match-data
    (nth 8 (syntax-ppss))))

(defun my-in-comment ()
  """"
  (let ((case-fold-search t)
        (limit (point))
        )
    (save-match-data
      (save-excursion
        (forward-line 0)
	(re-search-forward "%" limit t)
    ))
  ))

;;}}}

;; new functions, 2006.12.29

(defun fold-all-procs ()
  ""
  (interactive)
  (save-excursion
    (save-match-data
     (goto-char (point-min))
     (let ((case-fold-search t)
	   block-beg
	   block-end
	   (loop t))
       (while  (and loop ( re-search-forward "\\_<BLOCK\\_>" nil t))
	 (cond ((or (my-in-comment) (my-in-quote/comment))
		())
	       (t
		(setq block-beg (point))
		(search-endblock-for-block-f)
		(save-excursion
		  (forward-line 0)
		  (backward-char 1)
		  (setq block-end (point)))
		(if (> block-end block-beg)
		    (progn
		      (my-hide-region-hide block-beg block-end)))

		;;(setq loop nil)

		)))))))


(defun narrow-proc ()
  ""
  (interactive)
  (save-excursion
    (save-match-data
      (let (
	    proc-end
	    proc-beg
	    (success nil)
	    )
	(widen)
	(if (which-proc)
	    (progn
	      (setq proc-beg (point))
	      (save-excursion
		(forward-word 1)
		(when (setq success (search-endblock-for-block-f))
		  (forward-line 1)
		  (setq proc-end (point))))
	      (if success
		  (progn
		    (if (not (search-uncommented-b "\\_<ENDBLOCK\\_>"))
			(progn
			  (goto-char (point-min))
			  (setq proc-beg (point)))
		      (forward-line 1)
		      (setq proc-beg (point)))
		    (narrow-to-region proc-beg proc-end)
		    t))
	))))))

(defun narrow-block ()
  ""
  (interactive)
  (save-excursion
    (save-match-data
      (let (
	    proc-end
	    proc-beg
	    block-string
	    )
	(setq block-string (which-block))
	(if block-string
	    (progn
	      (save-excursion
		(cond ((string-equal (upcase block-string) "THEN")
		       (search-uncommented-b "\\_<\\(IF\\|ELSEIF\\)\\_>"))
		      ((string-equal (upcase block-string) "DO")
		       (search-uncommented-b "\\_<\\(FOR\\|WHILE\\)\\_>"))
		      (t ()))
		(forward-line 0)
		(setq proc-beg (point)))
	      (forward-word 1)
	      (if (which-block-forward)
		  (progn
		    (forward-line 1)
		    (setq proc-end (point))
		    (narrow-to-region proc-beg proc-end)
		    t)))
      )))))

(defun protel-fold-comment()
  ""
  (interactive)
  (let (pos
	from
	to
	)
    (setq pos (1- (my-in-comment)))
    (if pos
	(save-excursion
	  (goto-char pos)
	  (while (forward-comment -1) ())
	  (skip-chars-forward " \n\t\f")
	  (setq from (point))
	  (goto-char pos)
	  (while (forward-comment 1)())
	  (skip-chars-backward " \n\t\f")
	  (setq to (point))
	  (my-hide-region-hide from to)
	  (list from to)
	  )
      )
      ;;not in-comment
      nil
      )
    )

(defun protel-fringe-click-fold (event)
  (interactive "e")
  (mouse-set-point event)
  (smart-fold)
  )

(defun protel-mouse-fold-unfold (event)
  (interactive "e")
  (if (mark-active-p)
      (fold-region)
    (mouse-set-point event)
    (cond ((my-in-comment)
	   (protel-fold-comment)
	   )
	  (t
	   (more-smarter-fold-unfold)
	   )
	  )))

;; (defun my-hide-all-comments (&optional n)
;;   "Hide the comment only line."
;;   (interactive)
;;   (let (from
;;         (hide
;;          (or (null n)
;;              (if (> n 0)
;;                  t
;;                nil))
;;          ))
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "^[ \t]*%" nil t)
;;       (forward-line 0)
;;       (setq from (1- (point)))
;;       (end-of-line)
;;       (put-text-property from (point) 'invisible hide)
;;       )
;;     )))

;; (defun my-show-all-comments ()
;;   ""
;;   (interactive)
;;   (protel-hide-all-comments -1)
;;   )




;; sdelta support!!!

;; The <>| is in 89, 95
(defvar protel-sdelta-column-list (list 89 95))

(defun protel-set-sdelta-column (n)
  ""
  (interactive
   (list (read-number "Column: " (current-column))))
  (pushnew n protel-sdelta-column-list))

(defalias 'sc 'protel-set-sdelta-column)

(defun protel-sdelta-line-has-notion-p ()
  (dolist (col protel-sdelta-column-list)
    (move-to-column col)
    (case (char-after (point))
      ((?> ?< ?|)
       (return col))
      (otherwise nil))))

(defun protel-sdelta-goto-next-change ()
  (interactive)
  (let (pos)
    (save-excursion
      (catch 'loop
        (while (not (eobp))
          (forward-line)
          (when (protel-sdelta-line-has-notion-p)
            (setq pos (point))
            (throw 'loop t))
          )))
    (when pos
      (goto-char pos))))

(defun protel-sdelta-goto-previous-change ()
  (interactive)
  (let ((loop t) pos)
    (save-excursion
      (catch 'loop
        (while loop
          (forward-line -1)
          (when (bobp)
            (setq loop nil))
          (when (protel-sdelta-line-has-notion-p)
            (setq pos (point))
            (throw 'loop t))
          )))
    (when pos
      (goto-char pos))))

(defun protel-sdelta-goto-next-change-block ()
  (interactive)
  (let (pos)
    (cond ((protel-sdelta-line-has-notion-p)
           (setq pos (point))
           ;; we are in the change block. find the end of the block.
           (forward-line)
           (while (protel-sdelta-line-has-notion-p)
             (setq pos (point))
             (forward-line)
             (when (eobp)
               (error "Reach the end of the buffer.")))
           (forward-line -1)
           (when pos
             (goto-char pos))
           )))
  (protel-sdelta-goto-next-change))

(defun protel-sdelta-goto-previous-change-block ()
  (interactive)
  (let (pos)
    (cond ((protel-sdelta-line-has-notion-p)
           (setq pos (point))
           ;; we are in the change block. find the end of the block.
           (forward-line -1)
           (while (protel-sdelta-line-has-notion-p)
             (setq pos (point))
             (forward-line -1)
             (when (bobp)
               (error "Reach the beginning of the buffer.")))
           (forward-line)
           (when pos
             (goto-char pos))
           )))
  (protel-sdelta-goto-previous-change))
