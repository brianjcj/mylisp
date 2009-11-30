;;; protel.el --- Major mode for editing PROTEL language code.

;; Authors: Rick McConney and Liem Nguyen
;; Maintainer: Mentor Helpline <mentor.helpline@nortel.ca>
;; Keywords: language protel

;; $Revision: 1.7+ $

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

;; Purpose
;;
;; Major mode for editing PROTEL language code. PROTEL is the NORTEL
;; proprietary telephony language.

;; Installation
;;
;; - Put this file somewhere Emacs can find it (i.e., load-path) and
;;   "byte-compile-file" it.
;;
;; - Add this line either in your ~/.emacs or site setup file:
;;
;;   (require 'protel)
;;
;; Note: To make use of full capability of this mode, GNUSERV package
;;       must be installed first.

;; Documentation and where to get protel.el
;;
;;    http://mentor/homebrew/emacs/protel-mode.html

;;; Change log

;; A complete change log is at the end of this file.

;;; Code:

(defconst protel-version-id
  "2.7.6")

(defconst protel-maintainer "mentor.helpline@nortel.ca"
  "Protel mode maintainer Email for bug reporting purposes.")


;------------------------ Required packages ----------------------------


(autoload 'dired-string-replace-match "dired"
  "Bring DIRED in when this function is called."
  t)

;; Used to "easily" define the menu for both GNU Emacs and XEmacs.
;; In some older XEmacs versions, easymenu was named as auc-menu???
(condition-case nil
    (require 'auc-menu)
  (error (require 'easymenu)))

;; Used in PROTEL/PLS/MSym buffers to fontify the Protel code.
(and window-system (require 'font-lock))

;; The following packages are not needed at top level as we only use
;; them in some functions.
(eval-when-compile

  ;;Used in various places to simplify the code
  (require 'cl)

  ;; Used in PROTEL buffers to hide/show Protel code
  (require 'outline)

  ;; Used to build the Index menu using either "imenu" package (for
  ;; GNU Emacs) or "func-menu" package (for XEmacs)
  (condition-case nil
      (require 'func-menu)
    (error (require 'imenu)))

  )

;; Brian
(require 'speedbar)
(require 'protel-util)

;------------------------ User variables ------------------------------

(defgroup protel nil
  "Major mode for editing Protel source in Emacs."
  :group 'languages)

(defcustom protel-indent 3
  "*This variable gives the indentation in protel-mode."
  :type 'integer
  :group 'protel)

(defcustom protel-comment-prefix "%% "
  "*The comment prefix to use in normal comments. A single white space at the
end of the prefix string is highly recommended."
  :type 'string
  :group 'protel)

(defcustom protel-class-template-file ""
  "*A file containing an alternate class interface template."
  :type 'file
  :group 'protel)


(defcustom protel-beautify-command "beautify"
  "*Command to beautify the PROTEL code.  This command must be on your system
path.  See http://mentor/homebrew/ for more info and how to 
get this command.  This command is provided in Mentor 5.4.0."
  :type 'string
  :group 'protel)

;; Brian: Don't use integer in concat. Use int-to-string to cast it to string
(defcustom protel-beautify-options (concat "-i" (int-to-string protel-indent))
  "*Options for the beautify command."
  :type 'string
  :group 'protel)

(defcustom protel-font-lock-highlight-level 2
  "*Specify what level of complexity to use when highlighting PROTEL codes.
The higher the complexity the slower the highlighting. Possible values are:

nil - no PROTEL-specific highlighting
t   - The standard highlighting: keywords, comments, strings, and $
      records (e.g., $LI MODENTRY RESTART)
1   - Standard highlighting + labels and scope references such as
      <module>::<id>
2   - 1 + the base, user-defined (see `protel-user-types'), and implied
      types (types enforced by the language such as PRT TO <type>)
3   - 2 + procedure names and their types
4   - 3 + variable names and their types
5   - 4 + numbers, square brackets, and table indices

Level 2 highlighting should be quite responsive on all but the largest
source files. The highlighting complexity is updated only when files are
loaded or `protel-mode' is issued in the current PROTEL buffer.

If you are using page-at-a-time highlighting, such as in the \"lazy-lock\"
package, you should be able to use level 5 highlighting without any
performance problems.

Highlighting in PROTEL buffers is controlled by the following faces:

- font-lock-keyword-face: used for PROTEL reserved keywords.
- font-lock-comment-face: used for comments.
- font-lock-string-face: used for literal string (in quote).
- font-lock-warning-face: used for $ records (e.g., $LI MODENTRY RESTART).
- font-lock-function-name-face: used for procedure/function names.
- font-lock-variable-name-face: used for variable names.
- font-lock-type-face: used for PROTEL types.
- font-lock-builtin-face: used for numeric constants.
- font-lock-reference-face: used for scope references
- font-lock-label-face: used for labels.

To control the highlighting/coloring, the user can set different X resources
of the form:

   Emacs.<face_name>.attributeFont: <font>
   Emacs.<face_name>.attributeForeground: <color>
   Emacs.<face_name>.attributeBackground: <color>
   Emacs.<face_name>.attributeUnderline: <nil or t>

There are 2 other alternatives that face attributes can be set:

- Have some lisp code in the .emacs file to set the
  variable font-lock-face-attributes directly (see font-lock.el for more
  details) or using set-face-... functions family to alter each individual
  face attribute.

- Use of \"Customize\" function available under \"Help\" menu to customize
  each individual face attribute.

Fontification in PROTEL buffers can also be activated by:

- Enabling global-font-lock-mode (only available with GNU Emacs); i.e., add
  this line in your .emacs:

     (global-font-lock-mode t)

- Turning ON font-lock in an \"Emacs\" way; i.e., add this line in your
  .emacs:

     (add-hook 'protel-mode-hook 'turn-on-font-lock)

See also the Emacs User's Guide for the description on how to set attributes
(colors & fonts) for these faces."
  :type 'integer
  :group 'protel)

(defcustom protel-user-types ""
  "*This variable contains the regular expression of user-defined Protel
types in addition to `protel-base-types' to be highlighted."
  :type 'regexp
  :group 'protel)

(defcustom protel-auto-build-index nil
  "*non-nil to always build the menu Index of types, procedures, and
variables globally declared in the current PROTEL buffer. Presently,
declarations starting from the first column are considered as global.
This variable is only applicable in GNU Emacs."
  :type 'boolean
  :group 'protel)

;; Brian::
(defcustom protel-upcase-keyword t
  "*This variable determine whether upcase keyword automatically"
  :type 'boolean
  :group 'protel)

;---------------------- Internal variables ----------------------------

(defvar search-last-string nil
  "Last search string.")

(defvar end-comment-column 70
  "Maximum comment column.")

(defvar using-xemacs
  (let ((case-fold-search t))
    (string-match "lucid\\|xemacs" (emacs-version)))
  "non-nil if using XEmacs.")

(defvar using-gnuemacs
  (let ((case-fold-search t))
    (string-match "GNU Emacs " (emacs-version)))
  "non-nil if using GNU Emacs.")

(defvar protel-mode-syntax-table nil
  "Syntax table in use in protel buffers.")

(defvar protel-mode-map nil
  "Keymap used in protel mode.")

(defvar protel-templates-menu nil
  "\"Templates\" menu for PROTEL buffers.")

(defvar protel-menu nil
  "\"Protel\" menu for PROTEL buffers.")

;; Brian
(defvar navigate-menu nil
  "\"Navigate\" menu for PROTEL buffers.")


(defconst protel-keywords-regexp
  (concat "\\<\\("
          ;; ABSTRACT ALIGNED ANY AREA AS
          "A\\(BSTRACT\\|LIGNED\\|NY\\|REA\\|S\\)\\|"
          ;; BIND BLOCK BLOCKING BOOL BY BYVALUE
          "B\\(IND\\|LOCK\\(\\|ING\\)\\|OOL\\|Y\\(\\|VALUE\\)\\)\\|"
          ;; CASE CAST CLASS CREATE
          "C\\(AS\\(E\\|T\\)\\|LASS\\|REATE\\)\\|"
          ;; DCL DEFINITIONS DESC DO DOWN
          "D\\(CL\\|E\\(FINITIONS\\|SC\\)\\|O\\(\\|WN\\)\\)\\|"
          ;; ELSE ELSEIF ENDALIGNED ENDAREA ENDBLOCK ENDCASE ENDCLASS
          ;; ENDDO ENDIF ENDINSPECT ENDOVLY ENDSELECT ENDSTRUCT ENTRY
          ;; EXCLUSIVE EXIT EXTERN
          "E\\(LSE\\(\\|IF\\)\\|N\\(D\\(A\\(LIGNED\\|REA\\)\\|"
          "BLOCK\\|C\\(ASE\\|LASS\\)\\|DO\\|I\\(F\\|"
          "NSPECT\\)\\|OVLY\\|S\\(ELECT\\|TRUCT\\)\\)\\|TRY\\)\\|"
          "X\\(CLUSIVE\\|IT\\|TERN\\)\\)\\|"
          ;; FALSE FAST FIXED FOR FORWARD FROM
          "F\\(A\\(LSE\\|ST\\)\\|IXED\\|OR\\(\\|WARD\\)\\|ROM\\)\\|"
          ;; HANDLE HIDDEN
          "H\\(ANDLE\\|IDDEN\\)\\|"
          ;; IF IN INCL INIT INSPECT INTERFACE INTRINSIC IS
          "I\\(F\\|N\\(\\|CL\\|IT\\|SPECT\\|T\\(ERFACE\\|RINSIC\\)\\)\\|"
          "S\\)\\|"
          ;; LITERAL
          "LITERAL\\|"
          ;; METHOD MOD
          "M\\(ETHOD\\|OD\\)\\|"
          ;; NIL NOTINCL
          "N\\(IL\\|OTINCL\\)\\|"
          ;; OBSOLETE OF OPERATIONS OUT OVER OVERRIDING OVLY
          "O\\(BSOLETE\\|F\\|PERATIONS\\|"
          "UT\\|V\\(E\\(R\\|RRIDING\\)\\|LY\\)\\)\\|"
          ;; PACK PERPROCESS PRIVATE PROC PROTECTED PTR
          "P\\(ACK\\|ERPROCESS\\|R\\(IVATE\\|OC\\|OTECTED\\)\\|TR\\)\\|"
          ;; RAISE READABLE REF REFINES RESOLVE RETRY RETURN RETURNS
          "R\\(AISE\\|E\\(ADABLE\\|F\\(\\|INES\\)\\|"
          "SOLVE\\|T\\(RY\\|URN\\(\\|S\\)\\)\\)\\)\\|"
          ;; SCRATCH SECTION SELECT SELF SET SHARED STRUCT SUPER
          "S\\(CRATCH\\|E\\(CTION\\|LECT\\|LF\\|T\\)\\|"
          "HARED\\|TRUCT\\|UPER\\)\\|"
          ;; TABLE TDSIZE THEN TO TRUE TYPE TYPEDESC TYPE_SIZE_IN_BITS
          "T\\(ABLE\\|DSIZE\\|HEN\\|"
          "O\\|RUE\\|YPE\\(\\|DESC\\|_SIZE_IN_BITS\\)\\)\\|"
          ;; UNRESTRICTED UP UPB UPDATES USES
          "U\\(NRESTRICTED\\|P\\(\\|B\\|DATES\\)\\|SES\\)\\|"
          ;; VAL
          "VAL\\|"
          ;; WHILE WRITABLE WRITEBLOCKING
          "W\\(HILE\\|RIT\\(ABLE\\|EBLOCKING\\)\\)"
          "\\)\\>"
          )
  "*Protel keywords.")

(defvar protel-outline-regexp
  (concat "[ \t]*\\(\\("
          ;; label : BLOCK
          "\\([a-zA-Z0-9$_]+[ \t]*:[ \t]*\\)?BLOCK\\|"
          ;; CASE
          "CASE\\|"
          ;; DCL DEFINITIONS DO
          "D\\(CL\\|EFINITIONS\\|O\\)\\|"
          ;; ELSE ELSEIF ENDBLOCK ENDCASE ENDDO ENDIF
          ;; ENDINSPECT ENDSELECT EXIT
          "E\\(LSE\\(\\|IF\\)\\|ND\\(BLOCK\\|CASE\\|"
          "DO\\|I\\(F\\|NSPECT\\)\\|SELECT\\)\\|XIT\\)\\|"
          ;; label : FOR
          "\\([a-zA-Z0-9$_]+[ \t]*:[ \t]*\\)?FOR\\|"
          ;; HANDLE
          "HANDLE\\|"
          ;; IF INSPECT INTERFACE
          "I\\(F\\|N\\(SPECT\\|TERFACE\\)\\)\\|"
          ;; OUT
          "OUT\\|"
          ;; PERPROCESS PRIVATE PROTECTED
          "P\\(ERPROCESS\\|R\\(IVATE\\|OTECTED\\)\\)\\|"
          ;; RAISE RETRY RETURN
          "R\\(AISE\\|ET\\(RY\\|URN\\)\\)\\|"
          ;; SECTION SELECT SHARED
          "S\\(E\\(CTION\\|LECT\\)\\|HARED\\)\\|"
          ;; label : WHILE
          "\\([a-zA-Z0-9$_]+[ \t]*:[ \t]*\\)?WHILE\\)\\>\\|"
          ;; CASE/SELECT labels
          "{.+\\(}[ \t]*:\\)?"
          "\\)")
  "*Protel keywords for outline-mode.")

(defconst protel-identifier "[A-Za-z_$][A-Za-z0-9_$]*"
  "The syntax for an identifier.")

(defconst protel-endblocks "END\\(BLOCK\\|CASE\\|DO\\|IF\\|INSPECT\\|SELECT\\)"
  "Keywords which end a block statements.")

(defconst protel-procfollowups "METHOD\\|PROC"
  "Keywords that follow procedure definitions.")

(defconst protel-proc-obsolete "NONTRANSPARENT\\|QUICK"
  "Obsolete keywords preceding the PROC keyword, possibly.")

(defconst protel-postis
  "RETURN\\|BLOCK\\|FORWARD\\|EXTERN\\|ENTRY\\|INTRINSIC\\|NIL"
  "Keywords following the IS keyword in procedure definitions.")

(defconst protel-declwords "DCL\\|PRIVATE\\|PROC\\|PROTECTED\\|SHARED"
  "Declaration keywords.")

(defconst protel-base-types
  (concat
   ;; $anything $classdesc $create_class_ptr $descriptor $dint $field
   ;; $int $longint $object $pointer $refdesc $size $typedesc $typemark
   ;; $uint $universal_ptr $universal_read_ptr $vardesc
   "$\\(anything\\|c\\(lassdesc\\|reate_class_ptr\\)\\|"
   "d\\(escriptor\\|int\\)\\|field\\|int\\|"
   "longint\\|object\\|pointer\\|refdesc\\|size\\|type\\(desc\\|mark\\)\\|"
   "u\\(int\\|niversal\\(_read\\)?_ptr\\)\\|vardesc\\)\\|"
   ;; address_as_bytes  area_size a_word
   "a\\(ddress_as_bytes\\|rea_size\\|_word\\)\\|"
   ;; bcd bcdtod bintod bit bits_[0-9][0-9] bool bool_in_byte byte
   "b\\(cd\\(\\|tod\\)\\|i\\(ntod\\|t\\(\\|s_[0-9][0-9]\\)\\)\\|"
   "ool\\(\\|_in_byte\\)\\|yte\\)\\|"
   ;; callid char chars_[0-9][0-9] chartod ciretcode command_doc
   ;; command_indices compl_cd connection_id cp_time_stamp
   "c\\(allid\\|har\\(\\|s_[0-9][0-9]\\|tod\\)\\|iretcode\\|"
   "o\\(m\\(mand_\\(doc\\|indices\\)\\|pl_cd\\)\\|nnection_id\\)\\|"
   "p_time_stamp\\)\\|"
   ;; descofbit descofchar descofint descriptor_as_bytes dialing_plan
   ;; digit dint dkbf_access_mode dkbf_access_type dkbf_device
   ;; dkbf_fcbid dkbf_fid dkbf_fileinfo dkbf_file_org_type
   ;; dkbf_file_type dkbf_nil_fcbid dkbf_regid dkbf_root_fid
   ;; dkbf_volume_id dkbf_volume_info dynamic_type_mismatch
   "d\\(esc\\(of\\(bit\\|char\\|int\\)\\|riptor_as_bytes\\)\\|"
   "i\\(aling_plan\\|git\\|nt\\)\\|kbf_\\(access_\\(mode\\|type\\)\\|"
   "device\\|f\\(cbid\\|i\\(d\\|le\\(info\\|_\\(org_type\\|type\\)\\)\\)\\)"
   "\\|nil_fcbid\\|r\\(egid\\|oot_fid\\)\\|volume_i\\(d\\|nfo\\)\\)\\|"
   "ynamic_type_mismatch\\)\\|"
   ;; eflag event_types exception exception_during_constructor
   ;; exception_during_destructor exception_during_exception
   "e\\(flag\\|vent_types\\|xception\\(\\|_during_"
   "\\(constructor\\|destructor\\|exception\\)\\)\\)\\|"
   ;; fid field_spec file_ref fullint 
   "f\\(i\\(d\\|eld_spec\\|le_ref\\)\\|ullint\\)\\|"
   ;; halfint
   "halfint\\|"
   ;; id int inttod int_as_bytes
   "i\\(d\\|nt\\(\\|tod\\|_as_bytes\\)\\)\\|"
   ;; level_index lint log_id longword
   "l\\(evel_index\\|int\\|o\\(g_id\\|ngword\\)\\)\\|"
   ;; magnitude max_set mbid module_id msgbody
   "m\\(a\\(gnitude\\|x_set\\)\\|bid\\|odule_id\\|sgbody\\)\\|"
   ;; parm_aspct pool_?id posint power_of_2 procid procvar
   ;; programmer_exception ptrtoint
   "p\\(arm_aspct\\|o\\(ol_?id\\|sint\\|wer_of_2\\)\\|"
   "ro\\(c\\(id\\|var\\)\\|grammer_exception\\)\\|trtoint\\)\\|"
   ;; q1index q1way q2header q2index q2way quadword qword
   "q\\(1\\(index\\|way\\)\\|2\\(header\\|index\\|way\\)\\|"
   "uadword\\|word\\)\\|"
   ;; report_id restartreason restrict_cl
   "re\\(port_id\\|st\\(artreason\\|rict_cl\\)\\)\\|"
   ;; stretcode string stringbuff system_exception
   ;; system_software_exception
   "s\\(tr\\(etcode\\|ing\\(\\|buff\\)\\)\\|"
   "ystem\\(_software\\)?_exception\\)\\|"
   ;; tbcd_digit timeinvals trint two_ints type_id type_name
   "t\\(bcd_digit\\|imeinvals\\|rint\\|wo_ints\\|ype_\\(id\\|name\\)\\)\\|"
   ;; udint uint ulint unsignedint utint
   "u\\(dint\\|int\\|lint\\|nsignedint\\|tint\\)\\|"
   ;; varsize
   "varsize\\|"
   ;; zero_to_[0-9]+
   "zero_to_[0-9]+"
   )
  "Protel predefined and base types.")

(defvar protel-types protel-base-types
  "The Protel types to be highlighted. See also `protel-base-types'
and `protel-user-types'.")

(defconst protel-constants "[0-9]+"
  "Decimal numbers.")

(defconst protel-hexconstants "[0-9A-F]+"
  "Hexadecimal numbers.")

(defvar protel-font-lock-keywords nil
  "The actual highlighting regexp for PROTEL mode. It is set only on the
first `protel-mode' activation.")

(defvar protel-font-lock-keywords-0 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides standard highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-font-lock-keywords-1 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides the level 1 highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-font-lock-keywords-2 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides the level 2 highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-font-lock-keywords-3 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides the level 3 highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-font-lock-keywords-4 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides the level 4 highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-font-lock-keywords-5 nil
  "For consideration as a value of `protel-font-lock-keywords'. This
provides the level 5 highlighting in the PROTEL mode.
See also `protel-font-lock-highlight-level'.")

(defvar protel-imenu-generic-expression
  '(("TYPE" "^TYPE[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 1)
    ("DCL"
     "^\\(BLOCKING[ \t\n]+\\|WRITEBLOCKING[ \t\n]+\\|\\b\\)DCL[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 2)
    ("SHARED"
     "^\\(BLOCKING[ \t\n]+\\|WRITEBLOCKING[ \t\n]+\\|\\b\\)SHARED[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 2)
    ("PRIVATE"
     "^\\(BLOCKING[ \t\n]+\\|WRITEBLOCKING[ \t\n]+\\|\\b\\)PRIVATE[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 2)
    ("PROTECTED"
     "^\\(BLOCKING[ \t\n]+\\|WRITEBLOCKING[ \t\n]+\\|\\b\\)PROTECTED[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 2)
    ("SCRATCH"
     "^SCRATCH[ \t\n]+\\([a-zA-Z0-9$_]+\\)" 1)
    ("OBSOLETE"
     "^OBSOLETE[ \t\n]+\\(TYPE\\|DCL\\|SHARED\\|PRIVATE\\|PROTECTED\\)[ \t\n]+\\([a-zA-Z0-9$_]+\\)"
     2)
    )
  "Regexp to make the Index menu. See `imenu-generic-expression' for further
explanation.  Available only in GNU Emacs.")


(defvar fume-function-name-regexp-protel
  "^[ \t]*dcl[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \t]+\\(proc[ \t\n(]\\|method[ \t\n]\\|[_a-zA-Z][_a-zA-Z0-9]*_proc\\)"
  "Expression to get function/procedure names.  Available only in XEmacs.")


;--------------- Specific setting for GNU Emacs & XEmacs ---------------

(if using-xemacs
    (progn
      (require 'func-menu)
      (defvar pop-up-frames nil)
      (defvar font-lock-global-modes nil) ; interaction with vixen.emacs
      )
  )


;---------------------- PROTEL type files -----------------------------

(setq auto-mode-alist
      (cons '("SYMS" . protel-mode)
            auto-mode-alist))
(setq auto-mode-alist
      (cons '("sym[0-9][0-9][0-9][0-9][0-9]" . protel-mode)
            auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.protel$" 'protel-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.protdms$" 'protel-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.[a-z][a-z][0-9][0-9]$" 'protel-mode)
            auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.[a-z][a-z][0-9][0-9].*$" 'protel-mode)
            auto-mode-alist))

(setq auto-mode-alist
      (cons (cons "\\.fix$" 'protel-mode) auto-mode-alist))

(setq auto-mode-alist
      (cons (cons "\\.sdelta$" 'protel-mode) auto-mode-alist))



;; Brian: for speedbar
;; Add supported extension name
(let ((mode-alist auto-mode-alist))
  (while mode-alist
    (when (eq (cdr (car mode-alist)) 'protel-mode)
      (speedbar-add-supported-extension (car (car mode-alist))))
    (setq mode-alist (cdr mode-alist))))

;------------------------- font-lock stuff --------------------------

;; Adding more faces

(if (null window-system)
    () ; do nothing, font-lock cannot be supported

  ;; The following checks for faces only available in GNU Emacs 20
  ;; or later...
  (if (not (boundp 'font-lock-builtin-face))
      (progn
        (defvar font-lock-builtin-face 'font-lock-builtin-face)
        (copy-face 'bold 'font-lock-builtin-face))
    )

  (if (not (boundp 'font-lock-warning-face))
      (progn
        (defvar font-lock-warning-face 'font-lock-warning-face)
        (copy-face 'italic 'font-lock-warning-face))
    )

  ;; This face is not available in font-lock as of GNU Emacs 20.2.
  (if (not (boundp 'font-lock-label-face))
      (progn
        (defvar font-lock-label-face 'font-lock-label-face)
        (copy-face 'underline 'font-lock-label-face))
    )
  )

;; PROTEL buffers

(defun protel-initialize-font-lock-keywords ()
  "Initialize font-lock keywords for all levels. This function should only
be called when the `protel-mode' is invoked for the first time."
  ;; First add user-defined types
  (if (not (string= protel-user-types ""))  ; protel-user-types is set
      (setq protel-types (concat protel-base-types "\\|" protel-user-types))
    )

  ;; Level 0 (standard)
  (setq protel-font-lock-keywords-0
        (list
         '("\\(^\\$.*$\\)" 1 font-lock-warning-face t)
         '("\\(%.*$\\)"  1 font-lock-comment-face t)
         (list (concat "\\("
                       protel-keywords-regexp
                       "\\)")
               1 'font-lock-keyword-face)
         ))

  ;; Level 1
  (setq protel-font-lock-keywords-1
        (append protel-font-lock-keywords-0
                (list
                 (list
                  (concat "\\<\\(" protel-identifier ":\\)[^:]")
                  '(1 font-lock-label-face nil t))
                 (list
                  (concat "\\<\\(" protel-identifier "::\\)")
                  '(1 font-lock-reference-face nil t))
                 (list
                  (concat "\\<" protel-endblocks
                          "\\([ \t\n]+\\(" protel-identifier
                          "\\)\\)?[ \t\n]*;")
                  '(3 font-lock-label-face nil t))
                 (list
                  (concat "\\<EXIT[ \t\n]+\\(" protel-identifier
                          "\\)[ \t\n]*;")
                  '(1 font-lock-label-face nil t))
                 )))

  ;; Level 2
  (setq protel-font-lock-keywords-2
        (append protel-font-lock-keywords-1
                (list
                 (list
                  (concat "[^.]\\<\\(" protel-types "\\)\\>")
                  '(1 font-lock-type-face nil t))
                 (list
                  (concat "\\<\\(A\\(NY\\|S\\)\\|"
                          "DESC\\([ \t]*\\[[a-zA-Z0-9_$]+\\]\\)?[ \t\n]+OF\\|"
                          "OV\\(ER\\|LY\\)\\|PTR[ \t\n]+TO\\|"
                          "RE\\(FINES\\|TURNS\\)\\|T\\("
                          "ABLE[ \t]*\\[[a-zA-Z0-9_$]+\\][ \t\n]+OF\\|"
                          "YPE\\(DESC\\)?\\)\\)\\>"
                          "[ \t\n]+\\(" protel-identifier "\\)")
                  '(8 font-lock-type-face nil t))
                 )))

  ;; Level 3
  (setq protel-font-lock-keywords-3
        (append protel-font-lock-keywords-2
                (list
                 (list
                  (concat "^[ \t]*\\<DCL\\>[ \t\n]+\\(" protel-identifier
                          "\\)[ \t\n]+"
                          ;; Cover the possibility of optional comments
                          "\\(%.*[ \t\n]+\\)*"
                          ;; As well as optional obsolete keywords
                          "\\(" protel-proc-obsolete "[ \t\n]+\\)?"
                          ;; Followed either by PROC or METHOD or
                          "[ \t\n]*\\(\\<" protel-procfollowups "\\>\\|"
                          ;; a type identifier and the IS keyword.
                          "\\(" protel-identifier
                          "\\)[ \t\n]+\\(%.*[ \t\n]+\\)*IS[ \t\n]+"
                          "\\(%.*[ \t\n]+\\)*\\(" protel-postis "\\)\\)")
                  '(1 font-lock-function-name-face nil t)
                  '(5 font-lock-type-face nil t))
                 )))

  ;; Level 4
  (setq protel-font-lock-keywords-4
        (append protel-font-lock-keywords-3
                (list
                 (list
                  (concat "\\<\\(" protel-declwords "\\)\\>[ \t\n]+"
                          ;; followed by at least one variable
                          "\\(\\(" protel-identifier "\\)"
                          ;; and an optional list of variables & comment
                          "\\([ \t\n]*,[ \t\n]*\\(%.*[ \t\n]+\\)*"
                          "\\(" protel-identifier "\\)\\)*[ \t\n]+\\("
                          ;; then the type and comment; the whole is repeated
                          protel-identifier
                          "\\)[ \t\n]*,[ \t\n]*\\(%.*[ \t\n]+\\)*\\)*"
                          ;; last declaration
                          "\\(" protel-identifier "\\)"
                          "\\([ \t\n]*,[ \t\n]*\\(%.*[ \t\n]+\\)*"
                          "\\(" protel-identifier "\\)\\)*[ \t\n]+\\("
                          protel-identifier "\\)[ \t\n]*;")
                  '(3 font-lock-variable-name-face nil t)
                  '(6 font-lock-variable-name-face nil t)
                  '(7 font-lock-type-face nil t)
                  '(9 font-lock-variable-name-face nil t)
                  '(12 font-lock-variable-name-face nil t)
                  '(13 font-lock-type-face nil t))
                 (list
                  (concat "\\<\\(" protel-declwords "\\)[ \t\n]+"
                          ;; followed by a variable and its type
                          "\\(\\(" protel-identifier
                          "\\)[ \t\n]+\\(" protel-identifier
                          ;; possibly with an INIT or IS
                          "\\)[ \t\n]*\\(I\\(NIT\\|S\\).*"
                          "[ \t\n]*\\)?,[ \t\n]*"
                          ;; followed possibly by a comment;
                          ;; the whole is repeated
                          "\\(%.*[ \t\n]+\\)*\\)*"
                          ;; last declaration
                          "\\(" protel-identifier "\\)[ \t\n]+\\("
                          protel-identifier
                          "\\)[ \t\n]*\\(INIT\\|IS.*[ \t\n]*\\)?;"
                          )
                  '(3 font-lock-variable-name-face nil t)
                  '(4 font-lock-type-face nil t)
                  '(8 font-lock-variable-name-face nil t)
                  '(9 font-lock-type-face nil t))
                 )))

  ;; Level 5
  (setq protel-font-lock-keywords-5
        (append protel-font-lock-keywords-4
                (list
                 (list
                  (concat "\\(\\<\\(" protel-constants "\\)\\>\\|"
                          "\\(#\\)\\<\\(" protel-hexconstants "\\)\\>\\)")
                  '(2 font-lock-builtin-face nil t)
                  '(3 font-lock-builtin-face nil t)
                  '(4 font-lock-builtin-face nil t))
                 (list
                  (concat "\\(\\[[^][]*\\(\\[[^][]*\\]\\)?[^][]*\\]\\)")
                  '(1 font-lock-warning-face keep t))
                 )))
  )

(defun protel-font-lock-setup ()
  "Set the `protel-font-lock-keywords' based on the current value of
`protel-font-lock-highlight-level'."

  (if (null protel-font-lock-keywords)
      ;; protel-mode is being invoked for the first time
      ;; Initialize font-lock keywords for all levels
      (protel-initialize-font-lock-keywords))

  ;; Re-adjust the level
  (cond ((eq protel-font-lock-highlight-level nil)
	 (setq protel-font-lock-keywords nil))
	((eq protel-font-lock-highlight-level 1)
	 (setq protel-font-lock-keywords protel-font-lock-keywords-1))
	((eq protel-font-lock-highlight-level 2)
	 (setq protel-font-lock-keywords protel-font-lock-keywords-2))
	((eq protel-font-lock-highlight-level 3)
	 (setq protel-font-lock-keywords protel-font-lock-keywords-3))
	((eq protel-font-lock-highlight-level 4)
	 (setq protel-font-lock-keywords protel-font-lock-keywords-4))
	((eq protel-font-lock-highlight-level 5)
	 (setq protel-font-lock-keywords protel-font-lock-keywords-5))
        (t
         (setq protel-font-lock-keywords protel-font-lock-keywords-0))
	)
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(protel-font-lock-keywords nil t))
  )

;; (defun pls-id-section-p (buf)
;;   "Return t if buf contains an ID section. If buf is nil, current buffer is
;; checked instead."
;;   (if (null buf) (setq buf (current-buffer)))
;;   (save-excursion
;;     (set-buffer buf)
;;     (goto-char (point-min))
;;     (if (search-forward ".* DDOC STATUS ***NOTE*** May Promote DS->FR->DC->DT"
;;                         nil
;;                         t )
;;         t
;;       nil)
;;   ))
;; 
;; (defun protel-turn-on-font-lock ()
;;   (if (pls-id-section-p nil)
;;       (text-mode)
;;     (and (eq window-system 'x) (turn-on-font-lock))
;;   ))
;; 
;; (add-hook 'protel-mode-hook 'protel-turn-on-font-lock)
;; 
;; PLS command buffers

;;(defun pls-font-lock-setup ()
;;  (make-local-variable 'font-lock-defaults)
;;  (setq font-lock-defaults '(pls-font-lock-keywords t))
;;  )


;---------- PROTEL Index menu based on "imenu" or "func-menu" ------------

; Brian Jiang - enhance imenu 2007-7-19

(defun in-comment-simple ()
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

(defsubst protel-is-listing-file? ()
  "work for now. but don't know in future."
  (save-excursion
    (save-match-data
      (goto-char 1)
      (looking-at "^Copyright "))))

(defun backward-word-out-of-comment ()
  ""
  (interactive)
  (let (ret
	(kloop t)
	pos
	)
    (setq ret (forward-word -1))
    (while (and ret kloop)
      (if (setq pos(in-comment-simple))
	  (progn
	    (setq ret nil)
	    ;(forward-line)
	    (goto-char pos)
	    (setq ret (forward-word -1)))
	(setq kloop nil)
	))
    ret
    ))

(defun backward-word-out-of-comment-listing ()
  ""
  (interactive)
  (let (ret
	(kloop t)
	pos
	)
    (setq ret (forward-word -1))
    (while (and ret kloop)
      (cond ((setq pos(in-comment-simple))
             (setq ret nil)
             (goto-char pos)
             (setq ret (forward-word -1))
             )
            ((< (current-column) 29)
             (forward-line 0)
             (setq ret nil)
             (setq ret (forward-word -1))
             )
            (t
             (setq kloop nil)
             )))
    ret
    ))

(defun forward-word-out-of-comment ()
  ""
  (interactive)
  (let (ret
	(kloop t)
	)
    (setq ret (forward-word))
    (while (and ret kloop)
      (if (in-comment-simple)
	  (progn
	    (setq ret nil)
	    (forward-line)
	    (setq ret (forward-word)))
	(setq kloop nil)
	))
    ret
    ))

(defun forward-word-out-of-comment-listing ()
  ""
  (interactive)
  (let (ret
	(kloop t)
	)
    (setq ret (forward-word))
    (while (and ret kloop)

      (cond ((in-comment-simple)
             (setq ret nil)
             (forward-line)
             (setq ret (forward-word))
             )
            ((< (current-column) 29)
             (move-to-column 29)
             (setq ret nil)
             (setq ret (forward-word))
             )
            (t
             (setq kloop nil)
             )))
    ret
    ))

(defvar protel-imenu-seperate-var-dcl t)
;; (setq protel-imenu-seperate-var-dcl t)

(defun determine-dcl-name (forward-word-out-of-comment-func backward-word-out-of-comment-func)
  ""
  (interactive)
  (let ((cw (current-word))
        now-word)
    (save-excursion
      (save-match-data
        (condition-case nil
            (progn
              (funcall forward-word-out-of-comment-func)
              (setq now-word (upcase (current-word)))
              (cond ((string= now-word "METHOD")
                     (dotimes (i 5) (funcall backward-word-out-of-comment-func))
                     (if (string= (upcase (current-word)) "IN")
                         (progn
                           (dotimes (i 2) (funcall forward-word-out-of-comment-func))
                           (concat "[" (current-word) "]::" cw))
                       cw))
                    (protel-imenu-seperate-var-dcl
                     (cond ((or (string= now-word "PROC")
                                (string= now-word "NONTRANSPARENT")
                                (string= now-word "RARE")
                                )
                            cw)
                           (t
                            (funcall forward-word-out-of-comment-func)
                            (setq now-word (upcase (current-word)))
                            (cond ((string= now-word "IS")
                                   (funcall forward-word-out-of-comment-func)
                                   (setq now-word (upcase (current-word)))
                                   (cond ((or (string= now-word "BLOCK")
                                              (string= now-word "RETURN")
                                              (string= now-word "FORWARD")
                                              (string= now-word "ENTRY"))
                                          cw)
                                         (t
                                          nil)))
                                  (t
                                   nil)))))
                    (t
                     cw)))
          (error
           ;; likely (current-word) returns nil when reaching the end of buffer.
           nil
           ))
        ))))

(defvar protel-dcl-regex nil)
(setq protel-dcl-regex (concat "\\(\\_<DCL\\)[ \t\n]+[a-zA-Z0-9$_]+"
                               "\\|\\(\\_<BLOCK\\_>\\)"
                               "\\|\\(\\_<TYPE\\)[ \t\n]+[a-zA-Z0-9$_]+"
                               ))
                          
(defun pp-create-index-function ()
  ""
  (let (alist
        match-s alist-type  beg-m name alist-dcl
        forward-word-out-of-comment-func
        backward-word-out-of-comment-func
        ) ;; alist-var
    ;; (save-restriction
    ;;  (widen)

    (cond ((protel-is-listing-file?)
           (setq forward-word-out-of-comment-func 'forward-word-out-of-comment-listing)
           (setq backward-word-out-of-comment-func 'backward-word-out-of-comment-listing)
           )
          (t
           (setq forward-word-out-of-comment-func 'forward-word-out-of-comment)
           (setq backward-word-out-of-comment-func 'backward-word-out-of-comment)
           ))
    
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward protel-dcl-regex nil t)
          (setq beg-m (match-beginning 0))
          (cond  ((in-quote)
                  ())
                 (t
                                        ;(setq match-s (upcase (match-string 1)))
                  (cond (              ;(string-equal match-s "BLOCK")
                         (match-beginning 2)
                         (unless (in-comment-simple)
                           (search-endblock-for-block-f))
                         )
                        (               ;(string-equal match-s "TYPE")
                         (match-beginning 3)
                         (if (save-excursion (goto-char beg-m) (in-comment-simple))
                             (progn
                               (goto-char beg-m)
                               (forward-line)
                               )
                           ;; (setq alist-type (append alist-type (list (cons (downcase (current-word)) (save-excursion (forward-line 0)(point-marker))))))
                           (setq alist-type (push (cons (downcase (current-word)) (save-excursion (forward-line 0)(point-marker))) alist-type))
                           )
                         )
                        (t         ; now it match DCL (match-string 1)
                         (if (save-excursion (goto-char beg-m) (in-comment-simple))
                             (progn
                               (goto-char beg-m)
                               (forward-line)
                               )
                           (if (setq name (determine-dcl-name forward-word-out-of-comment-func backward-word-out-of-comment-func))
                               ;; (setq alist (append alist (list (cons (downcase name) (save-excursion (forward-line 0)(point-marker))))))
                               (setq alist (push (cons (downcase name) (save-excursion (forward-line 0)(point-marker))) alist))
                             (setq alist-dcl (append alist-dcl (list (cons (downcase (current-word)) (save-excursion (forward-line 0)(point-marker))))))
                             ))))
                  ))                    ;cond or
        
          )))
    (setq alist (nreverse alist))
    (when alist-type
      (setq alist-type (nreverse alist-type))
      (setq alist (push (cons "+Types+..." alist-type) alist)))

    (when alist-dcl
      (setq alist (append alist (list (cons "+Dcl+..." alist-dcl)))))
    
    alist
    ))

;; (defun pp-create-index-function ()
;;   ""
;;   (let (alist )
;; 					; (save-restriction
;; 					;  (widen)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward protel-dcl-regex nil t)
;; 	(if (or
;; 	     (in-comment-simple)
;; 	     ;;(save-excursion
;; 	     (which-block-forward) ;; skip this block too.
;; 	     ;;  )
;; 	     )
;; 	    ;; ignore
;; 	    ()
;; 	  (setq alist (append alist (list (cons (determine-dcl-name) (save-excursion (forward-line 0)(point))))))
;; 	  ))
;; 					;	)
;;       )
;;     alist
;;     ))



(defun protel-turn-on-imenu ()
  "Index menu setup and installation."
  ;; imenu case: brian
  (setq imenu-case-fold-search t)
  ;; end
  
  ;; brian
  ;;(setq imenu-generic-expression protel-imenu-generic-expression)
  (setq imenu-sort-function 'imenu--sort-by-name)
  (setq imenu-create-index-function 'pp-create-index-function)
  ;; (setq imenu-sort-function 'imenu--sort-by-position)
  ;; end
  )

(add-hook 'protel-mode-hook (lambda ()
                              (protel-turn-on-imenu)
                              (imenu-add-menubar-index)
                              ))




;; (defun protel-build-index ()
;;   "Build the menu Index using \"imenu\" package and the pattern set in
;; `protel-imenu-generic-expression'.  For XEmacs, the \"func-menu\" and
;; `fume-function-name-regexp-protel' are used instead."
;;   (interactive)
;;   (if using-xemacs
;;       (fume-add-menubar-entry)
;;     (let ((imenu-present (local-key-binding [menu-bar index])))
;;       (if imenu-present
;;           () ; do nothing
;;         (imenu-add-to-menubar "Index")
;;         (force-mode-line-update)
;;         ))
;;     ))


;---------------------- PROTEL syntax table ---------------------------

(let ((table (make-syntax-table)))
  (modify-syntax-entry ?_ "w" table)
  (modify-syntax-entry ?\# "_" table)
  (modify-syntax-entry ?\( "()" table)
  (modify-syntax-entry ?\) ")(" table)
  (modify-syntax-entry ?$ "w" table)
  (modify-syntax-entry ?* "." table)
  (modify-syntax-entry ?/ "." table)
  (modify-syntax-entry ?+ "." table)
  (modify-syntax-entry ?- "." table)
  (modify-syntax-entry ?= "." table)
  (modify-syntax-entry ?\& "." table)
  (modify-syntax-entry ?\| "." table)
  (modify-syntax-entry ?< "." table)
  (modify-syntax-entry ?> "." table)
  (modify-syntax-entry ?\[ "(]" table)
  (modify-syntax-entry ?\] ")[" table)
  (modify-syntax-entry ?\{ "(}" table)
  (modify-syntax-entry ?\} "){" table)
  (modify-syntax-entry ?. "." table)
  (modify-syntax-entry ?\\ "." table)
  (modify-syntax-entry ?: "." table)
  (modify-syntax-entry ?^ "." table)
  (modify-syntax-entry ?% "<" table)
  (modify-syntax-entry ?\n ">" table)
  (modify-syntax-entry ?@ "." table)
  (modify-syntax-entry ?\; "." table)
  (modify-syntax-entry ?\' "\"" table)
  (modify-syntax-entry ?\" "\"" table)
  (setq protel-mode-syntax-table table))


;---------------------- PROTEL key map --------------------------------

(let ((map (make-sparse-keymap)))

  ;; Key binding
  (cond
   (using-xemacs
    (define-key map [tab] 'protel-tab)
    (define-key map [(shift backtab)] 'protel-untab)
    (define-key map [kp_tab] 'indent-relative)
    (define-key map [(shift kp_backtab)] 'protel-untab)
    (define-key map [(control >)] 'protel-tab)
    (define-key map [(control <)] 'protel-untab)
    (define-key map [return] 'protel-newline)
    ;; Brian:: It is conflict with cua.el
    ;; (define-key map [(shift return)] 'newline)
      
    ;; This is to overcome the bug with XEmacs whereby backspace and
    ;; delete keys generate the same code.  The assignment below with
    ;; a hook in delbackspace-compute-delete-value in delbackspace.el
    ;; (to add protel-untab to the list) will only modify backspace;
    ;; delete key remains as is.
    (define-key map "\177" 'protel-untab))
   (using-gnuemacs
    (define-key map [tab] 'protel-tab)
    (define-key map [S-backtab] 'protel-untab)
    (define-key map [kp-tab] 'indent-relative)
    (define-key map [S-kp-backtab] 'protel-untab)
    (define-key map [?\C->] 'protel-tab)
    (define-key map [?\C-<] 'protel-untab)
    (define-key map [return] 'protel-newline)
    (define-key map [backspace] 'protel-untab)
    ;; Brian:: It is conflict with cua.el
    ;; (define-key map [S-return] 'newline)
   )

   )
  (if (not (eq window-system 'x))   ; Running Emacs at home...
      (progn
	(define-key map "\^i" 'protel-tab)
	(define-key map "\^?" 'protel-untab)
	(define-key map "\C-m" 'protel-newline)
	))



  (define-key map "\C-c{" 'protel-begin-comment)
  (define-key map "\C-c}" 'protel-end-comment)
  (define-key map "\C-c%" 'protel-comment-region)
  (define-key map "\C-c5" 'protel-uncomment-region)
  (define-key map "\C-c(" 'protel-paired-parens)
  (define-key map "\C-c<" 'unindent-code-rigidly)
  (define-key map "\C-c>" 'indent-code-rigidly)
  (define-key map "\C-c." 'protel-hide-body)
  (define-key map "\C-c," 'show-all)


  ;(define-key map "\C-c-" 'protel-hide-region)

  (define-key map "\C-c^" 'protel-fix-case-in-region)

  ;(define-key map "\C-c\C-z" 'end-of-block)
  ;(define-key map "\C-c\C-y" 'protel-show-region)

  (define-key map "\C-c\C-w" 'protel-strip-whitespaces)
  ;(define-key map "\C-c\C-v" 'show-subtree)
  ;(define-key map "\C-c\C-u" 'outline-up-heading)
  ;(define-key map "\C-c\C-t" 'line-to-top)

  ;(define-key map "\C-c\C-r" 'protel-collapse-region)

  (define-key map "\C-c\C-p" 'protel-backward-to-same-indent)
  (define-key map "\C-c\C-o" 'protel-open-module-goto-symbol)
  (define-key map "\C-c\C-n" 'protel-forward-to-same-indent)


  ;(define-key map "\C-c\C-k" 'show-branches)
  ;(define-key map "\C-c\C-j" 'prev-code-thread)

  ;(define-key map "\C-c\C-h" 'hide-subtree)
  ;(define-key map "\C-c\C-f" 'outline-forward-same-level)



  ;(define-key map "\C-c\C-b" 'outline-backward-same-level)


  (define-key map "\C-cZ" 'protel-module-header)
  (define-key map "\C-cz" 'protel-section-header)
  (define-key map "\C-cX" 'protel-show-occurrences)
  (define-key map "\C-cW" 'protel-write-protds)
  (define-key map "\C-cw" 'protel-while-loop)
  (define-key map "\C-cV" 'protel-cond-unprotds)
  (define-key map "\C-cv" 'protel-align)
  (define-key map "\C-cU" 'protel-unprotds)
  (define-key map "\C-cu" 'protel-until)
  (define-key map "\C-cT" 'protel-table)
  (define-key map "\C-ct" 'protel-type)
  (define-key map "\C-cS" 'protel-inspect)
  (define-key map "\C-cs" 'protel-struct)
  ;(define-key map "\C-cR" 'show-protel-construct)
  ;(define-key map "\C-cr" 'hide-protel-construct)
  (define-key map "\C-cQ" 'protel-show-all-comments)
  (define-key map "\C-cq" 'protel-hide-all-comments)
  ;(define-key map "\C-cP" 'outline-previous-visible-heading)
  (define-key map "\C-cp" 'protel-unpreemptable)
  (define-key map "\C-cO" 'protel-class)
  (define-key map "\C-co" 'protel-ovly)
  ;(define-key map "\C-cN" 'outline-next-visible-heading)
  (define-key map "\C-cM" 'protel-mutexon)
  (define-key map "\C-cm" 'protel-method)
;;  (define-key map "\C-cL" 'protel-build-index)
  (define-key map "\C-cl" 'protel-lock)
  (define-key map "\C-cJ" 'protel-if-then-elseif)
  (define-key map "\C-cI" 'protel-if-then-else)
  (define-key map "\C-ci" 'protel-if-then)
  (define-key map "\C-cH" 'protel-big-header)
  (define-key map "\C-ch" 'protel-function-header)
  (define-key map "\C-cg" 'goto-line)
  (define-key map "\C-cF" 'protel-forward)
  (define-key map "\C-cf" 'protel-for-loop)
  ;(define-key map "\C-cE" 'show-entry)
  ;(define-key map "\C-ce" 'hide-entry)
  (define-key map "\C-cD" 'protel-function-spec)
  (define-key map "\C-cd" 'protel-do-loop)
  (define-key map "\C-cC" 'protel-select)
  (define-key map "\C-cc" 'protel-case)
  (define-key map "\C-cB" 'protel-beautify-buffer)
  (define-key map "\C-cb" 'protel-block)
  ;(define-key map "\C-cA" 'show-children)
  (define-key map "\C-ca" 'protel-area)

  ;; Brian:: Bind the key
  (define-key map " " 'protel-trigger-upcase-keyword)
  (define-key map ";" 'protel-trigger-upcase-keyword)
  (define-key map "(" 'protel-trigger-upcase-keyword)
  (define-key map ")" 'protel-trigger-upcase-keyword)
  (define-key map "[" 'protel-trigger-upcase-keyword)
  (define-key map "," 'protel-trigger-upcase-keyword)
  ;; Brian

  (setq protel-mode-map map))


;; brian: key binding for sdelta search.
;; (define-key  protel-mode-map "\C-c\C-j" 'protel-sdelta-goto-next-change-block)
;; (define-key  protel-mode-map "\C-c\C-k" 'protel-sdelta-goto-previous-change-block)

;; (define-key  protel-mode-map "\C-cj" 'protel-sdelta-goto-next-change-block)
;; (define-key  protel-mode-map "\C-ck" 'protel-sdelta-goto-previous-change-block)

;; (define-key  protel-mode-map "\C-cn" 'protel-sdelta-goto-next-change-block)
;; (define-key  protel-mode-map "\C-cp" 'protel-sdelta-goto-previous-change-block)

(define-key  protel-mode-map "\C-c\C-n" 'protel-sdelta-goto-next-change-block)
(define-key  protel-mode-map "\C-c\C-p" 'protel-sdelta-goto-previous-change-block)



;; Define "Protel" and "Templates" menus. "easymenu" package is used to
;; simplify the code and at the same time, to support both Emacsen.

;; (setq protel-templates-menu
;;       '("Templates"
;;         ["AREA" protel-area t]
;;         ["BLOCK" protel-block t]
;;         ("Decisions"
;;          ["IF-THEN" protel-if-then t]
;;          ["IF-THEN-ELSE" protel-if-then-else t]
;;          ["IF-THEN-ELSEIF" protel-if-then-elseif t])
;;         ("Headers"
;;          ["Function Header" protel-function-header t]
;;          ["Module Header" protel-module-header t]
;;          ["Section Header" protel-section-header t]
;;          ["Big Header" protel-big-header t]
;;          ["Class Header" protel-class t])
;;         ("Loops"
;;          ["DO loop" protel-do-loop t]
;;          ["FOR loop" protel-for-loop t]
;;          ["OVER loop" protel-until t]
;;          ["WHILE loop" protel-while-loop t])
;;         ["METHOD" protel-method t]
;;         ("Mutual Exclusion"
;;          ["Lock" protel-lock t]
;;          ["Mutex on/off" protel-mutexon t]
;;          ["Unpreemptable" protel-unpreemptable t])
;;         ["OVLY" protel-ovly t]
;;         ["Procedure/Function" protel-function-spec t]
;;         ("Protected Store"
;;          ["Unprotectds"  protel-unprotds t]
;;          ["Cond Unprotectds" protel-cond-unprotds t]
;;          ["Write Protected Store" protel-write-protds t])
;;         ["INSPECT" protel-inspect t]
;;         ("Selections"
;;          ["CASE" protel-case t]
;;          ["SELECT" protel-select t])
;;         ["STRUCT" protel-struct t]
;;         ("$LI records"
;;          ("MODENTRY"
;;           ["RESTART" protel-li-modentry-restart t]
;;           ["IPLUNLOAD" protel-li-modentry-iplunload t]
;;           ["IPL" protel-li-modentry-ipl t]
;;           ["PERMPROC" protel-li-modentry-permproc t]
;;           ["INITPROC" protel-li-modentry-initproc t]
;;           ["PPVINIT" protel-li-modentry-ppvinit t])
;;          ["MODINCR" protel-li-modincr t]
;;          ["MODALIAS" protel-li-modalias t]
;;          ["MODSTACK" protel-li-modstack t]
;;          ["NEEDSINIT" protel-li-needsinit t]
;;          ["INITS" protel-li-inits t]
;;          ["INITAFTER" protel-li-initafter t]
;;          ["MUSTEXIST" protel-li-mustexist t]))
;;       )

(setq protel-menu
      '("Protel"
        ("Beautification"
         ["Fix Case in Region" protel-fix-case-in-region (mark-active-p)]
         ["Strip Trailing Spaces" protel-strip-whitespaces t]
         ["Beautify buffer" protel-beautify-buffer t])
        ("Miscellaneous"
         ["Comment Region" protel-comment-region (mark-active-p)]
         ["Uncomment Region" protel-uncomment-region (mark-active-p)]
         ["Show Occurrences" protel-show-occurrences t]
;;         ["Build Index menu" protel-build-index
;;          (null (local-key-binding [menu-bar index]))]
         )
        ("Templates"
        ["AREA" protel-area t]
        ["BLOCK" protel-block t]
        ("Decisions"
         ["IF-THEN" protel-if-then t]
         ["IF-THEN-ELSE" protel-if-then-else t]
         ["IF-THEN-ELSEIF" protel-if-then-elseif t])
        ("Headers"
         ["Function Header" protel-function-header t]
         ["Module Header" protel-module-header t]
         ["Section Header" protel-section-header t]
         ["Big Header" protel-big-header t]
         ["Class Header" protel-class t])
        ("Loops"
         ["DO loop" protel-do-loop t]
         ["FOR loop" protel-for-loop t]
         ["OVER loop" protel-until t]
         ["WHILE loop" protel-while-loop t])
        ["METHOD" protel-method t]
        ("Mutual Exclusion"
         ["Lock" protel-lock t]
         ["Mutex on/off" protel-mutexon t]
         ["Unpreemptable" protel-unpreemptable t])
        ["OVLY" protel-ovly t]
        ["Procedure/Function" protel-function-spec t]
        ("Protected Store"
         ["Unprotectds"  protel-unprotds t]
         ["Cond Unprotectds" protel-cond-unprotds t]
         ["Write Protected Store" protel-write-protds t])
        ["INSPECT" protel-inspect t]
        ("Selections"
         ["CASE" protel-case t]
         ["SELECT" protel-select t])
        ["STRUCT" protel-struct t]
        ("$LI records"
         ("MODENTRY"
          ["RESTART" protel-li-modentry-restart t]
          ["IPLUNLOAD" protel-li-modentry-iplunload t]
          ["IPL" protel-li-modentry-ipl t]
          ["PERMPROC" protel-li-modentry-permproc t]
          ["INITPROC" protel-li-modentry-initproc t]
          ["PPVINIT" protel-li-modentry-ppvinit t])
         ["MODINCR" protel-li-modincr t]
         ["MODALIAS" protel-li-modalias t]
         ["MODSTACK" protel-li-modstack t]
         ["NEEDSINIT" protel-li-needsinit t]
         ["INITS" protel-li-inits t]
         ["INITAFTER" protel-li-initafter t]
         ["MUSTEXIST" protel-li-mustexist t]))
        "----"
        ["Protel mode Version" protel-version t]
        ["Protel mode Help" (describe-function 'protel-mode) t])
      )

;; Brian
(define-key  protel-mode-map [(shift f7)] 'fold-ifcaseselect-branches)

(setq navigate-menu
      '("Navigate"
;; 	["Smart Block Search" smart-search t]
;; 	["Smart Block Search Backward" smart-search-backward t]
;;         "----"
	["More Smarter Fold/Unfold" more-smarter-fold-unfold t]
	["Smart Fold" smart-fold t]
	["Smart Unfold" smart-unfold t]
	["Fold whole IF/CASE/SELECT" fold-ifcaseselect-block t]
        ["Fold IF/CASE/SELECT Branches" fold-ifcaseselect-branches t]
        "----"
	["Which Block" which-block t]
	["Which Block Forward" which-block-forward t]
	["Which Procedure" which-proc t]
        "----"
	["Search IF/CASE/SELECT Backward" search-ifcaseselect-backward t]
	["Search END IF/CASE/SELECT Forward" search-endifcaseselect-forward t]
        "----"
;; 	("RAW Search and Fold"
;; 	 ["Next IF Branch"  search-ifbranch-forward t]
;; 	 ["Next CASE/SELECT Branch" search-case-select-branch-forward t]
;; 	 ["ENDDO" search-enddo-for-do-f t]
;; 	 ["ENDBLOCK" search-endblock-for-block-f t]
;; 	 ["ENDIF" search-endif-for-if-f t]
;; 	 "----"
;; 	 ["Previous IF Branch" search-ifbranch-backward t]
;; 	 ["Previous CASE/SELECT Branch" search-case-select-branch-backward t]
;; 	 ["DO" search-do-for-enddo-b t]
;; 	 ["BLOCK" search-block-for-endblock-b t]
;; 	 ["IF" search-if-for-endif-b t]
;; 	 "----"
;; 	 ["Fold IF Branch" fold-ifbranch t]
;; 	 ["Fold CASE/SELECT" fold-case-branch t]
;; 	 ["Fold DO" fold-do t]
;; 	 ["Fold BLOCK" fold-block t]
;; 	 "----"
;; 	 ["Unfold IF Branch" unfold-ifbranch t]
;; 	 ["Unfold CASE/SELECT" unfold-case-branch t]
;; 	 ["Unfold DO" unfold-do t]
;; 	 ["Unfold BLOCK" unfold-block t])
;;         "----"
	["Fold Procedure" fold-proc t]
	["Unfold Procedure" unfold-proc t]
	"----"
	["Fold Region" fold-region t]
	["Fold Comment" protel-fold-comment t]
        "----"
	["Unfold Region" unfold-region t]
	["Unfold ALL" unfold-all t]
	"----"
	["Fold All Procedure" fold-all-procs t]
	["Narrow Procedure" narrow-proc t]
        ["Narrow Block" narrow-block t]
        ["Widen Procedure/Block" widen t]
        "-"
        ["Search" codepilot-search-hi-1 :help "Protel Aware Search."]
        ["Search text" codepilot-search-hi-string t]
        ["Search id" codepilot-search-hi :help "Search the Protel id ignoring comments/quote."]
        ["Search forward" codepilot-search-hl-again-f t]
        ["Search backard" codepilot-search-hl-again-b t]
        ["Occurrance" current-word-occur t]
        ["Pop to Mark" pop-to-mark-command t]
        "-"
        ("Sdelta"
         ["Next Sdelta Block" protel-sdelta-goto-next-change-block t]
         ["Prev Sdelta Block" protel-sdelta-goto-previous-change-block t]
         "----"
         ["Set Sdelta Column" protel-set-sdelta-column t])
        "-"
        ["Toggle MyIMenu" myimenu-toggle :help "List procs and type in the buffer."]
        "-"
        ["Block Traceback" proc-outline-where-we-are :help "Show how to reach the current line of codes"]
        ["List all blocks" protel-proc-outline :help "Show the block outline of the current procedure"]
        ["Procs contain me" proc-outline-which-procs-i-in :help "List the procs contain search text."]
        ["3 Window layer" proc-outline-blocktrace-and-procs-layout t]
        ["1 Window" delete-other-windows t]
	))
	

;; Brian
(easy-menu-define protel-menu-symbol
                  protel-mode-map
                  "Navigate menu"
                  navigate-menu)

;; (easy-menu-define protel-templates-menu-symbol
;;                   protel-mode-map
;;                   "Templates menu"
;;                   protel-templates-menu)
(easy-menu-define protel-menu-symbol
                  protel-mode-map
                  "Protel menu"
                  protel-menu)




;---------------------- PROTEL mode entry -----------------------------

(require 'which-func)

(defun protel-mode ()
  "This is a mode intended to support program development in PROTEL.
The source code, installation procedure, and the user's guide can be found
at the web page below:

   http://mentor/homebrew/emacs/protel-mode.html

Summary of Protel mode features
-------------------------------

- Automatic code generation: there are several PROTEL construct
  templates (including $LI records) saving user's typing time and
  errors, e.g., semi-colon is missing.

- Special motion commands helping to traverse PROTEL code efficiently
  and easily.

- Ability to collapse, hide, and expand PROTEL code to easily see the
  program flow/skeleton while ignoring the code detail.

- Ability to color/fontify the PROTEL code (keywords, comments,
  string, $LI or compiler directives in different colors/fonts). Reading
  the code is more pleasure and comfortable, speeding up thus the
  understanding of existing code.

- Finding the cross-reference, definition, or  implementation of an ID
  under cursor right within Emacs based on either SYM or MSym.

- On-screen compilation/linking with automatic error tracking.

- Miscellaneous utilities to manipulate PROTEL code, e.g., insert
  comments at the beginning of each line in a region...

These capabilities, when combined with existing Emacs features, increase
tremendously the productivity of a DMS designer specially during coding
phase or during code digging to gain an understanding on the existing DMS
code.

The PROTEL mode is set atomatically when LOOK/EDIT from PLS DMS or
DMSPL, or can be set manually by typing  M-x protel-mode.

All commands and control constructs of PROTEL can be invoked by either
clicking on the menu bar or typing a control key sequence (e.g.,
\"control-c i\" gives the IF construct).

To get help on each individual function below, do a help on that
function, i.e., M-x help-for-help then select f and enter the function
name.

\\{protel-mode-map}"

  (interactive)

  ;; Standard stuff

  (unless (eq major-mode 'protel-mode)

    (kill-all-local-variables)
  
    (use-local-map protel-mode-map)
    (setq major-mode 'protel-mode)
    (setq mode-name "PROTEL")
    (set-syntax-table protel-mode-syntax-table)

    ;; Basic formatting
    (make-local-variable 'comment-column)
    (setq comment-column 40)
    (make-local-variable 'end-comment-column)
    (setq end-comment-column 70)

    ;;<brian
    (make-local-variable 'comment-start)
    (setq comment-start "%")
    ;; brian>
  
    (make-local-variable 'paragraph-start)
    (setq page-delimiter "\\$EJECT")
    (setq paragraph-start (concat "^[ \t]*$\\|^\\:\\|^\\.\\|" page-delimiter))
    (make-local-variable 'paragraph-separate)
    (setq paragraph-separate paragraph-start)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (setq paragraph-ignore-fill-prefix t)
    (make-local-variable 'adaptive-fill-regexp)
    (setq adaptive-fill-regexp "^[ \t]*%+\\(.*%+\\)?[ \t]+")
    (make-local-variable 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp adaptive-fill-regexp)

    ;; Indentation
    (make-local-variable 'indent-line-function)
    (setq indent-line-function 'protel-tab)
    (setq indent-tabs-mode nil) ; just spaces so compile does not complain

    ;; Outline stuff
    ;; Brian: comment out
    (setq selective-display t)
    (make-local-variable 'outline-regexp)
    (setq outline-regexp protel-outline-regexp)
    (make-local-variable 'outline-level)
    (setq outline-level 'protel-outline-level)
    ;;   (outline-minor-mode 1)

    ;; Misc
    (setq blink-matching-paren t)
    (make-local-variable 'require-final-newline)
    (setq require-final-newline t)
                                        ;(setq mark-even-if-inactive t)
    (setq case-fold-search t)

;;     ;;Brian
;;     (easy-menu-add navigate-menu
;;                    protel-mode-map)

;;     ;; Install "Protel" and "Templates" menu. Only needed for XEmacs
;;     (easy-menu-add protel-templates-menu
;;                    protel-mode-map)
;;     (easy-menu-add protel-menu
;;                    protel-mode-map)

    (protel-font-lock-setup)

    (run-mode-hooks 'protel-mode-hook))

  )

;---------------------- Utilities -------------------------------------

(defun get-identifier (string)
  (let* ((default (identifier-under-point))
	 (spec (read-string string default)))
    spec))

(defun identifier-under-point ()
  (let (beg end symbol)
    (save-excursion
      (if (not (looking-at "\\<"))
          (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (setq end (point)))
    (setq symbol (buffer-substring-no-properties beg end))))

(defun get-token-from-str (str num &optional regexp)
  "Extract the nth word from str. Word is sub-string of str which matches the
regexp. If not supplied, regexp is set to \"[^ \\t]+\"."
  (if (not regexp) (setq regexp "[^ \t]+"))
  (let* ((case-fold-search t)
         (beg (string-match regexp str))
         (end (match-end 0)))
    (while (and (> num 1) beg)
      (setq num (1- num))
      (setq beg (string-match regexp str end))
      (setq end (match-end 0)))
    (if beg
        (substring str beg end)
      "")
  ))

(defun is-protel-blank-line ()
  "Returns t if the current line is a blank line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (eolp)))

;; (defmacro rotate-item-on-stack (stack)
;;   "Rotate item on top of stack to bottom of stack. This item is then returned
;; to the calling function."
;;   (`(let ((item (pop (, stack))))
;;       (setq (, stack) (reverse (, stack)))
;;       (push item (, stack))
;;       (setq (, stack) (reverse (, stack)))
;;       item)
;;   ))

(defun protel-version ()
  "Display the version"
  (interactive)
  (message protel-version-id))

(defun mark-active-p ()
  "Test the mark for both Emacsen."
  (if using-xemacs
      (mark)
    (and transient-mark-mode mark-active))
  )

(defun bind-comint-output-filter (filter)
  "Append FILTER to the list of output process filters associated with the
process in the current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
        (progn
          (message "Binding the output process filter for buffer %s"
                   (buffer-name))
          (add-hook 'comint-output-filter-functions filter t t)
        ))
  ))

;----------------------- Routines to search PROTEL files --------------

(defun remember-search-string (str)
  "Add string to search ring"
  (interactive "sSearch String: ")
  (setq str (downcase str))
  (setq search-last-string str)
  (if (or (null search-ring)
          (not (string= str (car search-ring))))
      (progn
        (setq search-ring (cons str search-ring))
        (if (> (length search-ring) search-ring-max)
            (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))

(defun protel-id-search (symbol start end)
  (let ((case-fold-search t)
        found hit-end-of-file)
    (goto-char (point-min))
    (while (and (not found) (not hit-end-of-file))
      (if (re-search-forward (concat start symbol end) nil t)
        (if (not (in-comment))
          (progn
            (forward-word -1)
            (setq found t)))
        (setq hit-end-of-file t)))
    (if found 't (goto-char (point-min)) 'nil)))

(defun protel-occur-font-lock-keywords ()
  "This function is called within font-lock routines to determine the keywords
to highlight."
  (let* ((cur-buf (buffer-name))
         (end (string-match "\\[" cur-buf))
         )
    (list (substring cur-buf 0 end))
    )
  )

(defun protel-occur-font-lock-setup ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(protel-occur-font-lock-keywords t t))
  )

(defun protel-show-occurrences ()
  "Show all lines with occurences of the symbol entered by the user in a new
buffer named as \"<symbol>[<buffer name>]\".  The `occur-mode' is used."
  (interactive)
  (let* ((default (identifier-under-point))
         (symbol (read-string "Show occurrences of: " default))
         (cur-buf (buffer-name))
         (out-buf (concat symbol "[" cur-buf "]"))
         )
    (and (get-buffer out-buf) (kill-buffer out-buf))
    (occur symbol 0)
    (save-excursion
      (let ((occur-buf (get-buffer "*Occur*")))
        (if (null occur-buf)
            () ; do nothing, a message has already been printed by occur
          (set-buffer occur-buf)
          (rename-buffer out-buf)
          (protel-occur-font-lock-setup))
        ))
  ))


;---------------------- Routines to open PROTEL modules ---------------
;; deleted for win emacs

;------------------ Compile/Link/Listing functions ---------------------
;; deleted for win emacs

;---------------------- Wait functions --------------------------------
;; deleted for win emacs
;---------------------- PLS process/mode ----------------------------
;; deleted for win emacs

;---------------------- PLS utilities ------------------------------
;; deleted for win emacs

;---------------------- Movement commands -----------------------------

(defun protel-go-to-this-indent (step indent-level)
  "Move point repeatedly by <step> lines till the current line
has given indent-level or less, or the start/end of the buffer is hit.
Ignore blank lines, statement labels, block/loop names, and comments."
  (while (and
	  (zerop (forward-line step))
	  (or (looking-at "^[ 	]*$")
	      (looking-at "^[ 	]*--")
	      (looking-at "^<<[A-Za-z0-9_]+>>")
	      (looking-at "^[A-Za-z0-9_]+:")
	      (looking-at "^[ 	]*%")
	      (looking-at "^\\$")
	      (> (current-indentation) indent-level)))
    nil))

(defun protel-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer."
  (interactive)
  (protel-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun protel-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer."
  (interactive)
  (protel-go-to-this-indent 1 (current-indentation))
        (back-to-indentation))

(defun unindent-code-rigidly(arg)
  "Unindents a piece of code an amount supplied by the numeric arg."
  (interactive "p")
  (indent-code-rigidly
    (min (mark)(point)) (max (mark)(point)) (- arg)))

(defun in-quote ()
  "Returns t if point is in quote, nil otherwise."
  (let ((case-fold-search t)
        (limit (point))
        (count 0))
    (save-match-data
      (save-excursion
        (forward-line 0)
        (while (search-forward "'" limit 'move)
          (setq count (1+ count)))
        (if (= (% count 2) 0) nil t)))
  ))

;; Brian: Add a function here
(defun protel-upcase-keyword ()
  "Convert all keywords to upper case"
  (interactive)
  (save-excursion
    (backward-word 1)
    (if ( or (in-comment) (in-quote))
	() 
      (if (looking-at protel-keywords-regexp)
          (upcase-word  1)
	)
      )
    )
  )

;; Brian: Add a function here
(defun protel-trigger-upcase-keyword (arg)
  "For binding to the key trigger to upcase the keyword"
  (interactive "p")
  (self-insert-command (or arg 1))
  (if protel-upcase-keyword
      (protel-upcase-keyword)
    )
  )


(defun in-comment ()
  "Returns the comment prefix if point is in comment. The comment prefix is
computed as follows in this order (<...> means optional):

- % + anything except space, tab, newline + more or one % + <one space>
- % + anything except space, tab, newline + one space
- one or more %

nil is returned if no comment prefix found."
  (let ((case-fold-search t)
        (limit (point))
        )
    (save-match-data
      (save-excursion
        (forward-line 0)
        (cond ((re-search-forward "%[^ \t\n]*%+[ \t]?" limit t)
               (match-string 0))
              ((re-search-forward "%[^ \t\n]*[ \t]" limit t)
               (match-string 0))
              ((re-search-forward "%+" limit t)
               (match-string 0))
              (t
               nil))
    ))
  ))

(defun hide-protel-comment (pos)
  "Hide the protel comment at point"
  (interactive "d")
  (let (start)
    (setq start pos)
    (beginning-of-line 0)
    (setq pos (point))
    (goto-char start)
    (re-search-forward "^[^%]" nil 'move)
    (if (eobp) (backward-char) (end-of-line 0))
    (outline-flag-region pos (point) ?\^M)))

(defun show-protel-comment (pos)
  "Show the hidden protel comment at point"
  (interactive "d")
  (let (start)
    (setq start pos)
    (beginning-of-line)
    (setq pos (point))
    (goto-char start)
    (re-search-forward "^[^%]" nil 'move)
    (if (eobp) (backward-char) (end-of-line 0))
    (outline-flag-region pos (point) ?\n)))

(defun protel-hide-all-comments ()
  "Hides all comment blocks in a protel file"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "%")
        (hide-protel-comment (point)))
    (while (re-search-forward "^%\\|^[ ]*%" nil t)
      (hide-protel-comment (point)))
  ))

(defun protel-show-all-comments ()
  "Reveals all hidden protel comments leaving other hides undisturbed"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "%")
        (show-protel-comment (point)))
    (while (re-search-forward "%" nil t)
      (show-protel-comment (point)))
  ))

;---------------------- Outline stuff ---------------------------------

(defun protel-outline-level ()
  "Compute the current indentation level for functions in outline.el."
  (let ((case-fold-search t))
    (save-match-data
      (save-excursion
        (looking-at "[ \t]*")
        (1+ (- (match-end 0) (match-beginning 0)))
      ))
  ))

(defun protel-hide-body (s)
  "Collapse PROTEL codes for heading supplied in the arg.
Use C-c C-v (show-subtree) to expand the current heading,
or C-c C-k (show-branches) to show branches of the current heading."
  (interactive "sWhich heading? (e.g., DCL, IF..., or [RET] to hide all) ")
  (setq selective-display t)
  (show-all)
  (if (string-equal s "")
      (hide-body)
    (let ((temp outline-regexp))
      (setq outline-regexp (concat "[ \t]*" s "\\b"))
      (hide-body)
      (setq outline-regexp temp)))
  )


;---------------------- PROTEL coding commands ------------------------

(defun protel-beautify-buffer ()
  "Apply the command set in `protel-beautify-command' with options set in
`protel-beautify-options' to the current buffer."
  (interactive)
  (let* ((replace (y-or-n-p "Replace the current buffer? "))
         (read-only buffer-read-only)
         (out-buf nil)
         (start (if replace
                    (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^% EDITION [A-Z][A-Z][0-9][0-9] ("
                                         nil t)
                      (beginning-of-line 2)
                      (point))
                  (point-min)))
        ;;(curr-pos (point))
        )

    (if (and replace (buffer-modified-p)
             (not (y-or-n-p "Buffer has been modified; continue anyway? "))
             )
        (error "Command aborted by user")) ; do nothing, just get out

    (if replace
        (setq buffer-read-only nil)
      (setq out-buf (get-buffer-create (concat "*" (buffer-name) "*"))))
    (shell-command-on-region start
                             (point-max)
                             (concat protel-beautify-command " "
                                     protel-beautify-options)
                             out-buf
                             replace)
    (if replace
        (goto-char (point-min))
      (pop-to-buffer out-buf)
      (protel-mode)) ; this is new buffer
    (setq buffer-read-only read-only)
    (set-buffer-modified-p nil)
    )
  )

(defun protel-fix-case-in-region (start end)
  "Convert all keywords to upper case in region, the rest is in lower case.
Text/code in comments or in quote remains unchanged."
  (interactive "r")
  (if (not (mark-active-p))
      (error "Region is not set.")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (forward-word 1)
          (backward-word 1)
          (if (or (in-comment) (in-quote))
              (forward-word 1) ; just skip over...
            (if (looking-at protel-keywords-regexp)
                ;; it is the actual code and is a keyword
                (upcase-word 1)
              (downcase-word 1)))
    )))
  ))

(defun protel-comment-region ()
  "Inserts `protel-comment-prefix' at the beginning of every line in the
region."
  (interactive)
  (if (not (mark-active-p))
      (error "Region is not set.")
    (save-excursion
      (if (< (point) (mark)) (exchange-point-and-mark))
      (if (or (bolp) (< (current-column) (current-indentation)))
          (forward-line -1))
      (save-restriction
        (let ((case-fold-search t)
              (hpos (if (is-protel-blank-line)
                        999
                      (current-indentation)))
              )
          (end-of-line)
          (exchange-point-and-mark)
          (beginning-of-line)
          (exchange-point-and-mark)
          (narrow-to-region (point) (mark))
          (while (not (bobp))
            (let ((cur-indent (progn
                                (forward-line -1)
                                (if (is-protel-blank-line)
				    999
				  (current-indentation))))
                  )
              (if (< cur-indent hpos)
                  (setq hpos cur-indent))))
          (indent-code-rigidly (point-min) (point-max) (- hpos))
          (save-match-data
            (while (re-search-forward "^" nil t)
              (replace-match protel-comment-prefix)))
          (indent-code-rigidly (point-min) (point-max) hpos))
	))
    ))

(defun protel-uncomment-region ()
  "Removes comment prefix from the beginning of every line in the region. The
comment prefix is not taken from `protel-comment-prefix' but rather computed
for each line as follows (see in-comment for more detail)

- % + anything except space, tab, newline + one or more % + <one space>
- % + anything except space, tab, newline + one space
- one or more %

<...> means optional."
  (interactive)
  (if (not (mark-active-p))
      (error "Region is not set."))
    (save-excursion
      (if (< (point) (mark)) (exchange-point-and-mark))
      (if (or (bolp) (< (current-column) (current-indentation)))
          (forward-line -1))
      (save-match-data
        (save-restriction
          (end-of-line)
          (exchange-point-and-mark)
          (beginning-of-line)
          (exchange-point-and-mark)
          (narrow-to-region (point) (mark))
          (while (not (bobp))
            (let ((case-fold-search t)
                  (comment-prefix (in-comment)))
              (if comment-prefix
                  (progn
                    (beginning-of-line)
                    (search-forward comment-prefix nil t)
                    (replace-match "" nil nil))
                ))
            (end-of-line 0))
        ))
    ))

(defun protel-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun protel-inline-comment ()
  "Start a comment after the end of the line, indented at least COMMENT-COLUMN.
If starting after `end-comment-column', start a new line."
  (interactive)
  (end-of-line)
  (if (> (current-column) end-comment-column) (newline))
  (if (< (current-column) comment-column) (indent-to comment-column))
  (insert protel-comment-prefix))


(defun protel-display-comment ()
"Inserts 3 comment lines, making a display comment."
  (interactive)
  (insert "\n" protel-comment-prefix "\n"
 	  protel-comment-prefix "\n" protel-comment-prefix "\n")
  (end-of-line -2))

(defun protel-align ()
  "Align the current line indentation with the previous non-blank line."
  (interactive)
  (beginning-of-line)
  (if (looking-at "[ \t]+") (replace-match "" nil nil))
  (indent-to (protel-previous-indentation))
  )

(defun protel-strip-whitespaces ()
  "Strips off unnecessary whitespaces at the end of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward " +$" nil t)
      (replace-match "" nil nil)
      (message "Stripped extra spaces"))))

(defun protel-newline ()
  "Insert a newline and indent following line like previous line.
If inside a comment block, the comment prefix of the current line
is automatically inserted. See help on in-comment function.
To get out, use S-RET."
  (interactive)
  (if (looking-at "[ \t]+$")
      (replace-match ""))
  (let ((hpos (current-indentation))
        (comment-prefix (in-comment))
        )
    (if (in-comment)
        (progn
          (if (eolp)
              (progn
                (beginning-of-line)
                (re-search-forward "%" nil t)
                (forward-char -1)
                (let ((cpos (current-column)))
                  (end-of-line)
                  (newline)
                  (indent-to cpos)
                  (insert comment-prefix)))
            (if (bolp)
                (progn
                  (newline)
                  (indent-to hpos)
                  (insert comment-prefix)
                  (end-of-line 0))
              (progn
                (newline)
                (indent-to hpos)
                (insert comment-prefix)
                ))))
      (if (eolp)
          (progn
            (newline)
	    ;; Brian: change here
	    (if protel-upcase-keyword
		(protel-upcase-keyword) )
	    ;; Brian: end change
            (indent-to hpos))
        (if (bolp)
            (progn
	      (newline)
	      ;; Brian::
	      (if protel-upcase-keyword
		  (protel-upcase-keyword))
	      ;; Brian
	      (end-of-line 0)
	      (indent-to hpos))
          (progn
	    (newline)
	    ;; Brian::
	    (if protel-upcase-keyword
		(protel-upcase-keyword))
	    ;; Brian
            (indent-to hpos)))))))

(defun protel-modulo-indentation (col)
  (let ((prev-indent (protel-previous-indentation)))
    (+ prev-indent (* (/ (- col prev-indent)
                         protel-indent) protel-indent))))

(defun protel-previous-indentation ()
  (save-excursion
    (loop (if (bobp) (return 0))
          (beginning-of-line 0)
          (unless (is-protel-blank-line) (return (current-indentation)))
          )
  ))

(defun protel-tab ()
  "If point is on the left side of the first non-whitespace character on the
current line, the current line is indented to the non-blank previous line
indentation or is simply indented an amount set in `protel-indent' if point
has passed the previous line indentation.

If point is on the right side of the first non-whitespace character or at
the end of line, indent-relative is used."
  (interactive)
  (let (cur-indent
        (cur-col (current-column))
        (prev-indent (protel-previous-indentation)))
    (if (<= cur-col (current-indentation))
	(progn
          (if (looking-at "[ \t]+$") (replace-match ""))
	  (back-to-indentation)
          (setq cur-indent (current-column))
	  (if (>= cur-indent prev-indent)
              (indent-to (+ (protel-modulo-indentation cur-indent)
                            protel-indent))
            (indent-to prev-indent)))
      (indent-relative nil))
  ))

(defun protel-untab ()
  "Unindent to previous tab stop."
  (interactive)
  (if (not (mark-active-p))
      (let ((end (current-column))
            (white-space nil))
        (save-excursion
          (skip-chars-backward " \t")
          (setq white-space (bolp)))
        (if white-space
            (if (>= end protel-indent)
                (backward-delete-char-untabify protel-indent ())
              (backward-delete-char 1))
          (backward-delete-char 1)))
    ;; mark is active, simply kill the region
    (kill-region (mark) (point)))
  )

(defun protel-struct ()
  "Insert a skeleton record type declaration."
  (interactive)
  (insert "STRUCT")
  (protel-newline)
  (protel-newline)
  (insert "ENDSTRUCT;")
  (end-of-line 0)
  (protel-tab))

(defun protel-area ()
  "Insert skeleton AREA."
  (interactive)
  (insert "AREA ")
  (protel-newline)
  (protel-newline)
  (insert "ENDAREA;")
  (end-of-line 0)
  (protel-tab))

(defun protel-class ()
  "Insert a class interface definition template. Either inserts the one defined
in the variable `protel-class-template-file' or a default."
  (interactive)
  (if (not (string= protel-class-template-file ""))
      (insert-file-contents protel-class-template-file)
    (insert "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   CLASS INTERFACE DEFINITION                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%@ Class:       class_name
%@
%@ Superclass:  superclass_name
%@ Owner:       owner name
%@ Purpose:     a brief description of this class and its purpose
%@
%@ Rationale:
%@ ~~~~~~~~~~
%@
%@ Description & Responsibilities:
%@ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%@
%@ Restart:
%@ ~~~~~~~~
%@
%@ Usage Notes:
%@ ~~~~~~~~~~~~
%@
%@ Subclassing Notes:
%@ ~~~~~~~~~~~~~~~~~~
%@
%@ Constraints:
%@ ~~~~~~~~~~~~
%@    - algorithm:
%@    - data integrity rules:
%@
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%@ Attribute:      attribute_name
%@
%@ - purpose & description:
%@ - range:
%@ - initial value:
%@ - interaction & dependencies:
%@
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%@ Method:  method_name
%@ Scope:   public/protected/private
%@
%@ Parameters:
%@ ~~~~~~~~~~~
%@    parameter_name:
%@       - purpose & description:
%@       - range:
%@
%@ Returns:
%@ ~~~~~~~~
%@
%@ Functional Description:
%@ ~~~~~~~~~~~~~~~~~~~~~~~
%@
%@ Pre-condition Checks:
%@ ~~~~~~~~~~~~~~~~~~~~~
%@
%@ Post-condition Checks:
%@ ~~~~~~~~~~~~~~~~~~~~~~
%@
%@ Intended Usage:
%@ ~~~~~~~~~~~~~~~
%@
%@ Override Instructions:
%@ ~~~~~~~~~~~~~~~~~~~~~~
%@
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

TYPE class_name
")
    (protel-tab)
    (insert "CLASS REFINES superclass_name")
    (protel-newline)
    (protel-tab)
    (insert "{}:           attr1 some_type;")
    (protel-newline)
    (insert "{EXCLUSIVE}:  attr2 other_type")
    (newline 2)
    (protel-tab)
    (insert "OPERATIONS")
    (protel-newline)
    (protel-tab)
    (insert "{OVERRIDING}: construct METHOD;")
    (protel-newline)
    (insert "{OVERRIDING}: destruct METHOD;")
    (protel-newline)
    (insert "{OVERRIDING}: print_object$ METHOD;")
    (protel-newline)
    (insert "{}:           get_attr1 METHOD (REF) RETURNS some_type;")
    (protel-newline)
    (insert "{}:           set_attr1 METHOD (UPDATES; REF new_value some_type);")
    (protel-newline)
    (protel-untab)
    (insert "ENDCLASS class_name;")
    (insert "
$EJECT

")))

(defun protel-block ()
  "Insert a BLOCK."
  (interactive)
  (let ((protel-block-name (read-string "[block name]: ")))
    (cond
     ((not (string-equal protel-block-name ""))
      (insert protel-block-name ":")
      (protel-newline)))
    (insert "BLOCK")
    (protel-newline)
    (protel-newline)
    (insert "ENDBLOCK")
    (if (not (string-equal protel-block-name ""))
	(insert " " protel-block-name))
    (insert ";")
    (end-of-line 0)
    (protel-tab)))

 (defun protel-table ()
  "Insert array type definition, prompting for component type,
leaving the user to type in the index subtypes."
  (interactive)
  (insert "TABLE ")
  (insert ";")
  (backward-char))

(defun protel-unprotds ()
  "Insert unprotectds() and protectds()"
  (interactive)
  (insert "unprotectds();")
  (protel-newline)
  (protel-newline)
  (insert "protectds();")
  (end-of-line 0)
  (protel-tab))

(defun protel-cond-unprotds ()
  "Insert cond_unprotectds() and protectds()"
  (interactive)
  (let (pos)
    (insert "IF ^cond_unprotectds()")
    (protel-newline)
    (insert "THEN\n")
    (protel-tab)
    (protel-tab)
    (insert protel-comment-prefix "Dump & Restore in progress...")
    (protel-newline)
    (insert "Take appropriate action (e.g., swerror) and exit.\n")
    (protel-tab)
    (setq pos (point))
    (protel-newline)
    (protel-untab)
    (insert "ENDIF;\n\n")
    (protel-tab)
    (insert protel-comment-prefix "Your success path goes here...\n\n")
    (protel-tab)
    (insert "protectds();")
    (newline)
    (goto-char pos)))

(defun protel-write-protds ()
  "Insert a WRITE_PROTECTED_STORE."
  (interactive)
  (insert "write_protected_store (value_to_write, target);\n"))

(defun protel-bind ()
  "Insert a skeleton bind declaration and help fill it."
  (interactive)
  (let ((bind-indent (current-column)))
    (insert "BIND ")
    (protel-get-list-return ",$")
    (insert ";")
    (newline)
    (indent-to bind-indent)))

(defun protel-with ()
  "initiate a WITH declaration, and read in the contents."
  (interactive)
  (insert "WITH ")
  (protel-get-list-return ",$")
  (insert " ;")
  (backward-char 2)
  (message "insert type definition."))

(defun protel-forward ()
  "Define the proc as forward in stead of the open block."
  (interactive)
  (previous-line 2)
  (end-of-line)
  (kill-line 4)
  (insert " FORWARD;")
  (newline)
  (indent-to 0))

(defun protel-select ()
  "Insert skeleton SELECT statment, prompting for the <expression>."
  (interactive)
  (insert "SELECT ")
  (let ((protel-select-name (read-string "Identifier: ")))
    (insert protel-select-name " IN")
    ;;(protel-newline)
    ;;(insert "IN")
    (protel-newline)
    (protel-tab)
    (insert "{" (read-string "Clause Identifier: ") "}:")
    (protel-newline)
    (protel-newline)
    (insert "OUT")
    (protel-newline)
    (protel-tab)
    (backward-delete-char-untabify protel-indent ())
    (backward-delete-char-untabify protel-indent ())
    (insert "ENDSELECT; " protel-comment-prefix protel-select-name)
    (end-of-line -1)
    (protel-tab)))

(defun protel-case ()
  "Insert skeleton CASE statment, prompting for the <expression>."
  (interactive)
  (insert "CASE ")
  (let ((protel-case-name (read-string "Identifier: ")))
    (insert protel-case-name " IN")
;    (protel-newline)
;    (insert "IN")
    (protel-newline)
    (protel-tab)
    (insert "{" (read-string "Clause Identifier: ") "}:")
    (protel-newline)
    (protel-newline)
    (insert "OUT")
    (protel-newline)
    (protel-tab)
    (backward-delete-char-untabify protel-indent ())
    (backward-delete-char-untabify protel-indent ())
    (insert "ENDCASE; " protel-comment-prefix protel-case-name)
    (end-of-line -1)
    (protel-tab)))

(defun protel-inspect ()
  "Insert skeleton INSPECT statment, prompting for the <expression>."
  (interactive)
  (insert "INSPECT ")
  (let ((protel-inspect-name (read-string "Object to inspect? : ")))
    (insert protel-inspect-name "@ IN")
    (protel-newline)
    (protel-tab)
    (insert "{" (read-string "Class? : "))
    (insert " => " (read-string "New local object? :") "}:")
    (protel-newline)
    (protel-newline)
    (insert "OUT")
    (protel-newline)
    (protel-tab)
    (backward-delete-char-untabify protel-indent ())
    (backward-delete-char-untabify protel-indent ())
    (insert "ENDINSPECT; " protel-comment-prefix protel-inspect-name)
    (end-of-line -1)
    (protel-tab)))

(defun protel-ovly ()
  "Insert skeleton OVLY statment, prompting for the selector expression."
  (interactive)
  (insert "OVLY ")
  (insert (read-string "selector expression: "))
  (protel-newline)
  (protel-newline)
  (insert "ENDOVLY;")
  (end-of-line 0)
  (protel-tab))

(defun protel-get-list-return (mask)
  "Read from user a procedure or function argument list.
Add parens unless arguments absent, and insert into buffer.
Individual arguments are arranged vertically if entered one-at-a-time.
Arguments ending with MASK presumed single and stacked."
  (let ((case-fold-search t)
        (protel-arg-indent (current-column)) first-line
	(protel-args (read-string "[arguments]: ")))
    (setq first-line protel-args)
    (if (not(string-equal protel-args ""))
      (progn
	(while (string-match mask protel-args)
	  (insert protel-args)
	  (protel-newline)
	  (indent-to protel-arg-indent)
	  (setq protel-args (read-string "next argument: ")))
	(insert protel-args )))
    (setq first-line first-line)))


(defun protel-declare-block ()
  "Insert a block with a declare part and indent for the 1st declaration."
  (interactive)
  (let ((block-indent (current-column))
	(protel-block-name (read-string "[block label]: "))
        (first-line "t"))
    (insert "DCL ")
    (cond
      ( (not (string-equal protel-block-name ""))
	(beginning-of-line)
	(protel-newline)
	(insert protel-block-name ":")
	(end-of-line 2)))
    (setq first-line (protel-get-list-return ",$"))
    (if (not (string-equal first-line ""))
      (progn
        (insert ";")
        (newline)
        (indent-to block-indent)))))

(defun protel-do-loop ()
  "Build a skeleton DO-loop statement, prompting for the loop parameters."
  (interactive)
  (let ((protel-loop-name (read-string "Loop name: "))
        )
    (if (string-equal protel-loop-name "")
        (setq protel-loop-name "default_loop_name"))
    (insert protel-loop-name ":")
    (protel-newline)
    (insert "DO")
    (protel-newline)
    (protel-newline)
    (protel-tab)
    (insert "IF exit_condition")
    (protel-newline)
    (insert "THEN")
    (protel-newline)
    (protel-tab)
    (insert "EXIT " protel-loop-name)
    (protel-newline)
    (protel-untab)
    (insert "ENDIF;")
    (protel-newline)
    (protel-untab)
    (insert "ENDDO")
    (insert " " protel-loop-name ";")
    (end-of-line -4)
    (protel-tab)
  ))

(defun protel-else ()
  "Insert ELSE keyword and indent for next line."
  (interactive)
  (protel-newline)
  (insert "ELSE")
  (protel-newline)
  (protel-tab))

(defun protel-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert protel-comment-prefix "\n" protel-comment-prefix  "Title: \t")
  (insert (read-string "Title: "))
  (insert "\n" protel-comment-prefix "Created:\t" (current-time-string))
  (insert "\n" protel-comment-prefix " Author: \t" (user-full-name))
  (insert "\n" protel-comment-prefix))

(defun protel-for-loop ()
  "Build a skeleton FOR-loop statement, prompting for the loop parameters.
UP TO or DOWN TO is automatically inserted based on the from/to values."
  (interactive)
  (insert "FOR ")
  (let* ((protel-loop-name (read-string "[loop name]: "))
	 (protel-loop-is-named (not (string-equal protel-loop-name "")))
	 (range-value "r")
	 (index-value "i")
         (up-or-down-string " DOWN TO ")
         start-index end-index start-int end-int by)
    (if protel-loop-is-named
	(progn
	  (beginning-of-line)
	  (protel-newline)
	  (insert protel-loop-name ":")
	  (end-of-line 2)))
    (setq index-value (read-string "index: "))
    (insert index-value " ")
    (insert "FROM ")
    (setq start-index (read-string "start: "))
    (if (string= start-index "")
        (setq end-index "")
      (setq start-int (string-to-number start-index))
      (if (= 0 start-int)
          ;; the if below is to assign a big value if a string is entered
          (if (/= ?0 (aref start-index 0)) (setq start-int 999999)))
      (setq end-index (read-string "end: "))
      (if (string= end-index "")
          () ; user hits return, nothing to be done here
        (setq end-int (string-to-number end-index))
        (if (= 0 end-int)
            ;; the if below is to assign a big value if a string is entered
            (if (/= ?0 (aref end-index 0)) (setq end-int 999999)))
        (if (< start-int end-int) (setq up-or-down-string " UP TO "))
      ))
    (insert start-index up-or-down-string end-index)
    (setq by (read-string "by: "))
    (if (not (string-equal by ""))
	(insert " BY " by))
    (setq range-value (read-string "while: "))
    (if (not (string-equal range-value ""))
        (progn
          (protel-newline)
          (insert "WHILE " range-value)))
    (insert " DO")
    (protel-newline)
    (protel-newline)
    (insert "ENDDO")
    (if protel-loop-is-named (insert " " protel-loop-name))
    (insert "; " protel-comment-prefix index-value
            " FROM " start-index up-or-down-string end-index)
    (if (not (string-equal by ""))
	(insert " BY " by))
    (if (not (string-equal range-value ""))
	(insert " WHILE " range-value ))
    (end-of-line 0)
    (protel-tab)))

(defun protel-if-then ()
  "Insert skeleton IF statment, prompting for a boolean-expression. If this
expression ends with either '|' or '&', the user is prompted again for
another expression and so on. The last expression must end with a RET."
  (interactive)
  (let ((protel-if-indent (current-column))
        first-line)
    (insert "IF ")
    (setq first-line (protel-get-list-return "[|&]$"))
    (newline)
    (indent-to protel-if-indent)
    (insert "THEN")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (newline)
    (indent-to protel-if-indent)
    (insert "ENDIF;")
    (if (string= "" first-line)
        () ; do nothing
      (insert " " protel-comment-prefix first-line))
    (end-of-line 0)
    ))

(defun protel-if-then-else ()
  "Insert skeleton IF statment including ELSE, prompting for a boolean
expression. If this expression ends with either '|' or '&', the user is
prompted again for another expression and so on. The last expression must
end with a RET."
  (interactive)
  (let ((protel-if-indent (current-column))
        first-line)
    (insert "IF ")
    (setq first-line (protel-get-list-return "[|&]$"))
    (newline)
    (indent-to protel-if-indent)
    (insert "THEN")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (newline)
    (indent-to protel-if-indent)
    (insert "ELSE")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (newline)
    (indent-to protel-if-indent)
    (insert "ENDIF;")
    (if (string= "" first-line)
        () ; do nothing
      (insert " " protel-comment-prefix first-line))
    (end-of-line -2)
    ))

(defun protel-if-then-elseif ()
  "Insert skeleton IF statment including ELSEIF/ELSE, prompting for a boolean
expression. If this expression ends with either '|' or '&', the user is
prompted again for another expression and so on. The last expression must
end with a RET."
  (interactive)
  (let ((protel-if-indent (current-column))
        first-line first-then)
    (insert "IF ")
    (setq first-line (protel-get-list-return "[|&]$"))
    (newline)
    (indent-to protel-if-indent)
    (insert "THEN")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (setq first-then (point))
    (newline)
    (indent-to protel-if-indent)
    (insert "ELSEIF ")
    (protel-get-list-return "[|&]$")
    (newline)
    (indent-to protel-if-indent)
    (insert "THEN")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (newline)
    (indent-to protel-if-indent)
    (insert "ELSE")
    (newline)
    (indent-to (+ protel-if-indent protel-indent))
    (newline)
    (indent-to protel-if-indent)
    (insert "ENDIF;")
    (if (string= "" first-line)
        () ; do nothing
      (insert " " protel-comment-prefix first-line))
    (goto-char first-then)
    ))

(defun protel-function-spec ()
  "Insert a function or procedure specification. Prompts for function name and
arguments. The user is reprompted for more arguments if the previous one ends
with a ','. The argument list is then automatically aligned."
  (interactive)
  (let ((case-fold-search t)
        (function-indent (current-column))
        (protel-func-name (read-string "Function or procedure name: "))
        proc-paren-end start-args empty-arg returns empty-ret
        arg-indent first-indent second-indent)
    (insert "DCL ")
    (insert protel-func-name)
    (insert " PROC (")
    (setq proc-paren-end (point))
    (protel-newline)
    (insert "%%  " (make-string (length protel-func-name) ?-))
    (newline)
    (setq start-args (point))
    (indent-to function-indent)
    (indent-relative nil)
    (setq arg-indent (current-column))
    (setq empty-arg (string= (protel-get-list-return ",$") ""))
    (setq returns (read-string "returns result type (optional): "))
    (setq empty-ret (string= returns ""))

    (if empty-arg
        (progn
          (kill-line 0)  ; undo the indent
          (goto-char proc-paren-end)
          (insert ") ")
          (if (not empty-ret)
              (insert "RETURN " returns " "))
          (insert "IS")
          (end-of-line 3)
          )

      ;; argument list is not empty, so try to align them
      (let ((arg-max-length 0)
            arg-length search-limit)
        (save-restriction
          (narrow-to-region start-args (point))
          (setq first-indent arg-indent)

          ;; compute the first and second level of indentation.
          ;; - arg-indent:    indentation of the overal argument list.
          ;; - first-indent:  indentation of the argument, is the same as
          ;;                arg-indent if no REF or UPDATES specified.
          ;; - second-indent: indentation of the argument type.
          (goto-char (point-min))
          (while (not (eobp))
            (setq search-limit (save-excursion (end-of-line) (point)))
            (back-to-indentation)
            (if (re-search-forward "REF[ \t]+\\|UPDATES[ \t]+" search-limit t)
                (setq first-indent (+ arg-indent 8)))
            (setq arg-length (length (buffer-substring
                                      (point)
                                      (save-excursion
                                        (re-search-forward "[ \t]+"
                                                           search-limit t)
                                        (point)))))
            (if (> arg-length arg-max-length)
                (setq arg-max-length arg-length))
            (forward-line 1))
          (setq second-indent (+ first-indent arg-max-length))

          ;; Indent/align the argument list
          (goto-char (point-min))
          (while (not (eobp))
            (back-to-indentation)
            (kill-line 0)  ; undo the indent
            (indent-to arg-indent)
            (re-search-forward "REF[ \t]+\\|UPDATES[ \t]+"
                               (save-excursion (end-of-line) (point)) t)
            (indent-to first-indent)
            (re-search-forward "[ \t]+"
                               (save-excursion (end-of-line) (point)) t)
            (indent-to second-indent)
            (forward-line 1))
        ))

      (insert " )")
      (if empty-ret
          (insert " ")
        (newline)
        (indent-to arg-indent)
        (insert "RETURNS")
        (indent-to second-indent)
        (insert returns " "))
      (insert "IS")
      (newline))

    (indent-to function-indent)
    (insert "BLOCK")
    (protel-newline)
    (protel-newline)
    (insert "ENDBLOCK " protel-func-name ";")
    (end-of-line 0)
    (protel-tab)
  ))

(defun protel-type ()
  "Start insertion of a TYPE declaration, prompting for the type name."
  (interactive)
  (insert "TYPE " (read-string "type name: "))
  (insert " ;")
  (backward-char 1)
  (message "insert type definition."))

(defun protel-function-header ()
  "Insert the header template for function/procedure definition."
  (interactive)
  (let ((case-fold-search t)
        (protel-func-name (read-string "Function/Procedure name: "))
        (comment-prefix (dired-string-replace-match " +$"
                                                    protel-comment-prefix
                                                    "" t)
                        )
        start end)
    (beginning-of-line)
    (insert comment-prefix)
    (insert (make-string 72 ?%) "\n")
    (insert comment-prefix "\n")
    (setq start (point))
    (insert "Name: " (upcase protel-func-name) "\n")
    (insert "
Description:
  <clearly describe what this proc does, usage notes, limitations...>

Parameters:
  <parm_1>     <describe this parm, what it does, how it is used,...
               not just this parm is of type... indent the text as
               shown here if span more than one line>

  <parm_2>     <describe this parm as above>
  ...

Returns:
  <ret_code>   <clearly explain all possible return codes and
               circumstances where these return codes are returned.
               Indent the text as shown here if span more than 1 line>\n")
    (save-excursion (setq end (progn (beginning-of-line 0)
                                     (point))))
    (string-rectangle start end protel-comment-prefix)
    (insert comment-prefix "\n")
    (insert comment-prefix)
    (insert (make-string 72 ?%) "\n\n")
  ))

(defun protel-until ()
  "Insert an OVER-loop."
  (interactive)
  (let (index)
    (insert "OVER ")
    (setq index (read-string "index: "))
    (insert index)
    (protel-newline)
    (insert "DO")
    (protel-newline)
    (protel-newline)
    (insert "ENDDO; " protel-comment-prefix "OVER " index)
    (end-of-line 0)
    (protel-tab)))

(defun protel-var ()
  "Obsolete - Do not use."
  (interactive)
  (insert "DCL "))

(defun protel-lock ()
  "Insert lock() and unlock()."
  (interactive)
  (insert "lock();")
  (protel-newline)
  (protel-newline)
  (insert "unlock();")
  (end-of-line 0)
  (protel-tab))

(defun protel-unpreemptable ()
  "Insert setunpreemptable() and setpreemptable()."
  (interactive)
  (insert "setunpreemptable();")
  (protel-newline)
  (protel-newline)
  (insert "setpreemptable();")
  (end-of-line 0)
  (protel-tab))

(defun protel-mutexon ()
  "Insert mutexon() and mutexoff()."
  (interactive)
  (insert "mutexon();")
  (protel-newline)
  (protel-newline)
  (insert "mutexoff();")
  (end-of-line 0)
  (protel-tab))

(defun protel-while-loop ()
  "Insert a skeleton WHILE-loop statement, prompting for the loop parameters."
  (interactive)
  (let ((while-indent (current-column)))
    (insert "WHILE ")
    (let* ((protel-loop-name (read-string "[loop name]: "))
	   (protel-loop-is-named (not (string-equal protel-loop-name "")))
           first-exp-line)
      (if protel-loop-is-named
	  (progn
	    (beginning-of-line)
	    (protel-newline)
	    (insert protel-loop-name ":")
	    (end-of-line 2)))
      (setq first-exp-line (protel-get-list-return "[|&]$"))
      (insert " DO")
      (newline)
      (indent-to (+ while-indent protel-indent))
      (newline)
      (indent-to while-indent)
      (insert "ENDDO")
      (if protel-loop-is-named (insert " " protel-loop-name))
      (insert "; " protel-comment-prefix first-exp-line))
    (end-of-line 0)
    ))

(defun protel-method ()
  "Insert a skeleton of METHOD implementation."
  (interactive)
  (let ((class-name (read-string "IN which class: "))
        (method-name (read-string "Method name: "))
        (method-indent (current-column))
        final-pos)
     (insert "IN  " class-name)
     (protel-newline)
     (insert "%%  " (make-string (length class-name) ?=))
     (newline)
     (indent-to method-indent)
     (insert "DCL " method-name " METHOD IS ")
     (protel-newline)
     (insert "%%  " (make-string (length method-name) ?-))
     (newline)
     (indent-to method-indent)
     (insert "BLOCK")
     (protel-newline)
     (protel-newline)
     (insert "ENDBLOCK " method-name ";")
     (newline)
     (end-of-line -1)
     (protel-tab)
     (setq final-pos (point))
     (insert "...")
     (newline 2)
     (indent-to method-indent)
     (protel-tab)
     (insert "IF unexpected_condition THEN")
     (protel-newline)
     (protel-tab)
     (insert "DCL my_exception my_exception_class;")
     (protel-newline)
     (insert "RAISE my_exception;")
     (protel-newline)
     (protel-untab)
     (insert "ENDIF;")
     (newline 2)
     (indent-to method-indent)
     (insert "HANDLE")
     (protel-newline)
     (protel-tab)
     (insert "{my_exception_class}:")
     (protel-newline)
     (protel-tab)
     (insert "%% exception handling code...")
     (newline 2)
     (indent-to method-indent)
     (protel-tab)
     (insert "RETURN;")
     (protel-newline)
     (insert "RETRY; or")
     (protel-newline)
     (insert "RAISE other_exception;")
     (newline)
     (goto-char final-pos))
  )

(defun protel-insert-li-record (record)
  "Find the % EDITION code line and insert LI record before this line. If the
% EDITION code line is not found, the LI record is inserted at point."
  (setq record (upcase record))
  (let ((case-fold-search t)
        found)
    (save-excursion
      (goto-char (point-min))
      (setq found (re-search-forward "^% EDITION [A-Z][A-Z][0-9][0-9] ("
                                     nil 'move))
      (if found
          (progn
            (beginning-of-line)
            (insert "$LI " record)
            (newline))
        ))
    (if found
        ()   ; already inserted
      (insert "$LI " record)
      (newline))
  ))

(defun protel-li-modentry-restart ()
  "Insert $LI MODENTRY RESTART."
  (interactive)
  (protel-insert-li-record "MODENTRY RESTART"))

(defun protel-li-modentry-iplunload ()
  "Insert $LI MODENTRY IPLUNLOAD."
  (interactive)
  (protel-insert-li-record "MODENTRY IPLUNLOAD"))

(defun protel-li-modentry-ipl ()
  "Insert $LI MODENTRY IPL."
  (interactive)
  (protel-insert-li-record "MODENTRY IPL"))

(defun protel-li-modentry-permproc ()
  "Insert $LI MODENTRY PERMPROC."
  (interactive)
  (protel-insert-li-record "MODENTRY PERMPROC"))

(defun protel-li-modentry-initproc ()
  "Insert $LI MODENTRY INITPROC."
  (interactive)
  (protel-insert-li-record "MODENTRY INITPROC"))

(defun protel-li-modentry-ppvinit ()
  "Insert $LI MODENTRY PPVINIT."
  (interactive)
  (protel-insert-li-record "MODENTRY PPVINIT"))

(defun protel-li-modstack ()
  "Insert $LI MODSTACK."
  (interactive)
  (let ((stack-size (read-string "Stack size: " "1500")))
    (protel-insert-li-record (concat "MODSTACK " stack-size))
  ))

(defun protel-li-modincr ()
  "Insert $LI MODINCR."
  (interactive)
  (let ((module (read-string "From which module: " "CIPROC")))
    (protel-insert-li-record (concat "MODINCR " module))
  ))

(defun protel-li-modalias ()
  "Insert $LI MODALIAS."
  (interactive)
  (let ((alias (read-string "Alias: ")))
    (protel-insert-li-record (concat "MODALIAS " alias))
  ))

(defun protel-li-needsinit ()
  "Insert $LI NEEDSINIT."
  (interactive)
  (let ((components (read-string "Components (e.g., L1 L2... SELF R1 R2...): ")))
    (protel-insert-li-record (concat "NEEDSINIT " components))
  ))

(defun protel-li-inits ()
  "Insert $LI INITS."
  (interactive)
  (let ((inits-info (read-string "Module/Component: ")))
    (protel-insert-li-record (concat "INITS " inits-info " % ..."))
  ))

(defun protel-li-initafter ()
  "Insert $LI INITAFTER."
  (interactive)
  (let* ((module (read-string "Module: "))
         (alias (if (y-or-n-p "Is it an alias: ") " ALIAS" ""))
         )
    (protel-insert-li-record (concat "INITAFTER " module alias " % ..."))
  ))

(defun protel-li-mustexist ()
  "Insert $LI MUSTEXIST."
  (interactive)
  (let* ((module (read-string "Module: "))
         (alias (if (y-or-n-p "Is it an alias: ") " ALIAS" ""))
         )
    (protel-insert-li-record (concat "MUSTEXIST " module alias " % ..."))
  ))

(defun protel-begin-comment ()
  "Insert `protel-comment-prefix' to start a comment block or an inline comment."
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert protel-comment-prefix))

(defun protel-end-comment ()
  "Insert `protel-comment-prefix' to close the comment line."
  (interactive)
  (if (not (bolp))
      (indent-to end-comment-column))
  (insert protel-comment-prefix))


;------------------------ Large header stuff --------------------------

(defun get-big-letters-file ()
  "Locate and fetch the big-letters file on either the load-path or on the
directory pointed to by the variable `protel-tool-dir'."
  (let ((file (locate-library "big-letters" t))
        )
    ;; for win emacs
    ;; (if (null file) (concat protel-tool-dir "big-letters"))
    (if (null file) ())

    (if (and file (file-readable-p file))
        (find-file-noselect file)
      (error "big-letters file is required and is not found on load-path."))
  ))

(defun get-box()
  "Gets the comment box"
  (let (rect)
    (save-excursion
      (if (null (get-buffer "big-letters")) (get-big-letters-file))
      (set-buffer "big-letters")
      (goto-char (point-min))
      (let ((beg (point)))
	(end-of-line 11)
	(setq rect (extract-rectangle beg (point)))))
    (beginning-of-line)
    (open-line 10)
    (insert-rectangle rect)
    (insert "\n"))
  )

(defun get-letter (arg)
  "Gets a large letter."
  (interactive "c")
  (let ((case-fold-search t)
        rect beg)
    (save-excursion
      (if (null (get-buffer "big-letters")) (get-big-letters-file))
      (set-buffer "big-letters")
      (goto-char (point-min))
      (search-forward (char-to-string arg))
      (move-to-column 0)
      (let ((beg (point)))
	(forward-line 4)
	(forward-char 5)
	(setq rect (extract-rectangle beg (point)))))
    (if (eq arg 32)
	(forward-char 7)
      (let ((start-col (current-column))
            )
	(setq beg (point))
	(forward-line 4)
	(move-to-column (+ start-col 5))
	(delete-rectangle beg (point))
	(forward-line -4)
        (move-to-column start-col)
	(insert-rectangle rect)
	(forward-line -4)
	(move-to-column (+ start-col 7))
      ))
  ))

(defun get-big-string (arg)
  "Gets a large string."
  (interactive "s")
  (let (len)
    (setq len (min (length arg) 10))
    (while (> len 0)
      (get-letter (string-to-char arg))
      (setq arg (substring arg 1))
      (setq len (- len 1)))
    (kill-buffer "big-letters"))
  )

(defun protel-big-header (name)
  "Gets a large string and centers it. The big-letters file is required for
this function and should be placed either on the load-path or the directory
pointed to by the variable `protel-tool-dir'."
  (interactive "sEnter header name: ")
  (let (len beg)
    (setq len (min (length name) 10))
    (setq beg (+ (/(- 71 (* 7 len)) 2) 3))
    (get-box)
    (forward-line -8)
    (move-to-column beg)
    (get-big-string name)
    (forward-line 8)
  ))

(defun protel-module-header ()
  "Insert a standard module header. The big-letters file is required for
this function and should be placed either on the load-path or the directory
pointed to by the variable `protel-tool-dir'."
  (interactive)
  (let ((name (file-name-sans-extension (buffer-name)))
        )
    (protel-big-header name)
    (insert "%%\n")
    (insert "%% Module: " (upcase name) "\n")
    (insert "%%
%% Description:
%%   <describe the functions provided by this module>
%%
%% Usage Notes:
%%   <any relevant info to use this module>
%%
%% Restriction/Limitations:
%%   <e.g., the procs in this module should not be called within CallP>
%%
%% Sections:
%%   <include the sections diagram here>
%%
%%   <SECTION1>:  <Brief description of functionality, indent the text as
%%                shown here if span more than 1 line>
%%
%%   .....
%%
%%   <SECTIONn>:  <Brief description of functionality as above>
%%
%% Other Useful Info:
%%   <e.g., related DDOC IDs>
%%
")
    (insert (make-string 75 ?%) "\n")
    (insert (make-string 75 ?%) "\n" "$EJECT\n\n")
    )
  )

(defun protel-section-header ()
  "Insert a standard section header. The big-letters file is required for
this function and should be placed either on the load-path or the directory
pointed to by the variable `protel-tool-dir'."
  (interactive)
  (let ((name (file-name-sans-extension (buffer-name)))
        )
    (protel-big-header name)
    (insert "%%\n")
    (insert "%% Section: " (upcase name) "\n")
    (insert "%%
%% Description:
%%   <describe the functions provided by this module>
%%
%% Restriction/Limitations:
%%   <e.g., the procs in this section should not be called within CallP>
%%
%% Other Useful Info:
%%   <e.g., related sections where the code is inspired from, docs,...>
%%
")
    (insert (make-string 75 ?%) "\n")
    (insert (make-string 75 ?%) "\n" "$EJECT\n\n")
    )
  )


;---------------------- SYM stuff (DMS library) ---------------------------
;; delete for win

;---------------------------- MSym stuff ---------------------------
;; MSym stuff is deleted for win

;----------------------------------------------------------------------

(provide 'protel)

;------------------------ Change Log ---------------------------------
; 
; Revision 2.7.2 97/09/29 16:03:54 hlnguyen
;
; -- The following changes were submitted by Mike Garvin --
;
; o Msym can lock up when the DISPLAY is set.  This is because when 
;   the DISPLAY is set, the CXE will try to connect to PLS via the BMS.
;   This not very reliable at the moment.  Since no one currently needs 
;   to display anything from within MSym (via the 'eval' or 'shell' 
;   commands that were added in Mentor 5.4.0), the quick fix was to 
;   invoken msym via 'env DISPLAY=', to ensure MSym does not see a 
;   DISPLAY variable.
;
; o I checked the completeness of REF_CLASS and REF_ATTRIBUTE.  They 
;   appear to be complete as for compiler version p2bw02.  If you find
;   REF_CLASS or REF_ATTRIBUTE key words that are not being highlighed 
;   please let me know.
; 
; o Compiled GNUEmacs 20.2 and placed a new package on the Homebrew page.
;   This version of Protel Mode does not work (to my knowledge) with 
;   GNUEmacs 19.34.  In addition development of Protel Mode will now move
;   forward with GNUEmacs 20.2.  All further development of Protel Mode
;   for GNUEmacs 19.34 (by Mentor) is frozen.  
;
; -- The following changes were submitted by Liem Nguyen --
;
; The following changes to support GNU Emacs 20 were submitted by Liem
; Nguyen.
; 
; - Maintainer is set to mentor.helpline@nortel.ca.
; 
; - PLS session hanging problem in Emacs 20 is fixed.
; 
; - Font Lock related:
; 
;   o choose-color package is no longer used (no support).
;   o font-lock-other-type-face, font-lock-emphasized-face, and
;     font-lock-other-emphasized-face are removed.
;   o 2 new font-lock faces and a new face (font-lock-label-face) are used
;     instead. See help of the variable protel-font-lock-highlight-level for
;     more info.
;   o The protel-highlight-code variable is removed given the new
;     fontification activation in both Emacsen (available before X/Emacs-20).
;   o The manual fontification is removed since X/Emacs doesn't support it
;     anymore.
; 
; - Motion and Collapse commands:
; 
;   o The outline-minor-mode is used instead of implementing these commands
;     inside protel.el. This simplifies considerably the maintenance and
;     ensures the upward compatibility with new Emacs releases.
; 
; - Support of CUSTOM package: variables can be customized in a
;   user-friendly manner.
; 
; The following changes were submitted by Isaac Lin and adapted by Liem
; Nguyen.
; 
; - Limited support of functions menu for XEmacs.
; 
; - Make use of adaptive-fill-mode for code comment writing (automatic
;   prefix).
; 
; ------------------------------
; 
; Also, someone reported that some REF_CLASSREF_CLASS and REF_ATTRIBUTE are not
; highlighted.  After investigaion, I've found that these are not
; documented in the MSYM online help and the man page.  Where can I get
; a complete list of these classes/attributes so that I can add them to
; the protel.el.  These changes are very easy and small; I suggest that
; we incorporate these before releasing 2.8.  (See Mikes comments above)
; 
; Concerning Isaac's filladapt mode, here is my suggestion.  Keep these
; additions in a separate file (e.g., my-protel-adds.el) and load it
; right after protel.el instead of keeping a separate/temporary
; protel.el version.  Anything loaded after protel.el will override
; protel.el; this is a "nice" feature of LISP.  For example:
; 
; (require 'protel)
; (require 'my-protel-adds)
; 
; Hope that helps,
; Liem
;
; $Log: protel.el,v $
; Revision 1.7  1998/04/28 21:15:29  mgarvin
; clean up
;
; Revision 1.5  1998/04/26 21:38:07  mgarvin
; clean up!
;
; Revision 1.4  1998/04/26 05:47:58  mgarvin
; added APMS logging!
;
; Revision 1.2  1998/04/13 19:21:22  mgarvin
; clean up of RCS id in the Mentor CVS repository
;
; Revision 1.1  1998/04/13 19:15:59  mgarvin
; clean up for first cut
;
; Revision 2.7  97/09/29  16:03:54  16:03:54  hlnguyen (Liem Nguyen)
; 
; The following changes were submitted by Liem Nguyen.
; 
; BUG FIXES
; 
; - When choose-color package is not available, there is an error message
;   printed for GNU Emacs complaining a font-lock face is not found (XEmacs
;   is OK though).  This bug is fixed by using default faces available with
;   Emacs on startup (underline and bold) instead of those created by
;   font-lock package.
; 
; Revision 2.6  97/09/26  19:13:23  19:13:23  hlnguyen (Liem Nguyen)
; 
; The following changes were submitted by Liem Nguyen, Kevin Blake, and Mark
; Flacy.
; 
; ENHANCEMENTS
; 
; - Re-write the protel-keywords-regexp and the outline-regexp to optimize
;   the search.
; 
; - Initial Support of Msym in Mentor 5.3 beta release.
; 
; - Other Msym enhancements: PLS decoupling, more color and flexible
;   show/hide ability in MSYM mode.
; 
; - font-lock-reference-face is now used for $LI records and compiler
;   directives in both XEmacs-19.14 and GNU Emacs.
; 
; - Fontification in PROTEL buffers is greatly enhanced. See
;   protel-font-lock-highlight-level.
; 
; - Binding protel-untab to a backspace code can now be interpreted by
;   delbackspace.el. In XEmacs, backspace and delete keys generate a same
;   code. (A hook in delbackspace.el is still needed to properly catch
;   protel-untab.)
; 
; - Add new function protel-beautify-buffer which invokes the beautify
;   command by default on the current buffer.
; 
; - Add new function protel-show-occurrences which show all occurrences in a
;   separate buffer (which can then be located using mouse-2).
; 
; BUG FIXES
; 
; - Fontification in Protel buffers can now be activated by:
; 
;   (add-hook 'protel-mode-hook 'turn-on-font-lock)
; 
; - pls-command-ok is enhanced.
; 
; - waiting-for-pls is now set to 'user-input during new PLS session
;   startup. 'user-input is the replacement of the old value 'interactive.
;   The user can now type in more info as requested by the 1st PLS command.
; 
; - The env variables PAGER, EDITOR_FDOCID, EDITOR_PRSID are set
;   automatically in any PLS session. See also pls-pager-cmd.
; 
; - Wrong buffer selection when PLS and MSYM are started is
;   fixed. (pls-command-done and msym-command-done only force the current
;   buffer if different from the one set before entering the handler loop.)
; 
; - PLS and MSYM command buffers now have their own syntax table avoiding
;   interaction with other modes.
; 
; - protel-untab now deletes the marked region only when transient-mark-mode
;   is turned on. This ensures that implicitly marked region is not deleted
;   (transient-mark-mode is off).
; 
; Revision 2.5  96/12/17  15:28:38  15:28:38  hlnguyen (Liem Nguyen)
;
; The following changes were submitted by Liem Nguyen.
;
; ENHANCEMENTS
;
; - pls-command-ok is enhanced to catch more error messages for re-use.
;
; - pls-quit now kills the current buffer if it is a PLS buffer. Otherwise,
;   DMSPL is assumed as before.
;
; - Initial support of MSYM in FIBER library. MSYM prompt search is also
;   fixed.
;
; BUG FIXES
;
; - go-to-symbol-in-module-done is fixed to properly locate PRIVATE
;   variables in a per-process section.
;
; - Send Bug Report now works in XEmacs.
;
; - The fix to avoid fontification twice done in previous release now works
;   also for XEmacs.
;
; - outline-regexp to match CASE/SELECT/INSPECT clauses is fixed to avoid
;   the regexp stack overflow error.
;
; - case-fold-search is now forced to be t in PROTEL, MSYM modes and in
;   PLS/MSYM command buffers.
;
; - Obsolete codes are cleaned up.
;
; The following changes were originally submitted by Reuven Della-Torre
; (trd@tmx100.telrad.co.il) and Roi Sasson (roi@tmx100.telrad.co.il), then
; modified and adapted by Liem Nguyen.
;
; ENHANCEMENTS
;
; - A default context can now be set automatically when MSYM session is
;   started. The default context pattern is set (and can be modified by the
;   user) in the variable msym-default-context-regexp.
;
; - Add new function (protel-lister) to generate the listing for the section
;   in the current buffer.
;
; - X-ref list produced by SYM now contains also the symbol codes.
;
; - A confirmation is prompted if the PLS or MSYM command buffer is being
;   killed. This behaviour is controlled by the new variable
;   pls-msym-buffers-quiet-kill; it is set to nil by default (i.e.,
;   prompted).
;
; BUG FIXES
;
; - mod-already-open is fixed to open the correct buffer.
;
; Revision 2.4  96/11/25  12:38:55  12:38:55  hlnguyen (Liem Nguyen)
;
; ENHANCEMENTS
;
; - Convert to use CL package to simplify and speed up the code.
;
; - Change the PLS handler stack to be Fist In Last Out (FILO) to make the
;   code more robust and easy to maintain.
;
; - Update the list of PROTEL keywords.
;
; - Enhance PROTEL TAB to look for the indentation of the 1st previous
;   non-blank line.
;
; - Add Protected Store sub-menu.
;
; - Add new variable msym-auto-start to control the starting of MSYM.
;
; - Add new function (protel-report-bug) to easily send bug report.
;
; - Support FIBER library (initial phase).
;
; BUG FIXES
;
; - Getting the big-letters file (get-big-letters-file) now works properly
;   to locate the file on the load-path.
;
; - When global-font-lock-mode is ON and the custom handler is used, the
;   section fetched into Emacs is no longer fontified twice.
;
; - Jumping to CASE/SELECT clauses which are split in several lines works
;   properly now.
;
; - The MSYM process output filter is now correctly set for XEmacs to use
;   comint package since the variable using-gnuemacs is no longer set for
;   XEmacs. The PLS process output filter was fixed in v2.3.
;
; - Remove old PLS/MSYM output filters (pls/msym-output-filter); they are no
;   longer used and add some overhead. New (currently used) PLS/MSYM output
;   filters (pls/msym-comint-output-filter) are now made local to each buffer
;   (PLS or MSYM.)
;
; - Section name/issue output by the compiler (DMSPL) is now parsed properly.
;
; Revision 2.3  96/10/18  18:55:25  18:55:25  hlnguyen (Liem Nguyen)
;
; The following changes were submitted by Liem Nguyen:
;
; - Some required packages are not loaded at top level for 2 reasons: 1)
;   they are only required in some functions -- efficient use of memory; 2)
;   to accommodate some hilit19 users -- preventing font-lock to load
;   unconditionally (requested by Kevin Blake.)
;
; - Fontification is enhanced to:
;
;   * avoid interaction with other modes
;
;   * support the "Emacs" way to turn ON font-lock:
;
;     (add-hook 'protel-mode-hook 'turn-on-font-lock)
;
;   * support of global-font-lock-mode
;
;   * add fontification ability to PLS and MSYM command buffers
;
; - Add a capability to build (automatically or manually - see
;   protel-auto-build-index) the "Index" menu. For the time being, the
;   "Index" menu contains a list of declarations (DCL, TYPE,...) starting at
;   the first column and only works in GNU Emacs; these limitations will be
;   removed in the future.
;
; - Minor modification to outline-regexp to support some weird coding, e.g.,
;   block_name: BLOCK instead of block_name: followed by new line and BLOCK.
;
; - handle-pls-edit-request is enhanced to fetch the ID section when LOOKed
;   instead of passing control back to vixen.emacs.
;
; - Re-organize the "Templates" and "Protel" menus.
;
; - case-fold-search is now set to t locally in functions which call Emacs
;   searching primitives. This is to avoid some interaction problems with
;   other packages which require case-sensitive search.
;
; - *.listing files no longer trigger Protel mode. This has caused several
;   problems since the Protel mode is not intended for these files.
;
; - The process output filter is now correctly set for XEmacs to use comint
;   package since the variable using-gnuemacs is no longer set for XEmacs.
;
; The following changes were submitted by Mark Flacy:
;
; - Enhance compilation-related functions to also position the cursor on the
;   identifier which has caused the error, warning, hint,...
;
; Revision 2.2  96/07/11  19:05:36  19:05:36  hlnguyen (Liem Nguyen)
;
; BUG FIXES
;
; - Minor bug in go-to-symbol-in-module-done: the ID in comment is located
;   when a section is opened. This is due to the use of "\\W" to delimit the
;   search regular expression (instead of "\\b").
;
; - Same bug happens in protel-hide-body: the text is not hidden properly.
;
; - WHILE template now correctly inserts the loop name after the ENDDO if
;   available and the first line of logical expression in the comment
;   following the ENDDO.
;
; - Minor bug in protel-module/section-header is fixed: the big header no
;   longer gets mixed up when called inside these functions. This bug is
;   caused by the use of next-line/previous-line. The latter are replaced by
;   appropriate functions (forward-line or end-of-line). The code is also
;   optimized a bit.
;
; - Use html-mode for html sourtype sections instead of html-helper-mode. The
;   latter is not always available.
;
; - Revisited msym-prompt-search to include a space after the prompt. This
;   allows to position correctly with C-c C-a (beginning of the command
;   string).
;
; ENHANCEMENTS
;
; - Use of easymenu package to build the menu for both PROTEL and MSYM
;   mode. This package makes menu design really easy and also, a single
;   interface for both GNU Emacs and XEmacs (code easy to maintain).
;
; - mark-active-p is introduced to check for region activated in both GNU
;   Emacs and XEmacs. This function replaces explicit code in several places.
;
; - protel-comment-prefix is now used in many places, however, the default
;   "%%" is still used for module, section, and class headers.
;
; - repeat-insert-char is obsolete and is removed.
;
;
; Revision 2.1  96/06/26  13:57:51  13:57:51  hlnguyen (Liem Nguyen)
; BUG FIXES
;
; - Update the HTTP address of the Protel mode web page.
;
; - Correct typos in comments.
;
; - Small bug in fontifying comments followed the compiler directives or LI
;   records is now fixed.
;
; ENHANCEMENTS
;
; - Functions dealing with the custom vixen are enhanced to work more
;   robustly, e.g., in case it cannot parse the string received from the
;   external custom handler (e.g., vixen.emacs), it simply pass it to the
;   default vixen.
;
; - protel-function-header is enhanced to insert user-defined comment prefix
;   (set in protel-comment-prefix) and more guidelines in the comment text.
;
; - protel-while-loop now inserts either loop name or the conditional
;   expression (if loop name is not supplied) after the "ENDDO;".
;
; NEW FEATURES
;
; - New functions: outline-previous-visible-heading,
;   outline-previous-visible-heading and associated key/menu binding.
;
; - New functions: protel-module-header protel-section-header and associated
;   key/menu binding. These functions insert the module/section headers.
;
; Revision 2.0  96/05/24  19:08:12  19:08:12  hlnguyen (Liem Nguyen)
; The following changes were submitted by Liem (H.L.) Nguyen
;
; Bug fixes
; ---------
;
; - Font-lock problem in XEmacs is now fixed - see protel-highlight-code.
;
; - New MSYM prompt (e.g., expect1.5>) is now recognized.
;
; Code cleanup
; ------------
;
; - Emacs 18 hooks removed - code more compact and easy to maintain.
;
; - Re-arrange and add more comments to the code.
;
; - Re-organize and enhance key/menu binding.
;
; - outline-regexp revisited.
;
; - protel-switch-to-buffer is removed, pop-up-buffer is used instead.
;
; - using-emacs19 is renamed to using-gnuemacs and is set correctly now. The
;   same fix is done for using-xemacs.
;
; - Variable protel-link-options default value is now set to ";+ mule=
;   warning nocheck" to ease the development phase.
;
; Enhancements
; ------------
;
; - Templates enhancement:
;
;   protel-class       protel-do-loop
;   protel-for-loop    protel-mothod
;
; - The big-letters file can be placed on the load-path, no need to set
;   protel-tool-dir.
;
; - New Motion commands
;
;   - Next/Previous Heading: move to next/previous visible heading.
;
; - New Collapse commands:Show Children, Show/Hide Body Right Under.
;
; - Comment manipulation enhancement
;
;   - protel-id-in-comment, protel-class-comment-prefix removed since
;     in-comment supersedes it.
;   - in-comment is enhanced to return also the comment prefix.
;   - protel-uncomment-region is enhanced to use in-comment.
;   - protel-newline is enhanced to use in-comment.
;
; - Custom PLS edit handler: pls-edit-handler and handle-pls-edit-request
;   implement the front-end processing for the default 'vixen'. This
;   eliminates many problems related to 'vixen'.
;
;   - New variables:
;
;     pls-edit-handler        pls-sourcetype-ddoc-id
;     pls-sourcetype-alist
;
;   - New functions:
;
;     extract-vixen-parms       pls-find-file-and-set-mode
;     handle-pls-edit-request   pls-set-edit-handler
;     pls-set-edit-handler
;
;   - Modified functions:
;
;     pls-run-command
;
;   Brian:
;   - Add functionality to auto upcase keyword
;   - Remove PLS/MSYM functions for NT Emacs
;   - Add support for speedbar
;

;;; protel.el ends here
