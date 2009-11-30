
;;    (defmacro with-find-then-action (regexp case-fold &rest body)
;;      `(if (not (mark-active-p))
;;           (error "Region is not set.")
;;           (save-excursion
;;             (save-restriction
;;               (let ((case-fold-search ,case-fold)
;;                     )
;;                 (narrow-to-region (region-beginning) (region-end))
;;                 (goto-char (point-min))
;;                 (save-match-data
;;                   (while (re-search-forward ,regexp nil t )
;;                     ,@body
;;                     )))))))
;;
;;    (defun adjust_tc_number()
;;      ""
;;      (interactive)
;;      (let ((cur_num 0)
;;            (replace-str nil)
;;            )
;;        (save-match-data
;;          (save-excursion
;;            (goto-char (point-min))
;;            (while (re-search-forward "^TC-[0-9]*" nil t )
;;              (progn
;;                (setq cur_num (+ cur_num 1))
;;                (setq replace-str (concat "TC-" (int-to-string cur_num)))
;;                (if (not (string-equal (match-string 0) replace-str))
;;                    (replace-match replace-str nil t))
;;                ))
;;
;;            (goto-char (point-min))
;;            (setq cur_num 0)
;;            (while (re-search-forward "^ENDTC-[0-9]*" nil t )
;;              (progn
;;                (setq cur_num (+ cur_num 1))
;;                (setq replace-str (concat "ENDTC-" (int-to-string cur_num)))
;;                (if (not (string-equal (match-string 0) replace-str))
;;                    (replace-match replace-str nil t))
;;                ))
;;
;;            )))
;;      )
;;
;;
;;    (defun adjust_tc_title()
;;      ""
;;      (interactive)
;;      (save-match-data
;;        (save-excursion
;;          (goto-char (point-min))
;;          (while (re-search-forward "\\(^TC-[0-9]* \\)[^-]" nil t )
;;            (progn
;;              (backward-char 1)
;;              (insert "- ")
;;              )))))
;;
;;
;;    (defun order-step-num-region ()
;;      ""
;;      (interactive)
;;      (let ((cur-num -1))
;;        (with-find-then-action
;;            "^\\( *\\)\\([0-9]+\\.\\)"
;;          t
;;          (if (= cur-num -1)
;;              (setq cur-num (string-to-number (match-string 2)))
;;              (setq cur-num (+ cur-num 1)))
;;          (replace-match (concat (match-string 1)(int-to-string cur-num) "."))
;;          )))
;;
;;
;;    (defun temp-find-sth-and-do-sth ()
;;      ""
;;      (interactive)
;;      (save-match-data
;;        (save-excursion
;;          (goto-char (point-min))
;;          (while (re-search-forward "^EXPECTED RESULTS -" nil t )
;;            (progn
;;              (forward-line 1)
;;              (newline)
;;              (insert "2. No SWERRs or TRAPs occur.")
;;              )))))
;;
;;    (defun temp-find-sth-and-do-sth-region ()
;;      ""
;;      (interactive)
;;      (if (not (mark-active-p))
;;          (error "Region is not set.")
;;        (save-excursion
;;          (if (< (point) (mark)) (exchange-point-and-mark))
;;          (if (or (bolp) (< (current-column) (current-indentation)))
;;              (forward-line -1))
;;          (save-restriction
;;            (let ((case-fold-search t)
;;                  (cur_num 0)
;;                  )
;;              (end-of-line)
;;              (exchange-point-and-mark)
;;              (beginning-of-line)
;;              (exchange-point-and-mark)
;;              (narrow-to-region (point) (mark))
;;              (goto-char (point-min))
;;              (save-match-data
;;                (while (re-search-forward "^\\* TEST  = [0-9]+\\." nil t )
;;                  (progn
;;                    (setq cur_num (+ cur_num 1))
;;                    (replace-match (concat "* TEST  = " (int-to-string cur_num) ".") nil t)
;;                    )
;;                  )))))))
;;
;;
;;    ;; this function can actuall be done by flush-lines.
;;    ;; related function: keep-lines
;;    ;; (if (and interactive transient-mark-mode mark-active)
;;    ;;      (setq rstart (region-beginning)
;;    ;;            rend (copy-marker (region-end)))
;;    ;;       (setq rstart (point)
;;    ;;          rend (point-max-marker)))
;;    (defun fndl (s)
;;      "Find lines match regexp and delete it."
;;      (interactive "sRegexp:")
;;      (with-find-then-action
;;          s
;;        t
;;        (forward-line 0)
;;        (kill-line 1)))
;;
;;
;;    ;; GPS
;;
;;    ;; Data Sample
;;    ;; OFEATCD     NNN NNN NNN NNN NNN YNY NYN NNN NNN NNY YN
;;    ;; TFEATCD     YNN YNN NNN NNN NNN NNN NNN NNN NNN NNN NNN NNN NNN NNN NNN NNY
;;
;;    (defvar cellular-ofeatcode
;;      '("DIALOGUE"
;;        "VOICE_MAIL_RETRIEVAL"
;;        "FEATURE_RESULT_SUCCESSFUL"
;;        "FEATURE_RESULT_UNSUCCESSFUL"
;;        "QOS_METRICS_1"
;;        "QOS_METRICS_2"
;;        "SPARE_BIT_3"
;;        "SPARE_BIT_4"
;;        "WPSIND"
;;        "ORIG_THREE_WAY_CALLING_USAGE"
;;        "ORIG_CALL_TRANSFER_USAGE"
;;        "SPARE_BIT_6"
;;        "ORIG_VERTICAL_FEATURE_FLAG,"
;;        "ORIG_DIRECT_INWARD_MOBILE_ACCESS"
;;        "ORIG_HOTLINE_FEATURE"
;;        "ORIG_INTERSYSTEM_HANDOFF"
;;        "SPARE_BIT_7"
;;        "ORIG_AUTOMATIC_ROAMING"
;;        "ORIG_FOLLOW_ME_ROAMING"
;;        "ANNOUNCEMENT_PLAYED"
;;        "ORIG_TARGET_SURVEILLANCE"
;;        "SPARE_BIT_9"
;;        "SPARE_BIT_10"
;;        "ORIG_WINIP"
;;        "SPARE_BIT_12"
;;        "ORIG_CALLING_NUMBER_IDENTIFICATION_RESTRICTION"
;;        "COEXCHANGE_FEATURE"
;;        "PER_CALL_FEATURE_CONTROL"
;;        "ORIG_RLT_FEATURE"
;;        "ORREQ_LAUNCHED"
;;        "ORIG_AUTH_ATTEMPT"
;;        "ORIG_AUTH_FAIL"
;;        ))
;;
;;    (defvar cellular-tfeatcode
;;      '("CALL_FORWARDING_UNCONDITIONAL_USAGE"
;;        "VOICE_MAIL_DEPOSIT"
;;        "PCA_CALL_REFUSED"
;;        "TERM_CALL_FORWARDING_BUSY_USAGE"
;;        "PSTN_TANDEM"
;;        "SCA_CALL_REFUSED"
;;        "TERM_CALL_FORWARDING_NO_ANSWER_USAGE"
;;        "CALL_FORWARDING_defAULT"
;;        "CALL_DELIVERY_VIA_PSTN"
;;        "CALL_DELIVERY_VIA_LOCAL"
;;        "CALL_DELIVERY_VIA_PRIVATE"
;;        "TERM_CALL_WAITING_USAGE"
;;        "TERM_VERTICAL_FEATURE_FLAG"
;;        "PRIVATE_TANDEM"
;;        "DMH_REDIRECTION_TREATMENT"
;;        "TERM_INTERSYSTEM_HANDOFF"
;;        "CALL_DELIVERY_UNSPECIFIED"
;;        "TERM_CALL_TRANSFER_USAGE"
;;        "TERM_FOLLOW_ME_ROAMING"
;;        "TERM_CALL_DELIVERY_ACTIVATABLE"
;;        "TERM_TARGET_SURVEILLANCE"
;;        "MOBILE_ACCESS_HUNTING"
;;        "FLEXIBLE_ALERTING"
;;        "TERM_WINIP"
;;        "TERM_CALLING_NUMBER_IDENTIFICATION_PRESENTATION"
;;        "SACT_FEATURE_ACTIVE"
;;        "TERM_CALLING_PARTY_PAYS"
;;        "ANNOUNCEMENT_PLAYED"
;;        "TERM_RLT_FEATURE"
;;        "TERM_BORDER_CELL_TERMINATION"
;;        "TERM_CALLING_NAME"
;;        "TERM_AUTH_ATTEMPT"
;;        "TERM_AUTH_FAIL"
;;        "CRBT_CALL"
;;        ))
;;
;;    (defun feature-code-analysis (fc-tab)
;;      ""
;;      (interactive)
;;      (let ((case-fold-search t)
;;            (pos1 (point))
;;             ch1
;;            (quit-loop nil)
;;            (str "")
;;            (f-index 0))
;;        (while (not quit-loop)
;;            (setq pos1 (1+ pos1))
;;            (setq ch1 (char-before pos1))
;;            (if (char-equal ch1 ?\n)
;;                (setq quit-loop t)
;;              (progn
;;                (cond ((char-equal ?Y ch1)
;;                       (setq str (concat str "\n" (int-to-string f-index) "\t" (char-to-string ch1)))
;;                       (setq str (concat str "\t" (nth f-index fc-tab)))
;;                       (setq f-index (1+ f-index)))
;;                      ((char-equal ?N ch1)
;;                       (setq f-index (1+ f-index)))
;;                      (t
;;                       ()))
;;                )))
;;        (end-of-line)
;;        (newline)
;;        (insert str)
;;        ))
;;
;;
;;    (defun analyze-featcd ()
;;      ""
;;      (interactive)
;;      (save-excursion
;;        (save-match-data
;;          (end-of-line)
;;          (insert "\n")
;;          (backward-char 1)
;;          (let
;;              ((case-fold-search t)
;;               (limit (point))
;;               )
;;            (beginning-of-line)
;;            (if (re-search-forward "\\<\\(OFEATCD\\|TFEATCD\\)\\>" limit t)
;;                (cond ((string= (match-string 0) "OFEATCD")
;;                       (feature-code-analysis cellular-ofeatcode))
;;                      ((string= (match-string 0) "TFEATCD")
;;                       (feature-code-analysis cellular-tfeatcode))
;;                      (t
;;                       ()))))
;;        )))
;;
;;
;;
;;    ;; Data Sample:
;;    ;; RECCD F7  ENTRYCD 50  VERINFO 1280  OFEATCD 00000000  TFEATCD 000010000000
;;
;;
;;    (defun hexl-hex-char-to-integer (character)
;;      "Take a char and return its value as if it was a hex digit."
;;      (if (and (>= character ?0) (<= character ?9))
;;          (- character ?0)
;;        (let ((ch (logior character 32)))
;;          (if (and (>= ch ?a) (<= ch ?f))
;;              (- ch (- ?a 10))
;;            (error "Invalid hex digit `%c'" ch)))))
;;
;;
;;    (defun analyze-hex-fc ()
;;      ""
;;      (interactive)
;;      (save-excursion
;;        (save-match-data
;;          (let (pos1
;;                word-len
;;                ch1
;;                int1
;;                f-tab
;;                (str "")
;;                )
;;            (setq word-len (length (current-word)))
;;            (if (not (looking-at "\\<"))
;;                (backward-word 1))
;;            (setq pos1 (point))
;;            (setq pos1 (1+ pos1))
;;            (cond ((= word-len 8)
;;                   (setq str (concat str "\nOOOOOOrignation Fature Code\n"))
;;                   (setq f-tab cellular-ofeatcode)
;;                   )
;;                  ((= word-len 12)
;;                   (setq str (concat str "\nTTTTTTermination Feature Code\n"))
;;                   (setq f-tab cellular-tfeatcode)
;;                   )
;;                  (t
;;                   (error "Wrong length '%d'" word-len)
;;                   ))
;;            (dotimes (i word-len)
;;              (setq ch1 (char-before pos1))
;;              (setq int1 (hexl-hex-char-to-integer ch1))
;;              (if (not (= int1 0))
;;                  (let (temp yn f-index)
;;                    (setq temp int1)
;;                    (dotimes (j 4)
;;                      (if (= (mod temp 2) 1)
;;                          (progn
;;                            (setq f-index (+ (* i 4) j))
;;                            (setq str (concat str (int-to-string f-index) "\t" (nth f-index f-tab) "\n"))))
;;                      (setq temp (/ temp 2))
;;                      (if (= temp 0) (return))
;;                      )
;;                    ))
;;              (setq pos1 (1+ pos1))
;;              )
;;
;;            (with-output-to-temp-buffer "*FCOut*"
;;              (save-excursion
;;                (set-buffer standard-output)
;;                (insert str)
;;                  ))))))
;;
;;
;;
;;    ;; 00002004
;;    ;; 11002004
;;    ;; FFFFFFFFf
;;
;;    ;; 002000180000
;;    ;; 1100L004
;;
;;
;;    ;; ========================
;;    ;; Add "#" before the word
;;    ;; ========================
;;
;;    (defun add-jin-before-word-region ()
;;      ""
;;      (interactive)
;;      (if (not (mark-active-p))
;;          (error "Region is not set.")
;;        (save-excursion
;;          (save-restriction
;;            (if (> (point) (mark)) (exchange-point-and-mark))
;;            (forward-word 1)
;;            (forward-word -1)
;;            (if (not (char-equal (char-before (point)) ?\#))
;;                (insert "#")
;;              )
;;            (narrow-to-region (point) (mark))
;;            (goto-char (point-max))
;;            (forward-word -1)
;;            (while (not (bobp))
;;              (if (not (char-equal (char-before (point)) ?\#))
;;                  (insert "#")
;;                  )
;;              (forward-word -1)
;;              )
;;          ))))
;;
;;    ;; l ($nodeversions ownedby CELLGROUP) stream mtx16 file
;;    (defun fmt-mtxdoc ()
;;      ""
;;      (interactive)
;;      (let ((case-fold-search t)
;;            act
;;            titile
;;            ver
;;            b e
;;            (get-buffer-create "*MTXDOC*")
;;            (ll "")
;;            )
;;        (save-excursion
;;          (save-match-data
;;            (goto-char (point-min))
;;            (while (re-search-forward " DEFAULT_PARTITION$" nil t)
;;              (forward-line)
;;              (delete-indentation)
;;              )
;;            (goto-char (point-min))
;;            (while (not (eobp))
;;              (forward-word 2)
;;              (setq act (current-word))
;;              (search-forward "DEFAULT_PARTITION " nil t)
;;              (setq b (point))
;;              (end-of-line)
;;              (setq e (point))
;;              (setq titile (buffer-substring b e))
;;              (forward-line)
;;              (setq ver (current-word))
;;              (setq ll(concat ll act "\t" ver "\t" titile "\n"))
;;              (forward-line)
;;              )
;;            (with-output-to-temp-buffer "*MTXDOC*"
;;              (save-excursion
;;                (set-buffer standard-output)
;;                (insert ll)
;;                  ))
;;            ))
;;        ))
;;
;;
;;
;;     ;; for .mif file
;;
;;     (defun mif-get-string-tag-content ()
;;       (when (re-search-forward "<\\(?:\\(Para[ \n]\\)\\|\\(String `\\)\\)" nil t)
;;         (cond ((match-beginning 1)
;;                "\n"
;;                )
;;               (t
;;                (let ((pos (point))
;;                      )
;;                  (when (search-forward "'>" nil t)
;;                    (buffer-substring-no-properties pos (- (point) 2))
;;                    ))))))
;;
;;     (defun mif-get-text-aa01 ()
;;       (interactive)
;;       (let ((str "")
;;             line
;;             (buf-name (buffer-name))
;;             buf
;;             )
;;         (setq buf (get-buffer (concat buf-name ".aa01")))
;;
;;         (cond (buf
;;                (switch-to-buffer buf)
;;                )
;;               (t
;;                (goto-char (point-min))
;;                (while (setq line (mif-get-string-tag-content))
;;                  (setq str (concat str line))
;;                  )
;;
;;                (setq buf (get-buffer-create (concat buf-name ".aa01")))
;;
;;                (when (switch-to-buffer buf)
;;
;;                  (setq str (replace-regexp-in-string "\\\\xd5 " "'" str))
;;                  (setq str (replace-regexp-in-string "\\\\xd2 " "\"" str))
;;                  (setq str (replace-regexp-in-string "\\\\>" ">" str))
;;
;;                  (insert str)
;;
;;                  )
;;                (protel-mode)
;;                (goto-char (point-min))
;;
;;                ))))


