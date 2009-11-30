
(defvar msym-global-menu
  '("MSymG"
    :active (and (msym-active) (not (eq major-mode 'protel-mode)))
    ["Show References" (lambda () (interactive) (call-interactively 'msym-show-ref)) t]
    "-"
    ["Goto Definition Globally" (lambda () (interactive) (call-interactively 'msym-goto-def-nosection)) t]
    ["Goto Implementation Globally" (lambda () (interactive) (call-interactively 'msym-goto-imp-nosection)) t]
    "-"
    ["Open Section from Context" (lambda () (interactive) (call-interactively 'msym-open-pls-section-from-context-i)) t]
    ))

(defvar codepilot-global-menu
  '("CodePilotG"
    :active (and (codepilot-active) (not (eq major-mode 'protel-mode)))
    ["Calltrak/Traceback" (lambda () (interactive) (call-interactively 'codepilot-open-calltrack)) t]
    "-"
    ["Goto Definition" (lambda () (interactive) (call-interactively 'codepilot-goto-def)) t]
    ["Goto Implementation" (lambda () (interactive) (call-interactively 'codepilot-goto-imp)) t]
    "-"
    ["Procedure" (lambda () (interactive) (call-interactively 'codepilot-query-proc)) t]
    ["Procedure (DIM)" (lambda () (interactive) (call-interactively 'codepilot-query-proc-dim)) t]
    ["Identifier" (lambda () (interactive) (call-interactively 'codepilot-query-id)) t]
    ["Identifier (DIM)" (lambda () (interactive) (call-interactively 'codepilot-query-id-dim)) t]
    ["Combined" (lambda () (interactive) (call-interactively 'codepilot-query-sym)) t]
    ["Combined (DIM)" (lambda () (interactive) (call-interactively 'codepilot-query-sym-dim)) t]
    "-"
    ["Comment" (lambda () (interactive) (call-interactively 'codepilot-query-comment)) t]
    ["Literal String" (lambda () (interactive) (call-interactively 'codepilot-query-string)) t]
    "-"
    ["Module Name" (lambda () (interactive) (call-interactively 'codepilot-query-module)) t]
    ["Section Name" (lambda () (interactive) (call-interactively 'codepilot-query-section)) t]
    ["Patch" (lambda () (interactive) (call-interactively 'codepilot-query-patch)) t]
    ))

;; (popup-menu codepilot-global-menu)

(easy-menu-define msym-global-menu-symbole
  (current-global-map)
  "MSym global menu"
  msym-global-menu)

(easy-menu-define codepilot-global-menu-symbole
  (current-global-map)
  "codepilot global menu"
  codepilot-global-menu)

(provide 'mycpgmenu)