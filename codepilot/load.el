
(defvar codepilot-dir
  (file-name-directory
   (or load-file-name (buffer-file-name))))


(add-to-list 'load-path (expand-file-name "common" codepilot-dir))
(add-to-list 'load-path (expand-file-name "cc" codepilot-dir))
(add-to-list 'load-path (expand-file-name "hack" codepilot-dir))
(add-to-list 'load-path (expand-file-name "misc" codepilot-dir))
(add-to-list 'load-path (expand-file-name "import" codepilot-dir))

(add-to-list 'exec-path (expand-file-name "bin" codepilot-dir))
  

(load (concat codepilot-dir "misc/mymisc.elc"))
(load (concat codepilot-dir "import/misccollect.elc"))

(require 'cp-layout)
(require 'cp-toolbar)
(require 'cplist)
(require 'cphistory)
(require 'cpnote)
(require 'cp-mark)
(require 'cpfilter)
(require 'cp-hl)
(require 'myocchack)
(require 'mytbhack)
(require 'myremember)
(require 'cpimenu)
(require 'cp-toolbar)
(require 'myishack)


(require 'myhshack)
(require 'mytbhack)

(require 'smart-hl)
(require 'mycutil)
(require 'myctagsmenu)
(require 'mypython)
(require 'cp-cc)

 


;; VS and UtraEdit style bookmark:
(require 'bm)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<M-f2>") 'bm-previous)
(global-set-key (kbd "<S-f2>") 'bm-show)

(define-key bm-show-mode-map [mouse-3] (lambda (e)
                                         (interactive "e")
                                         (mouse-set-point e)
                                         (bm-show-goto-bookmark)
                                         ))

;; cscope
;; ===============================
(require 'xcscope)

;; Global
;; =================================
(autoload 'gtags-mode "gtags" "" t)



;; config

;; (global-set-key "\M-n" 'ido-goto-symbol)
(global-set-key "\M-n" 'cpimenu-go)

(codepilot-ro-toggle-globally)




(add-hook 'c-mode-hook (lambda () (gtags-mode 1)
                         (hs-minor-mode 1)
                         (hide-ifdef-mode 1)
                         (local-set-key "\r" 'newline-and-indent)
                         ))

(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)
                           (hs-minor-mode 1)
                           (hide-ifdef-mode 1)
                           (local-set-key "\r" 'newline-and-indent)
                           ))


(add-hook 'gtags-mode-hook (lambda () (local-set-key "\er" 'gtags-find-rtag)))




(add-hook 'java-mode-hook (function cscope:hook))
(add-hook 'java-mode-hook (lambda ()
                            (gtags-mode 1)
                            (local-set-key "\r" 'newline-and-indent)
                            (hs-minor-mode 1)))


(require 'cplist-cc)
(add-hook 'find-file-hook 'cplist-update-buffer-list)
(add-hook 'cplist-fill-contents-hook 'cplist-cc-fill-cplist)
(add-hook 'cplist-action-hook 'cplist-cc-for-cplist-action)
(add-hook 'cplist-delete-history-entry 'tag-delete-in-tag-history)

(define-key cplist-mode-map "\r" 'cplist-action)
(define-key cplist-mode-map "\t" 'cplist-cc-tab)
(define-key cplist-mode-map "\M-n" 'cpfilter-erase)



(defun brian-activate-codepilot-cc ()
  (interactive)

  (codepilot-layout-activate)
  (codepilot-toolbar-activate)
  (cpimenu-activate)

  ;; (setq lazy-highlight-cleanup nil)
  )

(defun brian-deactivate-codepilot-cc ()
  (codepilot-layout-deactivate)
  (codepilot-toolbar-deactivate)
  (cpimenu-activate)
  )


;; VS style bookmark:

(require 'bm)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<M-f2>") 'bm-previous)
(global-set-key (kbd "<S-f2>") 'bm-show)

(define-key bm-show-mode-map [mouse-3] (lambda (e)
                                         (interactive "e")
                                         (mouse-set-point e)
                                         (bm-show-goto-bookmark)
                                         ))

(face-spec-set 'bm-face '((((class color)
                            (background light))
                           (:background "yellow"))))


(global-set-key [(f12)] 'smart-hl-current-word)
(defalias 'st 'smart-hl-text)


(brian-activate-codepilot-cc)