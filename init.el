;; Emacs configuration file
					; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


                                        ; Widgets and themes
(setq inhibit-splash-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(column-number-mode 1)
(display-time-mode 1)
(global-linum-mode -1)

(require 'golden-ratio)
(golden-ratio-mode)

                                        ; Paths
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(add-to-list 'Info-directory-list "~/.emacs.d/info")


                                        ; Keybindings
(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c e") 'erc-tls)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)

                                        ; Environment
(setenv "PAGER" "cat")

                                        ; Global parameters
(setq make-backup-files nil)
(setq default-directory "~/")
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq display-time-24hr-format t)
(setq ring-bell-function 'ignore)
(setq compilation-scroll-output 'first-error)

(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "utf-8")

(global-font-lock-mode 3)

(auto-save-mode 0)

                                        ; Helm
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-autoresize-mode                  t
      helm-ff-file-name-history-use-recentf t)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))


(require 'helm-gtags)

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a")
  'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(helm-mode 1)

                                        ; Trailing whitespaces

(defun disable-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(add-hook 'shell-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'comint-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'eshell-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'slime-repl-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'lisp-interaction-mode 'disable-show-trailing-whitespace)
(add-hook 'eww-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'Info-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'Buffer-menu-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'calendar-mode-hook 'disable-show-trailing-whitespace)
(add-hook 'erc-mode-hook 'disable-show-trailing-whitespace)

                                        ; Eshell
(require 'eshell)
(require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          '(lambda nil
             (setenv "PATH"
                     (concat (getenv "PATH") ":" (getenv "HOME") "/bin"))))

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

                                        ; Tables

(setq table-cell-horizontal-chars "\u2500")
(setq table-cell-vertical-char ?\u2502)
(setq table-cell-intersection-char ?\u253C)

                                        ; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

                                        ; Autopair
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

                                        ; Ace jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

                                        ; Irc
(require 'erc)

                                        ; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-mod-line nil)

                                        ; Semantic

(setq semantic-default-submodes
             '(global-semanticdb-minor-mode
               global-semantic-mru-bookmark-mode
               global-semantic-idle-scheduler-mode
               global-semantic-idle-completions-mode
               global-semantic-idle-summary-mode))
(semantic-mode 1)

(setq semantic-symref-tool 'global)

(require 'semantic/ia)

                                        ; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

                                        ; autocomplete
(require 'auto-complete-config)
(setq-default ac-sources '(ac-source-semantic-raw))

(ac-set-trigger-key "<tab>")


                                        ; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun qt-assistant-search (&optional word)
  (interactive)
  (let ((w (or word (current-word)))
        (p (or (get-process "assistant")
               (start-process "assistant" "*assistant*"
                              "/Qt/Qt5.2.1/5.2.1/msvc2012/bin/assistant"
                              "-enableRemoteControl"))))
    (process-send-string p (concat "activateKeyword " w))))

(setq c-default-style "stroustrup"
      c-basic-offset 4
      indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (auto-complete-mode 1)
              (c-set-offset 'innamespace 0)
              (local-set-key (kbd "<C-h q") 'qt-assistant-search)
              )))


                                        ; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

                                        ; QML

(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

                                        ; Python
(setq python-indent 4)

                                        ; Javascript
(setq js-indent-level 4)

                                        ; Gnus

(setq gnus-select-method '(nntp "news.gmane.org"))

                                        ; Mode line cleaner
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " y")
    (yas-minor-mode . " y")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (golden-ratio-mode . "")
    (projectile-mode . " P")
    (magit-auto-revert-mode . "")
    (helm-mode . " h")
    (helm-gtags-mode . " gh")
    (autopair-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "l")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "el")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(require 'local)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
