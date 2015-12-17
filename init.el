;;; init.el ---  emacs configuration file.
;;; Commentary:
;;; Starting point in emacs configuration.

;;; Code:

(require 'cl-lib)

					; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(defvar needed-packages
  '(auto-complete-c-headers
    autopair
    company
    magit
    magit-gerrit
    paredit
    projectile
    slime
    markdown-mode
    yasnippet
))

(defun ensure-packages ()
  (dolist (p needed-packages)
    (unless (package-installed-p p)
      (package-install p))))

(package-initialize)
(ensure-packages)
                                        ; Widgets and themes
(setq inhibit-splash-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(column-number-mode 1)
(setq display-time-24hr-format t)
(display-time-mode 1)
(global-linum-mode -1)

                                        ; Path for local customizations.
(setq load-path (cons "~/.emacs.d/lisp" load-path))

                                        ; Keybindings

;;(define-key key-translation-map [?\C-x] [?\C-u])
;;(define-key key-translation-map [?\C-u] [?\C-x])
;;(global-set-key (kbd "C-h") 'delete-backward-char)
;;(global-set-key (kbd "C-x C-h") 'execute-extended-command)
;;(global-set-key (kbd "C-x DEL") 'execute-extended-command)

(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c i") 'erc-tls)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c h") 'help-map)
(global-set-key (kbd "C-c j") 'org-jira-get-issues)

                                        ; Environment
(setenv "PAGER" "cat")

                                        ; Global parameters
(setq make-backup-files nil)
(setq default-directory "~/")
(setq-default indent-tabs-mode nil)
(setq display-time-24hr-format t)
(setq ring-bell-function 'ignore)
(setq compilation-scroll-output 'first-error)

(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "utf-8")

(global-font-lock-mode 3)

(auto-save-mode 0)

                                        ; Ido
(ido-mode 1)

                                        ; Rmail
(setq rmail-preserve-inbox t)
(setq rmail-movemail-program "/usr/bin/movemail")
(setq rmail-movemail-variant-in-use 'mailutils)
(setq rmail-primary-inbox-list '("maildir://.mail/Inbox"))

                                        ; Org
(add-hook 'org-mode-hook
          '(lambda nil
             (visual-line-mode)
             (disable-show-trailing-whitespace)))

                                        ; Markdown
(add-hook 'markdown-mode-hook
          '(lambda nil
             (visual-line-mode)
             (set-fill-column 50)))

                                        ; Eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

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

                                        ; Irc
(require 'erc)

                                        ; Gambit Scheme
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "gsi -:d-")

                                        ; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ido)
(setq projectile-mod-line nil)

                                        ; yasnippet
(require 'yasnippet)
(setq yas-sineppet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

                                        ; autocomplete
(require 'company)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-idle-delay 0)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-quick-help-delay 0)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map [return] nil)
(define-key ac-complete-mode-map "\r" nil)
(define-key ac-mode-map [C-return] 'auto-complete)
(setq ac-candidate-limit 100)

(defun auto-complete-mode-maybe ()
  "Auto complete everywhere."
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

(require 'auto-complete-clang)
;;(require 'c++-include-files)

                                        ; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (auto-complete-mode 1)))

                                        ; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "stroustrup"
      c-basic-offset 4
      indent-tabs-mode nil)

                                        ; Rtags
(require 'rtags)
(require 'popup)
(require 'rtags-ac)
(require 'company-rtags)

(rtags-enable-standard-keybindings c-mode-base-map)
(setq rtags-completions-enabled t)

(define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)


                                        ; cmake-ide
;; (cmake-ide-setup)
;; (setq cmake-ide-flags-C++ (append '("-std=c++1y")
;;                                   (mapcar (lambda (path) (concat "-I" path))
;;                                           (c++-include-paths))))
;; (setq cmake-ide-flags-c '("-I/usr/include"))

                                        ; C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (setq compilation-scroll-output 'first-error)
              (setq show-trailing-whitespace t)
              (c-set-offset 'innamespace 0))))

                                        ; autocomplete headers
(add-hook 'c++-mode-hook
          (lambda ()
            (require 'auto-complete-c-headers)
            (setq ac-sources
                  '(ac-source-c-headers ac-source-clang ac-source-yasnippet))
            (setq company-backends
                  '(company-rtags company-clang company-keywords
                                  company-yasnippet company-files))
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (auto-complete-mode 0)
            (company-mode 1)
            (global-set-key [C-return] 'company-complete-common)))

(defun toggle-ac ()
  "Toggle between auto-complete and company."
  (interactive)
  (if (bound-and-true-p auto-complete-mode)
      (progn
        (auto-complete-mode 0)
        (company-mode 1)
        (global-set-key [C-return] 'company-complete-common))
    (progn
      (company-mode 0)
      (auto-complete-mode 1)
      (global-set-key [C-return] 'auto-complete))))

                                        ; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11y")))

                                        ; misc
(autoload 'ace-jump-mode "ace-jump-mode")

                                        ; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

                                        ; QML

;;(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
;;(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

                                        ; Python
(setq python-indent 4)

                                        ; Javascript
(setq js-indent-level 4)

                                        ; Gnus

(setq gnus-select-method '(nntp "news.gmane.org"))

                                        ; W3m
(setq w3m-use-tab nil)

                                        ; Mode line cleaner
(defvar mode-line-cleaner-alist
  `((yas/minor-mode . " y")
    (yas-minor-mode . " y")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (golden-ratio-mode . "")
    (projectile-mode . " p")
    (magit-auto-revert-mode . "")
    (helm-mode . " h")
    (helm-gtags-mode . " gh")
    (autopair-mode . "")
    (auto-complete-mode . " ac")
    ;; Major modes
    (lisp-interaction-mode . "lisp")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "elisp")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  "Clean-up the mode-line according to rules defined in `mode-line-cleaner-alist'."
  (interactive)
  (dolist (cleaner mode-line-cleaner-alist)
    (let* ((mode (car cleaner))
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
 '(display-time-24hr-format t)
 '(send-mail-function (quote sendmail-send-it))
 '(w3m-enable-google-feeling-lucky nil)
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-session-crash-recovery nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here



