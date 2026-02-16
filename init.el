;;; Emacs.el --- Starting point in Emacs configuration.

;;; Commentary:
;;;
;;; Starting point in Emacs configuration.

;;; Code:

                                        ; Dvorak tweak

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])


(require 'cl-lib)  ; cl-set-difference

                                        ; Widgets and themes
(setq inhibit-splash-screen t)

(let* ((monospaced (if (string-equal system-type "windows-nt")
                       "Noto Sans Mono-11"
                     "Noto Sans Mono-13"))
       (sans-serif (if (string-equal system-type "windows-nt")
                       "Noto Sans-11"
                     "Noto Sans-13"))
       (serif "Noto Serif")
       (font monospaced))
  (setq default-frame-alist
        `((fullscreen . nil)
          (width . 80)
          (height . 40)
          (line-spacing . 3)
          (font . ,font))))

(add-to-list 'same-window-regexps "\*magit: .*\*")

(tool-bar-mode 0)
(menu-bar-mode 0)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

                                        ; Environment
(setenv "PAGER" "cat")

                                        ; Global parameters
(setq make-backup-files nil)
(setq default-directory "~/")
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)

(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-language-environment "utf-8")

(global-font-lock-mode -1)

(auto-save-mode 0)
(column-number-mode 1)

(display-time-mode)


					; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa". "https://melpa.org/packages/")))

(package-initialize)
(unless package-activated-list (package-refresh-contents))
(let ((needed-packages '(ace-jump-mode
                         eglot
                         go-mode
                         haskell-mode
                         helm
                         magit
                         paredit
                         restclient
                         slime
                         typit
                         use-package
                         )))
  (when
      (or (null package-activated-list)
          (cl-set-difference package-activated-list needed-packages))
    (mapc (lambda (p) (or (package-installed-p p) (package-install p)))
	  needed-packages)))

(require 'use-package)

                                        ; Helm
(require 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helf-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line        t)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-mode 1)

                                        ; Dvorak tweaks
(global-set-key (kbd "C-x h") 'helm-M-x)
(global-set-key (kbd "C-x C-h") 'helm-M-x)



                                        ; Paths
(setq load-path (cons "~/.emacs.d/lisp" load-path))

                                        ; Keybindings
(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c i") 'erc-tls)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c h") 'help-map)
(global-set-key (kbd "C-c n") 'gnus)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

                                        ; Gnus

;;(setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-select-method
      '(nnimap "imap.fastmail.com"
               (nnimap-expunge t)
               (nnimap-stream ssl)))

                                        ; SMTP
(setq user-full-name "Marco Benelli"
      user-mail-address "mbenelli@fastmail.com")

(setq message-send-mail-function 'smtpmail-send-it)

(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "mbenelli@fastmail.com")


                                        ; Haskell

(let ((ghcup-path (if (string-equal system-type "windows-nt")
                      "/ghcup/bin"
                    (expand-file-name "~/.ghcup/bin")))
      (cabal-path (if (string-equal system-type "windows-nt")
                      "/cabal/bin"
                    (expand-file-name "~/.cabal/bin"))))
  (setenv "PATH" (concat ghcup-path ":"
                         cabal-path ":"
                         (getenv "PATH")))
  (setq exec-path (append `(,ghcup-path ,cabal-path) exec-path)))

                                        ; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "stroustrup"
      c-basic-offset 4
      indent-tabs-mode nil)

                                        ; Eglot
(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c-or-c++-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '(:haskell (:plugin (:stan (:globalOn :json-false))
                                    :formattingProvider "stylish-haskell"))))


                                        ; Tramp
(setq tramp-default-method "ssh")

                                        ; Org
(add-hook 'org-mode-hook
          '(lambda nil
             (visual-line-mode)
             (turn-on-font-lock)
             (disable-show-trailing-whitespace)))

                                        ; Eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          '(lambda nil
             (setenv "PATH"
                     (concat (getenv "PATH") ":"
                             (expand-file-name "~/.local/bin") ":")
                     )))

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

                                        ; Irc
(require 'erc)

                                        ; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;;(auto-complete-mode 1)
            ))

                                        ; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'slime-mode-hook                       #'enable-paredit-mode)

                                        ; Mode line cleaner
(defvar mode-line-cleaner-alist
  `((yas/minor-mode . " y")
    (yas-minor-mode . " y")
    (paredit-mode . " π")
    (flycheck-mode . " φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (golden-ratio-mode . "")
    (projectile-mode . " Π")
    (magit-auto-revert-mode . "")
    (helm-mode . " h")
    (helm-gtags-mode . " gh")
    (autopair-mode . "")
    (auto-complete-mode . " α")
    (company-mode . " c")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (scheme-mode . "λ")
    (clojure-mode . "λ")
    (cider-repl-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "ɛλ")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")(defun clean-mode-line ()
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

;;(require 'local)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format "[%b%p%%]")
 '(compilation-scroll-output 'first-error)
 '(custom-safe-themes
   '("efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b" "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8" default))
 '(display-time-24hr-format t)
 '(eww-download-directory "~/downloads/")
 '(markdown-xhtml-header-content
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />")
 '(package-selected-packages
   '(typit slime restclient paredit magit helm haskell-mode go-mode ace-jump-mode))
 '(send-mail-function 'sendmail-send-it))


(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
