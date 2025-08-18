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

(let* ((monospaced "Noto Mono-11")
       (sans-serif "Noto Sans-13")
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

(electric-pair-mode 1)


					; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")
                         ("melpa". "https://melpa.org/packages/")))

(package-initialize)
(unless package-activated-list (package-refresh-contents))
(let ((needed-packages '(ace-jump-mode
                         adoc-mode
                         ag
                         elpy
                         go-mode
                         haskell-mode
                         helm
                         jq-mode
                         julia-mode
                         julia-repl
			 magit
                         monkeytype
                         monotropic-theme
			 paredit
			 slime
                         solarized-theme
                         markdown-mode
                         use-package
                         )))
  (when
      (or (null package-activated-list)
          (cl-set-difference package-activated-list needed-packages))
    (mapc (lambda (p) (or (package-installed-p p) (package-install p)))
	  needed-packages)))

(require 'use-package)

                                        ; Theme
(when (display-graphic-p)
  (setq x-underline-at-descent-line t)
  (load-theme 'monotropic t))

                                        ; Helm
(require 'helm)
;(require 'helm-config)
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
;;(helm-autoresize-mode 1)
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

                                        ; Special characters
(fset 'euro
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "EURO SIGN"))))

(fset 'epsilon
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "GREEK SMALL LETTER EPSILON"))))

(fset 'section
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "SECTION SIGN"))))


(fset 'diminished
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "DEGREE SIGN"))))

(fset 'half-diminished
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "EMPTY SET"))))

(fset 'flat
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "MUSIC FLAT SIGN"))))

(fset 'sharp
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "MUSIC SHARP SIGN"))))

(fset 'natural
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (insert (char-from-name "MUSIC NATURAL SIGN"))))

                                        ; Tramp
(setq tramp-default-method "ssh")

                                        ; Org
(add-hook 'org-mode-hook
          (lambda nil
            (visual-line-mode)
            (turn-on-font-lock)
            (disable-show-trailing-whitespace)))

                                        ; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda nil
            (visual-line-mode)))

                                        ; Lilypond
(autoload 'LilyPond-mode "lilypond-mode")
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook
          (lambda nil
            (define-key LilyPond-mode-map "\C-c\C-p"
                        'LilyPond-command-viewpdf)))

                                        ; Eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          (lambda nil
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
;; (require 'autopair)
;; (autopair-global-mode 1)
;; (setq autopair-autowrap t)

                                        ; Irc
(require 'erc)

                                        ; Projectile
;; (projectile-global-mode)
;; (setq projectile-completion-system 'helm)
;; (setq projectile-mod-line nil)
;; (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

                                        ; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;;(auto-complete-mode 1)
            ))

                                        ; Slime
(setq inferior-lisp-program "sbcl")

                                        ; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-process-type 'stack-ghci)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)


                                        ; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "stroustrup"
      c-basic-offset 4
      indent-tabs-mode nil)

                                        ; Golang
(add-hook 'before-save-hook 'gofmt-before-save)

                                        ; Julia
(require 'julia-mode)
(add-hook 'julia-mode-hook 'julia-repl-mode)

                                        ; misc
;; (autoload 'ace-jump-mode "ace-jump-mode")

                                        ; Paredit
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
(add-hook 'slime-mode-hook                       #'enable-paredit-mode)

                                        ; Javascript
(setq js-indent-level 4)

                                        ; Json

(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))

(add-hook 'js-json-mode-hook
          (lambda ()
            (define-key js-json-mode-map (kbd "C-c C-j") #'jq-interactively)))


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

(require 'local)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format "[%b%p%%]")
 '(compilation-scroll-output 'first-error)
 '(custom-safe-themes
   '("a1b21c4d7f9f82c600967cf349f0481c3db89842e28abd91b55f4899e5b3a3ce" "abccbf10aee9804f2e5fa8fc3480b271d1653b871dcda0259804ece106c11686" default))
 '(display-time-24hr-format t)
 '(eww-download-directory "~/downloads/")
 '(markdown-xhtml-header-content
   "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />")
 '(package-selected-packages
   '(haskell-mode solarized-theme slime paredit monotropic-theme monkeytype markdown-mode magit julia-repl julia-mode jq-mode helm go-mode flucui-themes eziam-themes elpy brutalist-theme almost-mono-themes ag adoc-mode ace-jump-mode))
 '(send-mail-function 'sendmail-send-it))


(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
