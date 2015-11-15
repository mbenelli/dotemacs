;; Emacs configuration file

					; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(defvar needed-packages
  '(autopair
    magit
    paredit
    projectile
    slime
    markdown-mode
    yasnippet
))

(defun ensure-packages ()
  (dolist (p needed-packages)
    (unless (pkg-info-package-version p)
      (install-package p))))

(package-initialize)

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

(when (not window-system)
  (set-face-background 'vertical-border "brightwhite"))

;; (when (not window-system)
;;   (set-face-inverse-video 'mode-line nil)
;;   (set-face-foreground 'mode-line "blue")
;;   (set-face-background 'mode-line "brightwhite")
;;   (set-face-foreground 'mode-line-inactive "grey70")
;;   (set-face-background 'mode-line-inactive "brightwhite"))

                                        ; Path for local customizations.
(setq load-path (cons "~/.emacs.d/lisp" load-path))

                                        ; Keybindings

(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x C-h") 'execute-extended-command)
(global-set-key (kbd "C-x DEL") 'execute-extended-command)

(global-set-key (kbd "C-c w") 'eww)
(global-set-key (kbd "C-c m") 'mu4e)
(global-set-key (kbd "C-c i") 'erc-tls)
(global-set-key (kbd "C-c s") 'shell)
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

                                        ; mu4e
;; (setq load-path (cons "/usr/share/emacs/site-lisp/mu4e" load-path))
;; (require 'mu4e)
;; (setq
;;  mu4e-maildir "~/.mail"
;;  mu4e-sent-folder "/Sent Items"
;;  mu4e-drafts-folder "/Drafts"
;;  mu4e-trash-folder "/Deleted Items")

;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
;; (add-hook 'mu4e-view-mode-hook
;;   (lambda()
;;      ;; try to emulate some of the eww key-bindings
;;     (local-set-key (kbd "<tab>") 'shr-next-link)
;;     (local-set-key (kbd "<backtab>") 'shr-previous-link)))

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

                                        ; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ido)
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
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

                                        ; Elisp
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (auto-complete-mode 1)))

                                        ; C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "stroustrup"
      c-basic-offset 4
      indent-tabs-mode nil)

                                        ; clang-complete
;;(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (setq show-trailing-whitespace t)
;;              (setq-default ac-sources '(ac-source-semantic-raw))
;;              (ac-cc-mode-setup)
;;              (auto-complete-mode 1)
              (c-set-offset 'innamespace 0))))


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
  `((auto-complete-mode . " α")
    (yas/minor-mode . " y")
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
 '(send-mail-function (quote sendmail-send-it))
 '(w3m-enable-google-feeling-lucky nil)
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-search-engine-alist
   (quote
    (("duckduckgo" "https://duckduckgo.com/?q=%s" utf-8)
     ("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
     ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
     ("alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8)
     ("blog" "http://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8)
     ("blog-en" "http://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8)
     ("google" "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8)
     ("google-en" "http://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8)
     ("google news" "http://news.google.com/news?q=%s&ie=utf-8&oe=utf-8" utf-8)
     ("google news-en" "http://news.google.com/news?q=%s&hl=en&ie=utf-8&oe=utf-8" nil)
     ("google groups" "http://groups.google.com/groups?q=%s" nil)
     ("All the Web" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=en" nil)
     ("All the Web-ja" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=ja&cs=euc-jp" euc-japan)
     ("technorati" "http://www.technorati.com/search/%s" utf-8)
     ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8)
     ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8)
     ("goo-ja" "http://search.goo.ne.jp/web.jsp?MT=%s" euc-japan)
     ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis)
     ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil)
     ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
     ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil)
     ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
     ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan)
     ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice" nil)
     ("waei" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=je" euc-japan)
     ("eiwa" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=ej" nil)
     ("kokugo" "http://dictionary.goo.ne.jp/search.php?MT=%s&kind=jn" euc-japan)
     ("eiei" "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67" nil)
     ("amazon" "http://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s")
     ("amazon-ja" "http://www.amazon.co.jp/gp/search?__mk_ja_JP=%%83J%%83%%5E%%83J%%83i&url=search-alias%%3Daps&field-keywords=%s" shift_jis)
     ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil)
     ("en.wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=%s" nil)
     ("de.wikipedia" "http://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8)
     ("ja.wikipedia" "http://ja.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
     ("msdn" "http://search.msdn.microsoft.com/search/default.aspx?query=%s" nil)
     ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil))))
 '(w3m-session-crash-recovery nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
