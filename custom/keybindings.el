(define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)

(evil-global-set-key 'normal "s" 'evil-forward-char)
(evil-global-set-key 'normal "h" 'evil-backward-char)
(evil-global-set-key 'normal "t" 'evil-next-visual-line)
(evil-global-set-key 'normal "n" 'evil-previous-visual-line)
(evil-global-set-key 'normal "j" 'spacemacs/anzu-evil-search-next)
(evil-global-set-key 'normal "k" 'spacemacs/anzu-evil-search-previous)
(evil-global-set-key 'normal "l" 'evil-substitute)

(evil-define-key 'normal helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
(evil-define-key 'normal helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)

(evil-global-set-key 'lisp "s" 'evil-forward-char)
(evil-global-set-key 'lisp "h" 'evil-backward-char)
(evil-global-set-key 'lisp "t" 'evil-next-visual-line)
(evil-global-set-key 'lisp "n" 'evil-previous-visual-line)
(evil-global-set-key 'lisp "j" 'spacemacs/anzu-evil-search-next)
(evil-global-set-key 'lisp "k" 'spacemacs/anzu-evil-search-previous)
(evil-global-set-key 'lisp "l" 'evil-substitute)

(evil-define-key 'lisp helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
(evil-define-key 'lisp helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)

