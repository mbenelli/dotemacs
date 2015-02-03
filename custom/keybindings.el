(define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)

;;; Tags related bindings
(evil-define-key 'normal helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
(evil-define-key 'normal helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)
(evil-define-key 'lisp helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
(evil-define-key 'lisp helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)

