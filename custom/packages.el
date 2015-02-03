(defvar custom-packages
  '(helm helm-gtags)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar custom-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function custom/init-<package>
;;
(defun custom/init-helm-gtags ()
  (evil-define-key 'normal helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
  (evil-define-key 'normal helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  )
