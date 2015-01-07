(defvar custom-packages
  '(helm helm-gtags org-jira)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar custom-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function parker/init-<package-parker>
;;
(defun custom/init-custom ()
  "Initialize custom package"
                                        ; Semantic
  (setq semantic-default-submodes
        '(global-semanticdb-minor-mode
          global-semantic-mru-bookmark-mode
          global-semantic-idle-scheduler-mode
          global-semantic-idle-completions-mode
          global-semantic-idle-summary-mode))
  (semantic-mode 1)
  (require 'semantic/ia)

                                        ; Hooks
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  
)

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
