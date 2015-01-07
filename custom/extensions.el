(defvar custom-pre-extensions
  '(
    ;; pre extension parkers go here
    )
  "List of all extensions to load before the packages.")

(defvar custom-post-extensions
  '(
    ;; post extension parkers go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function parker/init-<extension-parker>
;;
;; (defun parker/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
