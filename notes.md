## Alternative keybindigs.

The following key binding are good for dvorak layout:

    (define-key key-translation-map [?\C-x] [?\C-u])
    (define-key key-translation-map [?\C-u] [?\C-x])
    (global-set-key (kbd "C-h") 'delete-backward-char)
    (global-set-key (kbd "C-x C-h") 'execute-extended-command)
    (global-set-key (kbd "C-x DEL") 'execute-extended-command)

But they makes me unable to use every instance of emacs that doesn't
provide them :) so I decided to keep the standards.
