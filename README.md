# emado - Emacs interface for mado

A pretty Emacs interface for
[mado](https://github.com/laserattack/mado) - a markdown-based
task/notes organizer

## Installation

Add the following to your Emacs config:

```elisp
(require 'emado)
(global-set-key (kbd "C-c e") 'emado-info)

;; Optional: customize highlighting color
(set-face-attribute 'emado-field-face nil :foreground "#dcaf79" :weight 'bold)
```
