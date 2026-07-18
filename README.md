# emado - Emacs interface for mado

A pretty Emacs interface for
[mado](https://github.com/laserattack/mado) - a markdown-based
task/notes organizer

## Installation

Add the following to your Emacs config:

```elisp
(require 'emado)
(global-set-key (kbd "C-c e") 'emado-info)
```

## Requirements

- Emacs 28.1 or later
- `mado` executable in `PATH`
