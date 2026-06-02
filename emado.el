;;; emado.el --- Mado entry manager for Emacs -*- lexical-binding:t; coding:utf-8 -*-

;; Package-Requires: (
;;     (emacs        "28.1")
;;     (transient    "0.12"))

(require 'transient)

(defgroup emado nil
  "Mado entry manager for Emacs."
  :group 'tools)

(defcustom emado-executable "mado"
  "Path to mado executable."
  :type 'string
  :group 'emado)

(defcustom emado-directory nil
  "Default project directory for mado. If nil, use current directory."
  :type '(choice (const :tag "Current directory" nil)
                 (directory :tag "Project root"))
  :group 'emado)

(defface emado-field-face
  '((t :foreground "unspecified" :weight normal))
  "Face for highlighting field names and paths."
  :group 'emado)

(defun emado-next-line ()
  "Move to next logical line."
  (interactive)
  (forward-line 1))

(defun emado-previous-line ()
  "Move to previous logical line."
  (interactive)
  (forward-line -1))

(defun emado-quit ()
  "Close emado window and bury buffer."
  (interactive)
  (quit-window))

(defun emado-open-file-at-point ()
  "Open file at current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):")
      (let ((file (match-string 1))
            (line (string-to-number (match-string 2)))
            (col (string-to-number (match-string 3))))
        (find-file-other-window file)
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char (1- col))))))

(defvar emado-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'emado-next-line)
    (define-key map (kbd "p") 'emado-previous-line)
    (define-key map (kbd "q") 'emado-quit)
    (define-key map (kbd "RET") 'emado-open-file-at-point)
    map)
  "Keymap for emado buffer.")

(defconst emado-font-lock-keywords
  (list
   '("^\\(/[^:\n]+:[0-9]+:[0-9]+:\\)" 1 'emado-field-face)
   '("\\<\\(TIME\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\):" 1 'emado-field-face))
  "Font lock keywords for emado-mode.")

(define-derived-mode emado-mode fundamental-mode "Emado"
  "Major mode for viewing mado output."
  (read-only-mode 1)
  (use-local-map emado-mode-map)
  (setq-local font-lock-defaults '(emado-font-lock-keywords t))
  (font-lock-mode 1))

(defun emado--display (output)
  "Display OUTPUT."
  (let ((buf (get-buffer-create "*emado*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (emado-mode)))
    (pop-to-buffer buf)))

;;;###autoload
(defun emado-run (args &optional directory)
  "Run mado with ARGS (list of strings) in DIRECTORY."
  (let ((default-directory (or directory
                               emado-directory
                               default-directory)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process emado-executable nil t nil args)))))

;; Transient flags

(transient-define-argument emado-flag-only ()
  "Only mode: hide all fields first."
  :class 'transient-switch
  :argument "-o"
  :description "only mode")

(transient-define-infix emado-flag-name ()
  "Hide NAME field."
  :class 'transient-switch
  :argument "-N"
  :description "hide NAME"
  :transient t)

(transient-define-infix emado-flag-time ()
  "Hide TIME field."
  :class 'transient-switch
  :argument "-T"
  :description "hide TIME"
  :transient t)

(transient-define-infix emado-flag-deadline ()
  "Hide DEADLINE field."
  :class 'transient-switch
  :argument "-I"
  :description "hide DEADLINE"
  :transient t)

(transient-define-infix emado-flag-priority ()
  "Hide PRIORITY field."
  :class 'transient-switch
  :argument "-P"
  :description "hide PRIORITY"
  :transient t)

(transient-define-infix emado-flag-status ()
  "Hide STATUS field."
  :class 'transient-switch
  :argument "-S"
  :description "hide STATUS"
  :transient t)

(transient-define-infix emado-flag-tags ()
  "Hide TAGS field."
  :class 'transient-switch
  :argument "-A"
  :description "hide TAGS"
  :transient t)

(transient-define-suffix emado-init ()
  "Initialize main directory in current location."
  :description "init main directory"
  (interactive)
  (let ((output (emado-run (list "-i"))))
    (emado--display output)
    (transient-quit-one)))

(transient-define-suffix emado-new ()
  "Create new entry."
  :description "new entry"
  (interactive)
  (let ((output (emado-run (list "-n"))))
    (emado--display output)
    (transient-quit-one)))

(transient-define-suffix emado-print-suffix ()
  "Print entries matching query."
  :description "print by query"
  (interactive)
  (let ((args (transient-args 'emado-menu))
        (query (read-string "Query (print): ")))
    (emado--display (emado-run (append args (list "-p" query))))
    (transient-quit-one)))

(transient-define-suffix emado-remove-suffix ()
  "Romove entries matching query."
  :description "remove by query"
  (interactive)
  (let ((args (transient-args 'emado-menu))
        (query (read-string "Query (remove): ")))
    (emado--display (emado-run (append args (list "-r" query))))
    (transient-quit-one)))

;; Transient menu

(transient-define-prefix emado-menu ()
  "Mado entry manager."
   ["Action"
    ("p" "print by query" emado-print-suffix)
    ("r" "remove by query" emado-remove-suffix)
    ("i" "init main directory" emado-init)
    ("n" "new entry" emado-new)]
   ["Field visibility"
    ("o" "show only hidden fields" emado-flag-only)
    ("N" "hide NAME field" emado-flag-name)
    ("T" "hide TIME field" emado-flag-time)
    ("I" "hide DEADLINE field" emado-flag-deadline)
    ("P" "hide PRIORITY field" emado-flag-priority)
    ("S" "hide STATUS field" emado-flag-status)
    ("A" "hide TAGS field" emado-flag-tags)]
  ["Misc"
   ("q" "quit" transient-quit-one)])

(provide 'emado)

;;; emado.el ends here
