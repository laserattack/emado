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

(defun emado-delete-entry-at-point ()
  "Delete entry at current line after confirmation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):")
      (let ((file (match-string 1)))
        (when (string-match "/\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)/" file)
          (let ((timestamp (match-string 1 file)))
            (if (yes-or-no-p (format "Delete entry %s? " timestamp))
                (progn
                  (emado-run (list "-r" (format "time = %s" timestamp)))
                  (let ((inhibit-read-only t))
                    (delete-region (line-beginning-position) (line-beginning-position 2)))
                  (message "Entry deleted"))
              (message "Deletion cancelled"))))))))

(defun emado-repeat-query ()
  "Repeat the last query shown in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^Query (\\(print\\|remove\\)): \\(.*\\)")
      (let ((action (match-string 1))
            (query (match-string 2))
            (args (transient-args 'emado-action-menu)))
        (message "Repeating query...")
        (let ((output (emado-run (append args (list (format "-%c" (string-to-char action)) query)))))
          (if (string-empty-p (string-trim output))
              (emado--display (format "Query (%s): %s\n\nNo entries found" action query))
            (emado--display (format "Query (%s): %s\n\n%s" action query output)))
          (message "Query repeated"))))))

(defvar emado-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'emado-next-line)
    (define-key map (kbd "p") 'emado-previous-line)
    (define-key map (kbd "q") 'emado-quit)
    (define-key map (kbd "RET") 'emado-open-file-at-point)
    (define-key map (kbd "d") 'emado-delete-entry-at-point)
    (define-key map (kbd "g") 'emado-repeat-query)
    map)
  "Keymap for emado buffer.")

(defconst emado-font-lock-keywords
  (list
   '("^\\(/[^:\n]+:[0-9]+:[0-9]+\\):" 1 'emado-field-face)
   '("\\<\\(TIME\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\):" 1 'emado-field-face)
   '("^\\(Query (print)\\):" 1 'emado-field-face)
   '("^\\(Query (remove)\\):" 1 'emado-field-face))
  "Font lock keywords for emado-mode.")

(define-derived-mode emado-mode fundamental-mode "Emado"
  "Major mode for viewing mado output."
  (read-only-mode 1)
  (use-local-map emado-mode-map)
  (setq-local font-lock-defaults '(emado-font-lock-keywords t))
  (font-lock-mode 1))

;;;###autoload
(defun emado-run (args &optional directory)
  "Run mado with ARGS (list of strings) in DIRECTORY."
  (let ((default-directory (or directory
                               emado-directory
                               default-directory)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process emado-executable nil t nil args)))))

;; emado init menu

(transient-define-suffix emado-init ()
  "Initialize main directory in current location."
  (interactive)
  (let ((args (transient-args 'emado-init-menu)))
    (emado--display (emado-run (append args (list "-i"))))
    (transient-quit-one)))

(transient-define-prefix emado-init-menu ()
  "Initialize MADO directory options."
  ["Initialize"
   ("i" "Initialize main directory" emado-init)
   ("F" "Force initialize (even if exists above)" emado-flag-force)]
  ["Misc"
   ("q" "Quit" transient-quit-one)])

;; emado action menu

(transient-define-argument emado-flag-only ()
  "Only mode: hide all fields first."
  :class 'transient-switch
  :argument "-o")

(transient-define-infix emado-flag-name ()
  "Hide NAME field."
  :class 'transient-switch
  :argument "-N")

(transient-define-infix emado-flag-time ()
  "Hide TIME field."
  :class 'transient-switch
  :argument "-T")

(transient-define-infix emado-flag-deadline ()
  "Hide DEADLINE field."
  :class 'transient-switch
  :argument "-I")

(transient-define-infix emado-flag-priority ()
  "Hide PRIORITY field."
  :class 'transient-switch
  :argument "-P")

(transient-define-infix emado-flag-status ()
  "Hide STATUS field."
  :class 'transient-switch
  :argument "-S")

(transient-define-infix emado-flag-tags ()
  "Hide TAGS field."
  :class 'transient-switch
  :argument "-A")

(transient-define-infix emado-flag-force ()
  "Force init main directory in cwd even if exists above."
  :class 'transient-switch
  :argument "-F")

(transient-define-suffix emado-print ()
  "Print entries matching query."
  (interactive)
  (let ((args (transient-args 'emado-action-menu))
        (query (read-string "Query (print): ")))
    (let ((output (emado-run (append args (list "-p" query)))))
      (if (string-empty-p (string-trim output))
          (emado--display (format "Query (print): %s\n\nNo entries found" query))
        (emado--display (format "Query (print): %s\n\n%s" query output))))
    (transient-quit-one)))

(transient-define-suffix emado-remove ()
  "Remove entries matching query."
  (interactive)
  (let ((args (transient-args 'emado-action-menu))
        (query (read-string "Query (remove): ")))
    (let ((output (emado-run (append args (list "-r" query)))))
      (if (string-empty-p (string-trim output))
          (emado--display (format "Query (remove): %s\n\nNo entries found" query))
        (emado--display (format "Query (remove): %s\n\n%s" query output))))
    (transient-quit-one)))

(transient-define-suffix emado-save-flags ()
  "Save current flags for this session."
  :transient t
  (interactive)
  (transient-set)
  (message "Flags saved for this session"))

(transient-define-suffix emado-reset-flags ()
  "Reset all flags to default."
  :transient t
  (interactive)
  (transient-reset)
  (message "All flags reset"))

(transient-define-prefix emado-action-menu ()
  "Initialize MADO directory options."
  ["Actions"
   ("p" "Print by query" emado-print)
   ("r" "Remove by query" emado-remove)]
  ["Field visibility"
   ("o" "Show only hidden fields" emado-flag-only)
   ("N" "Hide NAME field" emado-flag-name)
   ("T" "Hide TIME field" emado-flag-time)
   ("I" "Hide DEADLINE field" emado-flag-deadline)
   ("P" "Hide PRIORITY field" emado-flag-priority)
   ("S" "Hide STATUS field" emado-flag-status)
   ("A" "Hide TAGS field" emado-flag-tags)]
  ["Save/Reset"
   ("s" "Memorize all flags" emado-save-flags)
   ("r" "Reset all flags" emado-reset-flags)]
  ["Misc"
   ("q" "Quit" transient-quit-one)])

;; emado main menu

(transient-define-suffix emado-new ()
  "Create new entry."
  (interactive)
  (when (yes-or-no-p "Create new entry? ")
    (let ((args (transient-args 'emado-menu)))
      (emado--display (emado-run (append args (list "-n"))))
      (transient-quit-one))))

(transient-define-prefix emado-menu ()
  "Mado entry manager."
  ["Commands"
   ("a" "Action" emado-action-menu)
   ("n" "Create new entry" emado-new)
   ("i" "Initialize main directory" emado-init-menu)]
  ["Misc"
   ("q" "Quit" transient-quit-one)])

(provide 'emado)

;;; emado.el ends here
