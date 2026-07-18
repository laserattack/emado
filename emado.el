;;; emado.el --- Mado entry manager for Emacs -*- lexical-binding:t; coding:utf-8 -*-

;; Package-Requires: (
;;     (emacs    "28.1")
;;     (transient "0.12"))

(require 'transient)

(defgroup emado nil
  "Mado entry manager for Emacs."
  :group 'tools)

(defcustom emado-executable "mado"
  "Path to mado executable."
  :type 'string
  :group 'emado)

;;; Buffer & Mode

(defun emado-next-line ()
  "Move to next logical line."
  (interactive)
  (forward-line 1))

(defun emado-previous-line ()
  "Move to previous logical line."
  (interactive)
  (forward-line -1))

(defvar emado-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'emado-repeat-last)
    (define-key map (kbd "RET") #'emado-open-entry-at-point)
    (define-key map (kbd "n") 'emado-next-line)
    (define-key map (kbd "p") 'emado-previous-line)
    (define-key map (kbd "h") #'emado-menu)
    map)
  "Keymap for emado buffer.")

(defface emado-face
  '((t :foreground "#dcaf79" :weight bold))
  "Face for highlighting field names."
  :group 'emado)

(defconst emado-font-lock-keywords
  (list
   ;; Section headers: Main directory:, Entries count:, Statuses:, Tags:
   '("^\\(Main directory\\|Entries count\\|Statuses\\|Tags\\):" 1 'emado-face)
   ;; Field labels: TIME:[...], NAME:[...], PRIORITY:[...], etc.
   '("\\_<\\(TIME\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\):" 1 'emado-face)
   ;; File paths: path/to/MAIN.md:1:
   '("^\\([^:\n]+\\):[0-9]+\\:" 1 'emado-face)
   '("^\\(Mado error [^:\n]+\\):" 1 'emado-face)
   )
  "Font lock keywords for emado-mode.")

(define-derived-mode emado-mode special-mode "Emado"
  "Major mode for viewing mado output."
  (setq-local font-lock-defaults '(emado-font-lock-keywords t)))

;;; Core helpers

(defvar emado--last-args nil
  "Last mado args for repeating.")

(defun emado--run (args)
  "Run mado with ARGS (list of strings) and return output string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process emado-executable nil t nil args))))

(defun emado--display (output)
  "Display OUTPUT in emado buffer."
  (let ((buf (get-buffer-create "*emado*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (emado-mode)))
    (pop-to-buffer buf)))

(defun emado-open-entry-at-point ()
  "Open mado entry at current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):")
      (let ((file (match-string 1)))
        (find-file-other-window file)
        (goto-char (point-min))))))

(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  (if emado--last-args
      (progn
        (emado--display (emado--run emado--last-args))
        (message "Repeated last emado command"))
    (message "No previous emado command to repeat")))

;;; Transient menus

;; ---- Init ----

(transient-define-prefix emado-init-menu ()
  "Initialize mado repository."
  [["Initialize"
    ("i" "Init here" emado--init-here)
    ("F" "Force init here" emado--init-force)]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defun emado--init-here ()
  "Run 'mado init'."
  (interactive)
  (let ((args '("init")))
    (emado--display (emado--run args))
    (setq emado--last-args args))
  (transient-quit-one))

(defun emado--init-force ()
  "Run 'mado init --force'."
  (interactive)
  (let ((args '("init" "--force")))
    (emado--display (emado--run args))
    (setq emado--last-args args))
  (transient-quit-one))

;; ---- New entry ----

(transient-define-infix emado--flag-template ()
  "Template for new entry (-t)."
  :class 'transient-option
  :argument "-t"
  :reader #'transient-read-file
  :prompt "Template: ")

(transient-define-prefix emado-new-menu ()
  "Create new mado entry."
  [["Create"
    ("n" "New entry" emado--new-entry)]
   ["Options"
    ("t" "Template" emado--flag-template)
    ("a" "Absolute paths" "-a")]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defun emado--new-entry (&optional args)
  "Create new entry with optional ARGS."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (append '("new") targs)))
    (emado--display (emado--run args))
    (setq emado--last-args args))
  (transient-quit-one))

;; ---- List ----

(transient-define-infix emado--flag-sort ()
  "Sort criteria (-s)."
  :class 'transient-option
  :argument "-s"
  :reader (lambda (prompt _initial _history)
            (read-string (concat prompt "(+/-field,...): ")))
  :prompt "Sort: ")

(transient-define-prefix emado-list-menu ()
  "List mado entries."
  [["List"
    ("a" "All entries" emado--list-all)
    ("l" "List with query" emado--list-query)]
   ["Options"
    ("s" "Sort" emado--flag-sort)
    ("A" "Absolute paths" "-a")
    ("i" "Case-insensitive" "-i")]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args `("list" ,@targs ,@extra-args ,query)))
    (emado--display (emado--run args))
    (setq emado--last-args args)))

(defun emado--list-all ()
  "List all entries."
  (interactive)
  (emado--list-entries "all")
  (transient-quit-one))

(defun emado--list-query ()
  "List entries matching a custom query."
  (interactive)
  (let ((query (read-string "Query: ")))
    (emado--list-entries query))
  (transient-quit-one))

;; ---- Remove ----

(transient-define-prefix emado-remove-menu ()
  "Remove mado entries."
  [["Remove"
    ("r" "Remove by query" emado--remove-query)]
   ["Options"
    ("a" "Absolute paths" "-a")
    ("i" "Case-insensitive" "-i")]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defun emado--remove-query ()
  "Remove entries matching a query."
  (interactive)
  (let* ((query (read-string "Remove entries matching: "))
         (targs (transient-args 'emado-remove-menu))
         (args `("remove" ,@targs ,query)))
    (when (yes-or-no-p (format "Really delete entries matching '%s'? " query))
      (emado--display (emado--run args))
      (setq emado--last-args args)))
  (transient-quit-one))

;; ---- Info ----

(defun emado-info ()
  "Show repository info."
  (interactive)
  (let ((args '("info")))
    (emado--display (emado--run args))
    (setq emado--last-args args)))

;; ---- Main Menu ----

(transient-define-prefix emado-menu ()
  "Mado entry manager for Emacs."
  [["Actions"
    ("l" "List entries"   emado-list-menu)
    ("n" "New entry"      emado-new-menu)
    ("r" "Remove entries" emado-remove-menu)]
   ["Repository"
    ("i" "Init"           emado-init-menu)
    ("h" "Info"           emado-info)]
   ["Quit"
    ("q" "Quit"           transient-quit-one)]])

;;;###autoload
(defun emado ()
  "Start the mado entry manager."
  (interactive)
  (emado-menu))

(provide 'emado)
;;; emado.el ends here
