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

(defvar emado-working-directory nil
  "Working directory for mado operations.
Set this once and all operations will use it.")

;;; Custom classes for default values support

(defclass emado--default-option (transient-option)
  ((default-value :initarg :default-value :initform nil)))

(cl-defmethod transient-init-value ((obj emado--default-option))
  (let ((val (oref obj default-value)))
    (when val
      (oset obj value val))))

(defclass emado--default-switch (transient-switch)
  ((default-value :initarg :default-value :initform nil)))

(cl-defmethod transient-init-value ((obj emado--default-switch))
  (let ((val (oref obj default-value)))
    (when val
      (oset obj value (oref obj argument)))))

;;; Buffer & Mode

(defun emado-quit ()
  "Close emado window and kill buffer."
  (interactive)
  (quit-window t))

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
    (define-key map (kbd "q") #'emado-quit)
    (define-key map (kbd "g") #'emado-repeat-last)
    (define-key map (kbd "RET") #'emado-open-entry-at-point)
    (define-key map (kbd "d") #'emado-delete-entry-at-point)
    (define-key map (kbd "k") #'emado-delete-entry-at-point)
    (define-key map (kbd "n") 'emado-next-line)
    (define-key map (kbd "p") 'emado-previous-line)
    (define-key map (kbd "h") #'emado-menu)
    (define-key map (kbd "l") #'emado-list-menu)
    (define-key map (kbd "e") #'emado-new-menu)
    (define-key map (kbd "i") #'emado-init-menu)
    (define-key map (kbd "r") #'emado-remove-menu)
    (define-key map (kbd "w") #'emado-set-working-directory)
    (define-key map (kbd "W") #'emado-clear-working-directory)
    map)
  "Keymap for emado buffer.")

(defface emado-face
  '((t :foreground "#dcaf79" :weight bold))
  "Face for highlighting field names."
  :group 'emado)

(defconst emado-font-lock-keywords
  (list
   ;; Section headers: Main directory:, Entries count:, Statuses:, Tags:
   '("^\\(Main directory\\|Entries count\\|Statuses\\|Tags\\|Hint\\):" 1 'emado-face)
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
  (let ((default-directory (or emado-working-directory default-directory)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process emado-executable nil t nil args)))))

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

(defun emado-delete-entry-at-point ()
  "Delete mado entry at current line after confirmation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):")
      (let ((file (match-string 1))
            (dir (file-name-directory (match-string 1))))
        (when (and dir (yes-or-no-p (format "Delete entry %s? " dir)))
          (delete-directory dir t)
          (let ((inhibit-read-only t))
            (delete-region (line-beginning-position) (line-beginning-position 2)))
          (message "Entry deleted"))))))

(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  (if emado--last-args
      (progn
        (emado--display (emado--run emado--last-args))
        (message "Repeated last command"))
    (message "No previous command to repeat")))

;;; Transient menus

(transient-define-suffix emado-save-options ()
  "Save current options for this session."
  :transient t
  (interactive)
  (transient-set)
  (message "Options saved for this session"))

(transient-define-suffix emado-reset-options ()
  "Reset all options to default."
  :transient t
  (interactive)
  (transient-reset)
  (message "All options reset"))

(transient-define-infix emado--flag-sort ()
  "Sort criteria (-s)."
  :class 'emado--default-option
  :default-value "-time"
  :argument "--sort="
  :reader (lambda (prompt _initial _history)
            (read-string (concat prompt "(+/-field,...): ")))
  :prompt "Sort: ")

(transient-define-infix emado--flag-ignore-case ()
  "Case-insensitive search (-i)."
  :class 'emado--default-switch
  :default-value t
  :argument "--ignore-case"
  :description "Case-insensitive")

(transient-define-infix emado--flag-template ()
  "Template name for new entry (-t)."
  :class 'transient-option
  :argument "--template="
  :reader (lambda (_prompt _initial _history)
            (read-string "Template: "))
  :prompt "Template: ")

;; ---- Init ----

(transient-define-prefix emado-init-menu ()
  "Initialize mado repository."
  ["Initialize"
   ("i" "Init" emado--init)]
  ["Options"
   ("f" "Try force init" "--force")]
  ["Save/Reset"
   ("S" "Save current options" emado-save-options)
   ("R" "Reset all options" emado-reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--init ()
  "Run 'mado init'."
  (interactive)
  (let* ((transient-current-prefix 'emado-init-menu)
         (targs (transient-args 'emado-init-menu))
         (args (append '("init" "--abs-path") targs)))
    (emado--display (emado--run args))
    (setq emado--last-args args))
  (transient-quit-one))

;; ---- New entry ----

(transient-define-prefix emado-new-menu ()
  "Create new mado entry."
  ["Create"
   ("e" "New entry" emado--new-entry)]
  ["Options"
   ("t" "Template" emado--flag-template)]
  ["Save/Reset"
   ("S" "Save current options" emado-save-options)
   ("R" "Reset all options" emado-reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--new-entry (&optional args)
  "Create new entry with optional ARGS."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (append '("new" "--abs-path") targs)))
    (emado--display (emado--run args))
    (setq emado--last-args args))
  (transient-quit-one))

;; ---- List ----

(transient-define-prefix emado-list-menu ()
  "List mado entries."
  ["List"
   ("a" "All entries" emado--list-all)
   ("l" "List with query" emado--list-query)]
  ["Main options"
   ("s" "Sort" emado--flag-sort)
   ("i" "Case-insensitive" emado--flag-ignore-case)]
  ["Hide fields options"
   ("o" "Only hidden" "--only-hidden")
   ("n" "Hide name" "--hide-name")
   ("t" "Hide time" "--hide-time")
   ("d" "Hide deadline" "--hide-deadline")
   ("p" "Hide priority" "--hide-priority")
   ("u" "Hide status" "--hide-status")
   ("g" "Hide tags" "--hide-tags")
   ("h" "Hide path" "--hide-path")]
  ["Save/Reset"
   ("S" "Save current options" emado-save-options)
   ("R" "Reset all options" emado-reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args `("list" "--abs-paths" ,@targs ,@extra-args ,query)))
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
  ["Remove"
   ("r" "Remove by query" emado--remove-query)]
  ["Options"
   ("i" "Case-insensitive" emado--flag-ignore-case)]
  ["Save/Reset"
   ("S" "Save current options" emado-save-options)
   ("R" "Reset all options" emado-reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--remove-query ()
  "Remove entries matching a query."
  (interactive)
  (let* ((query (read-string "Remove entries matching: "))
         (targs (transient-args 'emado-remove-menu))
         (args `("remove" "--abs-paths" ,@targs ,query)))
    (when (yes-or-no-p (format "Really delete entries matching '%s'? " query))
      (emado--display (emado--run args))
      (setq emado--last-args args)))
  (transient-quit-one))

;; ---- Info ----

(defun emado-info ()
  "Show repository info."
  (interactive)
  (let ((args '("info" "--abs-path")))
    (emado--display (emado--run args))
    (setq emado--last-args args)))

;; ---- Working directory ----

(transient-define-suffix emado-set-working-directory ()
  "Set working directory for mado operations."
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Working directory: " nil nil t)))
    (setq emado-working-directory dir)
    (message "Working directory set to: %s" dir)
    (emado-info)))

(transient-define-suffix emado-clear-working-directory ()
  "Clear working directory setting."
  :transient t
  (interactive)
  (setq emado-working-directory nil)
  (message "Working directory cleared, using default")
  (emado-info))

;; ---- Main Menu ----

(transient-define-prefix emado-menu ()
  "Mado entry manager for Emacs."
  ["Working directory"
   (:info (lambda () (if emado-working-directory
                         (format "Current: %s (forced)" emado-working-directory)
                       (format "Current: %s" default-directory))))
   ("w" "Set directory"     emado-set-working-directory)
   ("W" "Clear directory"   emado-clear-working-directory)]
  [["Actions"
    ("l" "List entries"   emado-list-menu)
    ("e" "New entry"      emado-new-menu)
    ("r" "Remove entries" emado-remove-menu)]
   ["Repository"
    ("i" "Init"           emado-init-menu)
    ("h" "Info"           emado-info)]]
  ["Quit"
   ("q" "Quit"           transient-quit-one)])

;;;###autoload
(defun emado ()
  "Start the mado entry manager."
  (interactive)
  (emado-menu))

(provide 'emado)
;;; emado.el ends here
