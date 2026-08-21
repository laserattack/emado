;;; emado.el --- Mado entry manager for Emacs -*- lexical-binding:t; coding:utf-8 -*-

;; Package-Requires: (
;;     (emacs    "28.1")
;;     (transient "0.12"))

(require 'transient)
(require 'seq)

(defgroup emado nil
  "Mado entry manager for Emacs."
  :group 'tools)

(defcustom emado-executable "mado"
  "Path to mado executable."
  :type 'string
  :group 'emado)

(defcustom emado-auto-switch t
  "If non-nil, automatically switch to emado buffer after command finishes."
  :type 'boolean
  :group 'emado)

(defface emado-face
  '((t :foreground "#dcaf79" :weight bold))
  "Face for highlighting."
  :group 'emado)

(defconst emado-font-lock-keywords
  (list
   '("^\\(Main directory\\|Entries count\\|Statuses\\|Tags\\|Priorities\\|Hint\\):" 1 'emado-face)
   '("\\_<\\(TIME\\|MTIME\\|PATH\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\):" 1 'emado-face)
   '("^\\(Mado error [^:\n]+\\):" 1 'error)
   '("^\\(Expected format\\):" 1 'error)
   '("^\\(Error\\) at line \\([0-9]+\\), column \\([0-9]+\\):"
     (1 'error)
     (2 'line-number)
     (3 'line-number))
   '("^[^:\n]+:[0-9]+: " 0 '(face nil invisible t))
   )
  "Font lock keywords for emado-mode.")

;;; Fringe indicators for outline sections

(define-fringe-bitmap 'emado--fringe-right
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'emado--fringe-down
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(defun emado--update-fringe-indicators ()
  "Update fringe indicators for all outline headings."
  (remove-overlays (point-min) (point-max) 'emado--fringe t)
  (save-excursion
    (goto-char (point-min))
    (while (outline-next-heading)
      (let* ((beg (point))
             (hidden (outline-invisible-p (line-end-position)))
             (bitmap (if hidden 'emado--fringe-right 'emado--fringe-down))
             (ov (make-overlay beg (1+ beg) nil t)))
        (overlay-put ov 'emado--fringe t)
        (overlay-put ov 'before-string
                     (propertize " " 'display `(left-fringe ,bitmap)))))))

;;; Internal variables

(defconst emado--buffer-name "*emado*"
  "Name of the emado output buffer.")

(defconst emado--output-buffer-name " *emado-output*"
  "Name of the temporary emado process output buffer.")

(defconst emado--process-name "emado"
  "Name of the emado process.")

(defconst emado--empty-message "Nothing here but us chickens"
  "Message shown when there are no mado output.")

(defconst emado--invalid-working-dir-path-message
  "Working directory path is as real as my will to live"
  "Message shown when working directory path does not exist.")

(defconst emado--outline-regexp "^\\(Statuses\\|Tags\\|Priorities\\):"
  "Regexp for outline headings in emado buffer.")

(defconst emado--bindings
  '(;; don't show in help
    ("q" emado-quit nil)
    ("g" emado-repeat-last nil)
    ("RET" emado-go-at-point nil)
    ("o" emado-open-dir-at-point nil)
    ("C-o" emado-open-dir-at-point nil)
    ("TAB" emado-toggle-section nil)
    ("<backtab>" emado-toggle-all-sections nil)
    ("d" emado-delete-at-point nil)
    ("k" emado-delete-at-point nil)
    ("x" emado-delete-at-point nil)
    ("n" emado-next-line nil)
    ("p" emado-previous-line nil)
    ("M-n" emado-next-heading nil)
    ("M-p" emado-previous-heading nil)
    ("h" emado-menu nil)
    ;; show in help
    ("l" emado-list-menu t)
    ("c" emado-new-menu t)
    ("i" emado-init-menu t)
    ("r" emado-remove-menu t)
    ("s" emado-settings-menu t))
  "Alist of emado keybindings (key command show-in-help).")

(defconst emado--loading-messages
  '("Mixing cocktails..."
    "Kneading the dough..."
    "Watering the plants..."
    "Mining bitcoin..."
    "Walking the dog..."
    "Folding origami..."
    "Baking cookies..."
    "Stirring the cauldron..."
    "Polishing the silverware..."
    "Counting sheep..."
    "Chasing rainbows..."
    "Inflating balloons..."
    "Whistling a tune..."
    "Tying shoelaces..."
    "Skipping stones..."
    "Painting the fence..."
    "Blowing bubbles..."
    "Stacking pancakes..."
    "Watching paint dry...")
  "Random loading messages for emado buffer.")

(defvar emado--working-directory nil
  "Working directory for mado operations.
Set this once and all operations will use it.")

(defvar emado-parallel-mode nil
  "If non-nil, enable parallel processing for mado commands.")

(defvar emado--last-query nil
  "Last query used for list or remove operations.")

(defvar emado--last-args nil
  "Last mado args for repeating.")

(defvar emado--current-process nil
  "Currently running mado process, if any.")

(defvar emado--all-sections-visible t
  "Track whether all outline sections are visible.")

(defvar emado--saved-menus nil
  "List of menu commands that have been saved this session.")

;;; Custom classes for default values support

(defclass emado--default-option (transient-option)
  ((default-value :initarg :default-value :initform nil)))

(defclass emado--default-switch (transient-switch)
  ((default-value :initarg :default-value :initform nil)))

(cl-defmethod transient-init-value ((obj emado--default-option))
  (if (memq (oref transient--prefix command) emado--saved-menus)
      (cl-call-next-method)
    (let ((val (oref obj default-value)))
      (when val
        (oset obj value val)))))

(cl-defmethod transient-init-value ((obj emado--default-switch))
  (if (memq (oref transient--prefix command) emado--saved-menus)
      (cl-call-next-method)
    (let ((val (oref obj default-value)))
      (when val
        (oset obj value (oref obj argument))))))

;;; Mode

(defun emado-toggle-section ()
  "Toggle visibility of section at point."
  (interactive)
  (beginning-of-line)
  (when (looking-at emado--outline-regexp)
    (let* ((beg (point))
           (end (save-excursion
                  (forward-line 1)
                  (if (re-search-forward emado--outline-regexp nil t)
                      (match-beginning 0)
                    (point-max))))
           (has-content (save-excursion
                          (goto-char beg)
                          (forward-line 1)
                          (< (point) end))))
      (when has-content
        (outline-toggle-children)))))

(defun emado-toggle-all-sections ()
  "Toggle visibility of all outline sections."
  (interactive)
  (if emado--all-sections-visible
      (progn
        (outline-hide-body)
        (setq emado--all-sections-visible nil))
    (outline-show-all)
    (setq emado--all-sections-visible t)))

(defun emado-quit ()
  "Close emado window and kill buffer."
  (interactive)
  (kill-buffer-and-window))

(defun emado-next-line ()
  "Move to next logical line."
  (interactive)
  (forward-visible-line 1))

(defun emado-previous-line ()
  "Move to previous logical line."
  (interactive)
  (forward-visible-line -1))

(defun emado-next-heading ()
  "Move to next heading."
  (interactive)
  (outline-next-heading)
  (beginning-of-line))

(defun emado-previous-heading ()
  "Move to previous heading."
  (interactive)
  (outline-previous-heading)
  (beginning-of-line))

(defvar emado-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (bind emado--bindings)
      (define-key map (kbd (car bind)) (cadr bind)))
    map)
  "Keymap for emado buffer.")

(define-derived-mode emado-mode special-mode "Emado"
  "Major mode for viewing mado output."
  (setq-local font-lock-defaults '(emado-font-lock-keywords t))
  (setq-local outline-regexp emado--outline-regexp)
  (setq-local outline-level
              (lambda ()
                (save-excursion
                  (beginning-of-line)
                  (cond
                   ((looking-at "^Statuses:") 3)
                   ((looking-at "^Tags:") 2)
                   ((looking-at "^Priorities:") 1)
                   (t 1)))))
  (outline-minor-mode 1)
  (display-line-numbers-mode -1)
  (add-hook 'outline-view-change-hook #'emado--update-fringe-indicators nil t)
  (unless (get-buffer-window emado--buffer-name)
    (let ((keys (mapcar (lambda (bind)
                          (propertize (car bind) 'face 'emado-face))
                        (seq-filter (lambda (b) (nth 2 b)) emado--bindings))))
      (message "Commands: %s; %s to quit; %s for help"
               (mapconcat 'identity keys ", ")
               (propertize "q" 'face 'emado-face)
               (propertize "h" 'face 'emado-face)))))

;;; Core helpers

(defun emado--with-parallel-if-set (args)
  "Prepend `--parallel' to ARGS if `emado-parallel-mode' is non-nil."
  (if emado-parallel-mode
      (cons "--parallel" args)
    args))

(defun emado--delete-directory-and-string (dir)
  "Delete directory DIR after confirmation and update buffer.
Return t if directory was deleted, nil otherwise."
  (let ((inhibit-read-only t))
    (when (yes-or-no-p (format "Emado (remove directory): %s ? " dir))
      (delete-directory dir t)
      (delete-region (line-beginning-position) (line-beginning-position 2))
      (when (string-empty-p (string-trim (buffer-string)))
        (erase-buffer)
        (insert emado--empty-message)
        (goto-char (point-min)))
      (message "Directory deleted")
      t)))

(defun emado--show-buffer (buf)
  "Display BUF according to `emado-auto-switch' setting."
  (if emado-auto-switch
      (pop-to-buffer buf)
    (let ((win (get-buffer-window buf)))
      (if win
          (set-window-buffer win buf)
        (display-buffer buf)))))

(defun emado--display (output)
  "Display OUTPUT in emado buffer."
  (let ((buf (get-buffer-create emado--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (string-empty-p output)
            (insert emado--empty-message)
          (insert output))
        (goto-char (point-min))
        (emado-mode)))
    (emado--show-buffer buf)))

(defun emado--show-status (&optional message)
  "Show random loading MESSAGE in emado buffer."
  (unless message
    (setq message (seq-random-elt emado--loading-messages)))
  (let ((buf (get-buffer-create emado--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert message)
        (goto-char (point-min))
        (emado-mode)))
    (emado--show-buffer buf)))

(defun emado--check-working-directory ()
  "Check if working directory exists. Clear if not."
  (let ((dir (or emado--working-directory default-directory)))
    (if (file-exists-p dir)
        t
      (progn
        (emado--show-status emado--invalid-working-dir-path-message)
        nil))))

(defun emado--run (args &optional callback)
  "Run mado with ARGS asynchronously.
When process finishes, call CALLBACK with output string.
If no CALLBACK, just display output in emado buffer."
  (when (emado--check-working-directory)
    (when (and emado--current-process
               (process-live-p emado--current-process))
      (kill-process emado--current-process))

    (let* ((default-directory (or emado--working-directory default-directory))
           (output-buffer (generate-new-buffer emado--output-buffer-name))
           (proc (make-process
                  :name emado--process-name
                  :buffer output-buffer
                  :command `(,emado-executable ,@args)
                  :sentinel
                  (lambda (process _event)
                    (when (eq emado--current-process process)
                      (let ((output
                             (with-current-buffer (process-buffer process)
                               (prog1 (buffer-string)
                                 (kill-buffer)))))
                        (if callback
                            (funcall callback output)
                          (emado--display output)))
                      (setq emado--current-process nil))))))
      (setq emado--current-process proc))))

(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  (if emado--last-args
      (if (and emado--current-process
               (process-live-p emado--current-process))
          (message "Command is already running...")
        (emado--show-status)
        (emado--run emado--last-args))
    (message "No previous command to repeat")))

(defun emado--remove-by-query (query)
  "Remove entries matching QUERY after confirmation."
  (when (yes-or-no-p (format "Emado (remove query): %s ? " query))
    (let* ((targs (transient-args 'emado-remove-menu))
           (args (emado--with-parallel-if-set `("remove" ,@targs ,query))))
      (setq emado--last-args args)
      (emado--show-status)
      (emado--run args #'emado--display))))

(defun emado--get-parent-header ()
  "Find the nearest parent header before current line."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at "^  "))
      (forward-line -1)
      (beginning-of-line))
    (cond
     ((looking-at "^Statuses:") 'statuses)
     ((looking-at "^Tags:") 'tags)
     ((looking-at "^Priorities:") 'priorities))))

(defun emado-go-at-point ()
  "Perform action based on current line context."
  (interactive)
  (beginning-of-line)
  (cond
   ;; File paths: path/to/file.md:1:
   ((looking-at "^\\(.*\\):\\([0-9]+\\):")
    (let ((file (match-string 1)))
      (find-file-other-window file)
      (goto-char (point-min))))
   ;; Main directory: /path/to/dir
   ((looking-at "^Main directory: \\(.*\\)$")
    (let ((dir (match-string 1)))
      (dired-other-window dir)))
   ;; Entries count: COUNT
   ((looking-at "^Entries count: \\([0-9]+\\)$")
    (emado--list-all))
   ;; Contents of sections (starts with two spaces)
   ((looking-at "^  \\(.*\\): \\([0-9]+\\)")
    (let* ((value (match-string 1))
           (header (emado--get-parent-header)))
      (when (and header value)
        (let ((query (pcase header
                       ('statuses (concat "status = '" value "'"))
                       ('tags (concat "tag = '" value "'"))
                       ('priorities (concat "priority = " value)))))
          (when query
            (let* ((targs (transient-args 'emado-list-menu))
                   (args `("list" ,@targs ,query)))
              (setq emado--last-args args)
              (emado--show-status)
              (emado--run args #'emado--display)))))))))

(defun emado-delete-at-point ()
  "Delete mado entry at current line after confirmation.
For status/tag/priority lines, remove all entries matching that value.
For Entries count line, remove all entries."
  (interactive)
  (beginning-of-line)
  (cond
   ;; File paths: path/to/file.md:1:
   ((looking-at "^\\(.*\\):\\([0-9]+\\):")
    (let ((dir (file-name-directory (match-string 1))))
      (emado--delete-directory-and-string dir)))
   ;; Main directory: /path/to/dir
   ((looking-at "^Main directory: \\(.*\\)$")
    (let ((dir (match-string 1)))
      (when (emado--delete-directory-and-string dir)
        (emado-repeat-last))))
   ;; Entries count: COUNT
   ((looking-at "^Entries count: \\([0-9]+\\)$")
    (emado--remove-by-query "all"))
   ;; Contents of sections (starts with two spaces)
   ((looking-at "^  \\(.*\\): \\([0-9]+\\)")
    (let* ((value (match-string 1))
           (header (emado--get-parent-header)))
      (when (and header value)
        (let ((query (pcase header
                       ('statuses (concat "status = '" value "'"))
                       ('tags (concat "tag = '" value "'"))
                       ('priorities (concat "priority = " value)))))
          (when query
            (emado--remove-by-query query))))))))

(defun emado-open-dir-at-point ()
  "Open entry directory in dired.
For entry lines, open the entry directory.
For Main directory line, open the main directory."
  (interactive)
  (beginning-of-line)
  (cond
   ;; Entry file: path/to/file.md:1:
   ((looking-at "^\\(.*\\):\\([0-9]+\\):")
    (dired-other-window (file-name-directory (match-string 1))))
   ;; Main directory: /path/to/dir
   ((looking-at "^Main directory: \\(.*\\)$")
    (dired-other-window (match-string 1)))))

;;; Transient menus

(transient-define-suffix emado--save-options ()
  "Save current options for this session."
  :transient t
  (interactive)
  (transient-set)
  (cl-pushnew (oref transient--prefix command) emado--saved-menus)
  (message "Options saved for this session"))

(transient-define-suffix emado--reset-options ()
  "Reset all options to default."
  :transient t
  (interactive)
  (setq emado--saved-menus (delq (oref transient--prefix command) emado--saved-menus))
  (transient-reset)
  (message "All options reset"))

(transient-define-infix emado--flag-sort ()
  "Sort criteria."
  :class 'emado--default-option
  :default-value "-priority,-time"
  :argument "--sort="
  :reader (lambda (prompt _initial _history)
            (read-string "Emado (sort, +/-field,...): "))
  :prompt "Emado (sort): ")

(transient-define-infix emado--flag-ignore-case ()
  "Case-insensitive search."
  :class 'emado--default-switch
  :default-value t
  :argument "--ignore-case"
  :description "Case-insensitive")

(transient-define-infix emado--flag-hide-time ()
  "Hide time field."
  :class 'emado--default-switch
  :default-value t
  :argument "--hide-time"
  :description "Hide time")

(transient-define-infix emado--flag-hide-mtime ()
  "Hide mtime field."
  :class 'emado--default-switch
  :default-value t
  :argument "--hide-mtime"
  :description "Hide mtime")

(transient-define-infix emado--flag-hide-path ()
  "Hide path field."
  :class 'emado--default-switch
  :default-value t
  :argument "--hide-path"
  :description "Hide path")

(transient-define-infix emado--flag-template ()
  "Template name for new entry."
  :class 'transient-option
  :argument "--template="
  :reader (lambda (prompt _initial _history)
            (let* ((default-directory (or emado--working-directory default-directory))
                   (dir (string-trim
                         (shell-command-to-string
                          (concat emado-executable " info --dir"))))
                   (templates-dir (when (and dir
                                             (not (string-empty-p dir))
                                             (file-directory-p dir))
                                    (expand-file-name ".templates/" dir)))
                   (files (when (and templates-dir
                                     (file-exists-p templates-dir))
                            (directory-files templates-dir nil "\\.md$")))
                   (names (mapcar (lambda (f)
                                    (file-name-sans-extension f))
                                  files)))
              (if names
                  (completing-read prompt names nil t)
                (progn
                  (message "No templates found")
                  nil))))
  :prompt "Emado (create entry template): ")

;; ---- Init ----

(transient-define-prefix emado-init-menu ()
  "Initialize mado repository."
  ["Initialize"
   ("i" "Init" emado--init)]
  ["Main"
   ("f" "Try force init" "--force")]
  ["Save/Reset (this session)"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--init ()
  "Run 'mado init'."
  (interactive)
  (let* ((transient-current-prefix 'emado-init-menu)
         (targs (transient-args 'emado-init-menu))
         (args (emado--with-parallel-if-set (append '("init") targs))))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display))
  (transient-quit-one))

;; ---- New entry ----

(transient-define-prefix emado-new-menu ()
  "Create new mado entry."
  ["Create"
   ("c" "New entry" emado--new-entry)]
  ["Main"
   ("t" "Template" emado--flag-template)]
  ["Save/Reset (this session)"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--new-entry ()
  "Create new entry."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (emado--with-parallel-if-set (append '("new") targs))))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display))
  (transient-quit-one))

;; ---- List ----

(transient-define-prefix emado-list-menu ()
  "List mado entries."
  ["List"
   ("a" "All entries" emado--list-all)
   ("l" "List with query" emado--list-query)]
  ["Main"
   ("s" "Sort" emado--flag-sort)
   ("i" "Case-insensitive" emado--flag-ignore-case)]
  [["Hide fields"
    ("n" "Name" "--hide-name")
    ("t" "Time" emado--flag-hide-time)
    ("m" "Mtime" emado--flag-hide-mtime)
    ("d" "Deadline" "--hide-deadline")
    ("o" "Only hidden" "--only-hidden")]
   [""
    ("p" "Priority" "--hide-priority")
    ("u" "Status" "--hide-status")
    ("g" "Tags" "--hide-tags")
    ("h" "Path" emado--flag-hide-path)]]
  ["Save/Reset (this session)"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args (emado--with-parallel-if-set `("list" ,@targs ,@extra-args ,query))))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display)))

(defun emado--list-all ()
  "List all entries."
  (interactive)
  (emado--list-entries "all")
  (transient-quit-one))

(defun emado--list-query ()
  "List entries matching a custom query."
  (interactive)
  (let* ((prompt (if emado--last-query
                     (format "Emado (search query, default: %s): " emado--last-query)
                   "Emado (search query): "))
         (query (read-string prompt nil nil emado--last-query)))
    (when (string-empty-p query)
      (setq query emado--last-query))
    (when (and query (not (string-empty-p query)))
      (setq emado--last-query query)
      (emado--list-entries query)))
  (transient-quit-one))

;; ---- Remove ----

(transient-define-prefix emado-remove-menu ()
  "Remove mado entries."
  ["Remove"
   ("r" "Remove by query" emado--remove-query)]
  ["Main"
   ("i" "Case-insensitive" emado--flag-ignore-case)]
  ["Save/Reset (this session)"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--remove-query ()
  "Remove entries matching a query."
  (interactive)
  (let* ((prompt (if emado--last-query
                     (format "Emado (remove query, default: %s): " emado--last-query)
                   "Emado (remove query): "))
         (query (read-string prompt nil nil emado--last-query)))
    (when (string-empty-p query)
      (setq query emado--last-query))
    (when (and query (not (string-empty-p query)))
      (setq emado--last-query query)
      (emado--remove-by-query query)))
  (transient-quit-one))

;; ---- Info ----

(defun emado-info ()
  "Show repository info."
  (interactive)
  (let ((args (emado--with-parallel-if-set '("info"))))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display)))

;; ---- Working directory ----

(transient-define-suffix emado-set-working-directory ()
  "Set working directory for mado operations."
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Emado (working directory): " nil nil t)))
    (setq emado--working-directory dir)
    (message "Working directory set to: %s" dir)
    (emado-info)))

(transient-define-suffix emado-clear-working-directory ()
  "Clear working directory setting."
  :transient t
  (interactive)
  (setq emado--working-directory nil)
  (message "Working directory cleared, using default")
  (emado-info))

;; ---- Parallel mode ----

(transient-define-suffix emado-toggle-parallel-mode ()
  "Toggle parallel mode for mado operations."
  :transient t
  (interactive)
  (setq emado-parallel-mode (not emado-parallel-mode))
  (message "Parallel mode %s" (if emado-parallel-mode "enabled" "disabled"))
  (emado-info))

;; ---- Settings Menu ----

(transient-define-prefix emado-settings-menu ()
  "Session settings for emado."
  ["Working directory"
   (:info (lambda () (if emado--working-directory
                         (format "Current: %s (forced)" emado--working-directory)
                       (format "Current: %s" default-directory))))
   ("w" "Set directory" emado-set-working-directory)
   ("W" "Clear directory" emado-clear-working-directory)]
  ["Parallel mode"
   (:info (lambda () (if emado-parallel-mode
                         "Current: enabled"
                       "Current: disabled")))
   ("p" "Toggle" emado-toggle-parallel-mode)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;; ---- Main Menu ----

(transient-define-prefix emado-menu ()
  "Mado entry manager for Emacs."
  [["Actions"
    ("l" "List entries" emado-list-menu)
    ("c" "New entry" emado-new-menu)
    ("r" "Remove entries" emado-remove-menu)]
   ["Repository"
    ("i" "Init" emado-init-menu)
    ("h" "Info" emado-info)]]
  ["Other"
   ("s" "Global session options" emado-settings-menu)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;;###autoload
(defun emado-default-bindings ()
  "Set default global keybindings for emado."
  (interactive)
  (global-set-key (kbd "C-c e") 'emado-info))

(provide 'emado)
;;; emado.el ends here
