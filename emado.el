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
   '("^\\(Main directory\\|Entries count\\):" 1 'emado-face)
   '("^\\(Statuses ([0-9]+)\\):" 1 'emado-face)
   '("^\\(Tags ([0-9]+)\\):" 1 'emado-face)
   '("^\\(Priorities ([0-9]+)\\):" 1 'emado-face)
   '("\\_<\\(TIME\\|MTIME\\|PATH\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\):" 1 'emado-face)
   '("^\\(.*\\):\\([0-9]+\\):\\(?: \\|$\\)" 0 '(face nil invisible t))
   )
  "Font lock keywords for emado-mode.")

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

(defconst emado--outline-regexp "^\\(Statuses ([0-9]+)\\|Tags ([0-9]+)\\|Priorities ([0-9]+)\\):"
  "Regexp for outline headings in emado buffer.")

(defconst emado--bindings
  '(
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
  "Kill the Emado buffer and close its window if it is not the only one."
  (interactive)
  (if (one-window-p t)
      (kill-buffer (current-buffer))
    (kill-buffer-and-window)))

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
                   ((looking-at "^Statuses ([0-9]+):") 3)
                   ((looking-at "^Tags ([0-9]+):") 2)
                   ((looking-at "^Priorities ([0-9]+):") 1)
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
  "Return t if the working path exists, otherwise show a diagnostic and return nil."
  (let ((dir (or emado--working-directory default-directory)))
    (if (file-exists-p dir)
        t
      (progn
        (emado--show-status emado--invalid-working-dir-path-message)
        nil))))

(defun emado--run (args)
  "Run Mado asynchronously with ARGS and display its output."
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
                        (emado--display output))
                      (setq emado--current-process nil))))))
      (setq emado--current-process proc))))

(defun emado--execute (args)
  "Run ARGS and remember them for repetition."
  (setq emado--last-args args)
  (emado--show-status)
  (emado--run args))

(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  (if emado--last-args
      (if (and emado--current-process
               (process-live-p emado--current-process))
          (message "Command is already running...")
        (emado--execute emado--last-args))
    (message "No previous command to repeat")))

(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args `("ls" ,@targs ,@extra-args ,query)))
    (emado--execute args)))

(defun emado--remove-by-query (query)
  "Remove entries matching QUERY after confirmation."
  (when (yes-or-no-p (format "Emado (remove query): %s ? " query))
    (let* ((targs (transient-args 'emado-remove-menu))
           (args `("rm" "-format" "path" ,@targs ,query)))
      (emado--execute args))))

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
  :argument "-sort="
  :reader (lambda (prompt _initial _history)
            (read-string "Emado (sort, +/-field,...): "))
  :prompt "Emado (sort): ")

(transient-define-infix emado--flag-hide-time ()
  "Hide time field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-time"
  :description "Hide time")

(transient-define-infix emado--flag-hide-mtime ()
  "Hide mtime field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-mtime"
  :description "Hide mtime")

(transient-define-infix emado--flag-hide-path ()
  "Hide path field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-path"
  :description "Hide path")

(defun emado--template-names ()
  "Return names from the template directory selected by the current Mado."
  (let ((default-directory (or emado--working-directory default-directory)))
    (with-temp-buffer
      (when (zerop (process-file emado-executable nil (list t nil) nil
                                 "info" "-only-dir"))
        (let* ((root (string-trim-right (buffer-string) "[\r\n]+"))
               (templates-dir
                (and (not (string-empty-p root))
                     (seq-find #'file-directory-p
                               (mapcar (lambda (name) (expand-file-name name root))
                                       '("templates" ".templates" "_templates"))))))
          (when templates-dir
            (mapcar #'file-name-base
                    (seq-filter #'file-regular-p
                                (directory-files templates-dir t "\\.md$")))))))))

(transient-define-infix emado--flag-template ()
  "Template name for new entry."
  :class 'transient-option
  :argument "-template="
  :reader (lambda (prompt _initial _history)
            (let ((names (emado--template-names)))
              (if names
                  (completing-read prompt names nil t)
                (message "No templates found")
                nil)))
  :prompt "Emado (create entry template): ")

(transient-define-infix emado--flag-suffix ()
  "Set an optional suffix for the entry directory name."
  :class 'transient-option
  :argument "-suffix="
  :reader (lambda (prompt initial history)
            (let ((suffix (read-string prompt (or initial "-") history)))
              (unless (member suffix '("" "-"))
                suffix)))
  :prompt "Emado (entry suffix): ")

;;; Init menu

(defun emado--init ()
  "Run 'mado init'."
  (interactive)
  (let* ((transient-current-prefix 'emado-init-menu)
         (targs (transient-args 'emado-init-menu))
         (args (cons "init" targs)))
    (emado--execute args))
  (transient-quit-one))

(transient-define-prefix emado-init-menu ()
  "Initialize mado repository."
  ["Initialize"
   ("i" "Init" emado--init)]
  ["Main"
   ("f" "Try force init" "-force")]
  [["Save/Reset options"
    ("C-s" "Save" emado--save-options)]
   [""
    ("C-r" "Reset" emado--reset-options)]]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;; New entry menu

(defun emado--new-entry ()
  "Create new entry."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (cons "new" targs)))
    (emado--execute args))
  (transient-quit-one))

(transient-define-prefix emado-new-menu ()
  "Create new mado entry."
  ["Create"
   ("c" "New entry" emado--new-entry)]
  ["Main"
   ("t" "Template" emado--flag-template)
   ("s" "Suffix" emado--flag-suffix)]
  [["Save/Reset options"
    ("C-s" "Save" emado--save-options)]
   [""
    ("C-r" "Reset" emado--reset-options)]]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--read-query (label initial)
  "Read a query for LABEL with optional INITIAL text."
  (let* ((prompt (if (and emado--last-query (not initial))
                     (format "Emado (%s, default: %s): " label emado--last-query)
                   (format "Emado (%s): " label)))
         (query (read-string prompt initial emado--last-query)))
    (when (string-empty-p query)
      (setq query emado--last-query))
    (when (and query (not (string-empty-p query)))
      (setq emado--last-query query))))

;;; List menu

(defun emado--list-all ()
  "List all entries."
  (interactive)
  (emado--list-entries "all")
  (transient-quit-one))

(defun emado--list-query (&optional initial)
  "List entries matching a custom query.
If INITIAL is non-nil, use it as initial input."
  (interactive)
  (when-let* ((query (emado--read-query "search query" initial)))
    (emado--list-entries query))
  (transient-quit-one))

(defun emado-list-fuzzy-any ()
  "Fuzzy list by any."
  (interactive)
  (emado--list-query "any ~~ "))

(defun emado-list-fuzzy-status ()
  "Fuzzy list by status."
  (interactive)
  (emado--list-query "status ~~ "))

(defun emado-list-fuzzy-name ()
  "Fuzzy list by name."
  (interactive)
  (emado--list-query "name ~~ "))

(defun emado-list-fuzzy-tag ()
  "Fuzzy list by tag."
  (interactive)
  (emado--list-query "tag ~~ "))

(defun emado-list-fuzzy-priority ()
  "Fuzzy list by priority."
  (interactive)
  (emado--list-query "priority ~~ "))

(defun emado-list-fuzzy-deadline ()
  "Fuzzy list by deadline."
  (interactive)
  (emado--list-query "deadline ~~ "))

(defun emado-list-fuzzy-path ()
  "Fuzzy list by path."
  (interactive)
  (emado--list-query "path ~~ "))

(defun emado-list-fuzzy-time ()
  "Fuzzy list by time."
  (interactive)
  (emado--list-query "time ~~ "))

(transient-define-prefix emado-list-menu ()
  "List mado entries."
  [["List"
    ("l" "Query" emado--list-query)]
   [""
    ("a" "All entries" emado--list-all)]]
  [["Quick fuzzy list"
    ("fa" "Any" emado-list-fuzzy-any)
    ("fs" "Status" emado-list-fuzzy-status)]
   [""
    ("fn" "Name" emado-list-fuzzy-name)
    ("ft" "Tag" emado-list-fuzzy-tag)]
   [""
    ("fp" "Priority" emado-list-fuzzy-priority)
    ("fd" "Deadline" emado-list-fuzzy-deadline)]
   [""
    ("fi" "Time" emado-list-fuzzy-time)
    ("fh" "Path" emado-list-fuzzy-path)]]
  ["Main"
   ("s" "Sort" emado--flag-sort)]
  [["Hide fields"
    ("N" "Name" "-hide-name")
    ("T" "Time" emado--flag-hide-time)
    ("M" "Mtime" emado--flag-hide-mtime)
    ("D" "Deadline" "-hide-deadline")
    ("O" "Only hidden" "-only-hidden")]
   [""
    ("P" "Priority" "-hide-priority")
    ("S" "Status" "-hide-status")
    ("G" "Tags" "-hide-tags")
    ("H" "Path" emado--flag-hide-path)]]
  [["Save/Reset options"
    ("C-s" "Save" emado--save-options)]
   [""
    ("C-r" "Reset" emado--reset-options)]]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;; Remove menu

(defun emado--remove-all ()
  "Remove all entries."
  (interactive)
  (emado--remove-by-query "all")
  (transient-quit-one))

(defun emado--remove-query (&optional initial)
  "Remove entries matching a custom query.
If INITIAL is non-nil, use it as initial input."
  (interactive)
  (when-let* ((query (emado--read-query "remove query" initial)))
    (emado--remove-by-query query))
  (transient-quit-one))

(defun emado-remove-fuzzy-any ()
  "Fuzzy remove by any."
  (interactive)
  (emado--remove-query "any ~~ "))

(defun emado-remove-fuzzy-status ()
  "Fuzzy remove by status."
  (interactive)
  (emado--remove-query "status ~~ "))

(defun emado-remove-fuzzy-name ()
  "Fuzzy remove by name."
  (interactive)
  (emado--remove-query "name ~~ "))

(defun emado-remove-fuzzy-tag ()
  "Fuzzy remove by tag."
  (interactive)
  (emado--remove-query "tag ~~ "))

(defun emado-remove-fuzzy-priority ()
  "Fuzzy remove by priority."
  (interactive)
  (emado--remove-query "priority ~~ "))

(defun emado-remove-fuzzy-deadline ()
  "Fuzzy remove by deadline."
  (interactive)
  (emado--remove-query "deadline ~~ "))

(defun emado-remove-fuzzy-path ()
  "Fuzzy remove by path."
  (interactive)
  (emado--remove-query "path ~~ "))

(defun emado-remove-fuzzy-time ()
  "Fuzzy remove by time."
  (interactive)
  (emado--remove-query "time ~~ "))

(transient-define-prefix emado-remove-menu ()
  "Remove mado entries."
  [["Remove"
    ("r" "Query" emado--remove-query)]
   [""
    ("a" "All entries" emado--remove-all)]]
  [["Quick fuzzy remove"
    ("fa" "Any" emado-remove-fuzzy-any)
    ("fs" "Status" emado-remove-fuzzy-status)]
   [""
    ("fn" "Name" emado-remove-fuzzy-name)
    ("ft" "Tag" emado-remove-fuzzy-tag)]
   [""
    ("fp" "Priority" emado-remove-fuzzy-priority)
    ("fd" "Deadline" emado-remove-fuzzy-deadline)]
   [""
    ("fi" "Time" emado-remove-fuzzy-time)
    ("fh" "Path" emado-remove-fuzzy-path)]]
  [["Save/Reset options"
    ("C-s" "Save" emado--save-options)]
   [""
    ("C-r" "Reset" emado--reset-options)]]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;; Output row actions

(defun emado--get-parent-header ()
  "Find the nearest parent header before current line."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at "^  "))
      (forward-line -1)
      (beginning-of-line))
    (cond
     ((looking-at "^Statuses ([0-9]+):") 'statuses)
     ((looking-at "^Tags ([0-9]+):") 'tags)
     ((looking-at "^Priorities ([0-9]+):") 'priorities))))

(defun emado--row-at-point ()
  "Classify the current output line and extract its path or query."
  (beginning-of-line)
  (cond
   ((looking-at "^\\(.*\\):\\([0-9]+\\):\\(?: \\|$\\)")
    (cons 'entry (match-string 1)))
   ((looking-at "^Main directory: \\(.*\\)$")
    (cons 'repository (match-string 1)))
   ((looking-at "^Entries count: \\([0-9]+\\)$")
    (cons 'total "all"))
   ((looking-at "^  \\(.*\\): \\([0-9]+\\)")
    (let ((value (match-string 1)))
      (pcase (emado--get-parent-header)
        ('statuses (cons 'filter (concat "status = '" value "'")))
        ('tags (cons 'filter (concat "tag = '" value "'")))
        ('priorities (cons 'filter (concat "priority = " value))))))))

(defun emado-go-at-point ()
  "Open or list entries according to the current output line."
  (interactive)
  (pcase (emado--row-at-point)
    (`(entry . ,path)
     (find-file-other-window path)
     (goto-char (point-min)))
    (`(repository . ,path) (dired-other-window path))
    ('(total . "all") (emado--list-all))
    (`(filter . ,query)
     (emado--execute `("ls" ,@(transient-args 'emado-list-menu) ,query)))))

(defun emado-delete-at-point ()
  "Delete the directory or matching entries represented by the current line."
  (interactive)
  (pcase (emado--row-at-point)
    (`(entry . ,path)
     (emado--delete-directory-and-string (file-name-directory path)))
    (`(repository . ,path)
     (when (emado--delete-directory-and-string path)
       (emado-repeat-last)))
    (`(,(or 'total 'filter) . ,query)
     (emado--remove-by-query query))))

(defun emado-open-dir-at-point ()
  "Open the entry or repository directory represented by the current line."
  (interactive)
  (pcase (emado--row-at-point)
    (`(entry . ,path) (dired-other-window (file-name-directory path)))
    (`(repository . ,path) (dired-other-window path))))

;;; Info menu

(defun emado-info ()
  "Show repository info."
  (interactive)
  (emado--execute '("info")))

;;; Settings menu

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

(transient-define-prefix emado-settings-menu ()
  "Session settings for emado."
  ["Working directory"
   (:info (lambda () (if emado--working-directory
                         (format "Current: %s (forced)" emado--working-directory)
                       (format "Current: %s" default-directory))))
   ("w" "Set directory" emado-set-working-directory)
   ("W" "Clear directory" emado-clear-working-directory)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;;; Main Menu

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
  "Bind C-c e globally to `emado-info'."
  (interactive)
  (global-set-key (kbd "C-c e") 'emado-info))

(provide 'emado)
;;; emado.el ends here
