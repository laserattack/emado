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

(defvar emado-working-directory nil
  "Working directory for mado operations.
Set this once and all operations will use it.")

(defvar emado--last-query nil
  "Last query used for list or remove operations.")

(defun emado-customize ()
  "Open customization group for emado."
  (interactive)
  (customize-group 'emado))

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
  (kill-buffer-and-window))

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
    (define-key map (kbd "c") #'emado-new-menu)
    (define-key map (kbd "i") #'emado-init-menu)
    (define-key map (kbd "r") #'emado-remove-menu)
    (define-key map (kbd "w") #'emado-set-working-directory)
    (define-key map (kbd "W") #'emado-clear-working-directory)
    (define-key map (kbd "C") #'emado-customize)
    map)
  "Keymap for emado buffer.")

(defface emado-face
  '((t :foreground "#dcaf79" :weight bold))
  "Face for highlighting."
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
  (setq-local font-lock-defaults '(emado-font-lock-keywords t))
  (unless (get-buffer-window "*emado*")
    (message (concat "Commands: "
                     (propertize "n" 'face 'emado-face) ", "
                     (propertize "p" 'face 'emado-face) ", "
                     (propertize "RET" 'face 'emado-face) ", "
                     (propertize "g" 'face 'emado-face) ", "
                     (propertize "d" 'face 'emado-face) ", "
                     (propertize "k" 'face 'emado-face) ", "
                     (propertize "l" 'face 'emado-face) ", "
                     (propertize "c" 'face 'emado-face) ", "
                     (propertize "i" 'face 'emado-face) ", "
                     (propertize "r" 'face 'emado-face) ", "
                     (propertize "h" 'face 'emado-face) ", "
                     (propertize "w" 'face 'emado-face) ", "
                     (propertize "W" 'face 'emado-face) ", "
                     (propertize "C" 'face 'emado-face) "; "
                     (propertize "q" 'face 'emado-face) " to quit; "
                     (propertize "h" 'face 'emado-face) " for help"))))

;;; Core helpers

(defvar emado--last-args nil
  "Last mado args for repeating.")

(defvar emado--current-process nil
  "Currently running mado process, if any.")

(defun emado--run (args &optional callback)
  "Run mado with ARGS asynchronously.
When process finishes, call CALLBACK with output string.
If no CALLBACK, just display output in emado buffer."
  (when (and emado--current-process
             (process-live-p emado--current-process))
    (kill-process emado--current-process))

  (let* ((default-directory (or emado-working-directory default-directory))
         (output-buffer (generate-new-buffer " *emado-output*"))
         (proc (make-process
                :name "emado"
                :buffer output-buffer
                :command `(,emado-executable ,@args)
                :sentinel
                (lambda (process _event)
                  (let ((output
                         (with-current-buffer (process-buffer process)
                           (prog1 (buffer-string)
                             (kill-buffer)))))
                    (if callback
                        (funcall callback output)
                      (emado--display output)))
                  (when (eq emado--current-process process)
                    (setq emado--current-process nil))))))
    (setq emado--current-process proc)))

(defvar emado--loading-messages
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

(defun emado--show-status (&optional message)
  "Show random loading MESSAGE in emado buffer."
  (unless message
    (setq message (seq-random-elt emado--loading-messages)))
  (let ((buf (get-buffer-create "*emado*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert message)
        (goto-char (point-min))
        (emado-mode)))
    (if emado-auto-switch
        (pop-to-buffer buf)
      (let ((win (get-buffer-window buf)))
        (if win
            (set-window-buffer win buf)
          (display-buffer buf))))))

(defun emado--display (output)
  "Display OUTPUT in emado buffer."
  (let ((buf (get-buffer-create "*emado*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (string-empty-p output)
            (insert "Nothing here but us chickens")
          (insert output))
        (goto-char (point-min))
        (emado-mode)))
    (if emado-auto-switch
        (pop-to-buffer buf)
      (let ((win (get-buffer-window buf)))
        (if win
            (set-window-buffer win buf)
          (display-buffer buf))))))

(defun emado-open-entry-at-point ()
  "Open mado entry at current line."
  (interactive)
  (beginning-of-line)
  (when (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):")
    (let ((file (match-string 1)))
      (find-file-other-window file)
      (goto-char (point-min)))))

(defun emado-delete-entry-at-point ()
  "Delete mado entry at current line after confirmation."
  (interactive)
  (beginning-of-line)
  (when (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):")
    (let ((dir (file-name-directory (match-string 1)))
          (inhibit-read-only t))
      (when (and dir (yes-or-no-p (format "Delete entry %s? " dir)))
        (delete-directory dir t)
        (delete-region (line-beginning-position) (line-beginning-position 2))
        (when (string-empty-p (string-trim (buffer-string)))
          (erase-buffer)
          (insert "Nothing here but us chickens")
          (goto-char (point-min)))
        (message "Entry deleted")))))

(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  (if emado--last-args
      (progn
        (emado--show-status)
        (emado--run emado--last-args))
    (emado--show-status "No previous command to repeat")))

;;; Transient menus

(transient-define-suffix emado--save-options ()
  "Save current options for this session."
  :transient t
  (interactive)
  (transient-set)
  (message "Options saved for this session"))

(transient-define-suffix emado--reset-options ()
  "Reset all options to default."
  :transient t
  (interactive)
  (transient-reset)
  (message "All options reset"))

(transient-define-infix emado--flag-sort ()
  "Sort criteria."
  :class 'emado--default-option
  :default-value "-priority,-time"
  :argument "--sort="
  :reader (lambda (prompt _initial _history)
            (read-string (concat prompt "(+/-field,...): ")))
  :prompt "Sort: ")

(transient-define-infix emado--flag-ignore-case ()
  "Case-insensitive search."
  :class 'emado--default-switch
  :default-value t
  :argument "--ignore-case"
  :description "Case-insensitive")

(transient-define-infix emado--flag-template ()
  "Template name for new entry."
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
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--init ()
  "Run 'mado init'."
  (interactive)
  (let* ((transient-current-prefix 'emado-init-menu)
         (targs (transient-args 'emado-init-menu))
         (args (append '("init" "--abs-path") targs)))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display))
  (transient-quit-one))

;; ---- New entry ----

(transient-define-prefix emado-new-menu ()
  "Create new mado entry."
  ["Create"
   ("c" "New entry" emado--new-entry)]
  ["Options"
   ("t" "Template" emado--flag-template)]
  ["Save/Reset"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--new-entry ()
  "Create new entry."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (append '("new" "--abs-path") targs)))
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
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args `("list" "--abs-paths" ,@targs ,@extra-args ,query)))
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
                     (format "Search by query (default: %s): " emado--last-query)
                   "Search by query: "))
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
  ["Options"
   ("i" "Case-insensitive" emado--flag-ignore-case)]
  ["Save/Reset"
   ("S" "Save current options" emado--save-options)
   ("R" "Reset all options" emado--reset-options)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defun emado--remove-query ()
  "Remove entries matching a query."
  (interactive)
  (let* ((prompt (if emado--last-query
                     (format "Remove by query (default: %s): " emado--last-query)
                   "Remove by query: "))
         (query (read-string prompt nil nil emado--last-query)))
    (when (string-empty-p query)
      (setq query emado--last-query))
    (when (and query (not (string-empty-p query)))
      (setq emado--last-query query)
      (when (yes-or-no-p (format "Really remove entries matching '%s'? " query))
        (let* ((targs (transient-args 'emado-remove-menu))
               (args `("remove" "--abs-paths" ,@targs ,query)))
          (setq emado--last-args args)
          (emado--show-status)
          (emado--run args #'emado--display)))))
  (transient-quit-one))

;; ---- Info ----

(defun emado-info ()
  "Show repository info."
  (interactive)
  (let ((args '("info" "--abs-path")))
    (setq emado--last-args args)
    (emado--show-status)
    (emado--run args #'emado--display)))

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
   ("w" "Set directory" emado-set-working-directory)
   ("W" "Clear directory" emado-clear-working-directory)]
  [["Actions"
    ("l" "List entries" emado-list-menu)
    ("c" "New entry" emado-new-menu)
    ("r" "Remove entries" emado-remove-menu)]
   ["Repository"
    ("i" "Init" emado-init-menu)
    ("h" "Info" emado-info)]]
  [["Other"
    ("C" "Customize" emado-customize)]
   ["Quit"
    ("q" "Quit" transient-quit-one)]])

;;;###autoload
(defun emado ()
  "Start the mado entry manager."
  (interactive)
  (emado-menu))

(provide 'emado)
;;; emado.el ends here
