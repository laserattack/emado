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
  "Face for highlighting field names and paths.
Customize this face to enable coloring."
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
   '("\\<\\(TIME\\|NAME\\|PRIORITY\\|DEADLINE\\|STATUS\\|TAGS\\)\\>"
     1 'emado-field-face))
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

;;;###autoload
(defun emado-print (query)
  "Print entries matching QUERY."
  (interactive "sQuery: ")
  (let ((output (emado-run (list "-p" query))))
    (emado--display output)))

;;;###autoload
(defun emado-remove (query)
  "Remove entries matching QUERY."
  (interactive "sQuery: ")
  (let ((output (emado-run (list "-r" query))))
    (emado--display output)))

;; Transient menu

(transient-define-prefix emado-menu ()
  "Mado entry manager."
  [["Query"
    ("p" "print by query" (lambda () (interactive)
                            (emado-print (read-string "Query (print): "))))
    ("r" "remove by query" (lambda () (interactive)
                             (emado-remove (read-string "Query (remove): "))))]]
  [["Essential commands"
    ("q" "quit" transient-quit-one)]])

(provide 'emado)

;;; emado.el ends here
