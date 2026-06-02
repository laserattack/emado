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

;;;###autoload
(defun emado-run (args &optional directory)
  "Run mado with ARGS (list of strings) in DIRECTORY."
  (let ((default-directory (or directory
                               emado-directory
                               default-directory)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process emado-executable nil t nil args)))))

(defun emado--display (output)
  "Display OUTPUT in compilation buffer."
  (with-current-buffer (get-buffer-create "*emado*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output)
      (compilation-mode)
      (goto-char (point-min)))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun emado-print (query)
  "Print entries matching QUERY in a compilation buffer."
  (interactive "sQuery: ")
  (emado--display (emado-run (list "-p" query))))

;;;###autoload
(defun emado-remove (query)
  "Remove entries matching QUERY in a compilation buffer."
  (interactive "sQuery: ")
  (emado--display (emado-run (list "-r" query))))

;; Transient menu

(transient-define-prefix emado-menu ()
  "Mado entry manager."
  [["Query"
    ("a" "all entries" (lambda () (interactive) (emado-print "all")))
    ("p" "print by query" (lambda () (interactive)
                            (emado-print (read-string "Query (print): "))))
    ("r" "remove by query" (lambda () (interactive)
                             (emado-remove (read-string "Query (remove): "))))]]
  [["Essential commands"
    ("q" "quit" transient-quit-one)]])

(provide 'emado)

;;; emado.el ends here
