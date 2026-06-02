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

;;;###autoload
(defun emado-print (query)
  "Print entries matching QUERY in a compilation buffer."
  (interactive "sQuery: ")
  (let ((output (emado-run (list "-p" query))))
    (with-current-buffer (get-buffer-create "*emado*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (compilation-mode)
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

;; Transient menu

(transient-define-prefix emado-menu ()
  "Mado entry manager."
  [["Query"
    ("a" "all entries" (lambda () (interactive) (emado-print "all")))
    ("p" "prompt for query" (lambda () (interactive)
                               (emado-print (read-string "Query (print): "))))]]
  [["Essential commands"
    ("q" "quit" transient-quit-one)]])

(provide 'emado)

;;; emado.el ends here
