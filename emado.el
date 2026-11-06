;;; emado.el --- Mado entry manager for Emacs -*- lexical-binding:t; coding:utf-8 -*-

;; Package-Requires: (
;;     (emacs    "28.1")
;;     (transient "0.12"))

(require 'transient)
(require 'seq)

;; Groups user-facing settings.
;; Объединяет пользовательские настройки.
(defgroup emado nil
  "Mado entry manager for Emacs."
  :group 'tools)

;; Selects the executable used for every Mado command.
;; Задаёт исполняемый файл для всех команд Mado.
(defcustom emado-executable "mado"
  "Path to mado executable."
  :type 'string
  :group 'emado)

;; Controls whether results receive focus.
;; Определяет, получает ли окно результатов фокус.
(defcustom emado-auto-switch t
  "If non-nil, automatically switch to emado buffer after command finishes."
  :type 'boolean
  :group 'emado)

;; Highlights command keys in the buffer and hints.
;; Выделяет клавиши команд в буфере и подсказках.
(defface emado-face
  '((t :foreground "#dcaf79" :weight bold))
  "Face for highlighting."
  :group 'emado)

;; Recognizes Mado output fields for highlighting.
;; Распознаёт поля вывода Mado для подсветки.
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
;;; Внутренние переменные

;; Names the shared results buffer.
;; Задаёт имя общего буфера результатов.
(defconst emado--buffer-name "*emado*"
  "Name of the emado output buffer.")

;; Names the temporary process buffer.
;; Задаёт имя временного буфера процесса.
(defconst emado--output-buffer-name " *emado-output*"
  "Name of the temporary emado process output buffer.")

;; Names the asynchronous Mado process.
;; Задаёт имя асинхронного процесса Mado.
(defconst emado--process-name "emado"
  "Name of the emado process.")

;; Replaces empty command output.
;; Заменяет пустой вывод команды.
(defconst emado--empty-message "Nothing here but us chickens"
  "Message shown when there are no mado output.")

;; Explains why a command cannot run in a missing directory.
;; Объясняет, почему команда не запускается в отсутствующем каталоге.
(defconst emado--invalid-working-dir-path-message
  "Working directory path is as real as my will to live"
  "Message shown when working directory path does not exist.")

;; Identifies headings in repository statistics.
;; Распознаёт заголовки статистики репозитория.
(defconst emado--outline-regexp "^\\(Statuses ([0-9]+)\\|Tags ([0-9]+)\\|Priorities ([0-9]+)\\):"
  "Regexp for outline headings in emado buffer.")

;; Defines buffer keys and which keys appear in the startup hint.
;; Задаёт клавиши буфера и их видимость в начальной подсказке.
(defconst emado--bindings
  '(;; don't show in help
    ;; Не показывать в подсказке
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
    ;; Показывать в подсказке
    ("l" emado-list-menu t)
    ("c" emado-new-menu t)
    ("i" emado-init-menu t)
    ("r" emado-remove-menu t)
    ("s" emado-settings-menu t))
  "Alist of emado keybindings (key command show-in-help).")

;; Supplies status messages while a command runs.
;; Хранит сообщения для показа во время выполнения команды.
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

;; Overrides the current buffer directory when set.
;; При заданном значении заменяет каталог текущего буфера.
(defvar emado--working-directory nil
  "Working directory for mado operations.
Set this once and all operations will use it.")

;; Remembers the query shared by list and remove prompts.
;; Хранит последний запрос для поиска и удаления.
(defvar emado--last-query nil
  "Last query used for list or remove operations.")

;; Keeps the argument list for repeating a command.
;; Хранит аргументы для повторного запуска команды.
(defvar emado--last-args nil
  "Last mado args for repeating.")

;; Tracks which process may publish results.
;; Отслеживает процесс, которому разрешено показать результаты.
(defvar emado--current-process nil
  "Currently running mado process, if any.")

;; Tracks the state of the bulk visibility toggle.
;; Хранит состояние общего переключателя видимости.
(defvar emado--all-sections-visible t
  "Track whether all outline sections are visible.")

;; Tracks menus whose session settings override defaults.
;; Хранит меню, где настройки сеанса заменяют значения по умолчанию.
(defvar emado--saved-menus nil
  "List of menu commands that have been saved this session.")

;;; Custom classes for default values support
;;; Классы для значений по умолчанию

;; Adds a default value to a Transient option.
;; Добавляет значение по умолчанию для параметра Transient.
(defclass emado--default-option (transient-option)
  ((default-value :initarg :default-value :initform nil)))

;; Adds a default state to a Transient switch.
;; Добавляет состояние по умолчанию для переключателя Transient.
(defclass emado--default-switch (transient-switch)
  ((default-value :initarg :default-value :initform nil)))

;; Initialize the control from session settings or its declared default.
;; Инициализирует элемент управления настройками сеанса или значением по умолчанию.
(cl-defmethod transient-init-value ((obj emado--default-option))
  ;; Prefer explicitly saved values over built-in defaults.
  ;; Использовать сохранённые значения вместо встроенных.
  (if (memq (oref transient--prefix command) emado--saved-menus)
      (cl-call-next-method)
    (let ((val (oref obj default-value)))
      (when val
        (oset obj value val)))))

;; Initialize the control from session settings or its declared default.
;; Инициализирует элемент управления настройками сеанса или значением по умолчанию.
(cl-defmethod transient-init-value ((obj emado--default-switch))
  ;; Prefer explicitly saved values over built-in defaults.
  ;; Использовать сохранённые значения вместо встроенных.
  (if (memq (oref transient--prefix command) emado--saved-menus)
      (cl-call-next-method)
    (let ((val (oref obj default-value)))
      (when val
        (oset obj value (oref obj argument))))))

;;; Fringe indicators for outline sections
;;; Метки разделов на полях окна

;; Marks a collapsed section.
;; Отмечает свёрнутый раздел.
(define-fringe-bitmap 'emado--fringe-right
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

;; Marks an expanded section.
;; Отмечает развёрнутый раздел.
(define-fringe-bitmap 'emado--fringe-down
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

;; Rebuilds heading markers after visibility changes.
;; Перестраивает метки заголовков после изменения видимости.
(defun emado--update-fringe-indicators ()
  "Update fringe indicators for all outline headings."
  (remove-overlays (point-min) (point-max) 'emado--fringe t)
  (save-excursion
    (goto-char (point-min))
    ;; Replace the marker at each heading using its current visibility.
    ;; Обновить метку каждого заголовка по текущей видимости.
    (while (outline-next-heading)
      (let* ((beg (point))
             (hidden (outline-invisible-p (line-end-position)))
             (bitmap (if hidden 'emado--fringe-right 'emado--fringe-down))
             (ov (make-overlay beg (1+ beg) nil t)))
        (overlay-put ov 'emado--fringe t)
        (overlay-put ov 'before-string
                     (propertize " " 'display `(left-fringe ,bitmap)))))))

;;; Mode
;;; Режим просмотра

;; Toggles a heading only when its section has content.
;; Переключает видимость раздела под курсором, если в нём есть содержимое.
(defun emado-toggle-section ()
  "Toggle visibility of section at point."
  (interactive)
  (beginning-of-line)
  ;; Only headings can be toggled.
  ;; Сворачивать можно только разделы под заголовком.
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
      ;; Skip headings that have no body to fold.
      ;; Пропустить заголовок, если под ним нет содержимого.
      (when has-content
        (outline-toggle-children)))))

;; Switches between hiding and showing all section bodies.
;; Сворачивает или разворачивает содержимое всех разделов.
(defun emado-toggle-all-sections ()
  "Toggle visibility of all outline sections."
  (interactive)
  ;; Switch the entire outline and keep the toggle in sync.
  ;; Переключить видимость всех разделов и обновить состояние.
  (if emado--all-sections-visible
      (progn
        (outline-hide-body)
        (setq emado--all-sections-visible nil))
    (outline-show-all)
    (setq emado--all-sections-visible t)))

;; Closes the results buffer and its window. Window errors propagate.
;; Закрывает буфер результатов и его окно. Ошибки окна передаются вызывающему коду.
(defun emado-quit ()
  "Close emado window and kill buffer."
  (interactive)
  (kill-buffer-and-window))

;; Moves forward by one visible line.
;; Переходит на следующую видимую строку.
(defun emado-next-line ()
  "Move to next logical line."
  (interactive)
  (forward-visible-line 1))

;; Moves backward by one visible line.
;; Переходит на предыдущую видимую строку.
(defun emado-previous-line ()
  "Move to previous logical line."
  (interactive)
  (forward-visible-line -1))

;; Moves to the start of the next outline heading.
;; Переходит к началу следующего заголовка.
(defun emado-next-heading ()
  "Move to next heading."
  (interactive)
  (outline-next-heading)
  (beginning-of-line))

;; Moves to the start of the previous outline heading.
;; Переходит к началу предыдущего заголовка.
(defun emado-previous-heading ()
  "Move to previous heading."
  (interactive)
  (outline-previous-heading)
  (beginning-of-line))

;; Builds the local keymap from the shared binding list.
;; Создаёт локальную раскладку из общего списка клавиш.
(defvar emado-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Install all local bindings, including shortcuts omitted from the hint.
    ;; Назначить все локальные клавиши, включая скрытые в подсказке.
    (dolist (bind emado--bindings)
      (define-key map (kbd (car bind)) (cadr bind)))
    map)
  "Keymap for emado buffer.")

;; Configures read-only output, highlighting, and folding. Hook errors propagate.
;; Настраивает буфер только для чтения, подсветку и сворачивание. Ошибки хуков передаются вызывающему коду.
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
  ;; Show the startup hint when the results buffer is not visible yet.
  ;; Показать начальную подсказку, пока буфер результатов не виден.
  (unless (get-buffer-window emado--buffer-name)
    (let ((keys (mapcar (lambda (bind)
                          (propertize (car bind) 'face 'emado-face))
                        (seq-filter (lambda (b) (nth 2 b)) emado--bindings))))
      (message "Commands: %s; %s to quit; %s for help"
               (mapconcat 'identity keys ", ")
               (propertize "q" 'face 'emado-face)
               (propertize "h" 'face 'emado-face)))))

;;; Core helpers
;;; Основные вспомогательные функции

;; Returns t after confirmed deletion or nil if declined. File errors and quit propagate.
;; Возвращает t после удаления или nil при отказе. Ошибки файловой системы и отмена передаются вызывающему коду.
(defun emado--delete-directory-and-string (dir)
  "Delete directory DIR after confirmation and update buffer.
Return t if directory was deleted, nil otherwise."
  (let ((inhibit-read-only t))
    ;; Ask for confirmation before any deletion.
    ;; Запросить подтверждение перед удалением.
    (when (yes-or-no-p (format "Emado (remove directory): %s ? " dir))
      (delete-directory dir t)
      (delete-region (line-beginning-position) (line-beginning-position 2))
      ;; Show an empty-state message after removing the last output row.
      ;; После удаления последней строки показать сообщение об отсутствии результатов.
      (when (string-empty-p (string-trim (buffer-string)))
        (erase-buffer)
        (insert emado--empty-message)
        (goto-char (point-min)))
      (message "Directory deleted")
      t)))

;; Shows results with optional focus. Window errors propagate.
;; Показывает результаты с учётом настройки фокуса. Ошибки окон передаются вызывающему коду.
(defun emado--show-buffer (buf)
  "Display BUF according to `emado-auto-switch' setting."
  ;; Select the results window only when configured to do so.
  ;; Перевести фокус на окно результатов согласно настройке.
  (if emado-auto-switch
      (pop-to-buffer buf)
    (let ((win (get-buffer-window buf)))
      ;; Reuse an existing window before requesting a new display.
      ;; Использовать существующее окно, если оно есть.
      (if win
          (set-window-buffer win buf)
        (display-buffer buf)))))

;; Replaces results and displays an empty-state message if needed. Display and mode errors propagate.
;; Обновляет результаты и при необходимости показывает сообщение об их отсутствии. Ошибки показа и режима передаются вызывающему коду.
(defun emado--display (output)
  "Display OUTPUT in emado buffer."
  (let ((buf (get-buffer-create emado--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Use the empty-state message when the command emits no text.
        ;; При пустом выводе показать сообщение об отсутствии результатов.
        (if (string-empty-p output)
            (insert emado--empty-message)
          (insert output))
        (goto-char (point-min))
        (emado-mode)))
    (emado--show-buffer buf)))

;; Shows the given status or a random loading message. Display and mode errors propagate.
;; Показывает заданное или случайное сообщение загрузки. Ошибки показа и режима передаются вызывающему коду.
(defun emado--show-status (&optional message)
  "Show random loading MESSAGE in emado buffer."
  ;; Pick a loading message when no explicit status was supplied.
  ;; Выбрать сообщение загрузки, если явное сообщение не задано.
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

;; Returns t for an existing path or shows a diagnostic and returns nil. File and display errors propagate.
;; Возвращает t для существующего пути или показывает сообщение и возвращает nil. Ошибки файловой системы и показа передаются вызывающему коду.
(defun emado--check-working-directory ()
  "Return t if the working path exists, otherwise show a diagnostic and return nil."
  (let ((dir (or emado--working-directory default-directory)))
    ;; Reject an unavailable working path before starting Mado.
    ;; Проверить доступность рабочего пути перед запуском Mado.
    (if (file-exists-p dir)
        t
      (progn
        (emado--show-status emado--invalid-working-dir-path-message)
        nil))))

;; Starts Mado asynchronously or returns nil for a missing working directory. Launch errors propagate and command diagnostics go through the output buffer.
;; Запускает Mado асинхронно или возвращает nil, если рабочий каталог отсутствует. Ошибки запуска передаются вызывающему коду, а диагностика команды попадает в буфер вывода.
(defun emado--run (args)
  "Run Mado asynchronously with ARGS and display its output."
  ;; Start the process only after checking the working path.
  ;; Запустить процесс только после проверки рабочего пути.
  (when (emado--check-working-directory)
    ;; Stop the previous process before starting its replacement.
    ;; Остановить предыдущий процесс перед запуском нового.
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
                    ;; Ignore late notifications from processes already replaced.
                    ;; Игнорировать поздние уведомления от заменённых процессов.
                    (when (eq emado--current-process process)
                      (let ((output
                             (with-current-buffer (process-buffer process)
                               (prog1 (buffer-string)
                                 (kill-buffer)))))
                        (emado--display output))
                      (setq emado--current-process nil))))))
      (setq emado--current-process proc))))

;; Records arguments for repeat and displays asynchronous results.
;; Запоминает аргументы для повтора и показывает результаты асинхронной команды.
;; Returns nil for a missing path. Process launch and display errors propagate.
;; Возвращает nil при отсутствующем пути. Ошибки запуска и показа передаются вызывающему коду.
(defun emado--execute (args)
  "Run ARGS and remember them for repetition."
  (setq emado--last-args args)
  (emado--show-status)
  (emado--run args))

;; Reuses saved arguments when idle. Launch and display errors propagate.
;; Повторяет сохранённую команду при отсутствии активного процесса. Ошибки запуска и показа передаются вызывающему коду.
(defun emado-repeat-last ()
  "Repeat the last mado command."
  (interactive)
  ;; Repeat only after a command has been recorded.
  ;; Повторять можно только после сохранения команды.
  (if emado--last-args
      ;; Avoid repeating a command while another one is running.
      ;; Не повторять команду, пока выполняется другой процесс.
      (if (and emado--current-process
               (process-live-p emado--current-process))
          (message "Command is already running...")
        (emado--execute emado--last-args))
    (message "No previous command to repeat")))

;; Runs a list query with the active options. Launch and display errors propagate.
;; Выполняет поиск с текущими параметрами. Ошибки запуска и показа передаются вызывающему коду.
(defun emado--list-entries (query &optional extra-args)
  "List entries matching QUERY with EXTRA-ARGS."
  (let* ((transient-current-prefix 'emado-list-menu)
         (targs (transient-args 'emado-list-menu))
         (args `("ls" ,@targs ,@extra-args ,query)))
    (emado--execute args)))

;; Runs removal only after confirmation. Quit, launch, and display errors propagate.
;; Запускает удаление только после подтверждения. Отмена и ошибки запуска и показа передаются вызывающему коду.
(defun emado--remove-by-query (query)
  "Remove entries matching QUERY after confirmation."
  ;; Ask for confirmation before any deletion.
  ;; Запросить подтверждение перед удалением.
  (when (yes-or-no-p (format "Emado (remove query): %s ? " query))
    (let* ((targs (transient-args 'emado-remove-menu))
           (args `("rm" "-format" "path" ,@targs ,query)))
      (emado--execute args))))

;;; Transient menus
;;; Меню Transient

;; Keeps the active menu options for this session.
;; Сохраняет текущие параметры меню на время сеанса.
(transient-define-suffix emado--save-options ()
  "Save current options for this session."
  :transient t
  (interactive)
  (transient-set)
  (cl-pushnew (oref transient--prefix command) emado--saved-menus)
  (message "Options saved for this session"))

;; Drops the session override and restores menu defaults.
;; Сбрасывает параметры сеанса и возвращает значения по умолчанию.
(transient-define-suffix emado--reset-options ()
  "Reset all options to default."
  :transient t
  (interactive)
  (setq emado--saved-menus (delq (oref transient--prefix command) emado--saved-menus))
  (transient-reset)
  (message "All options reset"))

;; Reads sort keys and defaults to descending priority and time. Minibuffer errors and quit propagate.
;; Читает поля сортировки. По умолчанию сортирует по убыванию приоритета и времени. Ошибки минибуфера и отмена передаются вызывающему коду.
(transient-define-infix emado--flag-sort ()
  "Sort criteria."
  :class 'emado--default-option
  :default-value "-priority,-time"
  :argument "-sort="
  :reader (lambda (prompt _initial _history)
            (read-string "Emado (sort, +/-field,...): "))
  :prompt "Emado (sort): ")

;; Hides the creation timestamp by default.
;; По умолчанию скрывает время создания.
(transient-define-infix emado--flag-hide-time ()
  "Hide time field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-time"
  :description "Hide time")

;; Hides the modification timestamp by default.
;; По умолчанию скрывает время изменения.
(transient-define-infix emado--flag-hide-mtime ()
  "Hide mtime field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-mtime"
  :description "Hide mtime")

;; Hides the path field by default.
;; По умолчанию скрывает поле пути.
(transient-define-infix emado--flag-hide-path ()
  "Hide path field."
  :class 'emado--default-switch
  :default-value t
  :argument "-hide-path"
  :description "Hide path")

;; Returns Markdown template names or nil if lookup fails with a nonzero exit or finds none. Launch and file errors propagate.
;; Возвращает имена шаблонов Markdown или nil при ненулевом коде выхода или отсутствии шаблонов. Ошибки запуска и файловой системы передаются вызывающему коду.
(defun emado--template-names ()
  "Return names from the template directory selected by the current Mado."
  (let ((default-directory (or emado--working-directory default-directory)))
    (with-temp-buffer
      ;; Read the repository path only when Mado reports success.
      ;; Использовать путь к репозиторию только при успешном завершении Mado.
      (when (zerop (process-file emado-executable nil (list t nil) nil
                                 "info" "-only-dir"))
        (let* ((root (string-trim-right (buffer-string) "[\r\n]+"))
               (templates-dir
                (and (not (string-empty-p root))
                     (seq-find #'file-directory-p
                               (mapcar (lambda (name) (expand-file-name name root))
                                       '("templates" ".templates" "_templates"))))))
          ;; List Markdown files only in the first supported template directory.
          ;; Читать файлы Markdown только из первого подходящего каталога шаблонов.
          (when templates-dir
            (mapcar #'file-name-base
                    (seq-filter #'file-regular-p
                                (directory-files templates-dir t "\\.md$")))))))))

;; Completes a template name or leaves the option unset if none are found. Lookup errors and quit propagate.
;; Дополняет имя шаблона или оставляет параметр пустым, если шаблонов нет. Ошибки поиска и отмена передаются вызывающему коду.
(transient-define-infix emado--flag-template ()
  "Template name for new entry."
  :class 'transient-option
  :argument "-template="
  :reader (lambda (prompt _initial _history)
            (let ((names (emado--template-names)))
              ;; Offer completion only if there are templates to choose from.
              ;; Предложить дополнение, если есть доступные шаблоны.
              (if names
                  (completing-read prompt names nil t)
                (message "No templates found")
                nil)))
  :prompt "Emado (create entry template): ")

;; Read an optional directory suffix. Empty input or a bare dash clears it.
;; Читает суффикс каталога. Пустой ввод или один дефис сбрасывает его.
;; Returns no errors. Minibuffer errors and quit propagate.
;; Не возвращает ошибок. Ошибки минибуфера и отмена передаются вызывающему коду.
(transient-define-infix emado--flag-suffix ()
  "Set an optional suffix for the entry directory name."
  :class 'transient-option
  :argument "-suffix="
  :reader (lambda (prompt initial history)
            (let ((suffix (read-string prompt (or initial "-") history)))
              ;; Treat the placeholder as an unset option.
              ;; Считать подсказку незаданным значением.
              (unless (member suffix '("" "-"))
                suffix)))
  :prompt "Emado (entry suffix): ")

;;; Init menu
;;; Меню инициализации

;; Runs repository initialization with the current options. Launch and display errors propagate.
;; Запускает инициализацию репозитория с текущими параметрами. Ошибки запуска и показа передаются вызывающему коду.
(defun emado--init ()
  "Run 'mado init'."
  (interactive)
  (let* ((transient-current-prefix 'emado-init-menu)
         (targs (transient-args 'emado-init-menu))
         (args (cons "init" targs)))
    (emado--execute args))
  (transient-quit-one))

;; Offers repository initialization and the force option.
;; Открывает меню инициализации репозитория с параметром принудительного создания.
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
;;; Меню создания записи

;; Creates an entry using the current options. Launch and display errors propagate.
;; Создаёт запись с текущими параметрами. Ошибки запуска и показа передаются вызывающему коду.
(defun emado--new-entry ()
  "Create new entry."
  (interactive)
  (let* ((transient-current-prefix 'emado-new-menu)
         (targs (transient-args 'emado-new-menu))
         (args (cons "new" targs)))
    (emado--execute args))
  (transient-quit-one))

;; Offers entry creation with an optional template and suffix.
;; Открывает меню создания записи с необязательными шаблоном и суффиксом.
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

;; Reads and remembers a query, falling back to the previous one on empty input.
;; Читает и запоминает запрос, при пустом вводе использует предыдущий.
;; Returns nil if neither is available. Minibuffer errors and quit propagate.
;; Возвращает nil, если запроса нет. Ошибки минибуфера и отмена передаются вызывающему коду.
(defun emado--read-query (label initial)
  "Read a query for LABEL with optional INITIAL text."
  (let* ((prompt (if (and emado--last-query (not initial))
                     (format "Emado (%s, default: %s): " label emado--last-query)
                   (format "Emado (%s): " label)))
         (query (read-string prompt initial emado--last-query)))
    ;; Reuse the last query only when the user submits empty input.
    ;; Использовать предыдущий запрос только при пустом вводе.
    (when (string-empty-p query)
      (setq query emado--last-query))
    ;; Keep an absent or empty query from reaching the command.
    ;; Не передавать команде отсутствующий или пустой запрос.
    (when (and query (not (string-empty-p query)))
      (setq emado--last-query query))))

;;; List menu
;;; Меню поиска

;; Lists entries without filtering. Launch and display errors propagate.
;; Показывает все записи. Ошибки запуска и показа передаются вызывающему коду.
(defun emado--list-all ()
  "List all entries."
  (interactive)
  (emado--list-entries "all")
  (transient-quit-one))

;; Reads or reuses a query and skips listing if none is available. Quit, launch, and display errors propagate.
;; Читает или повторно использует запрос и пропускает поиск, если запроса нет. Отмена и ошибки запуска и показа передаются вызывающему коду.
(defun emado--list-query (&optional initial)
  "List entries matching a custom query.
If INITIAL is non-nil, use it as initial input."
  (interactive)
  ;; Run the action only when the prompt returns a query.
  ;; Выполнить действие, только если введён или выбран запрос.
  (when-let* ((query (emado--read-query "search query" initial)))
    (emado--list-entries query))
  (transient-quit-one))

;; Lists entries by a prefilled fuzzy query on any field. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по любому полю. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-any ()
  "Fuzzy list by any."
  (interactive)
  (emado--list-query "any ~~ "))

;; Lists entries by a prefilled fuzzy query on status. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по статусу. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-status ()
  "Fuzzy list by status."
  (interactive)
  (emado--list-query "status ~~ "))

;; Lists entries by a prefilled fuzzy query on name. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по имени. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-name ()
  "Fuzzy list by name."
  (interactive)
  (emado--list-query "name ~~ "))

;; Lists entries by a prefilled fuzzy query on tag. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по тегу. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-tag ()
  "Fuzzy list by tag."
  (interactive)
  (emado--list-query "tag ~~ "))

;; Lists entries by a prefilled fuzzy query on priority. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по приоритету. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-priority ()
  "Fuzzy list by priority."
  (interactive)
  (emado--list-query "priority ~~ "))

;; Lists entries by a prefilled fuzzy query on deadline. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по сроку. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-deadline ()
  "Fuzzy list by deadline."
  (interactive)
  (emado--list-query "deadline ~~ "))

;; Lists entries by a prefilled fuzzy query on path. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по пути. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-path ()
  "Fuzzy list by path."
  (interactive)
  (emado--list-query "path ~~ "))

;; Lists entries by a prefilled fuzzy query on creation time. Query errors and quit propagate.
;; Ищет записи по заранее заполненному нечёткому запросу по времени создания. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-list-fuzzy-time ()
  "Fuzzy list by time."
  (interactive)
  (emado--list-query "time ~~ "))

;; Offers listing, query shortcuts, sorting, and field visibility.
;; Открывает меню поиска с быстрыми запросами, сортировкой и настройкой видимости полей.
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
;;; Меню удаления

;; Requests confirmation to remove all entries. Quit, launch, and display errors propagate.
;; Запрашивает подтверждение удаления всех записей. Отмена и ошибки запуска и показа передаются вызывающему коду.
(defun emado--remove-all ()
  "Remove all entries."
  (interactive)
  (emado--remove-by-query "all")
  (transient-quit-one))

;; Reads or reuses a query and asks for confirmation before removal. Quit, launch, and display errors propagate.
;; Читает или повторно использует запрос и просит подтвердить удаление. Отмена и ошибки запуска и показа передаются вызывающему коду.
(defun emado--remove-query (&optional initial)
  "Remove entries matching a custom query.
If INITIAL is non-nil, use it as initial input."
  (interactive)
  ;; Run the action only when the prompt returns a query.
  ;; Выполнить действие, только если введён или выбран запрос.
  (when-let* ((query (emado--read-query "remove query" initial)))
    (emado--remove-by-query query))
  (transient-quit-one))

;; Selects for removal entries by a prefilled fuzzy query on any field. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по любому полю. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-any ()
  "Fuzzy remove by any."
  (interactive)
  (emado--remove-query "any ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on status. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по статусу. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-status ()
  "Fuzzy remove by status."
  (interactive)
  (emado--remove-query "status ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on name. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по имени. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-name ()
  "Fuzzy remove by name."
  (interactive)
  (emado--remove-query "name ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on tag. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по тегу. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-tag ()
  "Fuzzy remove by tag."
  (interactive)
  (emado--remove-query "tag ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on priority. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по приоритету. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-priority ()
  "Fuzzy remove by priority."
  (interactive)
  (emado--remove-query "priority ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on deadline. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по сроку. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-deadline ()
  "Fuzzy remove by deadline."
  (interactive)
  (emado--remove-query "deadline ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on path. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по пути. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-path ()
  "Fuzzy remove by path."
  (interactive)
  (emado--remove-query "path ~~ "))

;; Selects for removal entries by a prefilled fuzzy query on creation time. Query errors and quit propagate.
;; Выбирает для удаления записи по заранее заполненному нечёткому запросу по времени создания. Ошибки выполнения запроса и отмена передаются вызывающему коду.
(defun emado-remove-fuzzy-time ()
  "Fuzzy remove by time."
  (interactive)
  (emado--remove-query "time ~~ "))

;; Offers removal by query, including fuzzy shortcuts.
;; Открывает меню удаления по запросу с быстрыми командами нечёткого поиска.
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
;;; Действия со строками вывода

;; Returns the enclosing statistics section or nil outside a recognized section.
;; Возвращает раздел статистики или nil вне известного раздела.
(defun emado--get-parent-header ()
  "Find the nearest parent header before current line."
  (save-excursion
    (beginning-of-line)
    ;; Walk back over indented statistics rows to their heading.
    ;; Пройти вверх по строкам статистики с отступом до заголовка.
    (while (and (not (bobp))
                (looking-at "^  "))
      (forward-line -1)
      (beginning-of-line))
    (cond
     ((looking-at "^Statuses ([0-9]+):") 'statuses)
     ((looking-at "^Tags ([0-9]+):") 'tags)
     ((looking-at "^Priorities ([0-9]+):") 'priorities))))

;; Returns a tagged path or query for a recognized output line, or nil.
;; Возвращает путь или запрос с меткой типа строки либо nil.
;; Signals no errors for ordinary buffer text.
;; При разборе обычного текста буфера ошибок не возникает.
(defun emado--row-at-point ()
  "Classify the current output line and extract its path or query."
  (beginning-of-line)
  (cond
   ;; Match the location prefix emitted by Mado's default entry format.
   ;; Распознать префикс пути и номера строки в стандартном выводе Mado.
   ((looking-at "^\\(.*\\):\\([0-9]+\\):\\(?: \\|$\\)")
    (cons 'entry (match-string 1)))
   ((looking-at "^Main directory: \\(.*\\)$")
    (cons 'repository (match-string 1)))
   ((looking-at "^Entries count: \\([0-9]+\\)$")
    (cons 'total "all"))
   ;; The heading determines which field an indented value belongs to.
   ;; Заголовок определяет поле, к которому относится строка с отступом.
   ((looking-at "^  \\(.*\\): \\([0-9]+\\)")
    (let ((value (match-string 1)))
      (pcase (emado--get-parent-header)
        ('statuses (cons 'filter (concat "status = '" value "'")))
        ('tags (cons 'filter (concat "tag = '" value "'")))
        ('priorities (cons 'filter (concat "priority = " value))))))))

;; Opens a path or lists entries for a statistics row. Unrecognized rows do nothing.
;; Открывает путь или показывает записи по строке статистики. Прочие строки пропускает.
;; File, display, and launch errors propagate.
;; Ошибки файловой системы, показа и запуска передаются вызывающему коду.
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

;; Deletes a path or matching entries after confirmation. Other rows do nothing.
;; После подтверждения удаляет каталог или подходящие записи. Прочие строки пропускает.
;; Quit, file, display, and launch errors propagate.
;; Отмена и ошибки файловой системы, показа и запуска передаются вызывающему коду.
(defun emado-delete-at-point ()
  "Delete the directory or matching entries represented by the current line."
  (interactive)
  (pcase (emado--row-at-point)
    (`(entry . ,path)
     (emado--delete-directory-and-string (file-name-directory path)))
    (`(repository . ,path)
     ;; Refresh only after confirmed repository deletion.
     ;; Обновить вывод только после подтверждённого удаления репозитория.
     (when (emado--delete-directory-and-string path)
       (emado-repeat-last)))
    (`(,(or 'total 'filter) . ,query)
     (emado--remove-by-query query))))

;; Opens Dired for a recognized path and ignores other rows. File and window errors propagate.
;; Открывает Dired для распознанного пути. Ошибки файловой системы и окон передаются вызывающему коду.
(defun emado-open-dir-at-point ()
  "Open the entry or repository directory represented by the current line."
  (interactive)
  (pcase (emado--row-at-point)
    (`(entry . ,path) (dired-other-window (file-name-directory path)))
    (`(repository . ,path) (dired-other-window path))))

;;; Info menu
;;; Сведения о репозитории

;; Shows statistics for the current repository. Launch and display errors propagate.
;; Показывает статистику текущего репозитория. Ошибки запуска и показа передаются вызывающему коду.
(defun emado-info ()
  "Show repository info."
  (interactive)
  (emado--execute '("info")))

;;; Settings menu
;;; Меню настроек

;; Chooses a session directory and reloads statistics. Quit, file, launch, and display errors propagate.
;; Выбирает каталог сеанса и обновляет статистику. Отмена и ошибки файловой системы, запуска и показа передаются вызывающему коду.
(transient-define-suffix emado-set-working-directory ()
  "Set working directory for mado operations."
  :transient t
  (interactive)
  (let ((dir (read-directory-name "Emado (working directory): " nil nil t)))
    (setq emado--working-directory dir)
    (message "Working directory set to: %s" dir)
    (emado-info)))

;; Restores directory selection from the buffer and reloads statistics. Launch and display errors propagate.
;; Возвращает выбор каталога по буферу и обновляет статистику. Ошибки запуска и показа передаются вызывающему коду.
(transient-define-suffix emado-clear-working-directory ()
  "Clear working directory setting."
  :transient t
  (interactive)
  (setq emado--working-directory nil)
  (message "Working directory cleared, using default")
  (emado-info))

;; Shows and changes the session working directory.
;; Показывает и позволяет менять рабочий каталог сеанса.
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
;;; Главное меню

;; Provides access to entry, repository, and session commands.
;; Открывает доступ к командам записей, репозитория и сеанса.
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
;; Registers the global shortcut for repository statistics.
;; Назначает глобальную клавишу для просмотра статистики репозитория.
(defun emado-default-bindings ()
  "Set default global keybindings for emado."
  (interactive)
  (global-set-key (kbd "C-c e") 'emado-info))

(provide 'emado)
;;; emado.el ends here
;;; Конец emado.el
