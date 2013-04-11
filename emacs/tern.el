;;; -*- lexical-binding: t -*-

;; usage:
;; 
;; (add-to-list 'load-path (expand-file-name "~/xxx/tern/emacs/"))
;; (autoload 'tern-mode "tern.el" nil t)
;; (defun js2-mode-customize-hook ()
;;    ;; customize...
;;   (tern-mode)
;;   )
;; (add-hook 'js2-mode-hook 'js2-mode-customize-hook)

(eval-when-compile (require 'cl))
(require 'json)

(require 'request-deferred)
(require 'concurrent)
(require 'widget-mvc)

(defvar tern-server-map nil
  "map (project-dir -> (port process)). This variable is initialized at `tern-mode-enable'.")

(defun tern-request (port doc)
  (deferred:nextc
    (request-deferred
     (format "http://localhost:%i/" port)
     :type "POST"
     :data (json-encode doc)
     :parser 'buffer-string)
    (lambda (res) 
      (with-temp-buffer
        (insert (request-response-data res))
        (goto-char (point-min))
        (json-read)))))

(defun tern-project-dir (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (or tern-project-dir
        (and (not (buffer-file-name)) (setf tern-project-dir ""))
        (let ((project-dir (file-name-directory (buffer-file-name))))
          (loop for cur = project-dir then (file-name-directory (substring cur 0 (1- (length cur))))
                while cur do
                (when (file-exists-p (concat cur ".tern-project"))
                  (return (setf project-dir cur))))
          (setf tern-project-dir project-dir)))))

(defun tern-find-server (&optional buf)
  (let ((tp-dir (tern-project-dir buf)))
    (deferred:nextc
      (cc:dataflow-get tern-server-map tp-dir)
      (lambda (pair) (car pair)))))

(defun tern-restart-server (project-dir)
  (let ((pair (cc:dataflow-get-sync tern-server-map project-dir)))
    (when (and pair (process-live-p (cadr pair)))
      (interrupt-process (cadr pair)))))

(defun tern-init-server-map ()
  (unless tern-server-map
    (setq tern-server-map (cc:dataflow-environment))
    (cc:dataflow-connect 
     tern-server-map 'get-first 
     (lambda (arg)
       (destructuring-bind (sym (dir)) arg
         ;(message ">> start [%s] " dir)
         (tern-start-server dir))))))

(defvar tern-home
  (let ((script-file (or load-file-name
                         (and (boundp 'bytecomp-filename) bytecomp-filename)
                         buffer-file-name)))
    (expand-file-name ".." (file-name-directory script-file)))
  "Return tern install directory."
  )

(defvar tern-command
  (let ((bin-file (expand-file-name "bin/tern" tern-home)))
    (list (if (file-exists-p bin-file) bin-file "tern")))
  "The command to be run to start the Tern server. Should be a
list of strings, giving the binary name and arguments.")

(defun tern-start-server (dir)
  (lexical-let*
      ((default-directory tern-project-dir)
       (proc (apply #'start-process "Tern" nil tern-command)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel 
     proc (lambda (_proc _event)
            (delete-process proc) 
            ;(message "tern-server-exit : [%s]" dir)
            (cc:dataflow-clear tern-server-map dir)))
    (set-process-filter
     proc (lambda (proc output)
            (when (string-match "Listening on port \\([0-9][0-9]*\\)" output)
              (setf tern-known-port (string-to-number (match-string 1 output)))
              (set-process-filter proc nil)
              ;(message ">> set [%s] [%s]" dir tern-known-port)
              (cc:dataflow-set tern-server-map dir (list tern-known-port proc)))))))

(defvar tern-command-generation 0)
(defvar tern-activity-since-command -1)
(defvar tern-last-point-pos nil)

(defvar tern-known-port nil)
(defvar tern-project-dir nil)

(defvar tern-last-completions nil)
(defvar tern-last-argument-hints nil)
(defvar tern-buffer-is-dirty nil)

(defun tern-project-relative-file ()
  (substring (buffer-file-name) (length (tern-project-dir))))

(defun tern-get-partial-file (at)
  (let* (min-indent start-pos end-pos
         (min-pos (max 0 (- at 2000))))
    (save-excursion
      (goto-char at)
      (loop
       (unless (re-search-backward "\\bfunction\\b" min-pos t) (return))
       (let ((indent (current-indentation))
             (pos (line-beginning-position)))
         (when (or (not min-indent) (< indent min-indent))
           (setf min-indent indent min-indent-pos pos))
         (goto-char pos)))
      (unless start-pos (goto-char min-pos) (setf start-pos (line-beginning-position))))
    (save-excursion
      (goto-char (min (+ at 1000) (point-max)))
      (let ((line-beg (line-beginning-position)))
        (setf end-pos (if (<= line-beg at) (line-end-position) line-beg))))
    `((type . "part")
      (name . ,(tern-project-relative-file))
      (offset . ,(1- start-pos))
      (text . ,(buffer-substring-no-properties start-pos end-pos)))))

(defun tern-modified-sibling-buffers ()
  (let (found)
    (dolist (buf (buffer-list))
      (when (and (not (eq buf (current-buffer)))
                 (buffer-local-value 'tern-mode buf)
                 (buffer-local-value 'tern-buffer-is-dirty buf)
                 (equal tern-project-dir (buffer-local-value 'tern-project-dir buf)))
        (with-current-buffer buf
          (push `((type . "full")
                  (name . ,(tern-project-relative-file))
                  (text . ,(buffer-string))) found))))
    (nreverse found)))

(defun tern-run-request (f doc)
  (deferred:$
    (tern-find-server)
    (deferred:nextc it
      (lambda (port) 
        (tern-request port doc)))
    (deferred:nextc it
      (lambda (json) 
        (funcall f nil json)))
    (deferred:error it
      (lambda (err) 
        (funcall err nil)))))

(defun tern-run-query (f query pos &optional mode)
  (when (stringp query) (setf query `((type . ,query))))
  (let ((generation (incf tern-command-generation))
        (doc `((query . ,query)))
        (files (and (eq mode :full-file) (tern-modified-sibling-buffers)))
        file-name
        (offset 0)
        (pos pos))
    (cond
     ((not tern-buffer-is-dirty) 
      (setf file-name (tern-project-relative-file)))
     ((and (not (eq mode :full-file)) (> (buffer-size) 8000))
      (push (tern-get-partial-file pos) files)
      (setf offset (cdr (assq 'offset (car files)))
            file-name "#0")
      (decf pos offset))
     (t
      (push `((type . "full") (text . ,(buffer-string)) (name . ,(tern-project-relative-file))) files)
      (setf file-name (tern-project-relative-file))))
    (when files (push `(files . ,(apply #'vector files)) doc))
    (push `(file . ,file-name) (cdr (assq 'query doc)))
    (push `(end . ,(1- pos)) (cdr (assq 'query doc)))
    (tern-run-request
     (lambda (err data)
       (when (< tern-activity-since-command generation)
         (cond ((not err)
                (dolist (file files)
                  (when (equal (cdr (assq 'type file)) "full")
                    (with-current-buffer (find-file-noselect (expand-file-name (cdr (assq 'name file)) tern-project-dir))
                      (setf tern-buffer-is-dirty nil))))
                (funcall f data offset))
               ((not (eq mode :silent)) (message "Request failed: %s" (cdr err))))))
     doc)))

(defun tern-send-buffer-to-server ()
  (tern-run-request (lambda (_err _data))
                    `((files . [((type . "full")
                                 (name . ,(tern-project-relative-file))
                                 (text . ,(buffer-string)))]))))

;; Completion

(defun tern-completion-at-point ()
  (or (tern-completion-matches-last)
      (lambda ()
        (tern-run-query #'tern-do-complete "completions" (point)))))

(defun tern-do-complete (data offset)
  (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 offset (cdr (assq 'start data))))
        (end (+ 1 offset (cdr (assq 'end data)))))
    (setf tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
    (completion-in-region start end cs)))

(defun tern-completion-matches-last ()
  (when tern-last-completions
    (destructuring-bind (word start end list) tern-last-completions
      (and (<= end (point-max))
           (equal word (buffer-substring-no-properties start end))
           (if (= (point) end)
               (cdr tern-last-completions)
             (and (>= (point) end)
                  (<= (point) (+ end 50))
                  (string-match-p "^[a-zA-Z0-9_$]*$" (buffer-substring-no-properties end (point)))
                  (let ((new-word (buffer-substring-no-properties start (point))))
                    (list start (point)
                          (loop for elt in list
                                when (eq (compare-strings word 0 (length word) new-word 0 (length word)) t)
                                collect elt)))))))))

;; Argument hints

(defun tern-update-argument-hints ()
  (let ((opening-paren (cadr (syntax-ppss))))
    (when (and opening-paren (equal (char-after opening-paren) ?\())
      (if (and tern-last-argument-hints (eq (car tern-last-argument-hints) opening-paren))
          (tern-show-argument-hints)
        (tern-run-query (lambda (data _offset)
                          (let ((type (tern-parse-function-type data)))
                            (when type
                              (setf tern-last-argument-hints (cons opening-paren type))
                              (tern-show-argument-hints))))
                        `((type . "type")
                          (preferFunction . t))
                        opening-paren
                        :silent)))))

(defun tern-skip-matching-brackets (end-chars)
  (let ((depth 0) (end (+ (point) 500)))
    (loop while (< (point) (point-max)) do
          (let ((next (char-after (point))))
            (cond
             ((and (<= depth 0) (find next end-chars)) (return t))
             ((or (eq next ?\)) (eq next ?\]) (eq next ?\})) (decf depth))
             ((or (eq next ?\() (eq next ?\[) (eq next ?\{)) (incf depth))
             ((> (point) end) (return nil)))
            (forward-char)))))

(defun tern-parse-function-type (data)
  (let ((type (cdr (assq 'type data)))
        (name (or (cdr (assq 'exprName data)) (cdr (assq 'name data)) "fn")))
    (when (string-match-p "^fn(" type)
      (with-temp-buffer
        (insert type)
        (goto-char 4)
        (let (args retval)
          (loop until (eq (char-after (point)) ?\)) do
                (let ((name (when (looking-at "\\([a-zA-Z0-9_$?]*\\):\\s-*")
                              (goto-char (match-end 0))
                              (match-string 1)))
                      (typestart (point)))
                  (tern-skip-matching-brackets '(?\) ?\,))
                  (push (cons name (buffer-substring typestart (point))) args))
                (when (eq (char-after (point)) ?\,) (forward-char 2)))
          (when (looking-at ") -> ")
            (setf retval (buffer-substring (+ (point) 5) (point-max))))
          (list name (nreverse args) retval))))))

(defun tern-find-current-arg (start)
  (when (< (point) (+ start 500))
    (save-excursion
      (let ((cur-point (point)))
        (goto-char (1+ start))
        (loop for i from 0 do
              (let ((found-end (tern-skip-matching-brackets '(?\) ?\,))))
                (when (>= (point) cur-point) (return i))
                (when (or (not found-end) (looking-at ")")) (return nil))
                (forward-char 1)))))))

(defun tern-show-argument-hints ()
  (declare (special message-log-max))
  (destructuring-bind (paren . type) tern-last-argument-hints
    (let ((parts ())
          (current-arg (tern-find-current-arg paren)))
      (destructuring-bind (name args ret) type
        (push (propertize name 'face 'font-lock-function-name-face) parts)
        (push "(" parts)
        (loop for arg in args for i from 0 do
              (unless (zerop i) (push ", " parts))
              (let ((name (or (car arg) "?")))
                (push (if (eq i current-arg) (propertize name 'face 'highlight) name) parts))
              (unless (equal (cdr arg) "?")
                (push ": " parts)
                (push (propertize (cdr arg) 'face 'font-lock-type-face) parts)))
        (push ")" parts)
        (when ret
          (push " -> " parts)
          (push (propertize ret 'face 'font-lock-type-face) parts)))
      (let (message-log-max
            (str (apply #'concat (nreverse parts))))
        (message str)
        (when (featurep 'popup)
          (tern-show-argument-hints-popup))))))

(defun tern-show-argument-hints-popup ()
  (let ((paren (car tern-last-argument-hints))
        (type (cdr tern-last-argument-hints)))
    (let ((parts ()) (poss ())
          (current-arg (tern-find-current-arg paren)))
      (destructuring-bind (name args ret) type
        (push "(" parts) (push " " poss)
        (loop for arg in args for i from 0 do
              (unless (zerop i) 
                (push ", " parts) (push "  " poss))
              (let ((posc (if (eq i current-arg) ?^ ? ))
                    (argname (car arg)) (argtype (cdr arg)))
                (when argname
                  (push (make-string (string-width argname) posc) poss)
                  (push argname parts)
                  (push ": " parts)
                  (push (make-string (string-width ": ") posc) poss))
                (push argtype parts)
                (push (make-string (string-width argtype) posc) poss)))
        (push ")" parts) (push " " poss)
        (when ret
          (push " -> " parts)
          (push ret parts))
        (popup-tip (concat 
                    (apply #'concat (nreverse parts)) "\n"
                    (apply #'concat (nreverse poss))
                    )
                   :point paren)))))

;; Refactoring ops

(defun tern-do-refactor (data _offset)
  (let ((per-file ())
        (orig-buffer (current-buffer)))
    (loop for change across (cdr (assq 'changes data)) do
          (let ((found (assoc-string (cdr (assq 'file change)) per-file)))
            (unless found (setf found (list (cdr (assq 'file change)))) (push found per-file))
            (push change (cdr found))))
    (loop for (file . changes) in per-file do
          (setf changes (sort changes (lambda (a b) (> (cdr (assq 'start a)) (cdr (assq 'start b))))))
          (find-file (expand-file-name file (tern-project-dir)))
          (loop for change in changes do
                (let ((start (1+ (cdr (assq 'start change))))
                      (end (1+ (cdr (assq 'end change)))))
                (delete-region start end)
                (save-excursion
                  (goto-char start)
                  (insert (cdr (assq 'text change)))))))
    (switch-to-buffer orig-buffer)))

(defun tern-rename-variable (new-name)
  (interactive "MNew variable name: ")
  (tern-run-query #'tern-do-refactor `((type . "rename") (newName . ,new-name)) (point) :full-file))

;; Jump-to-definition

(defvar tern-find-definition-stack ())

(defun tern-show-definition (data _offset)
  (let* ((file (cdr (assq 'file data)))
         (found (and file (setf file (expand-file-name (cdr (assq 'file data)) (tern-project-dir)))
                     (tern-find-position file data))))
    (if found
        (progn
          (push (cons (buffer-file-name) (point)) tern-find-definition-stack)
          (let ((too-long (nthcdr 20 tern-find-definition-stack)))
            (when too-long (setf (cdr too-long) nil)))
          (tern-go-to-position file found))
      (let ((url (cdr (assq 'url data))))
        (if url
            (browse-url url)
          (message "No definition found."))))))

(defun tern-at-interesting-expression ()
  (if (member (get-text-property (point) 'face)
              '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-string-face))
      nil
    (let ((around (buffer-substring-no-properties (max 1 (1- (point))) (min (1+ (point)) (point-max)))))
      (string-match "\\sw" around))))

(defun tern-find-definition (&optional prompt-var)
  (interactive)
  (let ((varname (and (or prompt-var (not (tern-at-interesting-expression)))
                      (read-from-minibuffer "Variable: "))))
    (tern-run-query #'tern-show-definition `((type . "definition") (variable . ,varname)) (point))))

(defun tern-find-definition-by-name ()
  (interactive)
  (tern-find-definition t))

(defun tern-find-position (file data)
  (with-current-buffer (find-file-noselect file)
    (let* ((start (1+ (cdr (assq 'start data))))
           (cx-start (- start (cdr (assq 'contextOffset data))))
           (cx (cdr (assq 'context data)))
           (cx-end (+ cx-start (length cx))))
      (if (and (<= (point-max) cx-end) (equal (buffer-substring-no-properties cx-start cx-end) cx))
          start
        (let (nearest nearest-dist)
          (save-excursion
            (goto-char (point-min))
            (loop
             (unless (search-forward cx nil t) (return))
             (let* ((here (- (point) (length cx)))
                    (dist (abs (- cx-start here))))
               (when (or (not nearest-dist) (< dist nearest-dist))
                 (setf nearest here nearest-dist dist)))))
          (when nearest
            (+ nearest (- start cx-start))))))))

(defun tern-pop-find-definition ()
  (interactive)
  (when tern-find-definition-stack
    (destructuring-bind (file . pos) (pop tern-find-definition-stack)
      (tern-go-to-position file pos))))

(defun tern-go-to-position (file pos)
  (find-file file)
  (goto-char (min pos (point-max))))

;; Query type

(defun tern-get-type ()
  (interactive)
  (tern-run-query (lambda (data _offset) (message (or (cdr (assq 'type data)) "Not found")))
                  "type"
                  (point)))

;; Display docs

(defvar tern-last-docs-url nil)
(defun tern-get-docs ()
  (interactive)
  (if (and tern-last-docs-url (eq last-command 'tern-get-docs))
      (progn
        (browse-url tern-last-docs-url)
        (setf tern-last-docs-url nil))
    (tern-run-query (lambda (data _offset)
                      (let ((url (cdr (assq 'url data))) (doc (cdr (assq 'doc data))))
                        (cond (doc
                               (setf tern-last-docs-url url)
                               (message doc))
                              (url
                               (browse-url url))
                              (t (message "Not found")))))
                    "documentation"
                    (point))))

;; Mode plumbing

(defun tern-before-change (start end)
  (if tern-buffer-is-dirty
      (setf (car tern-buffer-is-dirty) (min (car tern-buffer-is-dirty) start)
            (cdr tern-buffer-is-dirty) (max (cdr tern-buffer-is-dirty) end))
    (setf tern-buffer-is-dirty (cons start end)))
  (when (> (- (cdr tern-buffer-is-dirty) (car tern-buffer-is-dirty)) 4000)
    (run-at-time "200 millisec" nil (lambda ()
                                      (when tern-buffer-is-dirty
                                        (setf tern-buffer-is-dirty nil)
                                        (tern-send-buffer-to-server)))))
  (setf tern-last-point-pos nil)
  (when (and tern-last-argument-hints (<= (point) (car tern-last-argument-hints)))
    (setf tern-last-argument-hints nil)))

(defun tern-post-command ()
  (unless (eq (point) tern-last-point-pos)
    (setf tern-last-point-pos (point))
    (setf tern-activity-since-command tern-command-generation)
    (tern-update-argument-hints)))

(defun tern-left-buffer ()
  (declare (special buffer-list-update-hook))
  (when (and tern-buffer-is-dirty (not (buffer-file-name (car (buffer-list)))))
    (setf tern-buffer-is-dirty nil)
    (let ((buffer-list-update-hook ()))
      (tern-send-buffer-to-server))))

(defvar tern-mode-keymap (make-sparse-keymap))
(define-key tern-mode-keymap [(meta ?.)] 'tern-find-definition)
(define-key tern-mode-keymap [(control meta ?.)] 'tern-find-definition-by-name)
(define-key tern-mode-keymap [(meta ?,)] 'tern-pop-find-definition)
(define-key tern-mode-keymap [(control ?c) (control ?r)] 'tern-rename-variable)
(define-key tern-mode-keymap [(control ?c) (control ?c)] 'tern-get-type)
(define-key tern-mode-keymap [(control ?c) (control ?d)] 'tern-get-docs)

(define-minor-mode tern-mode
  "Minor mode binding to the Tern JavaScript analyzer"
  nil
  " Tern"
  tern-mode-keymap
  (if tern-mode (tern-mode-enable) (tern-mode-disable)))

(defun tern-mode-enable ()
  (tern-init-server-map)
  (set (make-local-variable 'tern-known-port) nil)
  (set (make-local-variable 'tern-project-dir) nil)
  (set (make-local-variable 'tern-last-point-pos) nil)
  (set (make-local-variable 'tern-last-completions) nil)
  (set (make-local-variable 'tern-last-argument-hints) nil)
  (set (make-local-variable 'tern-buffer-is-dirty) (and (buffer-modified-p) (cons (point-min) (point-max))))
  (make-local-variable 'completion-at-point-functions)
  (push 'tern-completion-at-point completion-at-point-functions)
  (add-hook 'before-change-functions 'tern-before-change nil t)
  (add-hook 'post-command-hook 'tern-post-command nil t)
  (add-hook 'buffer-list-update-hook 'tern-left-buffer nil t)
  (when auto-complete-mode
    (tern-ac-setup)))

(defun tern-mode-disable ()
  (setf completion-at-point-functions
        (remove 'tern-completion-at-point completion-at-point-functions))
  (remove-hook 'before-change-functions 'tern-before-change t)
  (remove-hook 'post-command-hook 'tern-post-command t)
  (remove-hook 'buffer-list-update-hook 'tern-left-buffer t))



;;; Completion

(defvar tern-ac-on-dot t)

(defvar tern-ac-complete-reply nil
  "tern-ac-complete-reply.")

(defvar tern-ac-complete-request-point 0
  ;; It is passed to `=', so do not initialize this value by `nil'.
  "The point where `tern-ac-complete-request' is called.")

(defun tern-ac-complete-request ()
  (setq tern-ac-complete-reply nil)
  (setq tern-ac-complete-request-point (point))
  (tern-run-query #'tern-ac-complete-response "completions" (point)))

(defun tern-ac-complete-response (data offset)
  (let ((cs (loop for elt across (cdr (assq 'completions data)) collect elt))
        (start (+ 1 offset (cdr (assq 'start data))))
        (end (+ 1 offset (cdr (assq 'end data)))))
    (setq tern-last-completions (list (buffer-substring-no-properties start end) start end cs))
    (setq tern-ac-complete-reply cs)))

(defun tern-ac-complete ()
  "Complete code at point."
  (interactive)
  (tern-ac-complete-request))

(defun tern-dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (deferred:nextc (tern-ac-complete-request)
    (lambda (x)
      (call-interactively 'ac-start))))

;;; AC source

(defun tern-ac-completion-matches ()
  (mapcar
   (lambda (word)
     (popup-make-item word
                      :symbol "f"))
   tern-ac-complete-reply))

(defun tern-ac-completion-prefix ()
  (or (ac-prefix-default)
      (when (= tern-ac-complete-request-point (point))
        tern-ac-complete-request-point)))

;; (makunbound 'ac-source-tern-completion)
(ac-define-source tern-completion
  '((candidates . tern-ac-completion-matches)
    (prefix . tern-ac-completion-prefix)
    (requires . -1)))

(defun tern-ac-setup ()
  (interactive)
  (if tern-ac-on-dot
      (define-key tern-mode-keymap "." 'tern-dot-complete)
    (define-key tern-mode-keymap "." nil))
  (add-to-list 'ac-sources 'ac-source-tern-completion))


;;; Make project file

(eval-when-compile 
  (defmacro tern-prjfix-collect-gen (target)
    `(loop with plugins-dir = (expand-file-name ,target tern-home)
           for fn in (directory-files plugins-dir t "^[^\\.]")
           collect (list (gensym ,target) 
                         (file-name-sans-extension (file-name-nondirectory fn)) 
                         fn))))

(defun tern-prjfix-collect-libs ()
  (tern-prjfix-collect-gen "defs"))

(defun tern-prjfix-collect-plugins ()
  (tern-prjfix-collect-gen "plugin"))

(defun tern-prjfix-find-by-name (name item-list)
  "ITEM-LIST -> (list (sym pname content) ... )"
  (unless (stringp name)
    (setq name (format "%s" name)))
  (loop for item in item-list
        for pname = (cadr item)
        if (equal name pname) return item))

(defun tern-prjfix-collect-jsfiles (dir &optional base-dir)
  (unless base-dir
    (setq base-dir dir))
  (loop 
   with ret = nil
   for fn in (directory-files dir nil "^[^\\.]")
   for path = (expand-file-name fn dir)
   if (file-directory-p path)
   do (setq ret (append (tern-prjfix-collect-jsfiles path base-dir) ret))
   else
   do (when (equal "js" (file-name-extension fn))
        (let ((name (file-relative-name path base-dir)))
          (setq ret (cons (list name name) ret))))
   finally return ret))

(defun tern-prjfix-make ()
  (interactive)
  (let* ((pdir (tern-project-dir))
         (pfile (expand-file-name ".tern-project" pdir))
         project-data)
    (when (file-exists-p pfile)
      (setq project-data 
            (let ((json-array-type 'list))
              (ignore-errors
                (json-read-file pfile)))))
    (tern-prjfix-dialog-show pdir project-data)))

(defvar tern-prjfix-dialog-before-win-num 0  "[internal] ")

(defun tern-prjfix-dialog-show (pdir project-data)
  (let* ((libs (tern-prjfix-collect-libs))
         (plugins (tern-prjfix-collect-plugins))
         (jsfiles (tern-prjfix-collect-jsfiles pdir))
         (src `(
               ,(propertize "JavaScript Project Setting" 'face 'info-title-1) BR
               "Project Directory : " ,pdir BR BR
               ,(propertize "Project Environments" 'face 'info-title-2) BR
               ,@(loop for (sym name path) in libs
                       append (list `(input :name ,sym :type checkbox)
                                     "  " name 'BR))
               BR ,(propertize "Tern Plugins" 'face 'info-title-2) BR
               ,@(loop for (sym name path) in plugins
                       append (list `(input :name ,sym :type checkbox)
                                    "  " name 'BR))
               BR ,(propertize "Load Eagerly" 'face 'info-title-2) BR
               ,@(loop for (sym name path) in jsfiles
                       append (list `(input :name ,sym :type checkbox)
                                    "  " name 'BR))
               BR BR
               "  " (button :title "OK" :action on-submit :validation t)
               "  " (button :title "Cancel" :action on-cancel)))
        (model 
         (let ((data-plugins (cdr (assoc 'plugins project-data)))
               (data-libs (cdr (assoc 'libs project-data)))
               (data-jsfiles (cdr (assoc 'loadEagerly project-data))))
           (append
            (loop for (sym pname content) in plugins
                  for (name . opts) = (assoc (intern pname) data-plugins)
                  collect (cons sym (and name t)))
            (loop for (sym pname content) in libs
                  collect (cons sym (and (member pname data-libs) t)))
            (loop for (path name) in jsfiles
                  collect (cons path (and (member path data-jsfiles) t))))))
        (validations nil)
        (action-mapping 
         '((on-submit . tern-prjfix-submit-action)
           (on-cancel . tern-prjfix-dialog-kill-buffer)))
        (attributes (list 
                     (cons 'project-dir pdir) (cons 'libs libs)
                     (cons 'jsfiles jsfiles) (cons 'plugins plugins))))
    (setq tern-prjfix-dialog-before-win-num (length (window-list)))
    (pop-to-buffer
     (wmvc:build-buffer 
      :buffer (wmvc:get-new-buffer)
      :tmpl src :model model :actions action-mapping
      :validations validations :attributes attributes))))

(defun tern-prjfix-submit-action (model)
  (let* ((ctx wmvc:context)
         (pdir (wmvc:context-attr-get ctx 'project-dir))
         (pfile (expand-file-name ".tern-project" pdir))
         (plugins (wmvc:context-attr-get ctx 'plugins))
         (libs (wmvc:context-attr-get ctx 'libs))
         (jsfiles (wmvc:context-attr-get ctx 'jsfiles))
         (coding-system-for-write 'utf-8)
         after-save-hook before-save-hook
         (json (json-encode 
                (list
                 (cons 'plugins
                       (loop for (sym pname content) in plugins
                             for (msym . val) = (assoc sym model)
                             if val collect (cons pname (list ))))
                 (cons 'libs
                       (loop for (sym pname content) in libs
                             for (msym . val) = (assoc sym model)
                             if val collect pname))
                 (cons 'loadEagerly
                       (loop for (path name) in jsfiles
                             for (path . val) = (assoc path model)
                             if val collect path)))))
         (buf (find-file-noselect pfile)))
    (unwind-protect
        (with-current-buffer buf
          (set-visited-file-name nil)
          (buffer-disable-undo)
          (erase-buffer)
          (insert json)
          (write-region (point-min) (point-max) pfile nil 'ok))
      (kill-buffer buf))
    (tern-restart-server pdir))
  (tern-prjfix-dialog-kill-buffer))

(defun tern-prjfix-dialog-kill-buffer (&optional model)
  (let ((cbuf (current-buffer))
        (win-num (length (window-list))))
    (when (and (not (one-window-p))
               (> win-num tern-prjfix-dialog-before-win-num))
      (delete-window))
    (kill-buffer cbuf)))

(provide 'tern)
