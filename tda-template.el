;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BEGIN tda-template

(require 'wid-edit)

(defvar tda-buffer-name "*DEMO*"
  "The buffer name of the demo buffer.")

(defvar tda-template-alist--var nil
  "An alist to be used when populating the widget.")
(make-variable-buffer-local 'tda-template-alist--var)

(defvar tda-template-notify--var nil
  "When pressing the SUBMIT button, its widget will process this variable as the
:notify function.  The function takes one argument, which is the return value for
the entire form; i.e., an alist.")
(make-variable-buffer-local 'tda-template-notify--var)

(defun tda-template-current-value ()
  "Get the current value for the alist widget."
  (interactive)
  (let ((alist (widget-value (tda-template-get 'tda-widget))))
    (message "%s" alist)))

(defface tda-widget-field-face
  '((((class color) (background dark))
     (:background "gray15" :foreground "cyan"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t nil))
  "Face used for field text."
  :group 'widget-field
  :group 'widget-faces)

(defvar tda-widget-field-face--var 'tda-widget-field-face
  "Face used for field strings in widgets.
This exists as a variable so it can be set locally in certain buffers.")

(defface tda-read-only-face
  '((t (:foreground "SaddleBrown")))
  "Face for `tda-read-only-face'."
  :group 'tda-template)

;;; Form buttons
(defface tda-template-button-face
  '((((type x w32 mac ns) (class color))
     :background "gray25"
     :box (:line-width 2 :color "magenta" :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify buttons in forms."
  :group 'tda-template)

(defface tda-template-button-mouse-face
  '((((type x w32 mac ns) (class color))
     :background "yellow" :foreground "black"
     :box (:line-width 2 :color "magenta" :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify focused buttons in forms."
  :group 'tda-template)

(defface tda-template-button-pressed-face
  '((((type x w32 mac ns) (class color))
     :background "DodgerBlue" :foreground "black"
     :box (:line-width 2 :color "OrangeRed" :style pressed-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify pressed buttons in forms."
  :group 'tda-template)

(defun tda-widget-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'tda-widget-forward-hook)
  (tda-widget-move arg))

(defun tda-widget-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (run-hooks 'tda-widget-backward-hook)
  (tda-widget-move (- arg)))

(defun tda-widget-move (arg &optional advance)
  "Move point to the ARG next field or button.
ARG may be negative to move backward."
  (or (bobp) (> arg 0) (backward-char))
  (let ((orig-arg arg)
        (wrapped 0)
        (number arg)
        (old (widget-tabable-at)))
    ;; Forward.
    (while (> arg 0)
      (cond ((eobp)
              (goto-char (point-min))
              (setq wrapped (1+ wrapped)))
            (widget-use-overlay-change
              (goto-char (next-overlay-change (point))))
            (t
              (forward-char 1)))
      (and (= wrapped 2)
           (eq arg number)
           (error "No buttons or fields found"))
      (let* ((new (widget-tabable-at))
             (tag (widget-get new :tag)))
        (when (or (and new (string-equal tag "tda-value"))
                  (and new (string-equal tag "SUBMIT")))
          (unless (eq new old)
            (setq arg (1- arg))
            (setq old new)))))
    ;; Backward.
    (while (< arg 0)
      (cond ((bobp)
              (goto-char (point-max))
              (setq wrapped (1+ wrapped)))
            (widget-use-overlay-change
              (goto-char (previous-overlay-change (point))))
            (t
              (backward-char 1)))
      (and (= wrapped 2)
           (eq arg number)
           (error "No buttons or fields found"))
      (let* ((new (widget-tabable-at))
             (tag (widget-get new :tag)))
        (when (and new (not (string-equal tag "tda-key")))
          (unless (eq new old)
            (setq arg (1+ arg))))))
    (let ((new (widget-tabable-at)))
      (while (eq (widget-tabable-at) new)
        (backward-char)))
    (forward-char))
  (widget-echo-help (point))
  (run-hooks 'tda-widget-move-hook))

(defun tda-widget-default-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-specify-insert
   (let ((from (point))
         (tda-tag (widget-get widget :tag))
         button-begin button-end
         sample-begin sample-end
         doc-begin doc-end
         value-pos)
     (insert (widget-get widget :format))
     (goto-char from)
     ;; Parse escapes in format.
     (while (re-search-forward "%\\(.\\)" nil t)
       (let ((escape (char-after (match-beginning 1))))
   (delete-char -2)
   (cond ((eq escape ?%)
    (insert ?%))
         ((eq escape ?\[)
    (setq button-begin (point))
    (insert (widget-get-indirect widget :button-prefix)))
         ((eq escape ?\])
    (insert (widget-get-indirect widget :button-suffix))
    (setq button-end (point)))
         ((eq escape ?\{)
    (setq sample-begin (point)))
         ((eq escape ?\})
    (setq sample-end (point)))
         ((eq escape ?n)
    (when (widget-get widget :indent)
      (insert ?\n)
      (insert-char ?\s (widget-get widget :indent))))
         ((eq escape ?t)
    (let ((image (widget-get widget :tag-glyph))
          (tag (substitute-command-keys
          (widget-get widget :tag))))
      (cond (image
              (widget-image-insert widget (or tag "image") image))
            (tag
              (unless (equal "tda-key" tag)
                (insert tag)))
            (t
              (princ (widget-get widget :value)
               (current-buffer))))))
                ((eq escape ?d)
    (let ((doc (widget-get widget :doc)))
      (when doc
        (setq doc-begin (point))
        (insert (substitute-command-keys doc))
        (while (eq (preceding-char) ?\n)
          (delete-char -1))
        (insert ?\n)
        (setq doc-end (point)))))
         ((eq escape ?h)
    (widget-add-documentation-string-button widget))
         ((eq escape ?v)
    (if (and button-begin (not button-end))
        (widget-apply widget :value-create)
      (setq value-pos (point))))
         (t
    (widget-apply widget :format-handler escape)))))
     ;; Specify button, sample, and doc, and insert value.
     (and button-begin button-end
    (widget-specify-button widget button-begin button-end))
     (and sample-begin sample-end
    (tda-widget-specify-sample widget sample-begin sample-end))
     (and doc-begin doc-end
    (widget-specify-doc widget doc-begin doc-end))
     (when value-pos
       (goto-char value-pos)
       (widget-apply widget :value-create)))
   (let ((from (point-min-marker))
   (to (point-max-marker)))
     (set-marker-insertion-type from t)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to)))
  (widget-clear-undo))

(defun tda-widget-specify-field (widget from to)
  "Specify editable button for WIDGET between FROM and TO."
  ;; Terminating space is not part of the field, but necessary in
  ;; order for local-map to work.  Remove next sexp if local-map works
  ;; at the end of the overlay.
  (save-excursion
    (goto-char to)
    (cond ((null (widget-get widget :size))
             (forward-char 1))
           (widget-field-add-space (insert-and-inherit " ")))
    (setq to (point)))
  (let* ((keymap (widget-get widget :keymap))
         (help-echo (widget-get widget :help-echo))
         (follow-link (widget-get widget :follow-link))
         (rear-sticky
          (or (not widget-field-add-space) (widget-get widget :size)))
         (tag (widget-get widget :tag))
         (face (cond
                 ((equal "tda-key" tag)
                   'tda-read-only-face)
                 ((widget-get widget :value-face))
                 (t 'tda-widget-field-face))))
    (if (functionp help-echo)
      (setq help-echo 'widget-mouse-help))
    (when (= (char-before to) ?\n)
      ;; When the last character in the field is a newline, we want to
      ;; give it a `field' char-property of `boundary', which helps the
      ;; C-n/C-p act more naturally when entering/leaving the field.  We
     ;; do this by making a small secondary overlay to contain just that
      ;; one character.
      (let ((overlay (make-overlay (1- to) to nil t nil)))
        (overlay-put overlay 'field 'boundary)
        ;; We need the real field for tabbing.
        (overlay-put overlay 'real-field widget)
        (overlay-put overlay 'real-tag tag)
        ;; Use `local-map' here, not `keymap', so that normal editing
        ;; works in the field when, say, Custom uses `suppress-keymap'.
        (overlay-put overlay 'local-map keymap)
        (overlay-put overlay 'face face)
        (overlay-put overlay 'follow-link follow-link)
        (overlay-put overlay 'help-echo help-echo))
      (setq to (1- to))
      (setq rear-sticky t))
    ;;; The INS/DEL list mechanisms are designed to include the new-line
    ;;; character.  In the current configuration using `tda-alist', we force
    ;;; the TDA-KEY to behave like a fixed size text box.  The _current_ value
    ;;; gathering mechanism relies upon the WIDGET :field-overlay that is one (1)
    ;;; character _beyond_ the TDA-KEY field.  The character immediately after
    ;;; the TDA-KEY is blank space preceding a semicolon pursuant to
    ;;; `tda-widget-field-value-create'.  In the event that a background color is
    ;;; desired for the TDA-KEY field, then using text-properties instead of
    ;;; overlays seems to be a viable solution because the WIDGET remains the same.
    ;;; In the event that FIELD is used by other functions such as C-n/C-p, we 
    ;;; leave that as an overlay irrespective of whether we are at a TDA-KEY.
    (let ((overlay (make-overlay from to nil nil rear-sticky)))
      (widget-put widget :field-overlay overlay)
      ;;; The key is editable by default, however, @lawlist wants it read-only.
      (when (equal "tda-key" tag)
        (put-text-property from (1+ from) 'front-sticky '(read-only))
        (put-text-property from (1- to) 'read-only 'fence))
      (overlay-put overlay 'field widget)
      (if (equal "tda-key" tag)
        (add-text-properties from (1- to) (list 'tag tag
                                                'local-map keymap
                                                'face face
                                                'help-echo help-echo
                                                'follow-link follow-link))
        (overlay-put overlay 'tag tag)
        (overlay-put overlay 'local-map keymap)
        (overlay-put overlay 'face face)
        (overlay-put overlay 'help-echo help-echo)
        (overlay-put overlay 'follow-link follow-link))))
  (widget-specify-secret widget))

(defun tda-widget-setup ()
  "Setup current buffer so editing string widgets works."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        field)
    (while widget-field-new
      (setq field (car widget-field-new)
      widget-field-new (cdr widget-field-new)
      widget-field-list (cons field widget-field-list))
      (let ((from (car (widget-get field :field-overlay)))
            (to (cdr (widget-get field :field-overlay))))
        (tda-widget-specify-field field
                  (marker-position from) (marker-position to))
        (set-marker from nil)
        (set-marker to nil))))
  (widget-clear-undo)
  (widget-add-change))

(defun tda-widget-alist-convert-widget (widget)
  ;; Handle `:options'.
  (let* ((options (widget-get widget :options))
         (widget-alist-value-type (widget-get widget :value-type))
         (other `(tda-editable-list :inline t
              (cons :format "%v"
                    ,(widget-get widget :key-type)
                    ,widget-alist-value-type)))
         (args (if options
             (list `(checklist :inline t
                   :greedy t
                   ,@(mapcar 'widget-alist-convert-option
                       options))
             other)
           (list other))))
          (widget-put widget :args args)
          widget))

(defun tda-widget-field-value-create (widget)
  "Create an editable text field."
  (let ((size (widget-get widget :size))
        (value (widget-get widget :value))
        (from (point))
        (tag (widget-get widget :tag))
        ;; This is changed to a real overlay in `widget-setup'.  We
        ;; need the end points to behave differently until
        ;; `widget-setup' is called.
        (overlay (cons (make-marker) (make-marker))))
    (widget-put widget :field-overlay overlay)
    (insert value)
    (and size
         (< (length value) size)
         (insert-char ?\s (- size (length value))))
    (unless (memq widget widget-field-list)
      (setq widget-field-new (cons widget widget-field-new)))
    (move-marker (cdr overlay) (point))
    (set-marker-insertion-type (cdr overlay) nil)
    (cond ((and (equal tag "tda-key")
                (null size))
            (let ((txt ": "))
              ;;; When `widget-field-use-before-change' is non-nil, there is no
              ;;; need to make TXT read-only; i.e., Emacs analyses location of point for us.
              ;; (add-text-properties 0 1 '(front-sticky (read-only)) txt)
              ;; (add-text-properties 0 (length txt) '(keymap dummy read-only fence) txt)
              (insert txt)))
          ((null size)
           (insert ?\n)))
    (move-marker (car overlay) from)
    (set-marker-insertion-type (car overlay) t)))

(defun tda-widget-sexp-value-to-internal (_widget value)
  ;; Use pp for printer representation.
  (let ((pp (if (symbolp value)
              (prin1-to-string value)
              (pp-to-string value))))
    (while (string-match "\n\\'" pp)
      (setq pp (substring pp 0 -1)))
    (cond ((or (string-match "\n\\'" pp)
              (> (length pp) 40))
            (concat "\n" pp))
          ((null value)
            "")
          (t pp))))

;;; NOTE:  This is useful to colorize the field even when there is no express
;;;        format defined `%{ %}` -- i.e., abusing the sample feature.  The color
;;;        may also be selected if the widget contains the :sample-face property.
;;;        In the latter situation, uncomment the line with (widget-apply widget :sample-face-get)
;;;        and comment out the line containing the fixed definition of `tda-widget-field-face'.
(defun tda-widget-specify-sample (widget from to)
  "Specify sample for WIDGET between FROM and TO."
  (let ((overlay (make-overlay from to nil t nil)))
    ;; (overlay-put overlay 'face (widget-apply widget :sample-face-get))
    (overlay-put overlay 'face 'tda-widget-field-face)
    (overlay-put overlay 'evaporate t)
    (widget-put widget :sample-overlay overlay)))

(defvar tda-widget-field-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'tda-template-current-value)
    (define-key map "\C-c\C-q" 'quit-window--kill-buffer)
    (define-key map "\t" 'tda-widget-forward)
    (define-key map "\e\t" 'tda-widget-backward)
    (define-key map [(shift tab)] 'tda-widget-backward)
    (put 'tda-widget-backward :advertised-binding [(shift tab)])
    (define-key map [backtab] 'tda-widget-backward)
    ;; Since the widget code uses a `field' property to identify fields,
    ;; ordinary beginning-of-line does the right thing.
    ;;  (define-key map "\C-a" 'widget-beginning-of-line)
    (define-key map "\C-e" 'widget-end-of-line)
    (define-key map "\C-k" 'widget-kill-line)
    (define-key map "\M-\t" 'widget-complete)
    (define-key map "\C-m" 'widget-field-activate)
    ;; The following definition needs to avoid using escape sequences that
    ;; might get converted to ^M when building loaddefs.el
    (define-key map [(control ?m)] 'widget-button-press)
    ;; (define-key map [return] 'widget-field-activate)
    (define-key map [return] 'widget-button-press)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.
Note that such modes will need to require wid-edit.")

(define-widget 'tda-default nil
  "Basic widget other widgets are derived from."
  :value-to-internal (lambda (_widget value) value)
  :value-to-external (lambda (_widget value) value)
  :button-prefix 'widget-button-prefix
  :button-suffix 'widget-button-suffix
  :completions-function #'widget-default-completions
  :create 'tda-widget-default-create
  :indent nil
  :offset 0
  :format-handler 'widget-default-format-handler
  :button-face-get 'widget-default-button-face-get
  :mouse-face-get 'widget-default-mouse-face-get
  :sample-face-get 'widget-default-sample-face-get
  :delete 'widget-default-delete
  :copy 'identity
  :value-set 'widget-default-value-set
  :value-inline 'widget-default-value-inline
  :value-delete 'ignore
  :default-get 'widget-default-default-get
  :menu-tag-get 'widget-default-menu-tag-get
  :validate #'ignore
  :active 'widget-default-active
  :activate 'widget-specify-active
  :deactivate 'widget-default-deactivate
  :mouse-down-action #'ignore
  :action 'widget-default-action
  :notify 'widget-default-notify
  :prompt-value 'widget-default-prompt-value)

(define-widget 'tda-editable-list 'tda-default
  "A variable list of widgets of the same type."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :offset 0
  :format "%v"
  :format-handler 'widget-editable-list-format-handler
  :entry-format "%v"
  :value-create 'widget-editable-list-value-create
  :value-get 'widget-editable-list-value-get
  :validate 'widget-children-validate
  :match 'widget-editable-list-match
  :match-inline 'widget-editable-list-match-inline
  :insert-before 'widget-editable-list-insert-before
  :delete-at 'widget-editable-list-delete-at)

(define-widget 'tda-editable-field 'tda-default
  "An editable text field.
Note: In an `editable-field' widget, the `%v' escape must be preceded
by some other text in the `:format' string (if specified)."
  :convert-widget 'widget-value-convert-widget
  :keymap tda-widget-field-keymap
  :format "%v"
  :help-echo "??? M-TAB: complete field; RET: enter value ???"
  :value ""
  :prompt-internal 'widget-field-prompt-internal
  :prompt-history 'widget-field-history
  :prompt-value 'widget-field-prompt-value
  :action 'widget-field-action
  :validate 'widget-field-validate
  :valid-regexp ""
  :error "Field's value doesn't match allowed forms"
  :value-create 'tda-widget-field-value-create
  :value-set 'widget-field-value-set
  :value-delete 'widget-field-value-delete
  :value-get 'widget-field-value-get
  :match 'widget-field-match)

(define-widget 'tda-sexp 'tda-editable-field
  "An arbitrary Lisp expression."
  :tag "Lisp expression"
  :format "%v"
  :value nil
  :validate 'widget-sexp-validate
  :match (lambda (_widget _value) t)
  :value-to-internal 'tda-widget-sexp-value-to-internal
  :value-to-external
    (lambda (widget value)
      (let ((tag (widget-get (widget-get widget :parent) :value))
            (error-func
              `(lambda (var-one var-two)
                 (let ((debug-on-quit nil)
                       (msg (format "tda-sexp:  tag %s | value (%s)" var-one var-two)))
                   (signal 'quit `(,msg))))))
        (if (string-equal value "")
          (funcall error-func tag value)
          (read value))))
  :prompt-history 'widget-sexp-prompt-value-history
  :prompt-value 'widget-sexp-prompt-value)

(define-widget 'tda-group 'tda-default
  "A widget which groups other widgets inside."
  :convert-widget 'widget-types-convert-widget
  :copy 'widget-types-copy
  :format "%v"
  :value-create 'widget-group-value-create
  :value-get 'widget-editable-list-value-get
  :default-get 'widget-group-default-get
  :validate 'widget-children-validate
  :match 'widget-group-match
  :match-inline 'widget-group-match-inline)

(define-widget 'tda-alist 'tda-group
  "An association list."
  :key-type '(tda-sexp :tag "tda-key")
  :value-type '(tda-sexp :tag "tda-value")
  :convert-widget 'tda-widget-alist-convert-widget
  :tag "Alist")

(defvar tda-template-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f5] 'sound)
    map)
  "Keymap for `tda-template-mode'.  The main body of the editable widget, however,
uses `tda-widget-field-keymap'.")

(define-derived-mode tda-template-mode nil "WDemo"
  "Widget demo.
\\{tda-template-mode-map}"
  (make-local-variable 'tda-form)
  (make-local-variable 'tda-anchors)
  (make-local-variable 'widget-button-face)
  (make-local-variable 'widget-mouse-face)
  (make-local-variable 'widget-button-pressed-face)
  (setq widget-button-face 'tda-template-button-face)
  (setq widget-button-pressed-face 'tda-template-button-pressed-face)
  (setq widget-mouse-face 'tda-template-button-mouse-face))

(defun tda-template-get (id)
  (cdr (assoc id widget-demo-form)))

(defun tda-template-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) widget-demo-form)))

(defun tda-template ()
  "Show widget demo."
  (interactive)
  (let ((buffer (get-buffer-create tda-buffer-name))
        (buffer-undo-list t))
    (select-window (display-buffer buffer))
    (with-current-buffer buffer
      (unless (eq major-mode 'tda-template-mode)
        (tda-template-mode))
      (setq widget-demo-form nil
            widget-demo-anchors nil
            widget-field-list nil
            tda-template-alist--var '((sym)
                                  (qty)
                                  (price-buy)
                                  (price-sell)
                                  (price-stop)
                                  (failsafe))
            tda-template-notify--var
              (lambda (return-value)
                (message "%s" return-value)))
      (let* ((inhibit-read-only t)
             (before-change-functions nil)
             (after-change-functions nil))
        (remove-overlays)
        (erase-buffer)
        (tda-template-create 'tda-widget
          (widget-create 'tda-alist
                         :format "%v"
                         :value tda-template-alist--var))
        (widget-create 'push-button
                       :tag "SUBMIT"
                       :format "%[%v%]"
                       :keymap tda-widget-field-keymap
                       :notify `(lambda (_wid &rest _ignore)
                                 (let ((ret (widget-value
                                              (tda-template-get 'tda-widget))))
                                   (if (not (null ,tda-template-notify--var))
                                     (funcall ,tda-template-notify--var ret)
                                     (message "tda-template:  Please set `tda-template-notify--var'.")))))
        (tda-widget-setup))
        (goto-char (point-min))
        (tda-widget-move 1)
      (setq-local face-remapping-alist '((default (:height 2.0736) default)))
        (setq buffer-undo-list nil
              undo-tree-list nil))))

;;; END tda-demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;