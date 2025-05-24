;;; bonk.el --- Context manager for local LLM usage -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: nlev
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (magit-section "3.0.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/nlevnaut/bonk

;;; Commentary:

;; Bonk provides lightweight, named "contexts" composed of files, buffers, or
;; arbitrary regions within them.  A context can then be exported as
;; XML-wrapped text, markdown, or any other format you configure for consumption
;; by LLM tooling.
;;
;;  * Create multiple named contexts.
;;  * Add / remove whole files, whole buffers, or specific line ranges.
;;  * View a context in a Magit-style, collapsible interface.
;;  * Quickly toggle inclusion from Dired (key `b`) or Ibuffer (key `B`).
;;  * Export or save the current context in XML (default) or Markdown.
;;  * Seamless switching via `completing-read` with history.
;;
;;  (M-x bonk-switch-context)   – switch / create context.
;;  (M-x bonk-add-file)         – add file (prefix C-u to specify line range).
;;  (M-x bonk-add-buffer)       – add buffer (prefix C-u for range).
;;  (M-x bonk-add-region)       – add active region (lines).
;;  (M-x bonk-view)             – inspect context.
;;  (M-x bonk-format-context)   – return formatted string.
;;  (M-x bonk-save-context)     – write formatted context to file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'xml)
(require 'dired)
(require 'magit-section nil t)          ; optional but recommended

(defgroup bonk nil
  "Context manager for local LLM usage."
  :group 'tools
  :prefix "bonk-")


;;; Customisation ------------------------------------------------------------

(defcustom bonk-context-export-backend 'xml
  "Backend used when exporting contexts.
Must be one of the symbols `'xml` or `'markdown`.  Users can switch backends on
the fly, e.g. (setq bonk-context-export-backend 'markdown)"
  :type '(choice (const :tag "XML" xml)
                 (const :tag "Markdown" markdown)))

(defcustom bonk-export-major-mode nil
  "Major mode for *Bonk Export* buffers.
Nil means leave the buffer in `fundamental-mode` (recommended, avoids XML parser
errors).  Set to `xml-mode` or `markdown-mode` if you prefer syntax
highlighting and your content is guaranteed to be clean."
  :type '(choice (const :tag "Fundamental" nil)
                 (function :tag "Specific major mode")))


;;; Internal data structures --------------------------------------------------

(cl-defstruct (bonk-entry (:constructor bonk-entry-create))
  "A single item in a Bonk context.
Supports either static line ranges (start-line/end-line) or dynamic
marker ranges (start-marker/end-marker).  If a buffer dies, we fall
back to :file-path + line numbers for persistence."
  type
  name
  file-path      ;; absolute file name or nil
  start-line     ;; last-known static start (integer)
  end-line       ;; last-known static end   (integer)
  start-marker   ;; dynamic marker for region start
  end-marker)    ;; dynamic marker for region end

(defvar bonk--contexts (make-hash-table :test #'equal)
  "Table of contexts keyed by name → plist (:entries :created :updated).")

(defvar bonk-current-context nil
  "Name of the active Bonk context.")


;; ------------------------------------------------------------------
;; Live view refresh helpers
;; ------------------------------------------------------------------

(defun bonk--refresh-view-buffers ()
  "If any *Bonk View* buffers are live, run `bonk--view-refresh` in them."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'bonk-view-mode)
        (bonk--view-refresh)))))

(defun bonk--ts ()
  "Return current timestamp (ISO-8601)."
  (format-time-string "%FT%T%z"))

(defun bonk--context-plist (name)
  "Return plist for context NAME; create if absent."
  (or (gethash name bonk--contexts)
      (puthash name
               (list :entries nil :created (bonk--ts) :updated (bonk--ts))
               bonk--contexts)))

(defun bonk--update-ts (ctx-name)
  "Update the :updated timestamp for context CTX-NAME in `bonk--contexts`."
  (let* ((plist    (bonk--context-plist ctx-name))
         (new-ts   (bonk--ts))
         (new-plist (plist-put plist :updated new-ts)))
    (puthash ctx-name new-plist bonk--contexts)))

(defun bonk--context-names ()
  "Return a list of existing context names as strings."
  (let (names)
    (maphash (lambda (k _v) (push k names)) bonk--contexts)
    names))

(defun bonk--read-context-name (&optional prompt default)
  "Read a context name from the minibuffer with completion.
PROMPT is the message shown in the minibuffer.  DEFAULT, when non-nil, is
suggested as the default value if the user simply hits RET.
Existing context names are provided as completion candidates, but any new
name is accepted as well."
  (let ((hist 'bonk--context-history))
    (completing-read (or prompt "Context: ")
                     (bonk--context-names)
                     nil                ; COLLECTION is list → all allowed
                     nil                ; PREDICATE → accept anything
                     nil                ; INITIAL-INPUT
                     hist               ; HISTORY
                     default)))


;;; Switching contexts --------------------------------------------------------

;;;###autoload
(defun bonk-switch-context (name)
  "Switch to (or create) context NAME.
If called interactively, offers `completing-read` completion over existing
contexts, with the current context as the default."  
  (interactive
   (list (bonk--read-context-name "Switch to context: " bonk-current-context)))
  (setq bonk-current-context name)
  (bonk--context-plist name)
  (bonk--refresh-view-buffers)
  (message "Bonk: current context is now '%s'" name))


;;; Entry helpers -------------------------------------------------------------

(defun bonk--equal-entry-p (a b)
  "Return non-nil if entries A and B refer to the same target & range.
Handles both marker-based and static line-based entries."
  (and (eq (bonk-entry-type a) (bonk-entry-type b))
       (equal (bonk-entry-name a) (bonk-entry-name b))
       (cond
        ;; both have markers → compare marker positions
        ((and (bonk-entry-start-marker a) (bonk-entry-start-marker b)
              (bonk-entry-end-marker   a) (bonk-entry-end-marker   b))
         (and (= (marker-position (bonk-entry-start-marker a))
                 (marker-position (bonk-entry-start-marker b)))
              (= (marker-position (bonk-entry-end-marker a))
                 (marker-position (bonk-entry-end-marker b)))))
        ;; fallback: compare static start/end line & file-path
        (t
         (and (equal (bonk-entry-start-line a) (bonk-entry-start-line b))
              (equal (bonk-entry-end-line   a) (bonk-entry-end-line   b))
              (equal (bonk-entry-file-path  a) (bonk-entry-file-path b)))))))

(defun bonk--toggle-entry (entry &optional context)
  "Toggle ENTRY in CONTEXT (defaults to `bonk-current-context`)."
  (let ((ctx (or context bonk-current-context)))
    (unless ctx
      (user-error "No current context.  Use bonk-switch-context."))
    (let* ((plist   (bonk--context-plist ctx))
           (entries (plist-get plist :entries))
           (existing (cl-find-if (lambda (e)
                                   (bonk--equal-entry-p e entry))
                                 entries))
           ;; build the new list of entries
           (new-entries
            (if existing
                (progn
                  (message "Bonk: removed %s" (bonk-entry-name entry))
                  (delq existing entries))
              (progn
                (message "Bonk: added %s" (bonk-entry-name entry))
                (cons entry entries))))
           ;; inject back into the stored plist
           (updated-plist (plist-put plist :entries new-entries)))
      (puthash ctx updated-plist bonk--contexts)
      (bonk--update-ts ctx)
      (bonk--refresh-view-buffers))))

(defun bonk--range-or-nil (arg prompt1 prompt2)
  "If ARG is non-nil read PROMPT1 & PROMPT2 numbers; return (start end)."
  (when arg
    (list (read-number prompt1)
          (read-number prompt2))))

;;;###autoload
(defun bonk-add-file (file &optional arg)
  "Toggle FILE (prefix ARG to specify line range) in current context."
  (interactive "fFile: \nP")
  (when (bonk--probably-binary-file-p file)
    (user-error "Refusing to add binary file %s to context" file))
  (let ((buf (get-file-buffer file)))
    (cl-destructuring-bind (start end)
        (or (bonk--range-or-nil arg "Start line: " "End line: ") '(nil nil))
      ;; If the file is already visited, treat it as a buffer entry so that
      ;; unsaved edits are always in scope.
      (bonk--toggle-entry
       (if (buffer-live-p buf)
           (bonk-entry-create :type 'buffer
                              :name (buffer-name buf)
                              :file-path (expand-file-name file)
                              :start-line start :end-line end)
         (bonk-entry-create :type 'file
                            :name (expand-file-name file)
                            :file-path (expand-file-name file)
                            :start-line start :end-line end))))))

;;;###autoload
(defun bonk-add-buffer (&optional buffer arg)
  "Toggle BUFFER (current by default) into context.  With ARG ask for range."
  (interactive "bBuffer: \nP")
  (let ((buf (or buffer (current-buffer))))
    (unless (bufferp buf) (setq buf (get-buffer buf)))
    (unless buf (user-error "No buffer"))
    (cl-destructuring-bind (start end)
        (or (bonk--range-or-nil arg "Start line: " "End line: ") '(nil nil))
      (bonk--toggle-entry (bonk-entry-create :type 'buffer
                                             :name (buffer-name buf)
                                             :file-path (buffer-file-name buf)
                                             :start-line start :end-line end)))))

;;;###autoload
(defun bonk-add-region (beg end)
  "Toggle the active region BEG..END of the current buffer into context.
Stores it as markers so edits elsewhere don’t shift our region."
  (interactive "r")
  (let* ((buf (current-buffer))
         (sm  (copy-marker beg t))   ; grows at front
         (em  (copy-marker end nil)) ; stays after insert
         ;; capture the static line numbers right away
         (ln1 (line-number-at-pos sm t))
         (ln2 (line-number-at-pos em t)))
    (bonk--toggle-entry
     (bonk-entry-create
      :type         'buffer
      :name         (buffer-name buf)
      :file-path    (buffer-file-name buf)
      :start-line   ln1
      :end-line     ln2
      :start-marker sm
      :end-marker   em))))


;;; Dired / Ibuffer integration ---------------------------------------------

(defvar bonk-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "b") #'bonk-dired-toggle)
    map)
  "Keymap activated in Dired when `bonk-dired-mode' is on.")

;;;###autoload
(define-minor-mode bonk-dired-mode
  "Minimal UI in Dired: press `b` to toggle file in Bonk context."
  :lighter " Bonk" :keymap bonk-dired-mode-map)

;;;###autoload
(defun bonk-dired-toggle ()
  "Add/remove file at point in Dired from context."
  (interactive)
  (bonk-add-file (dired-get-file-for-visit)))

(add-hook 'dired-mode-hook #'bonk-dired-mode)

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "B") #'bonk-ibuffer-toggle))

;;;###autoload
(defun bonk-ibuffer-toggle ()
  "Toggle buffer(s) in Ibuffer into context.
If any buffers are marked, operate on all those; otherwise, operate on the
buffer at point.  Provides feedback for each buffer processed."
  (interactive)
  (require 'ibuffer)
  (let* ((marked (ibuffer-get-marked-buffers))
         (bufs (if marked
                   marked
                 (let ((buf (ibuffer-current-buffer t)))
                   (if buf (list buf) nil)))))
    (if (null bufs)
        (user-error "No buffers to toggle")
      (dolist (buf bufs)
        (bonk-add-buffer buf)))))


;;; Content extraction -------------------------------------------------------

(cl-defmacro bonk--with-file ((file) &rest body)
  "Evaluate BODY in a temp buffer containing FILE's contents."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body))

 
(defun bonk--entry-live-buffer (entry)
  "Return a live buffer that should act as the content source for ENTRY,
or nil if no such buffer exists."
  (pcase (bonk-entry-type entry)
    ('buffer
     (get-buffer (bonk-entry-name entry)))
    ('file
     (get-file-buffer (bonk-entry-name entry)))
    (_ nil)))

(defun bonk--entry-content (entry)
  "Return the text content for ENTRY respecting its line range."
  (let ((live-buf (bonk--entry-live-buffer entry)))
    (cond
     ;; Error if file-path exists but isn’t readable
     ((let ((fp (bonk-entry-file-path entry)))
        (and fp (not (file-readable-p fp))))
      (error "Bonk: file missing or unreadable: %s"
             (bonk-entry-file-path entry)))
     (live-buf
      (with-current-buffer live-buf
        ;; if this entry has dynamic markers, use them
        (if (and (bonk-entry-start-marker entry)
                 (bonk-entry-end-marker   entry))
            (buffer-substring-no-properties
             (marker-position (bonk-entry-start-marker entry))
             (marker-position (bonk-entry-end-marker   entry)))
          ;; else fall back on static line numbers
          (bonk--slice-lines (bonk-entry-start-line entry)
                             (bonk-entry-end-line entry)))))
     ((and (eq (bonk-entry-type entry) 'buffer)
           (bonk-entry-file-path entry)
           (file-readable-p (bonk-entry-file-path entry)))
      ;; buffer entry, dead, fallback to file
      (bonk--with-file ((bonk-entry-file-path entry))
        (bonk--slice-lines (bonk-entry-start-line entry)
                           (bonk-entry-end-line entry))))
     (t
      ;; either a plain file entry or we gave up
      (bonk--with-file ((bonk-entry-name entry))
        (bonk--slice-lines (bonk-entry-start-line entry)
                           (bonk-entry-end-line entry)))))))

(defun bonk--slice-lines (start end)
  "Return region START..END (lines, inclusive) of current buffer, or full."
  (save-excursion
    (let ((beg (if start
                   (progn (goto-char (point-min))
                          (forward-line (1- start))
                          (line-beginning-position))
                 (point-min)))
          (fin (if end
                   (progn (goto-char (point-min))
                          (forward-line (1- end))
                          (line-end-position))
                 (point-max))))
      (buffer-substring-no-properties beg fin))))


;;; XML helper functions -----------------------------------------------------

(defun bonk--format-attrs (alist)
  "Return string of XML attributes from ALIST."  ; ((key . val) ...)
  (concat
   (mapconcat (lambda (kv)
                (format " %s=\"%s\"" (car kv) (bonk--escape-xml (cdr kv))))
              alist "")))

(defun bonk--escape-xml (str)
  "Return STR with XML special chars escaped.  Does **not** attempt to make a
valid XML document – that is intentional so we never need to scrub control
characters."
  (xml-escape-string str nil))

(defun bonk--entry-range-lines (entry)
  "Compute a pair (START . END) line numbers for ENTRY, preferring markers.
Ensures that line numbers are calculated in the context of the marker's buffer."
  (let ((start-m (bonk-entry-start-marker entry))
        (end-m (bonk-entry-end-marker entry)))
    (if (and start-m end-m) ; Markers are present in the entry structure
        (let* ((marker-buf (marker-buffer start-m)) ; Buffer the start marker belongs to
               ;; Get positions; these will be nil if marker's buffer was killed
               (start-pos (marker-position start-m))
               (end-pos (marker-position end-m)))
          ;; Check if markers are valid: pointing to the same, live buffer, and positions resolved
          (if (and marker-buf ; Start marker has a buffer
                   (eq marker-buf (marker-buffer end-m)) ; Both markers in the same buffer
                   (buffer-live-p marker-buf) ; That buffer is live
                   start-pos end-pos) ; And their positions are valid (i.e., not nil)
              (with-current-buffer marker-buf
                (cons (line-number-at-pos start-pos t)
                      (line-number-at-pos end-pos t)))
            ;; Fallback to static lines if markers are problematic (e.g., buffer killed, inconsistent)
            (cons (bonk-entry-start-line entry) (bonk-entry-end-line entry))))
      ;; Fallback if entry doesn't have markers set up (e.g. 'file' type entry)
      (cons (bonk-entry-start-line entry) (bonk-entry-end-line entry)))))

(defun bonk-format-entry-xml (entry &optional indent)
  "Return XML string for ENTRY with INDENT spaces (default 2), exporting
marker-based ranges as up-to-date line numbers."
  (let* ((i       (make-string (or indent 2) ?\s))
         (attrs   (list (cons 'type   (symbol-name (bonk-entry-type entry)))
                        (cons 'name   (bonk-entry-name entry))
                        (and (bonk-entry-file-path entry)
                             (cons 'file (bonk-entry-file-path entry)))))
         (rng     (bonk--entry-range-lines entry))
         (attrs   (if (car rng)
                      (append attrs
                              (list (cons 'start (number-to-string (car rng)))
                                    (cons 'end   (number-to-string (cdr rng)))))
                    attrs))
         (content (bonk--escape-xml (bonk--entry-content entry))))
    (concat i
            (format "<entry%s>\n%s\n%s</entry>"
                    (bonk--format-attrs attrs)
                    content
                    i))))


;;; Markdown helper ----------------------------------------------------------

(defun bonk-format-entry-markdown (entry)
  "Return a Markdown string representing ENTRY."
  (let* ((header (format "### %s %s%s\n"
                        (capitalize (symbol-name (bonk-entry-type entry)))
                        (bonk-entry-name entry)
                        ;; compute up-to-date start/end
                        (let ((rng (bonk--entry-range-lines entry)))
                          (if (car rng)
                              (format " (%d-%d)" (car rng) (cdr rng))
                            ""))))
         (body (bonk--entry-content entry)))
    (concat header "\n```\n" body "```\n\n")))


;;; Export backends ----------------------------------------------------------

(defun bonk--context-as-xml (ctx plist)
  "Return context CTX (plist PLIST) as pseudo-XML string."
  (let* ((entries (plist-get plist :entries)))
    (concat (format "<context name=\"%s\" created=\"%s\" updated=\"%s\">\n"
                    ctx (plist-get plist :created) (plist-get plist :updated))
            (mapconcat (lambda (e) (bonk-format-entry-xml e 2)) entries "\n")
            "\n</context>\n")))

(defun bonk--context-as-markdown (ctx plist)
  "Return context CTX (plist PLIST) formatted as Markdown."
  (let* ((entries (plist-get plist :entries)))
    (concat (format "# Context %s\n\n*Created:* %s  \n*Updated:* %s\n\n"
                    ctx (plist-get plist :created) (plist-get plist :updated))
            (mapconcat #'bonk-format-entry-markdown entries "\n")
            "\n")))


;;; Formatting & saving ------------------------------------------------------

;;;###autoload
(defun bonk-format-context (&optional context)
  "Return CONTEXT (string or symbol, default active) formatted per backend.
When called interactively, prompts for the context name using completion, with
current context offered as the default.  The backend is chosen by
`bonk-context-export-backend`."
  (interactive
   (list (bonk--read-context-name "Format context: " bonk-current-context)))
  (let* ((ctx (or context bonk-current-context))
         (plist (bonk--context-plist ctx)))
    (unless ctx (user-error "No active context"))
    (let ((txt (pcase bonk-context-export-backend
                 ('xml       (bonk--context-as-xml ctx plist))
                 ('markdown  (bonk--context-as-markdown ctx plist))
                 (_ (user-error "Unknown backend %s" bonk-context-export-backend)))))
      (if (called-interactively-p 'any)
          (with-current-buffer (get-buffer-create "*Bonk Export*")
            (erase-buffer) (insert txt)
            (when bonk-export-major-mode
              (funcall bonk-export-major-mode))
            (pop-to-buffer (current-buffer)))
        txt))))

;;;###autoload
(defun bonk-save-context (file &optional context)
  "Save CONTEXT (default current) as formatted text into FILE.  Overwrites
without asking if FILE exists.  Respects `bonk-context-export-backend`."  
  (interactive "FSave context to: ")
  (let ((txt (bonk-format-context context)))
    (with-temp-file file (insert txt))
    (message "Bonk: saved context to %s" file)))


;;; Magit-style viewer --------------------------------------------------------

(when (featurep 'magit-section)
  (define-derived-mode bonk-view-mode magit-section-mode "Bonk-View"
    "Major mode for visualising a Bonk context.")

  (define-key bonk-view-mode-map (kbd "d")   #'bonk-view-remove-entry)
  (define-key bonk-view-mode-map (kbd "DEL") #'bonk-view-remove-entry)

  ;; New function to remove entry at point in Bonk view
  (defun bonk-view-remove-entry (&optional section)
    "Remove the Bonk entry whose Magit SECTION is at point.

If SECTION is non-nil (or called non-interactively), operate on
that section instead of `magit-current-section'.  The entry is
deleted from the current context and the view is refreshed.

Bound to \\<bonk-view-mode-map>\\[bonk-view-remove-entry] and
\\<bonk-view-mode-map>DEL in `bonk-view-mode'."
    (interactive)
    ;; Figure out which section we’re on.
    (let* ((section (or section (magit-current-section)))
           ;; We created the section with (magit-insert-section (entry entry) …)
           ;; so its `type' is ’entry and its `value' slot *is* the
           ;; bonk-entry struct we need.
           (entry   (and section
			 (eq (oref section type) 'entry)
			 (oref section value))))
      (unless entry
	(user-error "Point is not on a Bonk entry section"))
      ;; Toggle-remove the entry from the context.
      (bonk--toggle-entry entry)
      (message "Bonk: removed %s" (bonk--entry-description entry))
      ;; The toggle already triggers a view refresh, but Magit may still
      ;; have us on a dead section – move to the next live one if possible.
      (when (magit-section-invisible-p section)
	(magit-section-forward))))

  (with-eval-after-load 'evil
    ;; Put Bonk-view buffers in `motion` state, same as Magit.
    (evil-set-initial-state 'bonk-view-mode 'motion)
    ;; Bind our two keys in that state.
    (evil-define-key 'motion bonk-view-mode-map
      (kbd "d")   #'bonk-view-remove-entry
      (kbd "<del>") #'bonk-view-remove-entry))

  ;;; Updated view refresh to use full content sections
  (defun bonk--view-refresh ()
    "Populate the Bonk view buffer with collapsible sections showing full content."
    (let* ((inhibit-read-only t)
	   (ctx bonk-current-context)
	   (plist (and ctx (bonk--context-plist ctx))))
      (erase-buffer)
      (if (null ctx)
	  (insert "No active Bonk context.\n")
	(let ((entries (plist-get plist :entries)))
	  (magit-insert-section (root)
	    (magit-insert-heading
	      (format "Context %s (items: %d)" ctx (length entries)))
	    (dolist (entry entries)
	      (bonk--insert-entry-section entry)))))
      (goto-char (point-min))))

  (defun bonk--insert-entry-section (entry)
    "Insert magit section for ENTRY, collapsible, using an indirect buffer.
If ENTRY has a file-path, it's labelled as 'File' in the view,
otherwise as 'Buffer'."
    (let* ((file-path (bonk-entry-file-path entry))
	   (display-type-str (if file-path "File" "Buffer"))
	   (name-for-header (bonk-entry-name entry)) ; Buffer name for 'buffer' type, full path for 'file' type
	   (rng (bonk--entry-range-lines entry)) ; Get (start . end) lines, using markers if available

           (header (format "%s %s%s"
                           (capitalize display-type-str)
                           name-for-header
                           (if (car rng) ; If specific start line is present in rng
                               (format " (%d–%d)" (car rng) (cdr rng))
                             "")))

           (live-buf (bonk--entry-live-buffer entry))
           ;; src-buf for content: existing live-buf, or try to open file without selecting
           (src-buf (or live-buf
			(and file-path (find-file-noselect file-path t nil t))))) ; RAWFILE=t, NOWARN=nil, NOSELECT=t

      (if (buffer-live-p src-buf)
          (let* ((start-line-for-narrow (or (car rng) 1)) ; 1-based start line for narrowing
		 (end-line-for-narrow   (or (cdr rng) ; 1-based end line for narrowing
                                            (with-current-buffer src-buf
                                              (line-number-at-pos (point-max) t))))
		 ;; Create an indirect buffer without selecting it
		 (ind-buf (make-indirect-buffer
                           src-buf
                           (generate-new-buffer-name
                            (format "%s<bonk-view>" (buffer-name src-buf))) ; Unique name for indirect buffer
                           t)) ; t means CLONEVARS is nil -> don't select buffer, don't run mode hooks
		 beg end)

            ;; Narrow the indirect buffer to the entry's region
            (with-current-buffer ind-buf
              (widen)
              (goto-char (point-min))
              (forward-line (1- start-line-for-narrow))
              (setq beg (point))
              (goto-char (point-min))
              (forward-line (1- end-line-for-narrow))
              (end-of-line)
              (setq end (point))
              (narrow-to-region beg end))

            ;; Insert a collapsible magit section with the full content
            ;; The (entry entry) part makes 'entry the type and the entry struct the value
            (magit-insert-section (entry entry)
              (magit-insert-heading header)
              (insert "\n") ; Newline before content
              (insert-buffer-substring ind-buf)
              (insert "\n")) ; Newline after content

            ;; Clean up the indirect buffer
            (kill-buffer ind-buf))

	;; Fallback: src-buf is not a live buffer (e.g., file not found or unreadable)
	(magit-insert-section (entry entry) ; Still provide section type/value for consistency
          (magit-insert-heading header) ; Show the header anyway
          (insert "\n")
          (insert (format "  [Content for %s not available. File may be missing or unreadable.]"
                          (or file-path name-for-header)))
          (insert "\n")))))

;;;###autoload
  (defun bonk-view ()
    "Open a buffer displaying the current context in a Magit-like UI."
    (interactive)
    (let ((buf (get-buffer-create "*Bonk View*")))
      (with-current-buffer buf
	(bonk-view-mode)
	(bonk--view-refresh))
      (pop-to-buffer buf)))
  )


(defun bonk--entry-description (entry)
  "Return a one-line string describing ENTRY (type, name, lines)."
  (let* ((type     (capitalize (symbol-name (bonk-entry-type entry))))
	 (name     (bonk-entry-name entry))
	 (rng      (bonk--entry-range-lines entry))
	 (range-str (if (car rng)
			(format " (%d–%d)" (car rng) (cdr rng))
		      "")))
    (concat type " " name range-str)))

;;;###autoload
(defun bonk-remove-entry (entry)
  "Prompt to remove a Bonk ENTRY from the current context."
  (interactive
   (let* ((plist  (bonk--context-plist bonk-current-context))
	  (ents   (plist-get plist :entries))
	  (alts   (mapcar (lambda (e) (cons (bonk--entry-description e) e))
			  ents))
	  (choice (completing-read "Remove entry: "
				   (mapcar #'car alts) nil t)))
     (list (cdr (assoc choice alts)))))
  (unless entry
    (user-error "No entry selected"))
  (bonk--toggle-entry entry)
  (message "Bonk: removed %s" (bonk--entry-description entry))
  (bonk--refresh-view-buffers))

;;;###autoload
(defun bonk-delete-context (name)
  "Delete Bonk context NAME.
Prompts for confirmation, unwires any BUFFER-based hooks, then removes the context."
  (interactive
   (list (bonk--read-context-name "Delete context: " bonk-current-context)))
  (unless (gethash name bonk--contexts)
    (user-error "No such context: %s" name))
  (when (yes-or-no-p (format "Really delete context '%s'? " name))
    ;; first, unwire any buffer‐based hooks, without touching the plist
    (let* ((plist   (bonk--context-plist name))
	   (entries (plist-get plist :entries)))
      (dolist (e entries)
	(when (eq (bonk-entry-type e) 'buffer)
	  (bonk--maybe-remove-buffer-hook (bonk-entry-name e)))))
    ;; now drop the context in one go
    (remhash name bonk--contexts)
    ;; clear if it was current
    (when (equal bonk-current-context name)
      (setq bonk-current-context nil))
    ;; one clean refresh
    (bonk--refresh-view-buffers)
    (message "Bonk: deleted context '%s'" name)))


;;; Binary detection helper ---------------------------------------------------

(defun bonk--probably-binary-file-p (file)
  "Return non-nil if FILE appears to be binary (contains a NUL byte)."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 1024) ; first KB is enough
    (save-excursion
      (goto-char (point-min))
      (search-forward (char-to-string 0) nil t))))
(defcustom bonk-auto-refresh-delay 0.5
  "Seconds of idle time before Bonk view refreshes after a change."
  :type 'number)

(defvar bonk--refresh-timer nil)

(defun bonk--schedule-refresh (&optional delay)
  "Refresh *Bonk View* after DELAY seconds of idle time (default `bonk-auto-refresh-delay')."
  (when bonk--refresh-timer
    (cancel-timer bonk--refresh-timer))
  (setq bonk--refresh-timer
        (run-with-idle-timer
         (or delay bonk-auto-refresh-delay) nil
         #'bonk--idle-update-and-refresh)))

(defun bonk--idle-update-and-refresh ()
  "Idle‐time worker:
1. Walk *this buffer’s* marker‐based entries across *all* contexts, update their
   `:start-line`/`:end-line` to the markers’ current lines.
2. Update timestamps.
3. Refresh any live `*Bonk View*` buffers."
  (let ((this-buf (current-buffer))
        (buf-name  (buffer-name)))
    (maphash
     (lambda (ctx-name plist)
       (dolist (entry (plist-get plist :entries))
         (when (and (eq (bonk-entry-type entry) 'buffer)
                    (string= (bonk-entry-name entry) buf-name)
                    (bonk-entry-start-marker entry)
                    (bonk-entry-end-marker   entry))
           (let* ((sm (marker-position (bonk-entry-start-marker entry)))
                  (em (marker-position (bonk-entry-end-marker   entry)))
                  (ln1 (when sm (line-number-at-pos sm t)))
                  (ln2 (when em (line-number-at-pos em t))))
             (when (and ln1 ln2)
               (setf (bonk-entry-start-line entry) ln1
                     (bonk-entry-end-line   entry) ln2)
               (bonk--update-ts ctx-name))))))
     bonk--contexts))
  (bonk--refresh-view-buffers))


;;; Helpers to see whether a file / buffer lives in ANY context --------------

(defun bonk--file-in-any-context-p (file)
  (let* ((abs (expand-file-name file))
         found)
    (maphash
     (lambda (_ctx plist)
       (unless found
         (setq found
               (seq-find (lambda (e)
                           (and (eq (bonk-entry-type e) 'file)
                                (equal (expand-file-name (bonk-entry-name e)) abs)))
                         (plist-get plist :entries)))))
     bonk--contexts)
    found))

(defun bonk--buffer-in-any-context-p (buf-name)
  (let (found)
    (maphash
     (lambda (_ctx plist)
       (unless found
         (setq found
               (seq-find (lambda (e)
                           (and (eq (bonk-entry-type e) 'buffer)
                                (equal (bonk-entry-name e) buf-name)))
                         (plist-get plist :entries)))))
     bonk--contexts)
    found))


;;; Hooks that actually request a refresh ------------------------------------

(defun bonk--after-save-refresh ()
  "Run from `after-save-hook'.  Schedules a view refresh when the saved
buffer’s file is part of any Bonk context."
  (when (and buffer-file-name
             (bonk--file-in-any-context-p buffer-file-name))
    (bonk--schedule-refresh)))

(add-hook 'after-save-hook #'bonk--after-save-refresh)

(defun bonk--buffer-change-refresh (_beg _end _len)
  "Buffer-local `after-change-functions' handler.
Schedules a view refresh.  Throttling is handled by `bonk--schedule-refresh'."
  (bonk--schedule-refresh))


;;; Managing buffer-local hooks ----------------------------------------------
(defun bonk--on-buffer-kill ()
  "On buffer kill, update static ranges and clear markers for entries in this buffer."
  (let ((buf-name (buffer-name)))
    (maphash
     (lambda (ctx-name plist)
       (dolist (entry (plist-get plist :entries))
         (when (and (eq (bonk-entry-type entry) 'buffer)
                    (equal (bonk-entry-name entry) buf-name)
                    (bonk-entry-start-marker entry)
                    (bonk-entry-end-marker entry))
           (let* ((sm (marker-position (bonk-entry-start-marker entry)))
                  (em (marker-position (bonk-entry-end-marker entry)))
                  (ln1 (when sm (line-number-at-pos sm t)))
                  (ln2 (when em (line-number-at-pos em t))))
             (when (and ln1 ln2)
               (setf (bonk-entry-start-line entry) ln1
                     (bonk-entry-end-line   entry) ln2
                     (bonk-entry-start-marker entry) nil
                     (bonk-entry-end-marker   entry) nil)
               (bonk--update-ts ctx-name))))))
     bonk--contexts)))

(defun bonk--maybe-add-buffer-hook (buf-name)
  "Ensure BUF-NAME has the after-change refresh hook installed."
  (let ((buf (get-buffer buf-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (unless (local-variable-p 'bonk--buffer-hook-installed)
          (add-hook 'after-change-functions
                    #'bonk--buffer-change-refresh nil t)
          (add-hook 'kill-buffer-hook
                    #'bonk--on-buffer-kill nil t)
          (setq-local bonk--buffer-hook-installed t))))))

(defun bonk--maybe-remove-buffer-hook (buf-name)
  "Remove the refresh hook from BUF-NAME if it is no longer referenced."
  (let ((buf (get-buffer buf-name)))
    (when (and (buffer-live-p buf)
               (not (bonk--buffer-in-any-context-p buf-name)))
      (with-current-buffer buf
        (when (and (bound-and-true-p bonk--buffer-hook-installed)
                   (member #'bonk--buffer-change-refresh
                           after-change-functions))
          (remove-hook 'after-change-functions
                       #'bonk--buffer-change-refresh t)
          (remove-hook 'kill-buffer-hook
                       #'bonk--on-buffer-kill t)
          (kill-local-variable 'bonk--buffer-hook-installed))))))


;;; Extend bonk--toggle-entry to wire / unwire hooks -------------------------

(advice-add
 #'bonk--toggle-entry :around
 (lambda (orig entry &optional context)
   (let ((adding (not (cl-find-if (lambda (e) (bonk--equal-entry-p e entry))
                                  (plist-get (bonk--context-plist (or context
                                                                     bonk-current-context))
                                             :entries)))))
     (prog1 (funcall orig entry context)
       (pcase (bonk-entry-type entry)
         ('buffer (if adding
                      (bonk--maybe-add-buffer-hook (bonk-entry-name entry))
                    ;; Removing – we don’t know *which* entry was removed;
                    ;; try to unhook if the buffer isn’t in ANY context any more.
                    (bonk--maybe-remove-buffer-hook (bonk-entry-name entry)))))))))

(defun bonk--after-find-refresh ()
  "If the visited file is part of a Bonk context (as a file entry),
attach the live-update hook so unsaved edits propagate to Bonk view."
  (when (and buffer-file-name
             (bonk--file-in-any-context-p buffer-file-name))
    (bonk--maybe-add-buffer-hook (buffer-name))
    (bonk--schedule-refresh)))

(add-hook 'find-file-hook #'bonk--after-find-refresh)


(provide 'bonk)
;;; bonk.el ends here
