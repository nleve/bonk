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
;; by LLM tooling. Bonk contexts are automatically persisted across Emacs sessions.
;;
;;  * Create multiple named contexts.
;;  * Add / remove whole files, whole buffers, or specific line ranges.
;;  * View a context in a Magit-style, collapsible interface.
;;  * Quickly toggle inclusion from Dired (key `b`) or Ibuffer (key `B`).
;;  * Export or save the current context in XML (default) or Markdown.
;;  * Seamless switching via `completing-read` with history.
;;  * Persistence: Contexts are saved automatically and reloaded on Emacs start.
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
Must be one of the symbols `'xml`, `'markdown` or `plain`.  Users can switch backends on
the fly, e.g. (setq bonk-context-export-backend 'markdown)"
  :type '(choice (const :tag "XML" xml)
                 (const :tag "Markdown" markdown)
                 (const :tag "Plain" plain)))

(defcustom bonk-export-major-mode nil
  "Major mode for *Bonk Export* buffers.
Nil means leave the buffer in `fundamental-mode` (recommended, avoids XML parser
errors).  Set to `xml-mode` or `markdown-mode` if you prefer syntax
highlighting and your content is guaranteed to be clean."
  :type '(choice (const :tag "Fundamental" nil)
                 (function :tag "Specific major mode")))

(defcustom bonk-auto-refresh-delay 0.5
  "Seconds of idle time before Bonk view refreshes after a change."
  :type 'number)

(defcustom bonk-prefix-key "C-x c"
  "Prefix key for bonk commands."
  :type 'string
  :group 'bonk)


;;; Internal data structures --------------------------------------------------

(cl-defstruct (bonk-entry (:constructor bonk-entry-create))
  "A single item in a Bonk context.
Supports either static line ranges (start-line/end-line) or dynamic
marker ranges (start-marker/end-marker).  If a buffer dies, we fall
back to :file-path + line numbers for persistence."
  type
  name
  file-path      ;; absolute file name or nil
  persisted-start-line     ;; last-known static start (integer)
  persisted-end-line       ;; last-known static end   (integer)
  start-marker   ;; dynamic marker for region start
  end-marker)    ;; dynamic marker for region end

(defvar bonk--contexts (make-hash-table :test #'equal)
  "Table of contexts keyed by name → plist (:entries :created :updated).")

(defvar bonk-current-context nil
  "Name of the active Bonk context.")

(defvar bonk--refresh-timer nil)

;; ------------------------------------------------------------------
;; Live view refresh helpers
;; ------------------------------------------------------------------

(defconst bonk--default-context-label "<default>"
  "Display name used for the implicit (nil) Bonk context.")

(defun bonk--display-context-name (name)
  "Return NAME prettified for the UI."
  (if (null name) bonk--default-context-label name))

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
  "Return plist for context NAME; create if absent. Saves state if new."
  (or (gethash name bonk--contexts)
      (let ((new-plist (list :entries nil :created (bonk--ts) :updated (bonk--ts))))
        (puthash name new-plist bonk--contexts)
        (bonk--save-state) ; Save when a new context is implicitly created
        new-plist)))

(defun bonk--update-ts (ctx-name)
  "Update the :updated timestamp for context CTX-NAME in `bonk--contexts`."
  (let* ((plist    (bonk--context-plist ctx-name)) ; Ensures context exists
         (new-ts   (bonk--ts))
         (new-plist (plist-put plist :updated new-ts)))
    (puthash ctx-name new-plist bonk--contexts)))
    ;; Saving is handled by callers like bonk--idle-update-and-refresh or bonk--toggle-entry

(defun bonk--context-names ()
  "Return a list of existing context names as strings, including <default>."
  (let (names)
    (maphash (lambda (k _v)
               (push (bonk--display-context-name k) names))
             bonk--contexts)
    names))

(defun bonk--read-context-name (&optional prompt default)
  "Read a context name, accepting the empty string as the default context."
  (let* ((hist 'bonk--context-history)
         (def-label (bonk--display-context-name default))
         (raw (completing-read (or prompt "Context: ")
                               (bonk--context-names)
                               nil nil nil hist def-label)))
    (cond
     ((string-empty-p raw)            nil)  ; user just hit RET
     ((string= raw bonk--default-context-label) nil)
     (t raw))))


;;; Persistence ---------------------------------------------------------------
(defvar bonk--persistence-version 1
  "Version number for the persistence format.")

(defun bonk--persistence-dir-path ()
  "Return the path to Bonk's persistence directory."
  (expand-file-name "bonk" user-emacs-directory))

(defun bonk--state-file-path ()
  "Return the full path to Bonk's state file."
  (concat (bonk--persistence-dir-path) "/state.el"))

(defun bonk--ensure-persistence-dir ()
  "Ensure Bonk's persistence directory exists."
  (let ((dir (bonk--persistence-dir-path)))
    (unless (file-directory-p dir)
      (make-directory dir t))))


(defun bonk--ensure-entry-markers (entry target-buffer)
  "Ensure live markers for ENTRY in TARGET-BUFFER if a live buffer exists.
  If ENTRY has markers, it validates them. If not, it creates them from
  the entry's start-line and end-line in TARGET-BUFFER.
  This applies only to 'buffer' entries which represent a specific range
  (i.e., `start-line` and `end-line` are non-nil).
  Returns t if markers were created/updated, nil otherwise."
  (let ((markers-updated nil))
    ;; Only attempt to create/validate markers for 'buffer' type entries
    ;; that specify a line range (not whole buffers).
    (when (and (bonk-entry-persisted-start-line entry)  ; Must have a start line
               (bonk-entry-persisted-end-line entry)    ; Must have an end line
               (buffer-live-p target-buffer))
      (with-current-buffer target-buffer
        (let* ((start-m (bonk-entry-start-marker entry))
               (end-m (bonk-entry-end-marker entry))
               (line1 (bonk-entry-persisted-start-line entry))
               (line2 (bonk-entry-persisted-end-line entry))
               (pos1 (save-excursion (goto-char (point-min)) (forward-line (1- line1)) (point)))
               (pos2 (save-excursion (goto-char (point-min)) (forward-line (1- line2)) (line-end-position))))

          ;; If markers are not set or are not pointing to the current buffer, recreate them.
          (unless (and start-m (marker-buffer start-m) (eq (marker-buffer start-m) target-buffer)
                       end-m (marker-buffer end-m) (eq (marker-buffer end-m) target-buffer))
            (setf (bonk-entry-start-marker entry) (copy-marker pos1 t)) ; t: inserts before
            (setf (bonk-entry-end-marker entry)   (copy-marker pos2 nil)) ; nil: inserts after
            ;; Also update the static line numbers to reflect current marker positions
            (setf (bonk-entry-persisted-start-line entry) line1)
            (setf (bonk-entry-persisted-end-line entry)   line2)
            (setq markers-updated t))))
    markers-updated)))

(defun bonk--prepare-entry-for-saving (entry)
  "Return a new `bonk-entry` struct suitable for saving (markers set to nil)."
  (bonk-entry-create
   :type (bonk-entry-type entry)
   :name (bonk-entry-name entry)
   :file-path (bonk-entry-file-path entry)
   :persisted-start-line (bonk-entry-persisted-start-line entry)
   :persisted-end-line (bonk-entry-persisted-end-line entry)
   :start-marker nil  ; Markers are not persisted
   :end-marker nil))

(defun bonk--prepare-contexts-for-saving (contexts-ht)
  "Return a new hash table, deep-copying contexts and preparing entries for saving."
  (let ((save-ht (make-hash-table :test #'equal)))
    (maphash
     (lambda (ctx-name plist)
       (let* ((entries (plist-get plist :entries))
              (saved-entries (mapcar #'bonk--prepare-entry-for-saving entries)))
         (puthash ctx-name
                  (list :entries saved-entries
                        :created (plist-get plist :created)
                        :updated (plist-get plist :updated))
                  save-ht)))
     contexts-ht)
    save-ht))

(defun bonk--save-state ()
  "Save `bonk--contexts` and `bonk-current-context` to the state file."
  (bonk--ensure-persistence-dir)
  (let ((state-file (bonk--state-file-path))
        (data-to-save
         (list :version bonk--persistence-version
               :contexts (bonk--prepare-contexts-for-saving bonk--contexts)
               :current-context bonk-current-context)))
    (condition-case err
        (with-temp-file state-file ; Atomic write
          (let ((print-level nil) (print-length nil)) ; Ensure full printing
            (print data-to-save (current-buffer))))
      (error (message "Bonk: Error saving state to %s: %s" state-file err)))))

(defun bonk--load-state ()
  "Load `bonk--contexts` and `bonk-current-context` from the state file."
  (let ((state-file (bonk--state-file-path)))
    (when (file-readable-p state-file)
      (condition-case err
          (let ((data (with-temp-buffer
                        (insert-file-contents state-file)
                        (read (current-buffer)))))
            (if (and (listp data) (eq (car data) :version)
                     (>= (plist-get data :version) 1)) ; Basic version check
                (progn
                  (setq bonk--contexts (plist-get data :contexts))
                  (setq bonk-current-context (plist-get data :current-context))
                  ;; Post-process loaded contexts
                  (let ((new-contexts (make-hash-table :test #'equal)))
                    (maphash
                     (lambda (ctx-name plist)
                       (let* ((loaded-entries (plist-get plist :entries))
                              (valid-entries '()))
                         (dolist (entry loaded-entries)
                           ;; Ensure markers are nil (should be from read, but belt-and-suspenders)
                           (setf (bonk-entry-start-marker entry) nil)
                           (setf (bonk-entry-end-marker entry) nil)

                           (if (and (eq (bonk-entry-type entry) 'buffer)
                                    (not (bonk-entry-file-path entry))
                                    (not (get-buffer (bonk-entry-name entry))))
                               (message "Bonk: Stale buffer entry '%s' (no file, buffer missing) removed from context '%s'."
                                        (bonk-entry-name entry) (bonk--display-context-name ctx-name))
                             ;; Entry is valid or will be handled by other mechanisms
                             (push entry valid-entries)
                             ;; If buffer is live, ensure hooks are set
                             (let ((buf-to-hook
                                    (cond
                                     ((bonk-entry-file-path entry)
                                      (get-file-buffer (bonk-entry-file-path entry)))
                                     ((eq (bonk-entry-type entry) 'buffer)
                                      (get-buffer (bonk-entry-name entry))))))
                               (when (buffer-live-p buf-to-hook)
                                 (bonk--ensure-entry-markers entry buf-to-hook) ; Re-establish/update markers if buffer is live
                                 (with-current-buffer buf-to-hook
                                   (bonk--maybe-add-buffer-hook (buffer-name buf-to-hook)))))))
                         (puthash ctx-name
                                  (plist-put plist :entries (nreverse valid-entries))
                                  new-contexts)))
                     bonk--contexts)
                    (setq bonk--contexts new-contexts))
                  (message "Bonk: Loaded %d context(s) from %s." (hash-table-count bonk--contexts) state-file))
              (message "Bonk: State file %s has unrecognized format or version." state-file)))
        (error (message "Bonk: Error loading state from %s: %s" state-file err)
               ;; Reset to a clean state on error
               (setq bonk--contexts (make-hash-table :test #'equal))
               (setq bonk-current-context nil)))))
  ;; Ensure default context exists if no contexts were loaded or after reset
  (bonk--context-plist nil))


;;; Switching contexts --------------------------------------------------------

;;;###autoload
(defun bonk-switch-context (name)
  "Switch to (or create) context NAME.
If called interactively, offers `completing-read` completion over existing
contexts, with the current context as the default."
  (interactive
   (list (bonk--read-context-name "Switch to context: " bonk-current-context)))
  (setq bonk-current-context name)
  (bonk--context-plist name) ; Ensures context exists, creates if new (and saves)
  (bonk--refresh-view-buffers)
  (bonk--save-state) ; Save current context change
  (message "Bonk: current context is now '%s'" (bonk--display-context-name name)))


;;; Entry helpers -------------------------------------------------------------

(defun bonk--equal-entry-p (a b)
  "Return non-nil if entries A and B refer to the same target & range.
Handles both marker-based and static line-based entries."
  (and (eq (bonk-entry-type a) (bonk-entry-type b))
       (equal (bonk-entry-name a) (bonk-entry-name b))
       (cond
        ;; both have markers → compare marker positions (only if both markers are live)
        ((and (bonk-entry-start-marker a) (marker-buffer (bonk-entry-start-marker a))
              (bonk-entry-end-marker   a) (marker-buffer (bonk-entry-end-marker   a))
              (bonk-entry-start-marker b) (marker-buffer (bonk-entry-start-marker b))
              (bonk-entry-end-marker   b) (marker-buffer (bonk-entry-end-marker   b)))
         (and (= (marker-position (bonk-entry-start-marker a))
                 (marker-position (bonk-entry-start-marker b)))
              (= (marker-position (bonk-entry-end-marker a))
                 (marker-position (bonk-entry-end-marker b)))))
        ;; fallback: compare static start/end line & file-path
        (t
         (and (equal (bonk-entry-persisted-start-line a) (bonk-entry-persisted-start-line b))
              (equal (bonk-entry-persisted-end-line   a) (bonk-entry-persisted-end-line   b))
              (equal (bonk-entry-file-path  a) (bonk-entry-file-path b)))))))

(defun bonk--toggle-entry (entry &optional context)
  "Toggle ENTRY in CONTEXT (defaults to `bonk-current-context`)."
  (let* ((ctx (or context bonk-current-context))
         (plist (bonk--context-plist ctx))
         (entries (plist-get plist :entries))
         (existing (cl-find-if (lambda (e) (bonk--equal-entry-p e entry)) entries))
         (is-buffer-entry (eq (bonk-entry-type entry) 'buffer))
         (buf-name (and is-buffer-entry (bonk-entry-name entry)))
         new-entries)

    (setq new-entries
          (if existing
              (progn ;
                (message "Bonk: removed %s" (bonk--entry-description entry existing))
                (delq existing entries))
            (progn
              (message "Bonk: added %s" (bonk--entry-description entry))
              (cons entry entries))))

    (puthash ctx (plist-put plist :entries new-entries) bonk--contexts)
    (bonk--update-ts ctx)
    (bonk--refresh-view-buffers)

    ;; Direct hook management:
    (when is-buffer-entry
      (if (and existing (not (cl-find-if (lambda (e) (bonk--equal-entry-p e entry)) new-entries))) ; If it was removed
          (bonk--maybe-remove-buffer-hook buf-name)
        (bonk--maybe-add-buffer-hook buf-name)))

    (bonk--save-state)))

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
  (let ((buf (get-file-buffer file))
        (abs-file (expand-file-name file)))
    (cl-destructuring-bind (start end)
        (or (bonk--range-or-nil arg "Start line: " "End line: ") '(nil nil))
      ;; If the file is already visited, treat it as a buffer entry so that
      ;; unsaved edits are always in scope.
      (bonk--toggle-entry
       (if (buffer-live-p buf)
           (bonk-entry-create :type 'buffer
                              :name (buffer-name buf)
                              :file-path abs-file
                              :persisted-start-line start :persisted-end-line end)
         (bonk-entry-create :type 'file
                            :name abs-file ; store abs path as name for file type
                            :file-path abs-file
                            :persisted-start-line start :persisted-end-line end))))))

;;;###autoload
(defun bonk-add-buffer (&optional buffer arg)
  "Toggle BUFFER (current by default) into context.  With ARG ask for range."
  (interactive "bBuffer: \nP")
  (let ((buf (if (stringp buffer) (get-buffer buffer) (or buffer (current-buffer)))))
    (unless (bufferp buf) (user-error "No valid buffer specified or found."))
    (cl-destructuring-bind (start end)
        (or (bonk--range-or-nil arg "Start line: " "End line: ") '(nil nil))
      (bonk--toggle-entry (bonk-entry-create :type 'buffer
                                             :name (buffer-name buf)
                                             :file-path (buffer-file-name buf) ; Can be nil
                                             :persisted-start-line start :persisted-end-line end)))))

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
         (ln2 (line-number-at-pos (max (point-min) (1- em)) t)))
    (bonk--toggle-entry
     (bonk-entry-create
      :type         'buffer
      :name         (buffer-name buf)
      :file-path    (buffer-file-name buf)
      :persisted-start-line   ln1
      :persisted-end-line     ln2
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
    ('file ; For file type, name is the file path.
     (get-file-buffer (bonk-entry-name entry)))
    (_ nil)))

(defun bonk--entry-content (entry)
  "Return the text content for ENTRY respecting its line range."
  (let ((live-buf (bonk--entry-live-buffer entry))
        (entry-file-path (bonk-entry-file-path entry))) ; Use this for file ops
    (cond
     ;; Error if file-path exists but isn’t readable
     ((and entry-file-path (not (file-readable-p entry-file-path)))
      (error "Bonk: file missing or unreadable: %s" entry-file-path))

     (live-buf
      (with-current-buffer live-buf
        ;; if this entry has dynamic markers (and they are valid in this buffer)
        (if (and (bonk-entry-start-marker entry) (marker-buffer (bonk-entry-start-marker entry))
                 (bonk-entry-end-marker   entry) (marker-buffer (bonk-entry-end-marker entry))
                 (eq (marker-buffer (bonk-entry-start-marker entry)) (current-buffer)))
            (buffer-substring-no-properties
             (marker-position (bonk-entry-start-marker entry))
             (marker-position (bonk-entry-end-marker   entry)))
          ;; else fall back on static line numbers
          (bonk--slice-lines (bonk-entry-persisted-start-line entry)
                             (bonk-entry-persisted-end-line entry)))))
     ;; If it's a file entry or a buffer entry with a file path, and file is readable
     ((and entry-file-path (file-readable-p entry-file-path))
      (bonk--with-file (entry-file-path)
        (bonk--slice-lines (bonk-entry-persisted-start-line entry)
                           (bonk-entry-persisted-end-line entry))))
     ;; Fallback for 'file' type entries where bonk-entry-name is the path
     ((and (eq (bonk-entry-type entry) 'file)
           (file-readable-p (bonk-entry-name entry)))
      (bonk--with-file ((bonk-entry-name entry))
        (bonk--slice-lines (bonk-entry-persisted-start-line entry)
                           (bonk-entry-persisted-end-line entry))))
     (t
      (error "Bonk: Cannot get content for entry %s" (bonk-entry-name entry))))))


(defun bonk--slice-lines (start end)
  "Return region START..END (lines, inclusive) of current buffer, or full."
  (save-excursion
    (let ((beg (if start
                   (progn (goto-char (point-min))
                          (forward-line (1- (max 1 start))) ; Ensure start > 0
                          (line-beginning-position))
                 (point-min)))
          (fin (if end
                   (progn (goto-char (point-min))
                          (forward-line (1- (max 1 end)))   ; Ensure end > 0
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
               (start-pos (and marker-buf (marker-position start-m))) ; Check marker-buf before position
               (end-pos (and marker-buf (marker-position end-m))))
          ;; Check if markers are valid: pointing to the same, live buffer, and positions resolved
          (if (and marker-buf ; Start marker has a buffer
                   (eq marker-buf (marker-buffer end-m)) ; Both markers in the same buffer
                   (buffer-live-p marker-buf) ; That buffer is live
                   start-pos end-pos) ; And their positions are valid (i.e., not nil)
              (with-current-buffer marker-buf
                (cons (line-number-at-pos start-pos t)
                      (line-number-at-pos end-pos t)))
            ;; Fallback to static lines if markers are problematic (e.g., buffer killed, inconsistent)
            (cons (bonk-entry-persisted-start-line entry) (bonk-entry-persisted-end-line entry))))
      ;; Fallback if entry doesn't have markers set up (e.g. 'file' type entry)
      (cons (bonk-entry-persisted-start-line entry) (bonk-entry-persisted-end-line entry)))))

(defun bonk--entry-display-type (entry)
  "Determine the appropriate type symbol for displaying ENTRY.
This can differ from the internal `bonk-entry-type` for a clearer external representation.
Example: a 'buffer' entry backed by `bonk-entry-file-path` is considered 'file'."
  (pcase (bonk-entry-type entry)
    ('buffer
     (if (bonk-entry-file-path entry)
	 'file    ; A buffer backed by a file is presented as a file
       'buffer))  ; A buffer not backed by a file (e.g., *scratch*) is a buffer
    (type type))) ; Default case: internal type is fine for display

(defun bonk-format-entry-xml (entry index &optional base-indent-level)
  "Return XML string for ENTRY as a <document> tag, with INDEX and optional BASE-INDENT-LEVEL.
Marker-based ranges are exported as up-to-date line numbers.
BASE-INDENT-LEVEL specifies the indentation for the 'parent' tags (like <documents>).
The <document> tag itself will be indented by +2, its children (+4), and content (+6)."
  (let* ((indent (or base-indent-level 0))
         (i-document (make-string (+ indent 2) ?\s))
         (rng (bonk--entry-range-lines entry))
         (entry-raw-content (bonk--entry-content entry))
         (display-type-sym (bonk--entry-display-type entry))
         ;; Initialize attributes list with 'index'
         (attrs (list (cons 'index (number-to-string index)))))

    ;; Add 'path' or 'name' attribute based on whether it's a file-backed entry
    (cond
     ((eq display-type-sym 'file)
      ;; This covers both 'file' type entries and 'buffer' entries with a file-path.
      ;; The value is the file-path, or bonk-entry-name if it's a pure file entry.
      (setq attrs (append attrs
                          (list (cons 'path (bonk--escape-xml
                                             (or (bonk-entry-file-path entry)
                                                 (bonk-entry-name entry))))))))
     ((eq display-type-sym 'buffer)
      ;; This covers 'buffer' type entries without a file-path (e.g., *scratch*).
      ;; The value is the buffer name.
      (setq attrs (append attrs
                          (list (cons 'name (bonk--escape-xml (bonk-entry-name entry))))))))

    ;; Add 'startLine' and 'endLine' attributes if a line range is present
    (when (car rng)
      (setq attrs (append attrs
                          (list (cons 'startLine (number-to-string (car rng)))
                                (cons 'endLine   (number-to-string (cdr rng)))))))

    ;; Construct the final XML string for this document entry
    (concat
     i-document "<document" (bonk--format-attrs attrs) ">"
     (if (string-empty-p entry-raw-content)
         ""
       (concat "\n" ; Start content on a new line
               entry-raw-content
               "\n"
               i-document)) ; Indent for closing tag to match opening
     "</document>")))


;;; Markdown helper ----------------------------------------------------------

(defun bonk-format-entry-markdown (entry)
  "Return a Markdown string representing ENTRY."
  (let* ((display-type-sym (bonk--entry-display-type entry))
         (entry-name       (if (eq display-type-sym 'file)
                               (bonk-entry-file-path entry)
                             (bonk-entry-name entry)))
         (header (format "### %s %s%s\n"
                        (capitalize (symbol-name display-type-sym))
                        entry-name
                        (let ((rng (bonk--entry-range-lines entry)))
                          (if (car rng)
                              (format " (%d-%d)" (car rng) (cdr rng))
                            ""))))
         (body (bonk--entry-content entry)))
    (concat header "\n```\n" body "```\n\n")))

(defun bonk-format-entry-plaintext (entry)
  "Return a plaintext string representing ENTRY."
  (let* ((display-type-sym (bonk--entry-display-type entry))
         (entry-name       (if (eq display-type-sym 'file)
                               (bonk-entry-file-path entry)
                             (bonk-entry-name entry)))
         (header (format "--- %s%s\n"
                        entry-name
                        (let ((rng (bonk--entry-range-lines entry)))
                          (if (car rng)
                              (format " (%d-%d)" (car rng) (cdr rng))
                            ""))))
         (body (bonk--entry-content entry)))
    (concat header body "\n")))


;;; Export backends ----------------------------------------------------------

(defun bonk--context-as-xml (ctx-name plist)
  "Return context named CTX-NAME (plist PLIST) as XML string.
Returns an empty string if there are no entries."
  (let* ((entries (plist-get plist :entries))
         (doc-index 0)
         (document-strings (list)))
    (when (seq-empty-p entries) ; Correctly returns empty string if no entries
      (cl-return-from bonk--context-as-xml ""))
    (dolist (entry entries)
      (cl-incf doc-index)
      (push (bonk-format-entry-xml entry doc-index 0) document-strings))
    (concat "<documents>"
            (if document-strings "\n" "")
            (mapconcat 'identity (nreverse document-strings) "\n")
            (if document-strings "\n" "")
            "</documents>")))

(defun bonk--context-as-markdown (ctx-name plist)
  "Return context CTX-NAME (plist PLIST) formatted as Markdown."
  (let* ((entries (plist-get plist :entries)))
    (concat (format "# Context %s\n\n*Created:* %s  \n*Updated:* %s\n\n"
                    (bonk--display-context-name ctx-name)
                    (plist-get plist :created) (plist-get plist :updated))
            (mapconcat #'bonk-format-entry-markdown entries "\n")
            "\n")))

(defun bonk--context-as-plain (ctx-name plist)
  "Return context CTX-NAME (plist PLIST) formatted as plaintext."
  (let* ((entries (plist-get plist :entries)))
    (concat (mapconcat #'bonk-format-entry-plaintext entries "\n")
            "---\n")))


;;; Formatting & saving ------------------------------------------------------

;;;###autoload
(defun bonk-format-context (&optional context-name-arg)
  "Return CONTEXT (string or symbol, default active) formatted per backend.
When called interactively, prompts for the context name using completion, with
current context offered as the default.  The backend is chosen by
`bonk-context-export-backend`."
  (interactive
   (list (bonk--read-context-name "Format context: " bonk-current-context)))
  (let* ((ctx-key (or context-name-arg bonk-current-context)) ; Actual key for hash table
         (plist (bonk--context-plist ctx-key)))
    (let ((txt (pcase bonk-context-export-backend
                 ('xml       (bonk--context-as-xml ctx-key plist))
                 ('markdown  (bonk--context-as-markdown ctx-key plist))
                 ('plain     (bonk--context-as-plain ctx-key plist))
                 (_ (user-error "Unknown backend %s" bonk-context-export-backend)))))
      (if (called-interactively-p 'any)
          (with-current-buffer (get-buffer-create "*Bonk Export*")
            (erase-buffer) (insert txt)
            (when bonk-export-major-mode
              (funcall bonk-export-major-mode))
            (goto-char (point-min))
            (pop-to-buffer (current-buffer)))
        txt))))

;;;###autoload
(defun bonk-save-context (file &optional context-name-arg)
  "Save CONTEXT (default current) as formatted text into FILE.  Overwrites
without asking if FILE exists.  Respects `bonk-context-export-backend`."
  (interactive
   (list (read-file-name "Save context to: " nil nil t)
         (bonk--read-context-name "Context to save: " bonk-current-context)))
  (let ((txt (bonk-format-context context-name-arg)))
    (with-temp-file file (insert txt))
    (message "Bonk: saved context '%s' to %s"
             (bonk--display-context-name (or context-name-arg bonk-current-context))
             file)))


;;; Magit-style viewer --------------------------------------------------------

(when (featurep 'magit-section)
  (defvar bonk-view-mode-map
    (let ((map (make-sparse-keymap)))
      ;; Inherit every binding Magit-section already defines (TAB, gj/gk, …)
      (set-keymap-parent map magit-section-mode-map)

      ;; Our extra commands
      (define-key map (kbd "d")   #'bonk-view-remove-entry)
      (define-key map (kbd "DEL") #'bonk-view-remove-entry)
      (define-key map (kbd "RET") #'bonk-view-goto-source)
      map)
    "Keymap for `bonk-view-mode'.  Inherits `magit-section-mode-map'.")

  (defun bonk-view-goto-source ()
    "From *Bonk View*, jump to the exact point in the source entry.
If point is on the entry header → first char of the region.
If point is inside the body     → same char offset inside the region."
    (interactive)
    (let* ((section (magit-current-section))
           (entry   (and section
                         (eq (oref section type) 'entry)
                         (oref section value))))
      (unless entry
	(user-error "Point is not on a Bonk entry"))

      (let* ((src-buf (or (bonk--entry-live-buffer entry)
                          (and (bonk-entry-file-path entry)
                               (find-file-noselect (bonk-entry-file-path entry) t nil t))))
             (rng (bonk--entry-range-lines entry))) ; Recalculate here
        (unless (buffer-live-p src-buf)
          (user-error "Source buffer for entry '%s' is not available" (bonk-entry-name entry)))

        (let* ((body-beg (or (oref section content)
                             (save-excursion
                               (goto-char (oref section start))
                               (forward-line) (point))))
               (offset (if (< (point) body-beg) 0 (- (point) body-beg)))
               (region-start-in-src
                (with-current-buffer src-buf
                  (if (bonk-entry-start-marker entry)
                      (marker-position (bonk-entry-start-marker entry))
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- (max 1 (or (car rng) 1))))
                      (point)))))
               (region-end-in-src
                (with-current-buffer src-buf
                  (if (bonk-entry-end-marker entry)
                      (marker-position (bonk-entry-end-marker entry))
                    ;; If no end marker, use static line number or end of buffer
                    (let ((end-line (cdr rng)))
                      (if end-line
                          (save-excursion
                            (goto-char (point-min))
                            ;; Ensure end-line doesn't exceed actual buffer lines
                            (when (> end-line (count-lines (point-min) (point-max)))
                              (setq end-line (count-lines (point-min) (point-max))))
                            (forward-line (1- (max 1 end-line)))
                            (line-end-position)) ; Get end of the specified end line
                        (point-max))))))        ; If end-line is nil, encompass till end of buffer
               (target-pos (+ region-start-in-src offset)))

          (with-current-buffer src-buf
            (setq target-pos (min target-pos (point-max))))
          (pop-to-buffer src-buf)
          (goto-char target-pos)
          (when (fboundp 'pulse-momentary-highlight-region) ; Ensure pulse is available
            (pulse-momentary-highlight-region region-start-in-src region-end-in-src))
          (recenter)))))

  (define-derived-mode bonk-view-mode magit-section-mode "Bonk-View"
    "Major mode for visualising a Bonk context.")

  (defun bonk-view-remove-entry (&optional section)
    "Remove the Bonk entry whose Magit SECTION is at point."
    (interactive)
    (let* ((current-section (or section (magit-current-section)))
           (entry (and current-section
                       (eq (oref current-section type) 'entry)
                       (oref current-section value))))
      (unless entry
        (user-error "Point is not on a Bonk entry section"))
      (bonk--toggle-entry entry) ; This already refreshes view and saves state
      (message "Bonk: removed %s" (bonk--entry-description entry))
      (when (magit-section-invisible-p current-section)
        (magit-section-forward))))

  (with-eval-after-load 'evil
    (evil-set-initial-state 'bonk-view-mode 'motion)

    ;; Tell Evil: “when you’re in this buffer, *my* map wins”.
    ;; That keeps TAB for folding, d/DEL for deletion, etc.,
    ;; without having to re-list any keys.
    (evil-make-overriding-map bonk-view-mode-map 'motion))

  (defun bonk--view-refresh ()
    "Populate the Bonk view buffer with collapsible sections."
    (let ((inhibit-read-only t)
	  ;; Rename 'plist' to make it clearer it's for the current context
	  (current-context-plist (bonk--context-plist bonk-current-context)))
      (erase-buffer)
      (let ((entries (plist-get current-context-plist :entries)))
	(magit-insert-section (root)
	  (magit-insert-heading
	    (format "Context %s (items: %d)"
		    ;; Directly use bonk-current-context (dynamic)
		    (bonk--display-context-name bonk-current-context)
		    (length entries)))
	  (dolist (entry entries)
	    (bonk--insert-entry-section entry))))
      (goto-char (point-min))))

  (defun bonk--insert-entry-section (entry)
    "Insert magit section for ENTRY, collapsible, using an indirect buffer."
    (let* ((display-type-sym (bonk--entry-display-type entry))
           (entry-primary-id (if (eq display-type-sym 'file)
                                 (bonk-entry-file-path entry)
                               (bonk-entry-name entry)))
           (rng (bonk--entry-range-lines entry))
           (header (format "%s %s%s"
                           (capitalize (symbol-name display-type-sym))
                           entry-primary-id
                           (if (car rng)
                               (format " (%d–%d)" (car rng) (cdr rng))
                             "")))
           (live-buf (bonk--entry-live-buffer entry))
           (src-buf (or live-buf
                        (and (bonk-entry-file-path entry)
                             (find-file-noselect (bonk-entry-file-path entry) t nil t)))))

      (if (and src-buf (buffer-live-p src-buf))
          (let* ((start-line-for-narrow (or (car rng) 1))
                 (end-line-for-narrow (or (cdr rng)
                                          (with-current-buffer src-buf
                                            (line-number-at-pos (point-max) t))))
                 (ind-buf-name (generate-new-buffer-name
                                (format "*%s<bonk-view>*" (buffer-name src-buf))))
                 (ind-buf (make-indirect-buffer src-buf ind-buf-name t))
                 content-to-insert) ; Variable to hold the extracted content

            (unwind-protect
                (with-current-buffer ind-buf
                  (widen)
                  (goto-char (point-min))
                  (forward-line (1- start-line-for-narrow))
                  (let ((beg (point)))
                    (goto-char (point-min))
                    (forward-line (1- end-line-for-narrow))
                    (let ((end (line-end-position))) ; Use line-end-position for inclusiveness
                      (narrow-to-region beg end)
                      (setq content-to-insert (buffer-string))))) ; Extract content from indirect buffer
              (when (buffer-live-p ind-buf) (kill-buffer ind-buf)))

            ;; Now, insert into the *current* buffer (which is *Bonk View*)
            (magit-insert-section (entry entry) ; Store bonk-entry in section's value
              (magit-insert-heading header)
              (insert content-to-insert)
              (unless (string-suffix-p "\n" content-to-insert) (insert "\n")))) ; Add newline if content doesn't end with one
        ;; Fallback: content not available
        (magit-insert-section (entry entry)
          (magit-insert-heading header)
          (insert (format "  [Content for %s not available. Source may be missing or unreadable.]"
                          entry-primary-id))
          (insert "\n")))))

;;;###autoload
  (defun bonk-view ()
    "Open a buffer displaying the current context in a Magit-like UI."
    (interactive)
    (unless (featurep 'magit-section)
      (user-error "Bonk view requires `magit-section'. Please install Magit."))
    (let ((buf (get-buffer-create "*Bonk View*")))
      (with-current-buffer buf
        (bonk-view-mode)
        (bonk--view-refresh))
      (pop-to-buffer buf)))
  )


(defun bonk--entry-description (entry &optional actual-entry)
  "Return a one-line string describing ENTRY (type, name, lines).
If ACTUAL-ENTRY is provided, use it for description (e.g. an existing entry from list)."
  (let* ((e (or actual-entry entry))
         (display-type-sym (bonk--entry-display-type e))
         (name (if (eq display-type-sym 'file)
                   (bonk-entry-file-path e)
                 (bonk-entry-name e)))
         (rng (bonk--entry-range-lines e))
         (range-str (if (car rng)
                        (format " (%d–%d)" (car rng) (cdr rng))
                      "")))
    (concat (capitalize (symbol-name display-type-sym)) " " name range-str)))

;;;###autoload
(defun bonk-remove-entry ()
  "Prompt to remove a Bonk ENTRY from the current context."
  (interactive)
  (let* ((plist  (bonk--context-plist bonk-current-context))
         (ents   (plist-get plist :entries)))
    (unless ents
      (message "Bonk: Current context '%s' is empty." (bonk--display-context-name bonk-current-context))
      (cl-return-from bonk-remove-entry nil))
    (let* ((alts   (mapcar (lambda (e) (cons (bonk--entry-description e) e)) ents))
           (choice-str (completing-read "Remove entry: " (mapcar #'car alts) nil t))
           (chosen-entry (cdr (assoc choice-str alts))))
      (if chosen-entry
          (progn
            (bonk--toggle-entry chosen-entry) ; This already messages and saves
            (bonk--refresh-view-buffers))
        (user-error "No entry selected or invalid choice")))))

;;;###autoload
(defun bonk-delete-context (name)
  "Delete Bonk context NAME.
Prompts for confirmation, then removes the context and saves state."
  (interactive
   (list (bonk--read-context-name "Delete context: " bonk-current-context)))
  (unless (gethash name bonk--contexts)
    (user-error "No such context to delete: %s" (bonk--display-context-name name)))
  (when (yes-or-no-p (format "Really delete context '%s'? " (bonk--display-context-name name)))
    (let* ((plist   (bonk--context-plist name)) ; ensure it exists
           (entries (plist-get plist :entries)))
      ;; No special hook unwiring needed here beyond what bonk--maybe-remove-buffer-hook handles
      ;; as entries are removed from bonk--contexts.
      )
    (remhash name bonk--contexts)
    (when (equal bonk-current-context name)
      (setq bonk-current-context nil) ; Switch to default if deleted context was current
      (message "Bonk: Current context reset to '%s'." (bonk--display-context-name nil)))
    (bonk--save-state) ; Persist deletion
    (bonk--refresh-view-buffers)
    (message "Bonk: deleted context '%s'" (bonk--display-context-name name))))


;;; Binary detection helper ---------------------------------------------------

(defun bonk--probably-binary-file-p (file)
  "Return non-nil if FILE appears to be binary (contains a NUL byte)."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 1024) ; first KB is enough
    (save-excursion
      (goto-char (point-min))
      (search-forward (char-to-string 0) nil t))))


;;; Auto-refresh and hook management ------------------------------------------

(defun bonk--schedule-refresh (&optional delay)
  "Refresh *Bonk View* after DELAY seconds of idle time."
  (when bonk--refresh-timer
    (cancel-timer bonk--refresh-timer))
  (setq bonk--refresh-timer
        (run-with-idle-timer
         (or delay bonk-auto-refresh-delay) nil
         #'bonk--idle-update-and-refresh)))

(defun bonk--idle-update-and-refresh ()
  "Idle‐time worker: Updates line numbers for marker-based entries, saves state, refreshes views."
  (let ((this-buf (current-buffer)) ; May not be relevant if called from timer
        changed-anything)
    (maphash
     (lambda (ctx-name plist)
       (dolist (entry (plist-get plist :entries))
         (when (and (eq (bonk-entry-type entry) 'buffer)
                    (bonk-entry-start-marker entry) (marker-buffer (bonk-entry-start-marker entry))
                    (bonk-entry-end-marker entry)   (marker-buffer (bonk-entry-end-marker entry))
                    (buffer-live-p (marker-buffer (bonk-entry-start-marker entry))) ; Marker's buffer must be live
                    (string= (bonk-entry-name entry) (buffer-name (marker-buffer (bonk-entry-start-marker entry))))) ; And name matches
           (with-current-buffer (marker-buffer (bonk-entry-start-marker entry)) ; Operate in marker's buffer
             (let* ((sm-pos (marker-position (bonk-entry-start-marker entry)))
                    (em-pos (marker-position (bonk-entry-end-marker entry)))
                    (ln1 (when sm-pos (line-number-at-pos sm-pos t)))
                    (ln2 (when em-pos (line-number-at-pos em-pos t))))
               (when (and ln1 ln2
                          (or (not (equal (bonk-entry-persisted-start-line entry) ln1))
                              (not (equal (bonk-entry-persisted-end-line entry) ln2))))
                 (setf (bonk-entry-persisted-start-line entry) ln1
                       (bonk-entry-persisted-end-line   entry) ln2)
                 (bonk--update-ts ctx-name) ; Update timestamp
                 (setq changed-anything t)))))))
     bonk--contexts)
    (when changed-anything (bonk--save-state)) ; Save if any line numbers changed
    (bonk--refresh-view-buffers)))

(defun bonk--on-buffer-kill ()
  "On buffer kill, update static ranges and clear markers for entries in this buffer."
  (let ((buf-name (buffer-name))
        changed-anything)
    (maphash
     (lambda (ctx-name plist)
       (dolist (entry (plist-get plist :entries))
         (when (and (eq (bonk-entry-type entry) 'buffer)
                    (equal (bonk-entry-name entry) buf-name) ; Matches the killed buffer
                    (bonk-entry-start-marker entry) ; Has markers
                    (bonk-entry-end-marker entry))
           ;; At this point, markers are still valid for position lookup *before* buffer is fully dead
           (let* ((sm (marker-position (bonk-entry-start-marker entry)))
                  (em (marker-position (bonk-entry-end-marker entry)))
                  (ln1 (when sm (with-current-buffer (marker-buffer (bonk-entry-start-marker entry)) (line-number-at-pos sm t))))
                  (ln2 (when em (with-current-buffer (marker-buffer (bonk-entry-end-marker entry)) (line-number-at-pos em t)))))
             (when (and ln1 ln2) ; If lines could be determined
               (setf (bonk-entry-persisted-start-line entry) ln1
                     (bonk-entry-persisted-end-line   entry) ln2))
             ;; Regardless, clear markers as buffer is being killed
             (setf (bonk-entry-start-marker entry) nil
                   (bonk-entry-end-marker   entry) nil)
             (bonk--update-ts ctx-name) ; Update timestamp
             (setq changed-anything t)))))
     bonk--contexts)
    (when changed-anything (bonk--save-state)))) ; Save if markers were cleared/lines updated

(defvar-local bonk--buffer-hook-installed nil
  "Buffer-local flag: t if Bonk's after-change/kill hooks are installed.")

(defun bonk--maybe-add-buffer-hook (buf-name-or-buffer)
  "Ensure buffer BUF-NAME-OR-BUFFER has after-change/kill hooks installed."
  (let ((buf (if (bufferp buf-name-or-buffer)
                 buf-name-or-buffer
               (get-buffer buf-name-or-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (unless bonk--buffer-hook-installed ; Use the -local defvar
          (add-hook 'after-change-functions #'bonk--buffer-change-refresh nil t) ; t for local
          (add-hook 'kill-buffer-hook #'bonk--on-buffer-kill nil t) ; t for local
          (setq bonk--buffer-hook-installed t))))))

(defun bonk--maybe-remove-buffer-hook (buf-name-or-buffer)
  "Remove refresh/kill hooks from BUF-NAME-OR-BUFFER if no longer in *any* context."
  (let ((buf (if (bufferp buf-name-or-buffer)
                 buf-name-or-buffer
               (get-buffer buf-name-or-buffer))))
    (when (and (buffer-live-p buf)
               (not (bonk--buffer-in-any-context-p (buffer-name buf)))) ; Check by name
      (with-current-buffer buf
        (when bonk--buffer-hook-installed ; Use a -local defvar
          (remove-hook 'after-change-functions #'bonk--buffer-change-refresh t) ; t for local
          (remove-hook 'kill-buffer-hook #'bonk--on-buffer-kill t) ; t for local
          (kill-local-variable 'bonk--buffer-hook-installed))))))


;;; Helpers to see whether a file / buffer lives in ANY context --------------

(defun bonk--file-in-any-context-p (file)
  "Check if FILE (absolute path) is registered as a 'file' type or
as a 'buffer' type with a matching file-path in any context."
  (let ((abs-file (expand-file-name file))
        found)
    (maphash
     (lambda (_ctx plist)
       (unless found
         (setq found
               (seq-find (lambda (e)
                           (or (and (eq (bonk-entry-type e) 'file)
                                    (equal (bonk-entry-name e) abs-file))
                               (and (eq (bonk-entry-type e) 'buffer)
                                    (bonk-entry-file-path e)
                                    (equal (bonk-entry-file-path e) abs-file))))
                         (plist-get plist :entries)))))
     bonk--contexts)
    found))

(defun bonk--buffer-in-any-context-p (buf-name)
  "Check if a buffer named BUF-NAME is registered as a 'buffer' type entry
in any context."
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

(defun bonk--after-save-refresh ()
  "After-save-hook: if saved file is in a context, schedule refresh and ensure hooks."
  (when (and buffer-file-name
             (bonk--file-in-any-context-p buffer-file-name))
    (bonk--maybe-add-buffer-hook (current-buffer)) ; Ensure hooks active on this buffer
    (bonk--schedule-refresh)))

(add-hook 'after-save-hook #'bonk--after-save-refresh)

(defun bonk--buffer-change-refresh (_beg _end _len)
  "Buffer-local `after-change-functions' handler. Schedules a view refresh."
  (bonk--schedule-refresh))

(defun bonk--after-find-refresh ()
  "Find-file-hook: if visited file is in a context, ensure hooks and schedule refresh."
  (when buffer-file-name
    (let ((file-path (expand-file-name buffer-file-name))
          (current-buf (current-buffer)))
      (maphash
       (lambda (_ctx plist)
         (dolist (entry (plist-get plist :entries))
           (when (or (and (eq (bonk-entry-type entry) 'file)
                           (equal (bonk-entry-name entry) file-path)) ; 'file' entries store abs path as name
                     (and (eq (bonk-entry-type entry) 'buffer)
                          (equal (bonk-entry-file-path entry) file-path)
                          (equal (bonk-entry-name entry) (buffer-name current-buf))))
             ;; Found a matching entry that corresponds to the newly visited file
             (bonk--ensure-entry-markers entry current-buf) ; Re-create/validate markers in *this* buffer
             (bonk--maybe-add-buffer-hook (buffer-name current-buf))))) ; Ensure buffer hooks are active
       bonk--contexts))
    (bonk--schedule-refresh)))

(add-hook 'find-file-hook #'bonk--after-find-refresh)


;;; GPTEL Integration & Keybindings ---------------------------------------------
(defun bonk-context-inserter ()
  "Inserts a predefined custom context at the beginning of the prompt.
This function is intended for `gptel-prompt-filter-hook'."
  (save-excursion
    ;; Go to the beginning of the temporary buffer containing the prompt.
    (goto-char (point-min))

    ;; Insert your custom context.
    ;; You can make this dynamic, read from a file, etc.
    (insert (bonk-format-context))

    ;; Return point to the end of the original prompt (or wherever it was)
    ;; This step is often implied when using `save-excursion`, but good to note.
    (goto-char (point-max))))


;; Define a keymap for bonk commands
(defvar bonk-map (make-sparse-keymap)
  "Keymap for bonk commands.")

(define-prefix-command 'bonk-map nil "Bonk")

(global-set-key (kbd bonk-prefix-key) bonk-map)

(define-key bonk-map (kbd "s") #'bonk-switch-context)
(define-key bonk-map (kbd "f") #'bonk-add-file)
(define-key bonk-map (kbd "b") #'bonk-add-buffer)
(define-key bonk-map (kbd "r") #'bonk-add-region)
(define-key bonk-map (kbd "v") #'bonk-view)
(define-key bonk-map (kbd "e") #'bonk-format-context) ; Export to buffer
(define-key bonk-map (kbd "w") #'bonk-save-context)   ; Write to file
(define-key bonk-map (kbd "k") #'bonk-remove-entry)   ; "Kill" entry
(define-key bonk-map (kbd "D") #'bonk-delete-context) ; Delete context


(provide 'bonk)

;; Load persisted state when bonk.el is loaded.
;; This should be one of the last things.
(bonk--load-state)

;;; bonk.el ends here
