;;; -*- lexical-binding: t; -*-

;;; Entry Data Structure and Accessors
(defun tws-make-entry (goal category &optional estimate)
  (list 'goal goal
        'category category
        'time 0                   ; in seconds
        'estimate (or estimate 0) ; in hours
        'completed nil            ; t or nil
        'created nil              ; a time like (curren-time)
        'last-touched nil         ; nil or a time like (current-time)
        'status 'in-the-future    ; 'in-the-future, 'on-the-move or 'waiting
        'id nil))

(defun tws-accessor-forms (prop)
  (let ((fname (intern (concat "tws-" (symbol-name prop)))))
    (list 'progn
          `(defun ,fname (entry) (getf entry ',prop))
          `(gv-define-setter ,fname (val entry)
             (list 'setf (list 'getf entry '',prop) val)))))

(defmacro def-tws-accessors (&rest props)
  (cons 'progn (mapcar #'tws-accessor-forms props)))

(def-tws-accessors goal category time estimate completed created last-touched status id)

(defun tws-waiting-on (entry reason)
  (setf (tws-status entry) 'waiting))

(defun tws-make-on-the-move (entry)
  (setf (tws-status entry) 'on-the-move))

(defun tws-make-in-the-future (entry)
  (setf (tws-status entry) 'in-the-future))

(defun tws-touch-entry (entry)
  (setf (tws-last-touched entry) (current-time)))

(defun tws-add-time (entry secs)
  (setf (tws-time entry)
        (+ (tws-time entry)
           secs)))

(defvar *tws-on-the-move-str* "DO")
(defvar *tws-in-the-future-str* "FU")
(defvar *tws-waiting-str* "WA")


(defvar *tws-entry-format-string* "[%s] %35s -- %-25s (≅ %-4s Hrs) %s %s %s" 
  "Shoots for 90 Charcters wide. Its format is
   GOAL CATEGORY  ESTIMATE STATUS FRESHNESS SPENT COMPLETE

  COMPLETE is either ✓ or is blank.
  STATUS is one of 🚀, 🤔, ⏳
  FRESHNESS is one of 👶, 👨, 👴
  SPENT is HH:MM
")



(defun tws-status-icon (entry)
  (case (tws-status entry)
    ('on-the-move *tws-on-the-move-str*)
    ('in-the-future *tws-in-the-future-str*)
    ('waiting *tws-waiting-str*)))

(defun tws-freshness-icon (entry)
  ;; TODO
  "FR")

(defun tws-time-to-hh-mm (secs)
  (format "%02d:%02d"
          (/ secs 3600)
          (/ (mod secs 3600) 60)))

(defun tws-entry-to-string (entry)
  (format *tws-entry-format-string*
          (if (tws-completed entry)  "✓" " ")
          (truncate-string-to-width (tws-goal entry) 35)
          (truncate-string-to-width (tws-category entry) 25)
          (truncate  (tws-estimate entry))
          (tws-status-icon entry)
          (tws-freshness-icon entry)
          (tws-time-to-hh-mm (tws-time entry))
          ))

;;; Utilities
(defun tws-print-to-file (file form)
  (with-temp-buffer
    (prin1 forms (current-buffer))
    (write-file file)))

(defun tws-read-from-file (file)
  (read-from-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

;;; Database Abstraction
(defun tws-make-fresh-db ()
  "Creates a fresh database instance."
  (list
   0       ; the next entry id
   ()      ; the entries
   nil     ; the working entry, nil or (start-time . entry-id) 
   ))

(defun tws-new-entry-id (db)
  "Returns a new entry ID, updates the internal database state."
  (incf (first db)))

(defun tws-db-entries (db)
  "Returns the database entries as a list."
  (second db))

(defun tws-db-working-entry (db)
  "Returns the database's working entry form"
  (third db))

(defun tws-lookup-entry (db entry-id)
  (find-if (tws-query 'eql 'tws-id entry-id)
           (tws-db-entries db)))

(defun tws-stop-working (db)
  "Clears the working entry and updates the database with the
corresponding entry's new time."
  (when (tws-db-working-entry db)
    (let* ((entry-id (cdr (tws-db-working-entry db)))
           (start-time (car (tws-db-working-entry db)))
           (entry (tws-lookup-entry db entry-id))
           (now (float-time (current-time))))
      ;; clear the working-entry
      (setf (third db) nil)
      ;; update the entry with new times
      (tws-add-time entry (- now start-time))
      (tws-touch-entry entry))))

(defun tws-work-on (db entry-id)
  "Start working on entry with id ENTRY-ID."
  ;; stop any active work
  (tws-stop-working db)
  ;; add the new working entry and record its time
  (setf (third db) (cons (current-time) entry-id)))

(defun tws-raw-insert-entry-into-db (entry db)
  "Performs no checks. Includes a entry into the DB. Meant to be
used by other functions that don't know about the internal
structure of the DB."
  (push entry (second db)))

(defun tws-delete-entry (id db)
  "Removes the entry with the provided Id from the database."
  (setf (second db)
        (delete-if (lambda (entry) (equal id (tws-id entry)))
                   (second db))))

(defun tws-run-query (pred db)
  (remove-if-not pred (tws-db-entries db)))

(defun tws-categories (db)
  (let ((result nil))
    (dolist (entry (tws-db-entries db) result)
      (pushnew (tws-category entry) result :test #'equal))))

;;; Utilities for building queries

(defun tws-query (compare accessor value)
  (lambda (entry) (funcall compare (funcall accessor entry) value)))

(defun tws-and (&rest preds)
  (lambda (entry)
    (dolist (pred preds t)
      (unless (funcall pred entry)
        (return nil)))))

(defun tws-or (&rest preds)
  (lambda (entry)
    (dolist (pred preds nil)
      (when (funcall pred entry)
        (return t)))))

(defun tws-days-ago (days)
  (- (float-time (curren-time))
     (* days 24 60 60)))

(defun tws-not (pred)
  (lambda (entry) (not (funcall pred entry))))

;;; Functions for Interacting with The Global Database 
(defvar *tws-db-file* "~/.time-well-spent"
  "The file where the database will persist between emacs
  sessions.")

(defvar *tws-db* nil
  "Holds a entry database instance.")

(defun tws-init-fresh-db ()
  (setq *tws-db* (tws-make-fresh-db)))

(defun tws-save-db ()
  (tws-print-to-file *tws-db-file* *tws-db*))

(defun tws-load-db ()
  (setq *tws-db*
        (tws-read-from-file *tws-db-file*)))

(defun tws-add-entry (entry)
  ;; if the entry has an id, remove any entrys in the db with the same
  ;; id, otherwise give the entry an id.
  (if (tws-id entry)
      (tws-delete-entry (tws-id entry) *tws-db*)
      (setf (tws-id entry) (tws-new-entry-id *tws-db*)))
  ;; then push it to into the db
  (tws-raw-insert-entry-into-db entry *tws-db*))


;;; Timers 

(defvar *tws-idle-timeout* (* 7 60))

(defun tws-stop-idle-timer ()
  ;; TODO
  )

(defun tws-start-idle-timer ()
  ;; TODO
  )

;;; TWS Buffer & Commands

(defvar *tws-buffer-name* "*Time Well Spent*")

(defvar *tws-displayed-entries* nil
  "List of entries currently being displayed.")

(defvar *tws-show-complete-also* nil)
(defvar *tws-show-future* t)
(defvar *tws-show-category* nil)

(defun tws-true (entry) t)

(defun tws-build-filter ()
  (let ((filter #'tws-true))
    (when *tws-show-category*
      (setq filter (tws-and filter (tws-query 'equal 'tws-category *tws-show-category*))))
    (unless *tws-show-future*
      (setq filter (tws-and filter (tws-not (tws-query 'equal 'tws-status 'in-the-future)))))
    (unless *tws-show-complete-also*
      (setq filter (tws-and filter (tws-query 'eql 'tws-completed nil))))
    filter))

(defun tws-refresh-buffer ()
  (interactive)
  (setq *tws-displayed-entries*
        (tws-run-query (tws-build-filter) *tws-db*))
  (with-output-to-temp-buffer *tws-buffer-name*
    (with-current-buffer *tws-buffer-name*

      (dolist (entry *tws-displayed-entries*)
        (princ (tws-entry-to-string entry))
        (terpri))

      (read-only-mode)
      
      )))

(defun entry-on-line ())



(defun tws-filter-by-category (category)
  (interactive
   (list (completing-read "Category: " (tws-categories *tws-db*))))
  (setq *tws-category-filter* category)
  (tws-refresh-buffer))

(defun tws-create-entry (category goal estimated)
  (interactive
   (list (completing-read "Category: " (cons "No Category" (tws-categories *tws-db*)))
         (read-string "Goal: ")
         (read-number "Estimated Hours: ")))
  (let ((entry (tws-make-entry goal category estimated)))
    (setf (tws-created entry) (current-time))
    (tws-add-entry entry)))

(defun tws-stop ()
  (interactive)
  (tws-stop-idle-timer)
  (tws-stop-working *tws-db*))

(defun tws-start-on-task-at-line ()
  (interactive)
  (let ((entry-on-line (tws-entry-on-line (line-number-at-pos (point)))))
    (when entry-on-line
      (tws-work-on *tws-db* entry-on-line)
      (tws-start-idle-timer))))








