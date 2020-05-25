;;; -*- lexical-binding: t; -*-

;;; Entry Data Structure and Accessors
(defun tws-make-entry (goal category &optional estimate)
  (list 'goal goal
        'category category
        'time 0 ;; in seconds
        'estimate (or estimate 0) ;; in hours
        'completed nil    ;; t or nil
        'created nil      ;; a time like (curren-time)
        'last-touched nil ;; nil or a time like (current-time)
        'status :future ;; 'future, 'active or '(waiting "description")
        'id nil))

(defun tws-accessor-forms (prop)
  (let ((fname (intern (concat "tws-" (symbol-name prop)))))
    (list 'progn
          `(defun ,fname (entry) (getf entry ',prop))
          `(gv-define-setter ,fname (val entry)
             (list 'setf (list 'getf entry '',prop) val)))))

(defmacro def-tws-accessors (&rest props)
  (cons 'progn (mapcar #'tws-accessor-forms props)))

(def-tws-accessors goal category time estimate completed created last-touched id)

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

(defun tws-stop-working (db)
  "Clears the working entry and updates the database with the
corresponding entry's new time."
  (when (tws-db-working-entry db)
    (let* ((entry-id (cdr (tws-db-working-entry db)))
           (start-time (car (tws-db-working-entry db)))
           (entry (tws-lookup-entry db entry-id))
           (now (current-time)))
      ;; clear the working-entry
      (setf (third db) nil)
      ;; update the entry with new times
      (setf (tws-time entry)
            (+ (tws-time entry)
               (- now start-time)))
      (setf (tws-last-touched entry)
            now))))

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

;;; Commands

(defun tws-create-entry (category goal estimated)
  (interactive
   (list (completing-read "Category: " (cons "No Category" (tws-categories *tws-db*)))
         (read-string "Goal: ")
         (read-number "Estimated Hours: ")))
  (let ((entry (tws-make-entry goal category estimated)))
    (setf (tws-created entry) (current-time))
    (tws-add-entry entry)))







