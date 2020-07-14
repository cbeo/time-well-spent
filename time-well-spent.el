;;; -*- lexical-binding: t; -*-

(require 'notifications)

(cl-defstruct tws-entry
  goal
  category
  (time nil)        ;; a list whose elements are numbers or pairs of numbers
  (estimate 0)
  (completed nil)
  (created nil)
  (last-touched nil)
  (status 'on-the-move)
  (id nil)
  (log nil))

(defun tws-correct-time (entry)
  "returns the time, possibly transforming into the the correct format first"
  (when (numberp (tws-entry-time entry))
    (setf (tws-entry-time entry)
          (list (tws-entry-time entry))))
  (tws-entry-time entry))

(defvar *tws-time-total-cache* (make-hash-table)
  "In-memory cache of entry time totals.")

(defun tws-invalidate-total-cache (entry)
  (remhash (tws-entry-id entry) *tws-time-total-cache*))

(defun tws-total-time (entry &optional start-time)
  "gets the total time worked on entry"
  (if start-time
      (let (switch)
        (cl-reduce (lambda (acc time)
                  (+ acc (cond (switch 0)             ; ignore everything before the star-time
                               ((numberp time) time)
                               ((time-less-p start-time (cdr time))
                                (float-time (subtract-time (car time) (cdr time))))
                               (t (setf switch t)
                                  0))))
                (tws-correct-time entry)
                :initial-value 0))
    (if (gethash (tws-entry-id entry) *tws-time-total-cache*)
        (gethash (tws-entry-id entry) *tws-time-total-cache*)
      (setf (gethash (tws-entry-id entry) *tws-time-total-cache*)
            (cl-reduce (lambda (acc time)
                      (+ acc
                         (if (numberp time) time
                           (float-time
                            (subtract-time (car time) (cdr time))))))
                    (tws-correct-time entry)
                    :initial-value 0)))))

(defun tws-waiting-on (entry)
  (setf (tws-entry-status entry) 'waiting))

(defun tws-mark-incomplete (entry)
  (setf (tws-entry-completed entry) nil))

(defun tws-mark-complete (entry)
  (setf (tws-entry-completed entry) t))

(defun tws-make-on-the-move (entry)
  (setf (tws-entry-status entry) 'on-the-move))

(defun tws-make-in-the-future (entry)
  (setf (tws-entry-status entry) 'in-the-future))

(defun tws-touch-entry (entry)
  (tws-invalidate-total-cache entry)
  (setf (tws-entry-last-touched entry) (current-time)))


(defun tws-long-enough-p (period)
  (let ((secs (if (numberp period) period
                (float-time (subtract-time (car period) (cdr period))))))
    (< (+ 10 *tws-idle-timeout*) secs)))


(defun tws-add-time (entry period)
  (when (tws-long-enough-p period)
    (tws-correct-time entry)
    (tws-touch-entry entry)
    (push period (tws-entry-time entry))))

(defvar *tws-on-the-move-str* "🚀")
(defvar *tws-in-the-future-str* "🤔")
(defvar *tws-waiting-str* "⏳")
(defvar *tws-in-progress-icon* "🚧")
(defvar *tws-goal-reached-icon* "☑")

(defvar *tws-entry-format-string* "%35s -- %-25s (≅ %-4s Hrs) %s %s %s"
  "Shoots for 90 Charcters wide. Its format is
   GOAL CATEGORY ESTIMATE STATUS SPENT COMPLETE

  COMPLETE is either ✓ or 🚧 or is blank.
  STATUS is one of 🚀, 🤔, ⏳
  SPENT is HH:MM
")



(defun tws-status-icon (entry)
  (cl-case (tws-entry-status entry)
    ('on-the-move *tws-on-the-move-str*)
    ('in-the-future *tws-in-the-future-str*)
    ('waiting *tws-waiting-str*)))


(defun tws-time-to-hh-mm (secs)
  (format "%02d:%02d"
          (/ secs 3600)
          (/ (mod secs 3600) 60)))

(defun tws-entry-to-string (entry &optional working-p)
  (format *tws-entry-format-string*
          (truncate-string-to-width (tws-entry-goal entry) 35)
          (truncate-string-to-width (tws-entry-category entry) 25)
          (truncate  (tws-entry-estimate entry))
          (tws-status-icon entry)
          (tws-time-to-hh-mm (tws-total-time entry))
          (if working-p *tws-in-progress-icon*
            (if (tws-entry-completed entry)  *tws-goal-reached-icon* " "))))

;;; Utilities
(defun tws-print-to-file (file form)
  (with-temp-buffer
    (prin1 form (current-buffer))
    (write-file file)))

(defun tws-read-from-file (file)
  (car (read-from-string
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string)))))

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
  (cl-incf (cl-first db)))

(defun tws-db-entries (db)
  "Returns the database entries as a list."
  (cl-second db))

(defun tws-db-working-entry (db)
  "Returns the database's working entry form"
  (cl-third db))

(defun tws-currently-working-on-p (db entry-id)
  (and entry-id
       (eql entry-id
            (cdr (tws-db-working-entry db)))))

(defun tws-lookup-entry (db entry-id)
  (cl-find-if (tws-query 'eql 'tws-entry-id entry-id)
           (tws-db-entries db)))

(defun tws-stop-working (db)
  "Clears the working entry and updates the database with the
corresponding entry's new time."
  (when (tws-db-working-entry db)
    (let* ((entry-id (cdr (tws-db-working-entry db)))
           (start-time  (car (tws-db-working-entry db)))
           (entry (tws-lookup-entry db entry-id))
           (now  (current-time)))
      ;; clear the working-entry
      (setf (cl-third db) nil)
      ;; update the entry with new times
      (tws-add-time entry (cons now start-time)))))

(defun tws-work-on (db entry)
  "Start working on entry with id ENTRY-ID."
  ;; stop any active work
  (tws-stop-working db)
  ;; add the new working entry and record its time
  (tws-make-on-the-move entry)
  (tws-mark-incomplete entry)
  (tws-touch-entry entry)
  (setf (cl-third db) (cons (current-time) (tws-entry-id entry))))

(defun tws-raw-insert-entry-into-db (entry db)
  "Performs no checks. Includes a entry into the DB. Meant to be
used by other functions that don't know about the internal
structure of the DB."
  (push entry (cl-second db)))

(defun tws-delete-entry (id db)
  "Removes the entry with the provided Id from the database."
  (setf (cl-second db)
        (delete-if (lambda (entry) (equal id (tws-entry-id entry)))
                   (cl-second db))))

(defun tws-run-query (pred db)
  (cl-remove-if-not pred (tws-db-entries db)))

(defun tws-categories-and-times (db &optional entries-p after-time)
  "Supply DB is either a db instance or is a list of entires. If
it is a list of entries, you must supply non-nill for ENTRIES-P."
  (let ((tab (make-hash-table :test 'equal)))
    (dolist (entry (if entries-p db (tws-db-entries db)) tab)
      (cl-incf (gethash (tws-entry-category entry) tab 0)
            (or  (tws-total-time entry after-time) 0)))))

(defun tws-categories (db)
  (hash-table-keys (tws-categories-and-times db)))


;;; Utilities for building queries

(defun tws-query (compare accessor value)
  (lambda (entry) (funcall compare (funcall accessor entry) value)))

(defun tws-and (&rest preds)
  (lambda (entry)
    (cl-labels ((rec (preds)
                  (if (not preds) t
                    (and (funcall (car preds) entry)
                         (rec (cdr preds))))))
      (rec preds))))

(defun tws-midnight-today ()
  (let ((parts (split-string (current-time-string))))
    (setf (fourth parts) "00:00:00")
    (date-to-time (string-join parts " "))))

(defun tws-hours-since-midnight ()
  (/ (float-time (subtract-time (current-time) (tws-midnight-today)))
     (* 60 60)))

(defun tws-days-ago (days)
  (- (float-time (current-time))
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
        (if (file-exists-p *tws-db-file*) (tws-read-from-file *tws-db-file*)
          (tws-make-fresh-db))))

(defun tws-add-entry (entry)
  ;; if the entry has an id, remove any entrys in the db with the same
  ;; id, otherwise give the entry an id.
  (if (tws-entry-id entry)
      (tws-delete-entry (tws-entry-id entry) *tws-db*)
      (setf (tws-entry-id entry) (tws-new-entry-id *tws-db*)))
  ;; then push it to into the db
  (tws-raw-insert-entry-into-db entry *tws-db*))


;;; Timers

(defvar *tws-idle-timeout* (* 10 60))
(defvar *tws-idle-timer-handle* nil)

(defun tws-stop-idle-timer ()
  (when *tws-idle-timer-handle*
    (cancel-timer *tws-idle-timer-handle*))
  (setq *tws-idle-timer-handle* nil))

(defun tws-start-idle-timer ()
  (setq *tws-idle-timer-handle*
        (run-with-idle-timer *tws-idle-timeout*
                             nil
                             'tws-notify-and-stop)))

(defun tws-notify-and-stop ()
  (tws-stop-working *tws-db*)
  (tws-save-db)
  (notifications-notify :title "Time Well Spent"
                        :body "Tracking Has Stopped Due to Idleness"
                        :timeout 0)
  (setq *tws-idle-timer-handle* nil)
  (tws-refresh-buffer))

;;; TWS Buffer & Commands

(defvar *tws-buffer-name* "*Time Well Spent*")

(defvar *tws-displayed-entries* nil
  "List of entries currently being displayed.")

(defvar *tws-show-complete-also* nil)
(defvar *tws-show-future* t)
(defvar *tws-show-category* nil)
(defvar *tws-show-goal-with-substring* nil)

(defun flipped-search (s1 s2) (search (downcase s2) (downcase s1)))

(defun tws-build-filter ()
  (let ((filter (lambda (entry) t)))
    (when *tws-show-category*
      (setq filter (tws-and filter (tws-query 'equal 'tws-entry-category *tws-show-category*))))
    (unless *tws-show-future*
      (setq filter (tws-and filter (tws-not (tws-query 'equal 'tws-entry-status 'in-the-future)))))
    (unless *tws-show-complete-also*
      (setq filter (tws-and filter (tws-query 'equal 'tws-entry-completed nil))))
    (when *tws-show-goal-with-substring*
      (setq filter (tws-and filter (tws-query 'flipped-search
                                              'tws-entry-goal
                                              *tws-show-goal-with-substring*))))
    filter))


(defun tws-sort (entries)
  "Sorts entries in 'barski' order"
  (let ((cats (tws-categories-and-times (tws-db-entries *tws-db*) t)))
    (cl-labels ((same-cat-p (a b)
                         (equal (tws-entry-category a) (tws-entry-category b)))

             (active-p (a)
                       (and (not (tws-entry-completed a))
                            (equal 'on-the-move (tws-entry-status a))))

             (both-active (a b) (and (active-p a) (active-p b)))

             (cat-lt-p (a b)
                       (< (gethash (tws-entry-category a) cats 0)
                          (gethash (tws-entry-category b) cats 0)))

             (more-worked-p (a b)
                            (> (tws-total-time a) (tws-total-time b)))

             (waiting-before-future-p (a b)
                                      (equal 'waiting (tws-entry-status a)))

             (most-in-least (a b)
                            (cond ((both-active a b)
                                   (if (same-cat-p a b) (more-worked-p a b)
                                     (cat-lt-p a b)))
                                  ((active-p a) t)
                                  ((active-p b) nil)
                                  (t (waiting-before-future-p a b)))))
      (setq entries
            (sort entries (lambda (a b) (most-in-least a b))))
      entries)))

(defun tws-toggle-future-goals ()
  (interactive)
  (setq *tws-show-future* (not *tws-show-future*))
  (tws-refresh-buffer))

(defun tws-toggle-completed-goals ()
  (interactive)
  (setq *tws-show-complete-also* (not *tws-show-complete-also*))
  (tws-refresh-buffer))

(defun tws-show-all-goals ()
  (interactive)
  (setq *tws-show-complete-also* t)
  (setq *tws-show-future* t)
  (setq *tws-show-category* nil)
  (tws-refresh-buffer))

(defun tws-show-default ()
  (interactive)
  (setq *tws-show-category* nil)
  (setq *tws-show-future* t)
  (setq *tws-show-complete-also* nil)
  (setq *tws-show-goal-with-substring* nil)
  (tws-refresh-buffer))

(define-derived-mode time-well-spent-mode fundamental-mode "tws")

(defvar *tws-last-known-point* 0)

(defun tws-refresh-buffer ()
  (interactive)
  (setq *tws-last-known-point* (point))
  (setq *tws-displayed-entries*
        (tws-sort (copy-sequence (tws-run-query (tws-build-filter) *tws-db*))))
  (with-output-to-temp-buffer *tws-buffer-name*
    (with-current-buffer *tws-buffer-name*
      (time-well-spent-mode)
      (hl-line-mode)

      (dolist (entry *tws-displayed-entries*)
        (princ (tws-entry-to-string entry
                                    (tws-currently-working-on-p
                                     *tws-db*
                                     (tws-entry-id entry))))
        (terpri))

      (read-only-mode)
      (use-local-map (copy-keymap (make-sparse-keymap)))
      (local-set-key (kbd "+") 'tws-create-entry)
      (local-set-key (kbd "<return>") 'tws-toggle-on-goal-at-line)
      (local-set-key (kbd "S") 'tws-stop)
      (local-set-key (kbd "A") 'tws-show-all-goals)
      (local-set-key (kbd "SPC") 'tws-show-goals-with-category-on-line)
      (local-set-key (kbd "C") 'tws-toggle-completed-goals)
      (local-set-key (kbd "F") 'tws-toggle-future-goals)
      (local-set-key (kbd "D") 'tws-show-default)
      (local-set-key (kbd "m") 'tws-toggle-mark-complete-on-line)
      (local-set-key (kbd "f") 'tws-put-into-future-on-line)
      (local-set-key (kbd "w") 'tws-mark-waiting-on-line)
      (local-set-key (kbd "d") 'tws-mark-on-the-move-on-line)
      (local-set-key (kbd "l") 'tws-log-to-entry-on-line)
      (local-set-key (kbd "R") 'tws-category-report)
      (local-set-key (kbd "r") 'tws-rename-entry)
      (local-set-key (kbd "M") 'tws-recategorize)
      (local-set-key (kbd "v") 'tws-view-entry-on-line)
      (local-set-key (kbd "q") 'tws-kill-current-buffer)
      (local-set-key (kbd "?") 'tws-help-buffer)
      (local-set-key (kbd "a") 'tws-add-time-to-entry-on-line)
      (local-set-key (kbd "x") 'tws-delete-entry-on-line)
      (local-set-key (kbd "/") 'tws-filter-goals)
      (switch-to-buffer *tws-buffer-name*)))
  (goto-char *tws-last-known-point*))

(defun tws-filter-goals (goal)
  (interactive "sFilter string: ")
  (setq *tws-show-goal-with-substring* goal)
  (tws-refresh-buffer))

(defun tws-add-time-to-entry-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (let* ((hours-to-add (* 3600 (read-number "Add Hours: ")))
             (stop-time
              (subtract-time (current-time)
                             (* 3600 (read-number "How Many Hours Ago Did You Stop? "))))
             (start-time (subtract-time stop-time hours-to-add)))

        (tws-add-time entry (cons stop-time start-time) ))
      (tws-save-db)
      (tws-refresh-buffer))))

(defun tws-delete-entry-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (tws-delete-entry (tws-entry-id entry) *tws-db*)
      (tws-refresh-buffer))))

(defun tws-rename-entry ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (setf (tws-entry-goal entry)
            (read-string "Rename Goal: "))
      (tws-save-db)
      (tws-refresh-buffer))))

(defun tws-recategorize ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (setf (tws-entry-category entry)
            (completing-read "Recategorize: " (tws-categories *tws-db*)))
      (tws-save-db)
      (tws-refresh-buffer))))


(defun tws-help-buffer ()
  (interactive)
  (with-output-to-temp-buffer "Time Well Spent: Help"
    (with-current-buffer "Time Well Spent: Help"
      (time-well-spent-mode)
      (princ "KEY") (terpri)
      (princ (format "%s -- visible on the goal you're working on right now" *tws-in-progress-icon*))
      (terpri)
      (princ (format "%s -- visible on completed goals" *tws-goal-reached-icon*))
      (terpri)
      (princ (format "%s -- visible on goals you are waiting on" *tws-waiting-str*))
      (terpri)
      (princ (format "%s -- visible on goals you are still thinking about" *tws-in-the-future-str*))
      (terpri)
      (princ (format "%s -- visible on goals you are definitely pursing" *tws-on-the-move-str*))
      (terpri)
      (newline)
      (princ "COMMANDS")
      (terpri)
      (princ "+        : Create a new goal")
      (terpri)
      (princ "<return> : Toggle tracking on a goal.")
      (terpri)
      (princ "S        : Stop tracking")
      (terpri)
      (princ "A        : Show all entries - i.e. remove filtering")
      (terpri)
      (princ "<SPC>    : Toggle filter by category of highlghted entry")
      (terpri)
      (princ "C        : Toggle filter by completed goals")
      (terpri)
      (princ "F        : Toggle filter by future goals")
      (terpri)
      (princ "D        : Default filter")
      (terpri)
      (princ "m        : Toggle an entry as being marked as completed")
      (terpri)
      (princ "f        : Mark an entry as a future goal")
      (terpri)
      (princ "d        : Mark an entry as a goal you're definitely doing")
      (terpri)
      (princ "w        : Mark an entry as being in wating, enter reason at prompt")
      (terpri)
      (princ "l        : Add a log message for the highlighted entry")
      (terpri)
      (princ "R        : Show a category report")
      (terpri)
      (princ "r        : Rename goal for the highlighted entry")
      (terpri)
      (princ "M        : Move the highlighted entry to a different category")
      (terpri)
      (princ "v        : View the detail of the highlighted entry")
      (terpri)
      (princ "a        : Add time to highlighted entry")
      (terpri)
      (princ "q        : close the Time Well Spent buffer (will continue tracking)")
      (terpri)
      (princ "a        : manually add time to a goal") (terpri)
      (princ "x        : remove this goal") (terpri)
      (princ "/        : add a search string to the display filter") (terpri)
      (princ "?        : show the help")
      (terpri)
      (read-only-mode)
      (local-set-key (kbd "q") 'tws-kill-current-buffer)
      )))

(defun tws-kill-current-buffer ()
  (interactive)
  (tws-save-db)
  (kill-buffer (current-buffer)))

(defvar *tws-view-entry-buffer-name* "Time Well Spent: Entry View")

(defun tws-view-entry-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (with-output-to-temp-buffer *tws-view-entry-buffer-name*
        (with-current-buffer *tws-view-entry-buffer-name*
          (time-well-spent-mode)
          (newline)
          (princ (string-trim (tws-entry-to-string entry)))
          (newline) (newline)
          (princ (format "Goal: %s" (tws-entry-goal entry)))
          (terpri)
          (princ (format "Category: %s" (tws-entry-category entry)))
          (terpri)
          (princ (format "Created: %s" (current-time-string (tws-entry-created entry))))
          (terpri)
          (princ (format "Last Touched: %s" (current-time-string (tws-entry-last-touched entry))))
          (newline)
          (when (tws-entry-log entry)
            (newline)
            (princ "NOTES")
            (terpri)
            (dolist (lentry (tws-entry-log entry))
              (princ (format "%s -- %s"
                             (current-time-string (cl-first lentry))
                             (cl-second lentry)))
              (terpri)))
          (when (tws-entry-time entry)
            (newline)
            (princ "WORK LOG")
            (terpri)
            (dolist (period (tws-entry-time entry))
              (if (numberp period)
                  (princ (tws-time-to-hh-mm period))
                (progn
                  (princ (tws-time-to-hh-mm (float-time (subtract-time (car period) (cdr period)))))
                  (princ "        ")
                  (princ (current-time-string (cdr period)))
                  (princ "---")
                  (princ (current-time-string (car period)))))
              (terpri)))
          (local-set-key (kbd "q") 'tws-kill-current-buffer)
          (read-only-mode)))
      (switch-to-buffer-other-window *tws-view-entry-buffer-name*))))

(defun tws-category-report (days-back)
  "Show a report of how much time has been spent on which
categories, in the past DAYS-BACK days"
  (interactive (list (read-number "Days Back To Start From (0 = today): " 0)))
  (when (zerop days-back)
    (setq days-back (/  (tws-hours-since-midnight) 24)))

  (let ((cats (tws-categories-and-times *tws-db* nil (tws-days-ago days-back))))
    (with-output-to-temp-buffer "Time Well Spent: Report"

      (princ "TIME WELL SPENT - CATEGORY REPORT")

      (if (<= 1.0 days-back)
          (progn (princ " [last ")
                 (princ  days-back)
                 (princ " days]"))
        (princ " [today]"))

      (terpri)
      (princ "----------------------------------------")
      (terpri)
      (let ((total 0))
        (dolist (key (hash-table-keys cats))
          (when (cl-plusp (gethash key cats 0))
            (princ (format "%25s -- %s"
                           key
                           (tws-time-to-hh-mm (gethash key cats 0))))
            (terpri)
            (cl-incf total (gethash key cats 0))))
        (princ "----------------------------------------")
        (terpri)
        (princ (format "%25s -- %s" "TOTAL" (tws-time-to-hh-mm total)))
        (terpri)))
    (switch-to-buffer-other-window "Time Well Spent: Report")))

(defun tws-log-to-entry-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (push (list (current-time) (read-string "Log Thought: "))
            (tws-entry-log entry))
      (tws-save-db))))

(defun tws-mark-waiting-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (tws-mark-incomplete entry)
      (tws-waiting-on entry)
      (push (list (current-time)
                  (concat "(Waiting) "
                          (read-string "Reason for Wait: ")))
            (tws-entry-log entry))
      (tws-save-db)
      (tws-refresh-buffer))))

(defun tws-mark-on-the-move-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (tws-mark-incomplete entry)
      (tws-make-on-the-move entry)
      (tws-refresh-buffer))))

(defun tws-put-into-future-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (tws-make-in-the-future entry)
      (tws-mark-incomplete entry)
      (tws-refresh-buffer))))

(defun tws-toggle-mark-complete-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      (setf (tws-entry-completed entry)
            (not (tws-entry-completed entry)))
      (if (tws-entry-completed entry)
          (tws-make-on-the-move entry))
      (tws-refresh-buffer))))

(defun tws-line-number-to-index (n)
  "Translates a line number from the TWS Buffer to an index into
the *tws-displayed-entries* list."
  (1- n))

(defun tws-entry-on-line ()
  (let ((n (tws-line-number-to-index (line-number-at-pos (point)))))
    (when (< -1 n (length *tws-displayed-entries*))
      (elt *tws-displayed-entries* n))))

(defun tws-show-goals-with-category-on-line ()
  (interactive)
  (let ((entry (tws-entry-on-line)))
    (when entry
      ;; if already showing a category, then clear it, otherwise show a category
      (setq *tws-show-category* (if *tws-show-category* nil (tws-entry-category entry)))
      (tws-refresh-buffer))))

(defun tws-create-entry (goal category estimated)
  (interactive
   (list 
    (read-string "Goal: ")
    (completing-read "Category: " (tws-categories *tws-db*))
    (read-number "Estimated Hours: ")))
  (let ((entry (make-tws-entry :goal goal
                               :category category
                               :estimate estimated
                               :created (current-time))))
    (tws-add-entry entry)
    (tws-refresh-buffer)))

(defun tws-stop ()
  (interactive)
  (tws-stop-idle-timer)
  (tws-stop-working *tws-db*)
  (tws-save-db))

(defun tws-toggle-on-goal-at-line ()
  (interactive)
  (let ((entry-on-line (tws-entry-on-line)))
    (when entry-on-line
      (if (tws-currently-working-on-p *tws-db* (tws-entry-id entry-on-line))
          (tws-stop)
        (progn
          (tws-stop)
          (tws-work-on *tws-db* entry-on-line)
          (tws-start-idle-timer)))
      (tws-refresh-buffer))))


(defun time-well-spent ()
  (interactive)
  (unless *tws-db*
    (tws-load-db))
  (tws-refresh-buffer))

