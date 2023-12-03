;;; ham-qso-logger.el --- Log QSOs in ADIF format  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Daniel Gonzalez Plaza

;; Author: Daniel Gonzalez Plaza <kk6gqj@danielgplaza.com>
;; Version: 1.0.0
;; Keywords: radio, adif, files, qso, logger, log
;; URL: https://TODO_GITHUB

;;; Commentary:

;; This pacakge provides an interactive logger for ham radio contacts.  It writes in ADIF file.
;; TODO better

(require 'widget)
(require 'wid-edit)

;;TODO:
;; - Follow naming best practices: https://github.com/bbatsov/emacs-lisp-style-guide#naming
;;  - E.g. Naming of private functions
;; - Add My Station custom vars
;; 
;; - Comment code
;; - Think of better name

;;Wanted features:
;; - Edit existing QSO
;; - Show and search for existing QSOs
;; - Personalize fields

;;; Customization

(defcustom qso-logfile-path "~/qso-log.adif"
  "Path to the QSO logfile."
  :type 'string
  :group 'qso-logger)

;;; Code:
(eval-when-compile
  (require 'wid-edit))

(cl-defstruct (adif-item (:constructor adif-item--create))
  widget field)

(defun adif-widget-create (text size)
"Create a single editable field widget with the given TEXT and SIZE."
  (widget-create 'editable-field
		 :size size
		 :format (concat text ": %v ")))

(defun adif-item-create (text size field)
"Use `adif-widget-create` to create a widget with given parameters.
Then encapsulate it in an adif-item struct.
   
TEXT is the label for the field.
SIZE is the width of the field in characters.
FIELD is the associated attribute in the logfile for the field."
  (adif-item--create :widget
		     (widget-create 'editable-field
				    :size size
				    :format (concat text ": %v "))
		     :field field))

(defun write-qso (w-list logfile-path)
  "Write QSO information to file.
Collect data from the form fields in the W-LIST list of widgets,
form the QSO string, and append it to the log file in LOGFILE-PATH.
After writing, refresh the QSO entry form."
  (let ((value "\n"))
    (dolist (elt w-list value)
      (setq value (concat value "<" (adif-item-field elt) ":" (number-to-string (length (widget-value (adif-item-widget elt)))) ">" (widget-value (adif-item-widget elt)))))
    (setq value (concat value "<EOR>"))
    (write-region value nil logfile-path 'append))
  ;; Reload QSO widget
  (setup-qso-widget logfile-path))

(defun main-widget ()
  "Create the main user interface widget.  UNUSED."
  (interactive)
  (switch-to-buffer "*QSO logger*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert "What do you want to do? \n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (setup-qso-widget qso-logfile-path))
                 "Add QSO")
  (use-local-map widget-keymap)
  (widget-setup)
  (widget-forward 1))

(defun setup-qso-widget (logfile-path)
  "Set up the QSO entry form, clearing all fields.
This prepares the interface for a new QSO entry which is written to LOGFILE-PATH"
  (interactive)
  (switch-to-buffer "*Add QSO*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((adif-item-list '()))
    ;;  (make-local-variable 'adif-item-list)
    (push (adif-item-create "Callsign" 10 "CALL") adif-item-list)
    (push (adif-item-create "Freq" 8 "FREQ") adif-item-list)
    (widget-insert "\n")
    (push (adif-item-create "Start time" 6 "TIME_ON") adif-item-list)
    (push (adif-item-create "Stop Time" 6 "TIME_OFF") adif-item-list)
    (push (adif-item-create "Mode" 10 "MODE") adif-item-list)
    (widget-insert "\n")
    (push (adif-item-create "QTH" 20 "QTH") adif-item-list)
    (push (adif-item-create "State" 2 "STATE") adif-item-list)
    (push (adif-item-create "Country" 20 "COUNTRY") adif-item-list)
    (widget-insert "\n-----\n")
    (push (adif-item-create "Notes" 100 "NOTES") adif-item-list)
    (widget-insert "\n")
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
                             (write-qso adif-item-list logfile-path))
		   "Records QSO")
    )
  (use-local-map widget-keymap)
  (widget-setup)
  (widget-forward 1))

(setup-qso-widget qso-logfile-path)
